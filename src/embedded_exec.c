/*
 * This file is covered by the Internet Software Consortium (ISC) License
 * Reference: /License.txt
 *
 * This replaces the spawning of the external ravenexec.
 */

#ifndef _WIN32

#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/wait.h>
#if defined __DragonFly__ || defined __FreeBSD__
#include <sys/procctl.h>
#endif
#if defined __linux__
#include <sys/prctl.h>
#include <sys/types.h>
#include <dirent.h>
#endif
#ifndef __unused
#define __unused __attribute__((__unused__))
#endif
#ifndef W_EXITCODE
#define W_EXITCODE(ret, sig)	((ret) << 8 | (sig))
#endif

volatile pid_t ravenexec[128] = {0};


/*
 * --------------------
 * --  reap_process  --
 * --------------------
 * reap_process kills per given pid and waits for it to return
 * returns 0 on success, -1 on failures
 */
static
int reap_process (pid_t dead_pid_walking)
{
   int status = 0;

   if (kill(dead_pid_walking, SIGKILL) == 0)
   {
      waitpid (dead_pid_walking, &status, 0);
      return (0);
   }
   return (-1);
}


/*
 * -------------------------
 * --  kill_process_tree  --
 * -------------------------
 */
static int
kill_process_tree (pid_t reaper_pid)
{
   if (reaper_pid <= 0) {
        return (-1);
   }
#if defined(__FreeBSD__) && defined(PROC_REAP_ACQUIRE)
   int    reap_rc;
   struct procctl_reaper_status info;
   struct procctl_reaper_kill killemall;

   reap_rc = procctl(P_PID, reaper_pid, PROC_REAP_STATUS, &info);
   if (reap_rc == 0)
   {
      if (info.rs_children != 0)
      {
         killemall.rk_sig = SIGKILL;
         killemall.rk_flags = 0;
         reap_rc = procctl(P_PID, reaper_pid, PROC_REAP_KILL, &killemall);
      }
   }
   procctl(P_PID, reaper_pid, PROC_REAP_RELEASE, NULL);
   return (reap_rc);
#elif defined(__DragonFly__) && defined(PROC_REAP_ACQUIRE)
   union reaper_info info;
   int keep_going = 1;

   while (keep_going)
   {
      keep_going = 0;
      if (procctl(P_PID, reaper_pid, PROC_REAP_STATUS, &info) == 0)
      {
         if (info.status.pid_head > 0)
         {
            if (reap_process (info.status.pid_head) == 0)
            {
               keep_going = 1;
            }
            else
            {
               return (-1);
            }
         }
      }
   }
   return (0);
#elif defined(__linux__)
   /*
    * Iterate /proc pseudo-filesystem repeatedly until no
    * process ID has the ravenexec process as its parent.
    */
   for (;;)
   {
      DIR *dir;
      struct dirent* ent;
      char *endptr;
      char buffer[512];
      int found_ppid = 0;

      dir = opendir("/proc");
      if (dir == NULL)
      {
         return (-1);
      }

      while ((ent = readdir(dir)) != NULL)
      {
         long lpid;
         char *s_ppid;
         pid_t parent_id;
         /*
          * if endptr is not a null character, the directory is not
          * numeric so ignore it
          */
         lpid = strtol(ent->d_name, &endptr, 10);
         if (*endptr != '\0')
         {
            continue;
         }
         snprintf(buffer, sizeof(buffer), "/proc/%ld/stat", lpid);
         FILE *fp = fopen(buffer, "r");
         if (fp)
         {
            size_t size = fread(buffer, sizeof(char), sizeof(buffer), fp);
            fclose (fp);
            if (size > 0)
            {
               strtok(buffer, " ");             // (1) pid   %d
               strtok(NULL, " ");               // (2) comm  %s
               strtok(NULL, " ");               // (3) state %c
               s_ppid = strtok(NULL, " ");      // (4) ppid  %d

               parent_id = (pid_t) atoi(s_ppid);
               if (parent_id == reaper_pid)
               {
                  found_ppid = 1;
                  if (reap_process ((pid_t) lpid) == -1)
                  {
                     closedir (dir);
                     return (-1);
                  }
               }
            }
         }
      }
      closedir(dir);

      if (!found_ppid) {
         break;
      }
   }
   return (0);
#else
   /* process reaping is not supported on this system */
   return (0);
#endif  /* __FreeBSD__ || __DragonFly__ || __linux__ */
}


/*
 * --------------------------
 * --  diagnostic_message  --
 * --------------------------
 */
static void
diagnostic_message (int fd, char *prog, int argc, char *argv[])
{
   write (fd, "Command execution failed: ", 26);
   write (fd, prog, strlen (prog));
   write (fd, "\n               arguments:", 26);
   for (int x = 0; x < argc; x++) {
      write (fd, " ", 1);
      write (fd, argv[x], strlen (argv[x]));
   }
   write (fd, "\n", 1);
}


/*
 * ---------------------
 * --  direct_scribe  --
 * ---------------------
 */
void
direct_scribe (int fd, const char *msg)
{
   write (fd, msg, strlen(msg));
}


/*
 * --------------------------
 * --  start_new_log_file  --
 * --------------------------
 */
int
start_new_log_file (const char *path)
{
   int flags = O_CREAT | O_WRONLY | O_TRUNC;

   return (open (path, flags, 0644));
}


/* ----------------------------
 * --  kill_stalled_process  --
 * ----------------------------
 */
int
kill_stalled_process (int builder)
{
   int zbuilder;

   if ((builder < 1) || builder > 128)
   {
      return (-1);
   }

   zbuilder = builder - 1;
   if (ravenexec[zbuilder] > 0) {
      return (kill(ravenexec[zbuilder], SIGKILL));
   }
   return (-1);
}


/*
 * -----------------------
 * --  phase_execution  --
 * -----------------------
 * argument builder : builder number (1 .. 128)
 * argument fd      : file description to open log file
 * argument prog    : full path to program to execute
 * argument argc    : number of arguments to pass to program
 * argument argv    : vector of arguments to pass to program
 *
 * return code  pid : success   (positive number)
 *               -1 : negative file descriptor
 *               -2 : file descriptor below standard error
 *               -3 : first fork failed
 *               -4 : failed to set up reaper
 *               -5 : illegal builder number
 *               -6 : second fork failed
 *               -7 : setsid failed
 */
int
phase_execution (int builder, int fd, char *prog, int argc, char *argv[])
{
   pid_t fork1_pid;
   pid_t fork2_pid;
   pid_t child_pid;
   pid_t wait_result;
   int zbuilder;
   int grand_status = 0;

   if ((builder < 1) || builder > 128)
   {
      return (-5);
   }
   if (fd < 0)
   {
      return (-1);
   }
   if (fd <= STDERR_FILENO)
   {
      return (-2);
   }

   zbuilder = builder - 1;
   fork1_pid = fork();
   if (fork1_pid < 0)
   {
      return (-3);
   }



   /***************************
    *  First fork successful  *
    ***************************/
   if (fork1_pid == 0)
   {
      /* child of fork #1 */
      child_pid = getpid();
      if (setsid() < 0)
      {
         _exit (-7);
      }
      signal(SIGINT, SIG_IGN);

#ifdef PROC_REAP_ACQUIRE
      /*
       * Set current process as the reaper for itself and future children
       * Interface identical for FreeBSD and DragonFly
       */
      if (procctl(P_PID, child_pid, PROC_REAP_ACQUIRE, NULL) < 0)
      {
         _exit (-4);
      }
#endif
#ifdef PR_SET_CHILD_SUBREAPER
      /*
       * Linux: Set current process as reaper for future children
       */
      if (prctl(PR_SET_CHILD_SUBREAPER, 1) < 0)
      {
         _exit (-4);
      }
#endif

      fork2_pid = fork();
      if (fork2_pid < 0)
      {
         _exit (-6);
      }

      /****************************
       *  Second fork successful  *
       ****************************/
      if (fork2_pid == 0)
      {
         /* grandchild */

         dup2 (fd, STDOUT_FILENO);
         dup2 (fd, STDERR_FILENO);

         execv (prog, argv);

         /* execv doesn't normally return, so command failed to execute */
         diagnostic_message(fd, prog, argc, argv);
         _exit (1);
      }

      /* parent of grandchild (a.k.a child) */
      ravenexec[zbuilder] = fork2_pid;
      wait_result = waitpid (fork2_pid, &grand_status, 0);
      if (wait_result < 0)
      {
         grand_status = W_EXITCODE(1, 0);
      };

      kill_process_tree (child_pid);
      ravenexec[zbuilder] = 0;

      _exit (WEXITSTATUS (grand_status));
   }

   /* Parent of fork #1 */
   return fork1_pid;
}

#endif /* __WIN32 */
