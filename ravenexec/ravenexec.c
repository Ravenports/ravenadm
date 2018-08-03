/*
 * This file is covered by the Internet Software Consortium (ISC) License
 * Reference: ../License.txt
 *
 * The first argument is the path to the log
 * The remaining arguments are passed to the spawn
 * We expect a minimum of 3 arguments
 */

#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
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
#include <stdio.h>
#endif
#ifndef __unused
#define __unused __attribute__((__unused__))
#endif
#ifndef W_EXITCODE
#define W_EXITCODE(ret, sig)	((ret) << 8 | (sig))
#endif

static pid_t ravenexec;

/*
 * reap_process kills per given pid and waits for it to return
 * returns 0 on success, -1 on failures
 */
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

int
kill_process_tree (pid_t reaper_pid)
{
   if (reaper_pid <= 0) {
        return (-1);
   }
#if defined(__FreeBSD__) && defined(PROC_REAP_ACQUIRE)
   struct procctl_reaper_status info;
   struct procctl_reaper_kill killemall;

   if (procctl(P_PID, reaper_pid, PROC_REAP_STATUS, &info) == 0)
   {
      if (info.rs_children != 0)
      {
         killemall.rk_sig = SIGKILL;
         killemall.rk_flags = 0;
         if (procctl(P_PID, reaper_pid, PROC_REAP_KILL, &killemall) != 0)
         {
             return (-1);
         }
      }
   }
   return (0); 
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

void
handler (int signo __unused)
{
   kill_process_tree (ravenexec);
   exit(1);
}

int
main (int argc, char *argv[])
{
   ravenexec = getpid();

   if (argc < 4)
   {
      return (-2);
   }

   int fd = open (argv[1], O_RDWR | O_APPEND | O_CREAT, 0644);
   if (fd < 0)
   {
      return (-3);
   }
   if (fd <= STDERR_FILENO)
   {
      return (-4);
   }

   dup2 (fd, STDOUT_FILENO);
   dup2 (fd, STDERR_FILENO);
   close (fd);
#ifndef __linux__
   closefrom (3);
#endif

#ifdef PROC_REAP_ACQUIRE
   /*
    * Set current process as the reaper for itself and future children
    * Interface identical for FreeBSD and DragonFly
    */
   if (procctl(P_PID, ravenexec, PROC_REAP_ACQUIRE, NULL) < 0)
   {
      return (-5);
   }
#endif
#ifdef PR_SET_CHILD_SUBREAPER
   /*
    * Linux: Set current process as reaper for future children
    */
   if (prctl(PR_SET_CHILD_SUBREAPER, 1) < 0)
   {
      return (-5);
   }
#endif

   pid_t pid;
   int status = 0;

   pid = fork();
   if (pid < 0)
   {
      return (-6);
   }
   else if (pid == 0)
   {
      /* child */
      execv (argv[2], (argv + 2));

      /* command failed to execute */
#ifndef __sun__
      dprintf (STDERR_FILENO, "Command execution failed: %s\n", argv[2]);
      dprintf (STDERR_FILENO, "               arguments:");
      for (int x = 2; x < argc; x++)
         dprintf (STDERR_FILENO, " %s", argv[x]);
      dprintf (STDERR_FILENO, "\n");
#endif
      _exit (1);
   }

   /* Parent */
   signal(SIGUSR1, handler);
   signal(SIGINT,  SIG_IGN);

   for (;;) {
      pid_t cpid;

      cpid = wait3(&status, 0, NULL);
      if (cpid == pid)
         break;
      if (cpid < 0 && errno != EINTR)
      {
         status = W_EXITCODE(1, 0);
         break;
      }
   }
   kill_process_tree (ravenexec);
   return (WEXITSTATUS (status));
}
