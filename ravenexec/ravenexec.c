/*
 * This file is covered by the Internet Software Consortium (ISC) License
 * Reference: /License.txt
 *
 * The first argument is the open file descriptor of the log
 * The remaining arguments are passed to the spawn
 * We expect a minimum of 3 arguments
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
   pid_t result;

   if (kill(dead_pid_walking, SIGKILL) == 0)
   {
      do {
         result = waitpid(dead_pid_walking, &status, 0);
      } while (result < 0 && errno == EINTR);
      return result == dead_pid_walking ? 0 : -1;
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
   pid_t  waitres;
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
   if (reap_rc == 0)
   {
      do {
         waitres = waitpid(-1, NULL, 0);
      } while (waitres > 0 || (waitres < 0 && errno == EINTR));

      if (waitres < 0 && errno != ECHILD)
         reap_rc = -1;
   }
   procctl(P_PID, reaper_pid, PROC_REAP_RELEASE, NULL);
   return (reap_rc);
#elif defined(__DragonFly__) && defined(PROC_REAP_ACQUIRE)
   int keep_going = 1;
   int reap_rc = 0;
   int status;
   pid_t waitres;
#  if defined(PROC_REAP_KILL) // New ability with DragonFly 6.5
   struct reaper_kill rk;

   memset(&rk, 0, sizeof(rk));
   rk.signal = SIGKILL;

   while (keep_going)
   {
      keep_going = 0;
      if (procctl(P_PID, reaper_pid, PROC_REAP_KILL, &rk) == 0)
      {
         if (rk.killed > 0)
         {
            keep_going = 1;
            do {
               waitres = wait3(&status, 0, NULL);
            } while (waitres > 0 || (waitres < 0 && errno == EINTR));
         }
      } else {
         reap_rc = -1;
      }
   }
#  else // BEGIN ORIGINAL DRAGONFLY PROC_REAP
   union reaper_info info;

   while (keep_going)
   {
      keep_going = 0;
      if (procctl(P_PID, reaper_pid, PROC_REAP_STATUS, &info) == 0)
      {
         if (info.status.pid_head > 0)
         {
            if (reap_process (info.status.pid_head) == 0) {
               keep_going = 1;
            } else {
               reap_rc = -1;
            }
         }
      } else {
         reap_rc = -1;
      }
   }
#  endif // END ORIGINAL DRAGONFLY PROC_REAP
   do {
      waitres = wait3(&status, 0, NULL);
   } while (waitres > 0 || (waitres < 0 && errno == EINTR));
   procctl(P_PID, reaper_pid, PROC_REAP_RELEASE, NULL);
   return (reap_rc);
#elif defined(__linux__)
   /*
    * Scan /proc repeatedly until waitpid confirms that this
    * supervisor has no children left.  Retry cleanup failures.
    */
   for (;;)
   {
      DIR *dir;
      struct dirent *ent;
      char *endptr;
      char buffer[512];
      char *stat_data = NULL;
      size_t capacity = 0;
      int reaped_child = 0;
      pid_t wait_result;

      do {
         wait_result = waitpid(-1, NULL, WNOHANG);
      } while (wait_result > 0 ||
               (wait_result < 0 && errno == EINTR));

      if (wait_result < 0)
      {
         if (errno == ECHILD)
            return (0);
         sleep(1);
         continue;
      }

      dir = opendir("/proc");
      if (dir == NULL)
      {
         sleep(1);
         continue;
      }

      for (;;)
      {
         long lpid;
         long parent_id;
         char state;
         char *comm_end;
         FILE *fp;
         ssize_t size;
         int read_error;

         ent = readdir(dir);
         if (ent == NULL)
            break;

         errno = 0;
         lpid = strtol(ent->d_name, &endptr, 10);
         if (errno != 0 || endptr == ent->d_name ||
             *endptr != '\0' || lpid <= 0 ||
             (long)(pid_t)lpid != lpid)
         {
            continue;
         }

         snprintf(buffer, sizeof(buffer), "/proc/%ld/stat", lpid);
         fp = fopen(buffer, "r");
         if (fp == NULL)
            continue;

         /* Read through EOF, including any newlines in comm. */
         errno = 0;
         size = getdelim(&stat_data, &capacity, '\0', fp);
         read_error = 0;
         if (size <= 0 || ferror(fp))
            read_error = errno ? errno : EIO;
         fclose(fp);
         if (read_error != 0)
            continue;

         /* comm may contain spaces and ')'; later fields contain no ')'. */
         comm_end = strrchr(stat_data, ')');
         if (comm_end == NULL ||
             sscanf(comm_end + 1, " %c %ld", &state, &parent_id) != 2 ||
             parent_id < 0)
         {
            continue;
         }

         if (parent_id == (long)reaper_pid)
         {
            if (reap_process((pid_t)lpid) == 0)
               reaped_child = 1;
         }
      }

      free(stat_data);
      closedir(dir);

      if (!reaped_child)
         sleep(1);
   }
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
 * ------------
 * --  main  --
 * ------------
 * argument argc    : number of arguments to pass to program
 * argument argv    : vector of arguments to pass to program
 * argv[1]  fd      : file descriptor of open log file
 * argv[2]  prog    : full path to program to execute
 * argv[3+] args    : program arguments
 *
 * Exit codes as observed by the caller:
 *                0 : command succeeded and cleanup reported no failure
 *                1 : command returned 1, exec failed, wait failed,
 *                    signal setup/wait failed, or cleanup failed
 *                    after command success
 *              255 : negative file descriptor (return -1)
 *              254 : file descriptor at or below standard error (return -2)
 *              253 : fork failed (return -3)
 *              252 : failed to set up reaper (return -4)
 *              251 : not enough arguments (return -5)
 *              249 : setsid failed (_exit(-7))
 * Otherwise preserves the command's exit code, or uses
 * 128 + signal number when the command was killed by a signal.
 * These codes can overlap with command exit codes.
 * SIGUSR1 sent to this supervisor requests command termination.
 */
int
main (int argc, char *argv[])
{
   int fd;
   char *prog;
   pid_t fork1_pid;
   pid_t reaper_pid;
   pid_t wait_result;
   int grand_status = 0;
   int signo;
   sigset_t waitset;
   sigset_t original_mask;
   struct sigaction action;

   if (argc < 4)
   {
      return (-5);
   }
   fd = atoi(argv[1]);
   prog = argv[2];
   if (fd < 0)
   {
      return (-1);
   }
   if (fd <= STDERR_FILENO)
   {
      return (-2);
   }

   /* Keep cancellation pending until the child PID is available. */
   sigemptyset(&waitset);
   sigaddset(&waitset, SIGUSR1);
   sigaddset(&waitset, SIGCHLD);
   if (sigprocmask(SIG_BLOCK, &waitset, &original_mask) < 0)
      return (1);

   memset(&action, 0, sizeof(action));
   action.sa_handler = SIG_DFL;
   sigemptyset(&action.sa_mask);
   if (sigaction(SIGUSR1, &action, NULL) < 0 ||
       sigaction(SIGCHLD, &action, NULL) < 0)
   {
      return (1);
   }

   /* The supervisor must retain reaper status across command execution. */
   reaper_pid = getpid();
#ifdef PROC_REAP_ACQUIRE
   if (procctl(P_PID, reaper_pid, PROC_REAP_ACQUIRE, NULL) < 0)
      return (-4);
#endif
#ifdef PR_SET_CHILD_SUBREAPER
   if (prctl(PR_SET_CHILD_SUBREAPER, 1) < 0)
      return (-4);
#endif

   fork1_pid = fork();
   if (fork1_pid < 0)
      return (-3);

   if (fork1_pid == 0)
   {
      if (setsid() < 0)
         _exit (-7);
      signal(SIGINT, SIG_IGN);
      if (sigprocmask(SIG_SETMASK, &original_mask, NULL) < 0)
         _exit (1);

      dup2 (fd, STDOUT_FILENO);
      dup2 (fd, STDERR_FILENO);

      execv (prog, (argv + 2));

      /* execv doesn't normally return, so command failed to execute */
      diagnostic_message(fd, prog, argc, argv);
      _exit (1);
   }

  /***********************
   *  Parent of fork #1  *
   ***********************/
   /* Wait for command exit or cancellation without an asynchronous handler. */
   for (;;)
   {
      wait_result = waitpid(fork1_pid, &grand_status, WNOHANG);
      if (wait_result == fork1_pid)
         break;
      if (wait_result < 0)
      {
         if (errno == EINTR)
            continue;
         grand_status = W_EXITCODE(1, 0);
         break;
      }

      do {
         signo = sigwaitinfo(&waitset, NULL);
      } while (signo < 0 && errno == EINTR);
      if (signo < 0)
      {
         grand_status = W_EXITCODE(1, 0);
         break;
      }
      if (signo == SIGUSR1)
         kill(fork1_pid, SIGKILL);
   }

   if (kill_process_tree(reaper_pid) < 0 &&
       WIFEXITED(grand_status) &&
       WEXITSTATUS(grand_status) == 0)
   {
      grand_status = W_EXITCODE(1, 0);
   }

   if (WIFEXITED(grand_status))
      return (WEXITSTATUS(grand_status));
   if (WIFSIGNALED(grand_status))
      return (128 + WTERMSIG(grand_status));
   return (1);
}

#endif /* __WIN32 */
