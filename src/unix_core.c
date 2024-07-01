/*
 * This file is covered by the Internet Software Consortium (ISC) License
 * Reference: ../License.txt
 *
 * __nohang_waitpid is linked directly into Synth but it's related to ravenexec
 * so let's keep the C files together.
 * return:
 *    0 when process is still running
 *    1 when process exited normally
 *    2 when process exited with error
 */

#ifndef _UNIX_CORE_
#define _UNIX_CORE_

#if defined __APPLE__ \
 || defined __DragonFly__ \
 || defined __FreeBSD__ \
 || defined __linux__ \
 || defined __NetBSD__ \
 || defined __OpenBSD__ \
 || defined __sun__

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <termios.h>
#include <unistd.h>
#include <signal.h>
#include <sys/fcntl.h>

#ifdef __sun__
#define u_int8_t	uint8_t
#endif

u_int8_t
__nohang_waitpid (pid_t process_pid)
{
  int status = 0;

  int pid = waitpid (process_pid, &status, WNOHANG);

  if (pid == 0)
    {
      return 0;
    }
  if (WIFEXITED (status) && (WEXITSTATUS (status) == 0))
    {
      return 1;
    }
  else
    {
      return 2;
    }
}

u_int8_t
__silent_control ()
{
  struct termios tp;

  /* Retrieve current terminal settings */
  if (tcgetattr(STDIN_FILENO, &tp) == -1)
    {
       return 1;
    }
  /* ECHO off, other bits unchanged */
  tp.c_lflag &= ~ECHO;

  /* Disable output flow control */
  tp.c_iflag &= ~IXON;

  /* update terminal settings */
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &tp) == -1)
    {
       return 2;
    }
  return 0;
}

u_int8_t
__chatty_control ()
{
  struct termios tp;

  /* Retrieve current terminal settings */
  if (tcgetattr(STDIN_FILENO, &tp) == -1)
    {
       return 1;
    }
  /* ECHO on, other bits unchanged */
  tp.c_lflag |= ECHO;

  /* Enable output flow control */
  tp.c_iflag |= IXON;

  /* update terminal settings */
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &tp) == -1)
    {
       return 2;
    }
  return 0;
}

/*
 * Watchdog: send signal to synthexec instance to shutdown now
 */
int __shut_it_down (pid_t dead_pid_walking)
{
   return (kill(dead_pid_walking, SIGUSR1));
}

/*
 * Ignore all background attempts to write to TTY
 * If this signal comes during an ncurses display, ravenadm will be suspended
 */
u_int8_t
__ignore_background_tty_writes ()
{
   /* return 1 on failure, 0 on success */
   if (signal(SIGTTOU, SIG_IGN) == SIG_ERR) { return 1; }
   return 0;
}

/*
 * Ignore all background attempts to read TTY
 */
u_int8_t
__ignore_background_tty_reads ()
{
   /* return 1 on failure, 0 on success */
   if (signal(SIGTTIN, SIG_IGN) == SIG_ERR) { return 1; }
   return 0;
}

/*
 * returns 1 if given file has permissions of 400 octal
 */
int
file_at_400 (const char * path) {
  struct stat sb;
  unsigned short perms;
  if (lstat(path, &sb) == 0) {
    perms = sb.st_mode & (S_IRWXU | S_IRWXG | S_IRWXO);
    if (perms == 256) {
      return 1;
    }
  }
  return 0;
}

#endif /* Supported opsys */
#endif /* _UNIX_CORE_ */
