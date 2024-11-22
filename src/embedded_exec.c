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


/*
 * -------------------
 * --  handler_xxx  --
 * -------------------
 */
static void handler_000 (int signo __unused) {kill_process_tree (ravenexec[0]);}
static void handler_001 (int signo __unused) {kill_process_tree (ravenexec[1]);}
static void handler_002 (int signo __unused) {kill_process_tree (ravenexec[2]);}
static void handler_003 (int signo __unused) {kill_process_tree (ravenexec[3]);}
static void handler_004 (int signo __unused) {kill_process_tree (ravenexec[4]);}
static void handler_005 (int signo __unused) {kill_process_tree (ravenexec[5]);}
static void handler_006 (int signo __unused) {kill_process_tree (ravenexec[6]);}
static void handler_007 (int signo __unused) {kill_process_tree (ravenexec[7]);}
static void handler_008 (int signo __unused) {kill_process_tree (ravenexec[8]);}
static void handler_009 (int signo __unused) {kill_process_tree (ravenexec[9]);}

static void handler_010 (int signo __unused) {kill_process_tree (ravenexec[10]);}
static void handler_011 (int signo __unused) {kill_process_tree (ravenexec[11]);}
static void handler_012 (int signo __unused) {kill_process_tree (ravenexec[12]);}
static void handler_013 (int signo __unused) {kill_process_tree (ravenexec[13]);}
static void handler_014 (int signo __unused) {kill_process_tree (ravenexec[14]);}
static void handler_015 (int signo __unused) {kill_process_tree (ravenexec[15]);}
static void handler_016 (int signo __unused) {kill_process_tree (ravenexec[16]);}
static void handler_017 (int signo __unused) {kill_process_tree (ravenexec[17]);}
static void handler_018 (int signo __unused) {kill_process_tree (ravenexec[18]);}
static void handler_019 (int signo __unused) {kill_process_tree (ravenexec[19]);}

static void handler_020 (int signo __unused) {kill_process_tree (ravenexec[20]);}
static void handler_021 (int signo __unused) {kill_process_tree (ravenexec[21]);}
static void handler_022 (int signo __unused) {kill_process_tree (ravenexec[22]);}
static void handler_023 (int signo __unused) {kill_process_tree (ravenexec[23]);}
static void handler_024 (int signo __unused) {kill_process_tree (ravenexec[24]);}
static void handler_025 (int signo __unused) {kill_process_tree (ravenexec[25]);}
static void handler_026 (int signo __unused) {kill_process_tree (ravenexec[26]);}
static void handler_027 (int signo __unused) {kill_process_tree (ravenexec[27]);}
static void handler_028 (int signo __unused) {kill_process_tree (ravenexec[28]);}
static void handler_029 (int signo __unused) {kill_process_tree (ravenexec[29]);}

static void handler_030 (int signo __unused) {kill_process_tree (ravenexec[30]);}
static void handler_031 (int signo __unused) {kill_process_tree (ravenexec[31]);}
static void handler_032 (int signo __unused) {kill_process_tree (ravenexec[32]);}
static void handler_033 (int signo __unused) {kill_process_tree (ravenexec[33]);}
static void handler_034 (int signo __unused) {kill_process_tree (ravenexec[34]);}
static void handler_035 (int signo __unused) {kill_process_tree (ravenexec[35]);}
static void handler_036 (int signo __unused) {kill_process_tree (ravenexec[36]);}
static void handler_037 (int signo __unused) {kill_process_tree (ravenexec[37]);}
static void handler_038 (int signo __unused) {kill_process_tree (ravenexec[38]);}
static void handler_039 (int signo __unused) {kill_process_tree (ravenexec[39]);}

static void handler_040 (int signo __unused) {kill_process_tree (ravenexec[40]);}
static void handler_041 (int signo __unused) {kill_process_tree (ravenexec[41]);}
static void handler_042 (int signo __unused) {kill_process_tree (ravenexec[42]);}
static void handler_043 (int signo __unused) {kill_process_tree (ravenexec[43]);}
static void handler_044 (int signo __unused) {kill_process_tree (ravenexec[44]);}
static void handler_045 (int signo __unused) {kill_process_tree (ravenexec[45]);}
static void handler_046 (int signo __unused) {kill_process_tree (ravenexec[46]);}
static void handler_047 (int signo __unused) {kill_process_tree (ravenexec[47]);}
static void handler_048 (int signo __unused) {kill_process_tree (ravenexec[48]);}
static void handler_049 (int signo __unused) {kill_process_tree (ravenexec[49]);}

static void handler_050 (int signo __unused) {kill_process_tree (ravenexec[50]);}
static void handler_051 (int signo __unused) {kill_process_tree (ravenexec[51]);}
static void handler_052 (int signo __unused) {kill_process_tree (ravenexec[52]);}
static void handler_053 (int signo __unused) {kill_process_tree (ravenexec[53]);}
static void handler_054 (int signo __unused) {kill_process_tree (ravenexec[54]);}
static void handler_055 (int signo __unused) {kill_process_tree (ravenexec[55]);}
static void handler_056 (int signo __unused) {kill_process_tree (ravenexec[56]);}
static void handler_057 (int signo __unused) {kill_process_tree (ravenexec[57]);}
static void handler_058 (int signo __unused) {kill_process_tree (ravenexec[58]);}
static void handler_059 (int signo __unused) {kill_process_tree (ravenexec[59]);}

static void handler_060 (int signo __unused) {kill_process_tree (ravenexec[60]);}
static void handler_061 (int signo __unused) {kill_process_tree (ravenexec[61]);}
static void handler_062 (int signo __unused) {kill_process_tree (ravenexec[62]);}
static void handler_063 (int signo __unused) {kill_process_tree (ravenexec[63]);}
static void handler_064 (int signo __unused) {kill_process_tree (ravenexec[64]);}
static void handler_065 (int signo __unused) {kill_process_tree (ravenexec[65]);}
static void handler_066 (int signo __unused) {kill_process_tree (ravenexec[66]);}
static void handler_067 (int signo __unused) {kill_process_tree (ravenexec[67]);}
static void handler_068 (int signo __unused) {kill_process_tree (ravenexec[68]);}
static void handler_069 (int signo __unused) {kill_process_tree (ravenexec[69]);}

static void handler_070 (int signo __unused) {kill_process_tree (ravenexec[70]);}
static void handler_071 (int signo __unused) {kill_process_tree (ravenexec[71]);}
static void handler_072 (int signo __unused) {kill_process_tree (ravenexec[72]);}
static void handler_073 (int signo __unused) {kill_process_tree (ravenexec[73]);}
static void handler_074 (int signo __unused) {kill_process_tree (ravenexec[74]);}
static void handler_075 (int signo __unused) {kill_process_tree (ravenexec[75]);}
static void handler_076 (int signo __unused) {kill_process_tree (ravenexec[76]);}
static void handler_077 (int signo __unused) {kill_process_tree (ravenexec[77]);}
static void handler_078 (int signo __unused) {kill_process_tree (ravenexec[78]);}
static void handler_079 (int signo __unused) {kill_process_tree (ravenexec[79]);}

static void handler_080 (int signo __unused) {kill_process_tree (ravenexec[80]);}
static void handler_081 (int signo __unused) {kill_process_tree (ravenexec[81]);}
static void handler_082 (int signo __unused) {kill_process_tree (ravenexec[82]);}
static void handler_083 (int signo __unused) {kill_process_tree (ravenexec[83]);}
static void handler_084 (int signo __unused) {kill_process_tree (ravenexec[84]);}
static void handler_085 (int signo __unused) {kill_process_tree (ravenexec[85]);}
static void handler_086 (int signo __unused) {kill_process_tree (ravenexec[86]);}
static void handler_087 (int signo __unused) {kill_process_tree (ravenexec[87]);}
static void handler_088 (int signo __unused) {kill_process_tree (ravenexec[88]);}
static void handler_089 (int signo __unused) {kill_process_tree (ravenexec[89]);}

static void handler_090 (int signo __unused) {kill_process_tree (ravenexec[90]);}
static void handler_091 (int signo __unused) {kill_process_tree (ravenexec[91]);}
static void handler_092 (int signo __unused) {kill_process_tree (ravenexec[92]);}
static void handler_093 (int signo __unused) {kill_process_tree (ravenexec[93]);}
static void handler_094 (int signo __unused) {kill_process_tree (ravenexec[94]);}
static void handler_095 (int signo __unused) {kill_process_tree (ravenexec[95]);}
static void handler_096 (int signo __unused) {kill_process_tree (ravenexec[96]);}
static void handler_097 (int signo __unused) {kill_process_tree (ravenexec[97]);}
static void handler_098 (int signo __unused) {kill_process_tree (ravenexec[98]);}
static void handler_099 (int signo __unused) {kill_process_tree (ravenexec[99]);}

static void handler_100 (int signo __unused) {kill_process_tree (ravenexec[100]);}
static void handler_101 (int signo __unused) {kill_process_tree (ravenexec[101]);}
static void handler_102 (int signo __unused) {kill_process_tree (ravenexec[102]);}
static void handler_103 (int signo __unused) {kill_process_tree (ravenexec[103]);}
static void handler_104 (int signo __unused) {kill_process_tree (ravenexec[104]);}
static void handler_105 (int signo __unused) {kill_process_tree (ravenexec[105]);}
static void handler_106 (int signo __unused) {kill_process_tree (ravenexec[106]);}
static void handler_107 (int signo __unused) {kill_process_tree (ravenexec[107]);}
static void handler_108 (int signo __unused) {kill_process_tree (ravenexec[108]);}
static void handler_109 (int signo __unused) {kill_process_tree (ravenexec[109]);}

static void handler_110 (int signo __unused) {kill_process_tree (ravenexec[110]);}
static void handler_111 (int signo __unused) {kill_process_tree (ravenexec[111]);}
static void handler_112 (int signo __unused) {kill_process_tree (ravenexec[112]);}
static void handler_113 (int signo __unused) {kill_process_tree (ravenexec[113]);}
static void handler_114 (int signo __unused) {kill_process_tree (ravenexec[114]);}
static void handler_115 (int signo __unused) {kill_process_tree (ravenexec[115]);}
static void handler_116 (int signo __unused) {kill_process_tree (ravenexec[116]);}
static void handler_117 (int signo __unused) {kill_process_tree (ravenexec[117]);}
static void handler_118 (int signo __unused) {kill_process_tree (ravenexec[118]);}
static void handler_119 (int signo __unused) {kill_process_tree (ravenexec[119]);}

static void handler_120 (int signo __unused) {kill_process_tree (ravenexec[120]);}
static void handler_121 (int signo __unused) {kill_process_tree (ravenexec[121]);}
static void handler_122 (int signo __unused) {kill_process_tree (ravenexec[122]);}
static void handler_123 (int signo __unused) {kill_process_tree (ravenexec[123]);}
static void handler_124 (int signo __unused) {kill_process_tree (ravenexec[124]);}
static void handler_125 (int signo __unused) {kill_process_tree (ravenexec[125]);}
static void handler_126 (int signo __unused) {kill_process_tree (ravenexec[126]);}
static void handler_127 (int signo __unused) {kill_process_tree (ravenexec[127]);}



/*
 * -------------------
 * --  set_handler  --
 * -------------------
 */
static
void set_handler (int zbuilder)
{
   switch (zbuilder)
   {
      case   0: signal(SIGUSR1, handler_000); break;
      case   1: signal(SIGUSR1, handler_001); break;
      case   2: signal(SIGUSR1, handler_002); break;
      case   3: signal(SIGUSR1, handler_003); break;
      case   4: signal(SIGUSR1, handler_004); break;
      case   5: signal(SIGUSR1, handler_005); break;
      case   6: signal(SIGUSR1, handler_006); break;
      case   7: signal(SIGUSR1, handler_007); break;
      case   8: signal(SIGUSR1, handler_008); break;
      case   9: signal(SIGUSR1, handler_009); break;

      case  10: signal(SIGUSR1, handler_010); break;
      case  11: signal(SIGUSR1, handler_011); break;
      case  12: signal(SIGUSR1, handler_012); break;
      case  13: signal(SIGUSR1, handler_013); break;
      case  14: signal(SIGUSR1, handler_014); break;
      case  15: signal(SIGUSR1, handler_015); break;
      case  16: signal(SIGUSR1, handler_016); break;
      case  17: signal(SIGUSR1, handler_017); break;
      case  18: signal(SIGUSR1, handler_018); break;
      case  19: signal(SIGUSR1, handler_019); break;

      case  20: signal(SIGUSR1, handler_020); break;
      case  21: signal(SIGUSR1, handler_021); break;
      case  22: signal(SIGUSR1, handler_022); break;
      case  23: signal(SIGUSR1, handler_023); break;
      case  24: signal(SIGUSR1, handler_024); break;
      case  25: signal(SIGUSR1, handler_025); break;
      case  26: signal(SIGUSR1, handler_026); break;
      case  27: signal(SIGUSR1, handler_027); break;
      case  28: signal(SIGUSR1, handler_028); break;
      case  29: signal(SIGUSR1, handler_029); break;

      case  30: signal(SIGUSR1, handler_030); break;
      case  31: signal(SIGUSR1, handler_031); break;
      case  32: signal(SIGUSR1, handler_032); break;
      case  33: signal(SIGUSR1, handler_033); break;
      case  34: signal(SIGUSR1, handler_034); break;
      case  35: signal(SIGUSR1, handler_035); break;
      case  36: signal(SIGUSR1, handler_036); break;
      case  37: signal(SIGUSR1, handler_037); break;
      case  38: signal(SIGUSR1, handler_038); break;
      case  39: signal(SIGUSR1, handler_039); break;

      case  40: signal(SIGUSR1, handler_040); break;
      case  41: signal(SIGUSR1, handler_041); break;
      case  42: signal(SIGUSR1, handler_042); break;
      case  43: signal(SIGUSR1, handler_043); break;
      case  44: signal(SIGUSR1, handler_044); break;
      case  45: signal(SIGUSR1, handler_045); break;
      case  46: signal(SIGUSR1, handler_046); break;
      case  47: signal(SIGUSR1, handler_047); break;
      case  48: signal(SIGUSR1, handler_048); break;
      case  49: signal(SIGUSR1, handler_049); break;

      case  50: signal(SIGUSR1, handler_050); break;
      case  51: signal(SIGUSR1, handler_051); break;
      case  52: signal(SIGUSR1, handler_052); break;
      case  53: signal(SIGUSR1, handler_053); break;
      case  54: signal(SIGUSR1, handler_054); break;
      case  55: signal(SIGUSR1, handler_055); break;
      case  56: signal(SIGUSR1, handler_056); break;
      case  57: signal(SIGUSR1, handler_057); break;
      case  58: signal(SIGUSR1, handler_058); break;
      case  59: signal(SIGUSR1, handler_059); break;

      case  60: signal(SIGUSR1, handler_060); break;
      case  61: signal(SIGUSR1, handler_061); break;
      case  62: signal(SIGUSR1, handler_062); break;
      case  63: signal(SIGUSR1, handler_063); break;
      case  64: signal(SIGUSR1, handler_064); break;
      case  65: signal(SIGUSR1, handler_065); break;
      case  66: signal(SIGUSR1, handler_066); break;
      case  67: signal(SIGUSR1, handler_067); break;
      case  68: signal(SIGUSR1, handler_068); break;
      case  69: signal(SIGUSR1, handler_069); break;

      case  70: signal(SIGUSR1, handler_070); break;
      case  71: signal(SIGUSR1, handler_071); break;
      case  72: signal(SIGUSR1, handler_072); break;
      case  73: signal(SIGUSR1, handler_073); break;
      case  74: signal(SIGUSR1, handler_074); break;
      case  75: signal(SIGUSR1, handler_075); break;
      case  76: signal(SIGUSR1, handler_076); break;
      case  77: signal(SIGUSR1, handler_077); break;
      case  78: signal(SIGUSR1, handler_078); break;
      case  79: signal(SIGUSR1, handler_079); break;

      case  80: signal(SIGUSR1, handler_080); break;
      case  81: signal(SIGUSR1, handler_081); break;
      case  82: signal(SIGUSR1, handler_082); break;
      case  83: signal(SIGUSR1, handler_083); break;
      case  84: signal(SIGUSR1, handler_084); break;
      case  85: signal(SIGUSR1, handler_085); break;
      case  86: signal(SIGUSR1, handler_086); break;
      case  87: signal(SIGUSR1, handler_087); break;
      case  88: signal(SIGUSR1, handler_088); break;
      case  89: signal(SIGUSR1, handler_089); break;

      case  90: signal(SIGUSR1, handler_090); break;
      case  91: signal(SIGUSR1, handler_091); break;
      case  92: signal(SIGUSR1, handler_092); break;
      case  93: signal(SIGUSR1, handler_093); break;
      case  94: signal(SIGUSR1, handler_094); break;
      case  95: signal(SIGUSR1, handler_095); break;
      case  96: signal(SIGUSR1, handler_096); break;
      case  97: signal(SIGUSR1, handler_097); break;
      case  98: signal(SIGUSR1, handler_098); break;
      case  99: signal(SIGUSR1, handler_099); break;

      case 100: signal(SIGUSR1, handler_100); break;
      case 101: signal(SIGUSR1, handler_101); break;
      case 102: signal(SIGUSR1, handler_102); break;
      case 103: signal(SIGUSR1, handler_103); break;
      case 104: signal(SIGUSR1, handler_104); break;
      case 105: signal(SIGUSR1, handler_105); break;
      case 106: signal(SIGUSR1, handler_106); break;
      case 107: signal(SIGUSR1, handler_107); break;
      case 108: signal(SIGUSR1, handler_108); break;
      case 109: signal(SIGUSR1, handler_109); break;

      case 110: signal(SIGUSR1, handler_110); break;
      case 111: signal(SIGUSR1, handler_111); break;
      case 112: signal(SIGUSR1, handler_112); break;
      case 113: signal(SIGUSR1, handler_113); break;
      case 114: signal(SIGUSR1, handler_114); break;
      case 115: signal(SIGUSR1, handler_115); break;
      case 116: signal(SIGUSR1, handler_116); break;
      case 117: signal(SIGUSR1, handler_117); break;
      case 118: signal(SIGUSR1, handler_118); break;
      case 119: signal(SIGUSR1, handler_119); break;

      case 120: signal(SIGUSR1, handler_120); break;
      case 121: signal(SIGUSR1, handler_121); break;
      case 122: signal(SIGUSR1, handler_122); break;
      case 123: signal(SIGUSR1, handler_123); break;
      case 124: signal(SIGUSR1, handler_124); break;
      case 125: signal(SIGUSR1, handler_125); break;
      case 126: signal(SIGUSR1, handler_126); break;
      case 127: signal(SIGUSR1, handler_127); break;
   }
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
 */
int
phase_execution (int builder, int fd, char *prog, int argc, char *argv[])
{
   pid_t root_pid;
   pid_t inner_pid;
   pid_t inner_wpid;
   int zbuilder;
   int inner_status = 0;

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
   root_pid = fork();
   if (root_pid < 0)
   {
      return (-3);
   }
   else if (root_pid == 0)
   {
      /* child */
      ravenexec[zbuilder] = getpid();

#ifdef PROC_REAP_ACQUIRE
      /*
       * Set current process as the reaper for itself and future children
       * Interface identical for FreeBSD and DragonFly
       */
      if (procctl(P_PID, ravenexec[zbuilder], PROC_REAP_ACQUIRE, NULL) < 0)
      {
         return (-4);
      }
#endif
#ifdef PR_SET_CHILD_SUBREAPER
      /*
       * Linux: Set current process as reaper for future children
       */
      if (prctl(PR_SET_CHILD_SUBREAPER, 1) < 0)
      {
         return (-4);
      }
#endif

      inner_pid = fork();
      if (inner_pid < 0)
      {
         return (-6);
      }
      else if (inner_pid == 0)
      {
         /* inner child */

         dup2 (fd, STDOUT_FILENO);
         dup2 (fd, STDERR_FILENO);

         execv (prog, argv);

         /* execv doesn't normally return, so command failed to execute */
         diagnostic_message(fd, prog, argc, argv);
         _exit (1);
      }

      /* inner parent */
      set_handler(zbuilder);
      signal(SIGINT, SIG_IGN);

      inner_wpid = waitpid (inner_pid, &inner_status, 0);
      if (inner_wpid < 0)
      {
         inner_status = W_EXITCODE(1, 0);
      };

      kill_process_tree (ravenexec[zbuilder]);
      _exit (WEXITSTATUS (inner_status));
   }

   /* Parent */
   return root_pid;
}

#endif /* __WIN32 */
