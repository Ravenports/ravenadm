--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Interfaces.C;
with Interfaces.C_Streams;
with Interfaces.C.Strings;
with HelperText;

package Unix is

   package HT  renames HelperText;
   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;
   package CSM renames Interfaces.C_Streams;

   type permissions is mod 2 ** 16;
   type process_exit is (still_running, exited_normally, exited_with_error);

   type Int32 is private;
   subtype pid_t is Int32;

   type int64 is range -(2**63) .. +(2**63 - 1);

   --  check if process identified by pid has exited or keeps going
   function process_status (pid : pid_t) return process_exit;

   --  Allows other packages to call external commands (e.g. Pilot)
   --  Returns "True" on success
   function external_command (command : String) return Boolean;

   --  wrapper for nonblocking spawn
   --  function launch_process (command : String) return pid_t;

   --  Returns True if pid is less than zero
   function fork_failed (pid : pid_t) return Boolean;

   --  Returns True if "variable" is defined in the environment.  The
   --  value of variable is irrelevant
   function env_variable_defined (variable : String) return Boolean;

   --  Return value of "variable" defined in environment.  If it's not
   --  defined than an empty string is returned;
   function env_variable_value (variable : String) return String;

   --  Execute popen and return stdout+stderr combined
   --  Also the result status is returned as an "out" variable
   function piped_command
     (command : String;
      status  : out Integer) return HT.Text;

   --  Run external command that is expected to have no output to standard
   --  out, but catch stdout anyway.  Don't return any output, but do return
   --  True of the command returns status of zero.
   function piped_mute_command
     (command  : String;
      abnormal : out HT.Text) return Boolean;

   --  When the cone of silence is deployed, the terminal does not echo
   --  and Control-Q/S keystrokes are not captured (and vice-versa)
   procedure cone_of_silence (deploy : Boolean);

   --  Returns True if a TTY device is detected
   function screen_attached return Boolean;

   --  Equivalent of libc's realpath() function
   --  It resolves symlinks and relative directories to get the true path
   function true_path (provided_path : String) return String;

   --  Ignore SIGTTOU signal (background process trying to write to terminal)
   --  If that happens, synth will suspend and ultimately fail.
   procedure ignore_background_tty;

   --  Attempts to create a hardlink and returns True on success
   function create_hardlink (actual_file : String; destination : String) return Boolean;

   --  Attempts to create a symlink and returns True on success
   function create_symlink (actual_file : String; link_to_create : String) return Boolean;

   --  Returns true if the provided path exists as a file or a symbolic link that dereferences
   --  to an existing file.
   function target_exists (provided_path : String) return Boolean;

   --  Returns true if given file is set at 400 permissions
   function file_secure (file_path : String) return Boolean;

   --  time(3) from libc (Use passthrough version to support NetBSD)
   function unix_time (tloc : access int64) return int64;
   pragma Import (C, unix_time, "rf_time");

private

   type uInt8 is mod 2 ** 16;
   type Int32 is range -(2 ** 31) .. +(2 ** 31) - 1;

   popen_re : constant IC.char_array := IC.To_C ("re");
   popen_r  : constant IC.char_array := IC.To_C ("r");

   function popen (Command, Mode : IC.char_array) return CSM.FILEs;
   pragma Import (C, popen);

   function pclose (FileStream : CSM.FILEs) return CSM.int;
   pragma Import (C, pclose);

   function ferror (FileStream : CSM.FILEs) return CSM.int;
   pragma Import (C, ferror);

   function realpath (pathname, resolved_path : IC.char_array)
                      return ICS.chars_ptr;
   pragma Import (C, realpath, "realpath");

   function nohang_waitpid (pid : pid_t) return uInt8;
   pragma Import (C, nohang_waitpid, "__nohang_waitpid");

   function silent_control return uInt8;
   pragma Import (C, silent_control, "__silent_control");

   function chatty_control return uInt8;
   pragma Import (C, chatty_control, "__chatty_control");

   function ignore_tty_write return uInt8;
   pragma Import (C, ignore_tty_write, "__ignore_background_tty_writes");

   function ignore_tty_read return uInt8;
   pragma Import (C, ignore_tty_read, "__ignore_background_tty_reads");

   function link (path1, path2 : IC.char_array) return IC.int;
   pragma Import (C, link);

   function symlink (path1, path2 : IC.char_array) return IC.int;
   pragma Import (C, symlink);

   function unix_access (path : IC.char_array; mode : IC.int) return IC.int;
   pragma Import (C, unix_access, "access");

   function file_at_400 (path : IC.char_array) return IC.int;
   pragma Import (C, file_at_400);

   --  internal pipe close command
   function pipe_close (OpenFile : CSM.FILEs) return Integer;

   --  internal pipe read command
   function pipe_read (OpenFile : CSM.FILEs) return HT.Text;

   --  Internal file error check
   function good_stream (OpenFile : CSM.FILEs) return Boolean;

end Unix;
