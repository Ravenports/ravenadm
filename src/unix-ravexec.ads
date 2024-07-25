--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Definitions; use Definitions;
private with Interfaces.C.Pointers;

package Unix.Ravexec is

   type File_Descriptor is new Integer;
   not_connected : constant File_Descriptor := -1;

   --  Closes file without checking if the attempt was successful or not
   procedure close_file_blind (fd : File_Descriptor);

   --  Creates new builder log
   function start_new_log (filename : String) return File_Descriptor;

   --  Write to builder log's file descriptor
   procedure write   (fd : File_Descriptor; line : String);
   procedure writeln (fd : File_Descriptor; line : String);

   --  wrapper for spawning external process that logs everything to the given file descriptor
   function launch_separate_process
     (builder   : builders;
      log_fd    : File_Descriptor;
      program   : String;
      arguments : String) return pid_t;

   --  Write stack out to log
   procedure dump_stack (fd : File_Descriptor);

private

   MAX_ARGS : constant IC.int := 255;

   subtype CNatural is IC.int range 0 .. IC.int'Last;
   type CSVector is array (CNatural range <>) of aliased ICS.chars_ptr;

   package Argv_Pointer is new IC.Pointers (
      Index              => CNatural,
      Element            => ICS.chars_ptr,
      Element_Array      => CSVector,
      Default_Terminator => ICS.Null_Ptr);

   subtype Chars_Ptr_Ptr is Argv_Pointer.Pointer;

   function C_Close (fd : IC.int) return IC.int;
   pragma Import (C, C_Close, "close");

   procedure C_Direct_Scribe (fd : IC.int; msg : ICS.chars_ptr);
   pragma Import (C, C_Direct_Scribe, "direct_scribe");

   function C_Start_Log (path : ICS.chars_ptr) return IC.int;
   pragma Import (C, C_Start_Log, "start_new_log");

   function C_Phase_Execution (builder : IC.int; fd : IC.int; prog : ICS.chars_ptr;
                               argv : Chars_Ptr_Ptr) return IC.int;
   pragma Import (C, C_Phase_Execution, "phase_execution");

   --  Given a string, fill out the arguments array.  Must be freed separately later.
   procedure set_argument_vector
     (Arg_String : String;
      argvector  : in out CSVector;
      num_args   : out IC.int);

end Unix.Ravexec;
