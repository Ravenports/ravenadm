--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with Interfaces.C.Strings;
private with Interfaces.C.Pointers;

package Ravexec is

   type File_Descriptor is new Integer;
   not_connected : constant File_Descriptor := -1;

   --  Closes file without checking if the attempt was successful or not
   procedure close_file_blind (fd : File_Descriptor);

   --  Creates new builder log
   function start_new_log (filename : String) return File_Descriptor;

   --  Write to builder log's file descriptor
   procedure scribe_to_log (fd : File_Descriptor; line : String);

private

   package IC renames Interfaces.C;

   subtype CNatural is IC.int range 0 .. IC.int'Last;
   type CSVector is array (CNatural range <>) of aliased IC.Strings.chars_ptr;

   package Argv_Pointer is new IC.Pointers (
      Index              => CNatural,
      Element            => IC.Strings.chars_ptr,
      Element_Array      => CSVector,
      Default_Terminator => IC.Strings.Null_Ptr);

   subtype Chars_Ptr_Ptr is Argv_Pointer.Pointer;

   function C_Close (fd : IC.int) return IC.int;
   pragma Import (C, C_Close, "close");

   procedure C_Direct_Scribe (fd : IC.int; msg : IC.Strings.chars_ptr);
   pragma Import (C, C_Direct_Scribe, "direct_scribe");

   function C_Start_Log (path : IC.Strings.chars_ptr) return IC.int;
   pragma Import (C, C_Start_Log, "start_new_log");

   function C_Phase_Execution (builder : IC.int; fd : IC.int; prog : IC.Strings.chars_ptr;
                               argv : Chars_Ptr_Ptr) return IC.int;
   pragma Import (C, C_Phase_Execution, "phase_execution");


end Ravexec;
