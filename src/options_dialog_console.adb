--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Text_IO;

package body Options_Dialog_Console is

   package TIO renames Ada.Text_IO;

   --------------------------------------------------------------------------------------------
   --  check_ravenports_version
   --------------------------------------------------------------------------------------------
   function launch_dialog (specification : in out PSP.Portspecs) return Boolean
   is
      pragma Unreferenced (specification);
   begin
      TIO.Put_Line ("ravenadm has been built without curses support.");
      TIO.Put_Line ("The ability to change standard options is therefore disabled.");
      return False;
   end launch_dialog;

end Options_Dialog_Console;
