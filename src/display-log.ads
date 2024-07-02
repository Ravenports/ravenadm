--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

private
with Ada.Text_IO;

package Display.Log is

   --  This supports logging exceptions during ncurses execution.  It is always on

   procedure start_logging;
   procedure stop_logging;
   procedure scribe (line : String);

private

   package TIO renames Ada.Text_IO;

   dlog_handle        : TIO.File_Type;

   function timestamp return String;

end Display.Log;
