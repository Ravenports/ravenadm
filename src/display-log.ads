--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

private
with Ada.Text_IO;

package Display.Log is

   --  This supports logging curses operations (for the purpose of troubleshooting)
   --  It is activated by setting LOG_RCURSES in the environment

   procedure start_logging;
   procedure stop_logging;
   procedure scribe (line : String);
   procedure log_builder_update (rec : builder_rec);

private

   package TIO renames Ada.Text_IO;

   dlog_handle        : TIO.File_Type;
   display_logging_on : Boolean := False;

   function timestamp return String;

end Display.Log;
