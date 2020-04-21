--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Calendar.Formatting;
with HelperText;
with Parameters;
with Unix;

package body Display.Log is

   package CFM renames Ada.Calendar.Formatting;
   package HT  renames HelperText;
   package PM  renames Parameters;


   --------------------------------------------------------------------------------------------
   --  start_logging
   --------------------------------------------------------------------------------------------
   procedure start_logging is
   begin
      --  Remaining routines will be ignored if display_logging_on remains false;
      if Unix.env_variable_defined ("LOG_RCURSES") then
         display_logging_on := True;
         declare
            logpath : constant String := HT.USS (PM.configuration.dir_logs) &
                                         "/logs/07_display_events.log";
         begin
            TIO.Create (File => dlog_handle,
                        Mode => TIO.Out_File,
                        Name => logpath);
            TIO.Put_Line (dlog_handle,
                          "Terminal events log started : " & timestamp);
            TIO.Put_Line (dlog_handle,
                          "==========================================");
            TIO.Flush (dlog_handle);
         end;
      end if;
   end start_logging;


   --------------------------------------------------------------------------------------------
   --  stop_logging
   --------------------------------------------------------------------------------------------
   procedure stop_logging is
   begin
      if display_logging_on then
         scribe ("Terminal closed.");
         TIO.Close (dlog_handle);
      end if;
   end stop_logging;


   --------------------------------------------------------------------------------------------
   --  timestamp
   --------------------------------------------------------------------------------------------
   function timestamp return String
   is
      stamp : Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      return CFM.Image (stamp) (11 .. 19) & " UTC";
   end timestamp;


   --------------------------------------------------------------------------------------------
   --  scribe
   --------------------------------------------------------------------------------------------
   procedure scribe (line : String) is
   begin
      if display_logging_on then
         TIO.Put_Line (dlog_handle, timestamp & " : " & line);
         TIO.Flush (dlog_handle);
      end if;
   end scribe;


   --------------------------------------------------------------------------------------------
   --  log_builder_update
   --------------------------------------------------------------------------------------------
   procedure log_builder_update (rec : builder_rec)
   is
      slave : constant String := "Slave " & rec.slavid;
      line  : constant String :=
        slave
        & " | " & rec.origin
        & " | " & rec.phase
        & " | " & rec.Elapsed
        & " | " & rec.LLines
        & " lines";
   begin
      if rec.shutdown then
         scribe (slave & " | SHUTDOWN");
      elsif rec.idle then
         scribe (slave & " | IDLE");
      else
         scribe (line);
      end if;
   end log_builder_update;


end Display.Log;
