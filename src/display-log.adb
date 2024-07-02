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
      logpath : constant String := HT.USS (PM.configuration.dir_logs) & "/logs/07_display_events";
   begin
      TIO.Create (File => dlog_handle, Mode => TIO.Out_File, Name => logpath);
      TIO.Put_Line (dlog_handle, "Terminal events log started : " & timestamp);
      TIO.Put_Line (dlog_handle, "==========================================");
      TIO.Flush (dlog_handle);
      TIO.Set_Output (dlog_handle);
      TIO.Set_Error (dlog_handle);
   end start_logging;


   --------------------------------------------------------------------------------------------
   --  stop_logging
   --------------------------------------------------------------------------------------------
   procedure stop_logging is
   begin
      scribe ("Terminal closed.");
      TIO.Set_Output (TIO.Standard_Output);
      TIO.Set_Error (TIO.Standard_Error);
      TIO.Close (dlog_handle);
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



end Display.Log;
