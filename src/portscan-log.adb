--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Calendar.Arithmetic;
with Ada.Calendar.Formatting;
with Ada.Characters.Latin_1;
with Ada.Directories;
with File_Operations;
with Parameters;

package body PortScan.Log is

   package DIR renames Ada.Directories;
   package CAR renames Ada.Calendar.Arithmetic;
   package CFM renames Ada.Calendar.Formatting;
   package LAT renames Ada.Characters.Latin_1;
   package FOP renames File_Operations;
   package PM  renames Parameters;

   --------------------------------------------------------------------------------------------
   --  log_duration
   --------------------------------------------------------------------------------------------
   function log_duration (start, stop : CAL.Time) return String
   is
      raw       : HT.Text := HT.SUS ("Duration:");
      diff_days : CAR.Day_Count;
      diff_secs : Duration;
      leap_secs : CAR.Leap_Seconds_Count;
      use type CAR.Day_Count;
   begin
      CAR.Difference (Left    => stop,
                      Right   => start,
                      Days    => diff_days,
                      Seconds => diff_secs,
                      Leap_Seconds => leap_secs);
      if diff_days > 0 then
         if diff_days = 1 then
            HT.SU.Append (raw, " 1 day and " &
                            CFM.Image (Elapsed_Time => diff_secs));
         else
            HT.SU.Append (raw, diff_days'Img & " days and " &
                            CFM.Image (Elapsed_Time => diff_secs));
         end if;
      else
         HT.SU.Append (raw, " " & CFM.Image (Elapsed_Time => diff_secs));
      end if;
      return HT.USS (raw);
   end log_duration;


   --------------------------------------------------------------------------------------------
   --  log_name
   --------------------------------------------------------------------------------------------
   function log_name (sid : port_id) return String
   is
      portvar : constant String := get_port_variant (all_ports (sid));
      origin  : constant String := HT.part_1 (portvar, ":");
      variant : constant String := HT.part_2 (portvar, ":");
   begin
      return HT.USS (PM.configuration.dir_logs) & "/" & origin & "___" & variant & ".log";
   end log_name;


   --------------------------------------------------------------------------------------------
   --  log_section
   --------------------------------------------------------------------------------------------
   function log_section (title : String) return String
   is
      hyphens : constant String := (1 .. 50 => '-');
   begin
      return LAT.LF & hyphens & LAT.LF & "--  " & title & LAT.LF & hyphens;
   end log_section;


   ------------------------------------------------------------------po--------------------------
   --  timestamp
   --------------------------------------------------------------------------------------------
   function timestamp (hack : CAL.Time; www_format : Boolean := False) return String
   is
      function MON   (num : CAL.Month_Number) return String;
      function WKDAY (day : CFM.Day_Name) return String;

      function MON (num : CAL.Month_Number) return String is
      begin
         case num is
            when 1 => return "JAN";
            when 2 => return "FEB";
            when 3 => return "MAR";
            when 4 => return "APR";
            when 5 => return "MAY";
            when 6 => return "JUN";
            when 7 => return "JUL";
            when 8 => return "AUG";
            when 9 => return "SEP";
            when 10 => return "OCT";
            when 11 => return "NOV";
            when 12 => return "DEC";
         end case;
      end MON;
      function WKDAY (day : CFM.Day_Name) return String is
      begin
         case day is
            when CFM.Monday    => return "Monday";
            when CFM.Tuesday   => return "Tuesday";
            when CFM.Wednesday => return "Wednesday";
            when CFM.Thursday  => return "Thursday";
            when CFM.Friday    => return "Friday";
            when CFM.Saturday  => return "Saturday";
            when CFM.Sunday    => return "Sunday";
         end case;
      end WKDAY;
   begin
      if www_format then
         return CAL.Day (hack)'Img & " " & MON (CAL.Month (hack)) & CAL.Year (hack)'Img & ", " &
           CFM.Image (hack)(11 .. 19) & " UTC";
      end if;

      return WKDAY (CFM.Day_Of_Week (hack)) & "," & CAL.Day (hack)'Img & " " &
        MON (CAL.Month (hack)) & CAL.Year (hack)'Img & " at" &
        CFM.Image (hack)(11 .. 19) & " UTC";
   end timestamp;


   --------------------------------------------------------------------------------------------
   --  elapsed_now
   --------------------------------------------------------------------------------------------
   function elapsed_now return String is
   begin
      return elapsed_HH_MM_SS (start => start_time, stop => CAL.Clock);
   end elapsed_now;


   --------------------------------------------------------------------------------------------
   --  elapsed_build
   --------------------------------------------------------------------------------------------
   function elapsed_build (head_time, tail_time : CAL.Time) return String is
   begin
      return elapsed_HH_MM_SS (start => head_time, stop => tail_time);
   end elapsed_build;


   --------------------------------------------------------------------------------------------
   --  elapsed_HH_MM_SS
   --------------------------------------------------------------------------------------------
   function elapsed_HH_MM_SS (start, stop : CAL.Time) return String
   is
      diff_days : CAR.Day_Count;
      diff_secs : Duration;
      leap_secs : CAR.Leap_Seconds_Count;
      secs_per_hour : constant Integer := 3600;
      total_hours   : Integer;
      total_minutes : Integer;
      work_hours    : Integer;
      work_seconds  : Integer;
      use type CAR.Day_Count;
   begin
      CAR.Difference (Left    => stop,
                      Right   => start,
                      Days    => diff_days,
                      Seconds => diff_secs,
                      Leap_Seconds => leap_secs);
      --  Seems the ACF image is shit, so let's roll our own.  If more than
      --  100 hours, change format to "HHH:MM.M"

      work_seconds := Integer (diff_secs);
      total_hours  := work_seconds / secs_per_hour;
      total_hours  := total_hours + Integer (diff_days) * 24;

      if total_hours < 24 then
         if work_seconds < 0 then
            return "--:--:--";
         else
            work_seconds := work_seconds - (total_hours * secs_per_hour);
            total_minutes := work_seconds / 60;
            work_seconds := work_seconds - (total_minutes * 60);
            return
              HT.zeropad (total_hours, 2) & LAT.Colon &
              HT.zeropad (total_minutes, 2) & LAT.Colon &
              HT.zeropad (work_seconds, 2);
         end if;
      elsif total_hours < 100 then
         if work_seconds < 0 then
            return HT.zeropad (total_hours, 2) & ":00:00";
         else
            work_hours := work_seconds / secs_per_hour;
            work_seconds := work_seconds - (work_hours * secs_per_hour);
            total_minutes := work_seconds / 60;
            work_seconds := work_seconds - (total_minutes * 60);
            return
              HT.zeropad (total_hours, 2) & LAT.Colon &
              HT.zeropad (total_minutes, 2) & LAT.Colon &
              HT.zeropad (work_seconds, 2);
         end if;
      else
         if work_seconds < 0 then
            return HT.zeropad (total_hours, 3) & ":00.0";
         else
            work_hours := work_seconds / secs_per_hour;
            work_seconds := work_seconds - (work_hours * secs_per_hour);
            total_minutes := work_seconds / 60;
            work_seconds := (work_seconds - (total_minutes * 60)) * 10 / 60;
            return
              HT.zeropad (total_hours, 3) & LAT.Colon &
              HT.zeropad (total_minutes, 2) & '.' &
              HT.int2str (work_seconds);
         end if;
      end if;
   end elapsed_HH_MM_SS;


   --------------------------------------------------------------------------------------------
   --  split_collection
   --------------------------------------------------------------------------------------------
   function split_collection (line : String; title : String) return String
   is
      --  Support spaces in two ways
      --  1) quoted,  e.g. TYPING="The Quick Brown Fox"
      --  2) Escaped, e.g. TYPING=The\ Quick\ Brown\ Fox

      linelen : constant Natural := line'Length;
      waiting : Boolean := True;
      escaped : Boolean := False;
      quoted  : Boolean := False;
      keepit  : Boolean;
      counter : Natural := 0;
      meatlen : Natural := 0;
      onechar : Character;
      meatstr : String (1 .. linelen);
   begin
      loop
         exit when counter > linelen;
         keepit  := True;
         onechar := line (line'First + counter);

         if onechar = LAT.Reverse_Solidus then
            --  A) if inside quotes, it's literal
            --  B) if it's first RS, don't keep but mark escaped
            --  C) If it's second RS, it's literal, remove escaped
            --  D) RS can never start a new NV pair
            if not quoted then
               if not escaped then
                  keepit := False;
               end if;
               escaped := not escaped;
            end if;
         elsif escaped then
            --  E) by definition, next character after an escape is literal
            --     We know it's not inside quotes. Keep this (could be a space)
            waiting := False;
            escaped := not escaped;
         elsif onechar = LAT.Space then
            if waiting then
               keepit := False;
            else
               if not quoted then
                  --  name-pair ended, reset
                  waiting := True;
                  quoted  := False;
                  onechar := LAT.LF;
               end if;
            end if;
         else
            waiting := False;
            if onechar = LAT.Quotation then
               quoted := not quoted;
            end if;
         end if;
         if keepit then
            meatlen := meatlen + 1;
            meatstr (meatlen) := onechar;
         end if;
         counter := counter + 1;
      end loop;
      return log_section (title) & LAT.LF & meatstr (1 .. meatlen) & LAT.LF & LAT.LF;
   end split_collection;


   --------------------------------------------------------------------------------------------
   --  dump_port_variables
   --------------------------------------------------------------------------------------------
   procedure dump_port_variables (log_handle : TIO.File_Type; contents : String)
   is
      type result_range is range 0 .. 6;
      markers : HT.Line_Markers;
      linenum : result_range := result_range'First;
   begin
      HT.initialize_markers (contents, markers);
      loop
         exit when not HT.next_line_present (contents, markers);
         linenum := linenum + 1;
         declare
            line : constant String := HT.extract_line (contents, markers);
         begin
            case linenum is
               when 0 => null;  --  impossible
               when 1 => TIO.Put_Line (log_handle, split_collection (line, "CONFIGURE_ENV"));
               when 2 => TIO.Put_Line (log_handle, split_collection (line, "CONFIGURE_ARGS"));
               when 3 => TIO.Put_Line (log_handle, split_collection (line, "MAKE_ENV"));
               when 4 => TIO.Put_Line (log_handle, split_collection (line, "MAKE_ARGS"));
               when 5 => TIO.Put_Line (log_handle, split_collection (line, "PLIST_SUB"));
               when 6 => TIO.Put_Line (log_handle, split_collection (line, "SUB_LIST"));
            end case;
         end;
      end loop;
   end dump_port_variables;


   --------------------------------------------------------------------------------------------
   --  log_phase_end
   --------------------------------------------------------------------------------------------
   procedure log_phase_end (log_handle : TIO.File_Type) is
   begin
      TIO.Put_Line (log_handle, "" & LAT.LF);
   end log_phase_end;


   --------------------------------------------------------------------------------------------
   --  log_phase_begin
   --------------------------------------------------------------------------------------------
   procedure log_phase_begin (log_handle : TIO.File_Type; phase : String)
   is
      hyphens : constant String := (1 .. 80 => '-');
      middle  : constant String := "--  Phase: " & phase;
   begin
      TIO.Put_Line (log_handle, LAT.LF & hyphens & LAT.LF & middle & LAT.LF & hyphens);
   end log_phase_begin;


   --------------------------------------------------------------------------------------------
   --  finalize_log
   --------------------------------------------------------------------------------------------
   procedure finalize_log
     (log_handle : in out TIO.File_Type;
      head_time  : CAL.Time;
      tail_time  : out CAL.Time) is
   begin
      TIO.Put_Line (log_handle, log_section ("Termination"));
      tail_time := CAL.Clock;
      TIO.Put_Line (log_handle,
                    "Finished: " & timestamp (tail_time));
      TIO.Put_Line (log_handle, log_duration (start => head_time, stop  => tail_time));
      TIO.Close (log_handle);
   end finalize_log;


   --------------------------------------------------------------------------------------------
   --  initialize_log
   --------------------------------------------------------------------------------------------
   function initialize_log
     (log_handle : in out TIO.File_Type;
      head_time  : out CAL.Time;
      seq_id     : port_id;
      slave_root : String;
      UNAME      : String;
      BENV       : String;
      COPTS      : String;
      PTVAR      : String) return Boolean
   is
      H_ENV : constant String := "Environment";
      H_OPT : constant String := "Options";
      CFG1  : constant String := "/etc/make.conf";
      CFG2  : constant String := "/etc/mk.conf";
   begin
      head_time := CAL.Clock;
      declare
         log_path : constant String := log_name (seq_id);
      begin
         if DIR.Exists (log_path) then
            DIR.Delete_File (log_path);
         end if;
         TIO.Create (File => log_handle,
                     Mode => TIO.Out_File,
                     Name => log_path);
      exception
         when error : others =>
            raise scan_log_error
              with "failed to create log " & log_path;
      end;

      TIO.Put_Line (log_handle, "=> Building " & get_port_variant (all_ports (seq_id)));
      TIO.Put_Line (log_handle, "Started : " & timestamp (head_time));
      TIO.Put      (log_handle, "Platform: " & UNAME);
      if BENV = discerr then
         TIO.Put_Line (log_handle, LAT.LF & "Environment definition failed, " &
                         "aborting entire build");
         return False;
      end if;
      TIO.Put_Line (log_handle, LAT.LF & log_section (H_ENV));
      TIO.Put      (log_handle, BENV);
      TIO.Put_Line (log_handle, "" & LAT.LF);
      TIO.Put_Line (log_handle, log_section (H_OPT));
      TIO.Put      (log_handle, COPTS);
      TIO.Put_Line (log_handle, "" & LAT.LF);

      dump_port_variables (log_handle, PTVAR);

      TIO.Put_Line (log_handle, log_section (CFG1));
      TIO.Put      (log_handle, FOP.get_file_contents (slave_root & CFG1));
      TIO.Put_Line (log_handle, "" & LAT.LF);
      return True;

   end initialize_log;
end PortScan.Log;
