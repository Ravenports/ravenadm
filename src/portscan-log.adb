--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Calendar.Arithmetic;
with Ada.Calendar.Formatting;
with Ada.Characters.Latin_1;
with Ada.Directories;
with File_Operations;
with Parameters;
with Utilities;

package body PortScan.Log is

   package DIR renames Ada.Directories;
   package CAR renames Ada.Calendar.Arithmetic;
   package CFM renames Ada.Calendar.Formatting;
   package LAT renames Ada.Characters.Latin_1;
   package FOP renames File_Operations;
   package PM  renames Parameters;
   package UTL renames Utilities;

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
   --  bulk_run_duration
   --------------------------------------------------------------------------------------------
   function bulk_run_duration return String is
   begin
      return log_duration (start_time, stop_time);
   end bulk_run_duration;


   --------------------------------------------------------------------------------------------
   --  log_name
   --------------------------------------------------------------------------------------------
   function log_name (sid : port_id) return String
   is
      portvar : constant String := get_port_variant (all_ports (sid));
      origin  : constant String := HT.part_1 (portvar, ":");
      variant : constant String := HT.part_2 (portvar, ":");
   begin
      return HT.USS (PM.configuration.dir_logs) & "/logs/" & origin & "___" & variant & ".log";
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
      function MON (T : CAL.Time) return String;
      function WKDAY (T : CAL.Time) return String;
      function daystring (T : CAL.Time) return String;

      function MON (T : CAL.Time) return String
      is
         num : CAL.Month_Number := CAL.Month (T);
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

      function WKDAY (T : CAL.Time) return String
      is
         day : CFM.Day_Name := CFM.Day_Of_Week (T);
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

      function daystring (T : CAL.Time) return String
      is
         daynum : Natural := Natural (CAL.Day (T));
      begin
         return HT.zeropad (daynum, 2);
      end daystring;

   begin
      if www_format then
         return daystring (hack) & " " & MON (hack) & CAL.Year (hack)'Img &
           "," & CFM.Image (hack)(11 .. 19) & " UTC";
      end if;

      return WKDAY (hack) & "," & daystring (hack) & " " & MON (hack) & CAL.Year (hack)'Img &
        " at" & CFM.Image (hack)(11 .. 19) & " UTC";
   end timestamp;


   --------------------------------------------------------------------------------------------
   --  scan_duration
   --------------------------------------------------------------------------------------------
   function scan_duration return String is
   begin
      return elapsed_HH_MM_SS (start => scan_start, stop => scan_stop);
   end scan_duration;


   --------------------------------------------------------------------------------------------
   --  elapsed_now
   --------------------------------------------------------------------------------------------
   function elapsed_now return String is
   begin
      return elapsed_HH_MM_SS (start => start_time, stop => CAL.Clock);
   end elapsed_now;


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
      --  Everything, including spaces, between quotes is preserved.
      --  Quotes preceded by backslashes within quotes are considered literals.
      --  Also supported is escaped spaces outside of quotes, e.g.
      --  TYPING=The\ Quick\ Brown\ Fox

      mask    : String := UTL.mask_quoted_string (line);
      linelen : constant Natural := line'Length;
      keepit  : Boolean;
      newline : Boolean := True;
      counter : Natural := 0;
      meatlen : Natural := 0;
      onechar : Character;
      meatstr : String (1 .. linelen);
   begin
      loop
         exit when counter = linelen;
         keepit  := True;
         if mask (mask'First + counter) = LAT.Reverse_Solidus then
            keepit := False;
         elsif mask (mask'First + counter) = LAT.Space then
            if newline then
               keepit := False;
            elsif mask (mask'First + counter - 1) = LAT.Reverse_Solidus then
               onechar := LAT.Space;
            else
               onechar := LAT.LF;
            end if;
         else
            onechar := line (mask'First + counter);
         end if;

         if keepit then
            meatlen := meatlen + 1;
            meatstr (meatlen) := onechar;
            newline := (onechar = LAT.LF);
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
         exit when linenum = result_range'Last;
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
      PTVAR      : String;
      block_dog  : Boolean) return Boolean
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
         FOP.mkdirp_from_filename (log_path);
         TIO.Create (File => log_handle,
                     Mode => TIO.Out_File,
                     Name => log_path);
      exception
         when error : others =>
            raise scan_log_error
              with "failed to create log " & log_path;
      end;

      TIO.Put_Line (log_handle, "=> Building " & get_port_variant (all_ports (seq_id)) &
                   " (version " & HT.USS (all_ports (seq_id).pkgversion) & ")");
      TIO.Put_Line (log_handle, "Started : " & timestamp (head_time));
      TIO.Put      (log_handle, "Platform: " & UNAME);
      if block_dog then
         TIO.Put   (log_handle, "Watchdog: Disabled");
      end if;
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


   --------------------------------------------------------------------------------------------
   --  set_scan_start_time
   --------------------------------------------------------------------------------------------
   procedure set_scan_start_time (mark : CAL.Time) is
   begin
      scan_start := mark;
   end set_scan_start_time;


   --------------------------------------------------------------------------------------------
   --  set_scan_complete
   --------------------------------------------------------------------------------------------
   procedure set_scan_complete (mark : CAL.Time) is
   begin
      scan_stop := mark;
   end set_scan_complete;


   --------------------------------------------------------------------------------------------
   --  set_scan_start_time
   --------------------------------------------------------------------------------------------
   procedure set_overall_start_time (mark : CAL.Time) is
   begin
      start_time := mark;
   end set_overall_start_time;


   --------------------------------------------------------------------------------------------
   --  set_scan_start_time
   --------------------------------------------------------------------------------------------
   procedure set_overall_complete (mark : CAL.Time) is
   begin
      stop_time := mark;
   end set_overall_complete;


   --------------------------------------------------------------------------------------------
   --  start_logging
   --------------------------------------------------------------------------------------------
   procedure start_logging (flavor : count_type)
   is
      logpath : String := HT.USS (PM.configuration.dir_logs) & "/logs/" & logname (flavor);
   begin
      if DIR.Exists (logpath) then
         DIR.Delete_File (logpath);
      end if;
      TIO.Create (File => Flog (flavor),
                  Mode => TIO.Out_File,
                  Name => logpath);
      if flavor = total then
         TIO.Put_Line
           (Flog (total),
              "-=>  Chronology of last build  <=-" & LAT.LF &
              "Started: " & timestamp (start_time) & LAT.LF &
              "Ports to build: " & HT.int2str (original_queue_size) & LAT.LF & LAT.LF &
              "Purging any ignored/broken ports first ...");
         TIO.Flush (Flog (total));
      end if;
   exception
      when others =>
         raise overall_log with "Failed to create " & logpath & bailing;
   end start_logging;


   --------------------------------------------------------------------------------------------
   --  stop_logging
   --------------------------------------------------------------------------------------------
   procedure stop_logging (flavor : count_type) is
   begin
      if flavor = total then
         TIO.Put_Line (Flog (flavor), "Finished: " & timestamp (stop_time));
         TIO.Put_Line (Flog (flavor), log_duration (start => start_time, stop  => stop_time));
         TIO.Put_Line
           (Flog (flavor), LAT.LF &
              "---------------------------" & LAT.LF &
              "--  Final Statistics" & LAT.LF &
              "---------------------------" & LAT.LF &
              " Initial queue size:" & bld_counter (total)'Img & LAT.LF &
              "        ports built:" & bld_counter (success)'Img & LAT.LF &
              "            ignored:" & bld_counter (ignored)'Img & LAT.LF &
              "            skipped:" & bld_counter (skipped)'Img & LAT.LF &
              "             failed:" & bld_counter (failure)'Img);
      end if;
      TIO.Close (Flog (flavor));
   end stop_logging;


   --------------------------------------------------------------------------------------------
   --  scribe
   --------------------------------------------------------------------------------------------
   procedure scribe (flavor : count_type; line : String; flush_after : Boolean) is
   begin
      TIO.Put_Line (Flog (flavor), line);
      if flush_after then
         TIO.Flush (Flog (flavor));
      end if;
   end scribe;


   --------------------------------------------------------------------------------------------
   --  flush_log
   --------------------------------------------------------------------------------------------
   procedure flush_log (flavor : count_type) is
   begin
      TIO.Flush (Flog (flavor));
   end flush_log;


   --------------------------------------------------------------------------------------------
   --  set_build_counters
   --------------------------------------------------------------------------------------------
   procedure set_build_counters (A, B, C, D, E : Natural) is
   begin
      bld_counter := (A, B, C, D, E);
   end set_build_counters;


   --------------------------------------------------------------------------------------------
   --  increment_build_counter
   --------------------------------------------------------------------------------------------
   procedure increment_build_counter (flavor : count_type; quantity : Natural := 1) is
   begin
      bld_counter (flavor) := bld_counter (flavor) + quantity;
   end increment_build_counter;


   --------------------------------------------------------------------------------------------
   --  start_obsolete_package_logging
   --------------------------------------------------------------------------------------------
   procedure start_obsolete_package_logging
   is
      logpath : constant String := HT.USS (PM.configuration.dir_logs)
        & "/logs/06_obsolete_packages.log";
   begin
      if DIR.Exists (logpath) then
         DIR.Delete_File (logpath);
      end if;
      TIO.Create (File => obsolete_pkg_log,
                  Mode => TIO.Out_File,
                  Name => logpath);
      obsolete_log_open := True;
   exception
      when others =>
         obsolete_log_open := False;
   end start_obsolete_package_logging;


   --------------------------------------------------------------------------------------------
   --  stop_obsolete_package_logging
   --------------------------------------------------------------------------------------------
   procedure stop_obsolete_package_logging is
   begin
      TIO.Close (obsolete_pkg_log);
   end stop_obsolete_package_logging;


   --------------------------------------------------------------------------------------------
   --  obsolete_notice
   --------------------------------------------------------------------------------------------
   procedure obsolete_notice (message : String; write_to_screen : Boolean)
   is
   begin
      if obsolete_log_open then
         TIO.Put_Line (obsolete_pkg_log, message);
      end if;
      if write_to_screen then
         TIO.Put_Line (message);
      end if;
   end obsolete_notice;


   --------------------------------------------------------------------------------------------
   --  www_timestamp_start_time
   --------------------------------------------------------------------------------------------
   function www_timestamp_start_time return String is
   begin
      return timestamp (start_time, True);
   end www_timestamp_start_time;


   --------------------------------------------------------------------------------------------
   --  ports_remaining_to_build
   --------------------------------------------------------------------------------------------
   function ports_remaining_to_build return Integer is
   begin
      return bld_counter (total)
        - bld_counter (success)
        - bld_counter (failure)
        - bld_counter (ignored)
        - bld_counter (skipped);
   end ports_remaining_to_build;


   --------------------------------------------------------------------------------------------
   --  port_counter_value
   --------------------------------------------------------------------------------------------
   function port_counter_value (flavor : count_type) return Integer is
   begin
      return bld_counter (flavor);
   end port_counter_value;


   --------------------------------------------------------------------------------------------
   --  hourly_build_rate
   --------------------------------------------------------------------------------------------
   function hourly_build_rate return Natural
   is
      pkg_that_count : constant Natural := bld_counter (success) + bld_counter (failure);
   begin
      return get_packages_per_hour (pkg_that_count, start_time);
   end hourly_build_rate;


   --------------------------------------------------------------------------------------------
   --  get_packages_per_hour
   --------------------------------------------------------------------------------------------
   function get_packages_per_hour (packages_done : Natural; from_when : CAL.Time) return Natural
   is
      diff_days    : CAR.Day_Count;
      diff_secs    : Duration;
      leap_secs    : CAR.Leap_Seconds_Count;
      result       : Natural;
      rightnow     : CAL.Time := CAL.Clock;
      work_seconds : Integer;
      work_days    : Integer;
      use type CAR.Day_Count;
   begin
      if packages_done = 0 then
         return 0;
      end if;
      CAR.Difference (Left    => rightnow,
                      Right   => from_when,
                      Days    => diff_days,
                      Seconds => diff_secs,
                      Leap_Seconds => leap_secs);

      work_seconds := Integer (diff_secs);
      work_days    := Integer (diff_days);
      work_seconds := work_seconds + (work_days * 3600 * 24);

      if work_seconds < 0 then
         --  should be impossible to get here.
         return 0;
      end if;
      result := packages_done * 3600;
      result := result / work_seconds;
      return result;
   exception
      when others => return 0;
   end get_packages_per_hour;


   --------------------------------------------------------------------------------------------
   --  impulse_rate
   --------------------------------------------------------------------------------------------
   function impulse_rate return Natural
   is
      pkg_that_count : constant Natural := bld_counter (success) + bld_counter (failure);
      pkg_diff : Natural;
      result   : Natural;
   begin
      if impulse_counter = impulse_range'Last then
         impulse_counter := impulse_range'First;
      else
         impulse_counter := impulse_counter + 1;
      end if;
      if impulse_data (impulse_counter).virgin then
         impulse_data (impulse_counter).hack     := CAL.Clock;
         impulse_data (impulse_counter).packages := pkg_that_count;
         impulse_data (impulse_counter).virgin   := False;

         return get_packages_per_hour (pkg_that_count, start_time);
      end if;
      pkg_diff := pkg_that_count - impulse_data (impulse_counter).packages;
      result   := get_packages_per_hour
                   (packages_done => pkg_diff,
                    from_when     => impulse_data (impulse_counter).hack);
      impulse_data (impulse_counter).hack     := CAL.Clock;
      impulse_data (impulse_counter).packages := pkg_that_count;

      return result;
   exception
      when others => return 0;
   end impulse_rate;

end PortScan.Log;
