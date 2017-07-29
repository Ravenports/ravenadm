--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Calendar;

package PortScan.Log is

   package CAL renames Ada.Calendar;

   overall_log : exception;

   --  Open log, dump diagnostic data and stop timer.
   function initialize_log
     (log_handle : in out TIO.File_Type;
      head_time  : out CAL.Time;
      seq_id     : port_id;
      slave_root : String;
      UNAME      : String;
      BENV       : String;
      COPTS      : String;
      PTVAR      : String) return Boolean;

   --  Stop time, write duration data, close log
   procedure finalize_log
     (log_handle : in out TIO.File_Type;
      head_time  : CAL.Time;
      tail_time  : out CAL.Time);

   --  Helper to format phase/section heading
   function log_section (title : String) return String;

   --  Format start of build phase in log
   procedure log_phase_begin (log_handle : TIO.File_Type; phase : String);

   --  Format end of build phase in log
   procedure log_phase_end (log_handle : TIO.File_Type);

   --  Standard log name based on port origin and variant.
   function log_name (sid : port_id) return String;

   --  Returns formatted difference in seconds between two times
   function elapsed_HH_MM_SS (start, stop : CAL.Time) return String;

   --  Returns formatted difference in seconds between overall start time and now.
   function elapsed_now return String;

   --  Establish times before the start and upon completion of a scan.
   procedure set_scan_start_time (mark : CAL.Time);
   procedure set_scan_complete   (mark : CAL.Time);

   --  Establish times before the start and upon completion of a bulk run
   procedure set_overall_start_time (mark : CAL.Time);
   procedure set_overall_complete   (mark : CAL.Time);

   --  build log operations
   procedure start_logging (flavor : count_type);
   procedure stop_logging (flavor : count_type);
   procedure scribe (flavor : count_type; line : String; flush_after : Boolean);
   procedure flush_log (flavor : count_type);

   --  Establish values of build counters
   procedure set_build_counters (A, B, C, D, E : Natural);

   --  Increments the indicated build counter by some quality.
   procedure increment_build_counter (flavor : count_type; quantity : Natural := 1);

   --  Open log to document packages that get deleted and the reason why
   procedure start_obsolete_package_logging;
   procedure stop_obsolete_package_logging;

   --  Write to log if open and optionally output a copy to screen.
   procedure obsolete_notice (message : String; write_to_screen : Boolean);

   --  Return WWW report-formatted timestamp of start time.
   function www_timestamp_start_time return String;

   --  Return current build queue size
   function ports_remaining_to_build return Integer;

   --  Return value of individual port counter
   function port_counter_value (flavor : count_type) return Integer;

   --  Return number of packages built since build started
   function hourly_build_rate return Natural;

   --  Return number of packages built in the last 600 seconds
   function impulse_rate return Natural;

   --  Show duration between overall start and stop times.
   function bulk_run_duration return String;

   --  Return formatted duration of scan
   function scan_duration return String;

   --  Former private function exposed for web page generator
   function timestamp (hack : CAL.Time; www_format : Boolean := False) return String;

private

   type impulse_rec is
      record
         hack     : CAL.Time;
         packages : Natural := 0;
         virgin   : Boolean := True;
      end record;

   subtype logname_field is String (1 .. 19);
   subtype impulse_range is Integer range 1 .. 600;
   type dim_handlers is array (count_type) of TIO.File_Type;
   type dim_counters is array (count_type) of Natural;
   type dim_logname  is array (count_type) of logname_field;
   type dim_impulse  is array (impulse_range) of impulse_rec;

   function log_duration (start, stop : CAL.Time) return String;
   function split_collection (line : String; title : String) return String;

   procedure dump_port_variables (log_handle : TIO.File_Type; contents : String);

   --  Simple time calculation (guts)
   function get_packages_per_hour (packages_done : Natural; from_when : CAL.Time) return Natural;

   --  bulk run variables

   Flog        : dim_handlers;
   start_time  : CAL.Time;
   stop_time   : CAL.Time;
   scan_start  : CAL.Time;
   scan_stop   : CAL.Time;
   bld_counter : dim_counters := (0, 0, 0, 0, 0);

   impulse_counter   : impulse_range := impulse_range'Last;
   impulse_data      : dim_impulse;

   obsolete_pkg_log  : TIO.File_Type;
   obsolete_log_open : Boolean := False;

   bailing : constant String := "  (ravenadm must exit)";
   logname : constant dim_logname := ("00_last_results.log",
                                      "01_success_list.log",
                                      "02_failure_list.log",
                                      "03_ignored_list.log",
                                      "04_skipped_list.log");

end PortScan.Log;
