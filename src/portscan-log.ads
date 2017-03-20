--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Calendar;

package PortScan.Log is

   package CAL renames Ada.Calendar;

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

   --  Establish times before the start and upon completion of a scan.
   procedure set_scan_start_time (mark : CAL.Time);
   procedure set_scan_complete   (mark : CAL.Time);

private

   type dim_handlers is array (count_type) of TIO.File_Type;
   type dim_counters is array (count_type) of Natural;

   function log_duration (start, stop : CAL.Time) return String;
   function elapsed_now return String;
   function elapsed_build (head_time, tail_time : CAL.Time) return String;
   function timestamp (hack : CAL.Time; www_format : Boolean := False) return String;
   function split_collection (line : String; title : String) return String;

   procedure dump_port_variables (log_handle : TIO.File_Type; contents : String);

   --  bulk run variables

   Flog        : dim_handlers;
   start_time  : CAL.Time;
   stop_time   : CAL.Time;
   scan_start  : CAL.Time;
   scan_stop   : CAL.Time;
   bld_counter : dim_counters := (0, 0, 0, 0, 0);

end PortScan.Log;
