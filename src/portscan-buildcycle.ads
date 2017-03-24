--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Display;
with Port_Specification;
private with Ada.Calendar;

package PortScan.Buildcycle is

   package PSP renames Port_Specification;

   cycle_cmd_error : exception;

   procedure initialize (test_mode : Boolean);

   function build_package (id            : builders;
                           sequence_id   : port_id;
                           specification : PSP.Portspecs;
                           interactive   : Boolean := False;
                           interphase    : String  := "") return Boolean;

   --  Compile status of builder for the curses display
   function builder_status (id       : builders;
                            shutdown : Boolean := False;
                            idle     : Boolean := False)
                            return Display.builder_rec;

   function last_build_phase (id : builders) return String;

   function assemble_history_record (slave  : builders;
                                     pid    : port_id;
                                     action : Display.history_action) return Display.history_rec;

   --  exposed for WWW report
   function load_core (instant_load : Boolean) return Float;

   --  records the current length of the build log.
   procedure set_log_lines (id : builders);

   --  Returns the formatted time difference between start and stop of package build
   function elapsed_build (id : builders) return String;

   --  Run make -C /port/ makesum (used by developer to generate distinfo)
   procedure run_makesum (id : builders);

   --  Exposed for Pilot to determine validity of test build request
   function valid_test_phase (afterphase : String) return Boolean;

private

   package CAL renames Ada.Calendar;

   type phases is (blr_depends, fetch, checksum, extract, patch, configure, build, stage,
                   test, check_plist, pkg_package, install, deinstall);

   type trackrec is
      record
         seq_id     : port_id;
         head_time  : CAL.Time;
         tail_time  : CAL.Time;
         log_handle : aliased TIO.File_Type;
         dynlink    : string_crate.Vector;
         loglines   : Natural := 0;
      end record;

   type dim_trackers       is array (builders) of trackrec;
   type dim_phase_trackers is array (builders) of phases;
   type execution_limit    is range 1 .. 720;

   phase_trackers : dim_phase_trackers;

   trackers  : dim_trackers;
   testing   : Boolean;
   uname_mrv : HT.Text;
   customenv : HT.Text;
   selftest  : constant String := "SELFTEST";
   chroot_make_program : constant String := "/usr/bin/make -m /xports/Mk";

   --  If the afterphase string matches a legal phase name then that phase
   --  is returned, otherwise the value of blr_depends is returned.  Allowed
   --  phases are: extract/patch/configure/build/stage/test/install/deinstall.
   --  blr_depends is considered a negative response
   --  stage includes check-plist

   function  valid_test_phase (afterphase : String) return phases;

   function  exec_phase (id : builders; phase : phases;
                         time_limit    : execution_limit;
                         phaseenv      : String := "";
                         depends_phase : Boolean := False;
                         skip_header   : Boolean := False;
                         skip_footer   : Boolean := False)
                         return Boolean;

   procedure mark_file_system (id : builders; action : String);
   procedure interact_with_builder (id : builders);
   procedure set_uname_mrv;
   procedure obtain_custom_environment;
   procedure stack_linked_libraries (id : builders; base, filename : String);
   procedure log_linked_libraries (id : builders; pkgversion : String);
   function  exec_phase_generic (id : builders; phase : phases) return Boolean;
   function  exec_phase_depends (id : builders) return Boolean;
   function  exec_phase_deinstall (id : builders; pkgversion : String) return Boolean;
   function  exec_phase_install   (id : builders; pkgversion : String) return Boolean;
   function  exec_phase_build (id : builders) return Boolean;
   function  phase2str (phase : phases) return String;
   function  max_time_without_output (phase : phases) return execution_limit;
   function  timeout_multiplier_x10 return Positive;
   function  detect_leftovers_and_MIA (id : builders; action : String;
                                       description : String) return Boolean;
   function  get_environment (id : builders) return String;
   function  get_port_variables (id : builders) return String;
   function  generic_system_command (command : String) return String;
   function  get_root (id : builders) return String;
   function  environment_override (enable_tty : Boolean := False) return String;
   function  dynamically_linked (base, filename : String) return Boolean;
   function  format_loglines (numlines : Natural) return String;
   function  watchdog_message (minutes : execution_limit) return String;
   function  generic_execute
     (id         : builders;
      command    : String;
      dogbite    : out Boolean;
      time_limit : execution_limit) return Boolean;

end PortScan.Buildcycle;
