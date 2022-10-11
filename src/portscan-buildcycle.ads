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
   procedure run_makesum (id : builders; ssl_variant : String);

   --  Exposed for Pilot to determine validity of test build request
   function valid_test_phase (afterphase : String) return Boolean;

   --  Exposed for Pilot to regenerate patches (Names and content are maintained)
   procedure run_patch_regen (id : builders; sourceloc : String; ssl_variant : String);

private

   package CAL renames Ada.Calendar;

   type phases is (blr_depends, fetch, extract, patch, configure, build, stage,
                   test, check_plist, pkg_package, install, deinstall);

   type trackrec is
      record
         seq_id      : port_id;
         head_time   : CAL.Time;
         tail_time   : CAL.Time;
         log_handle  : aliased TIO.File_Type;
         dynlink     : string_crate.Vector;
         runpaths    : string_crate.Vector;
         nonexistent : string_crate.Vector;
         seen_libs   : string_crate.Vector;
         rpath_fatal : Boolean;
         check_strip : Boolean;
         disable_dog : Boolean;
         loglines    : Natural := 0;
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
                         environ       : String;
                         phaseenv      : String := "";
                         depends_phase : Boolean := False;
                         skip_header   : Boolean := False;
                         skip_footer   : Boolean := False)
                         return Boolean;

   procedure mark_file_system (id : builders; action : String; environ : String);
   procedure interact_with_builder (id : builders; ssl_variant : String);
   procedure set_uname_mrv;
   procedure obtain_custom_environment;
   function  phase2str (phase : phases) return String;
   function  max_time_without_output (phase : phases) return execution_limit;
   function  timeout_multiplier_x10 return Positive;
   function  get_environment (id : builders; environ : String) return String;
   function  get_port_variables (id : builders; environ : String) return String;
   function  generic_system_command (command : String) return String;
   function  get_root (id : builders) return String;
   function  passed_runpath_check (id : builders) return Boolean;
   function  format_loglines (numlines : Natural) return String;
   function  watchdog_message (minutes : execution_limit) return String;
   function  get_port_prefix (id : builders; environ : String) return String;
   function  pkg_install_subroutine (id : builders; root, env_vars, line : String) return Boolean;

   function  environment_override (toolchain   : Boolean;
                                   ssl_variant : String;
                                   enable_tty  : Boolean := False) return String;

   function  exec_phase_generic
     (id            : builders;
      phase         : phases;
      environ       : String) return Boolean;

   function  exec_phase_build
     (id            : builders;
      environ       : String) return Boolean;

   function  exec_phase_install
     (id            : builders;
      pkgversion    : String;
      environ       : String) return Boolean;

   function  exec_phase_deinstall
     (id            : builders;
      pkgversion    : String;
      environ       : String) return Boolean;

   function  deinstall_all_packages
     (id            : builders;
      environ       : String) return Boolean;

   function  install_run_depends
     (specification : PSP.Portspecs;
      id            : builders;
      environ       : String) return Boolean;

   function  generic_execute
     (id            : builders;
      command       : String;
      dogbite       : out Boolean;
      time_limit    : execution_limit) return Boolean;

   function  exec_phase_depends
     (specification : PSP.Portspecs;
      phase_name    : String;
      id            : builders;
      environ       : String) return Boolean;

   function  dynamically_linked
     (base          : String;
      filename      : String;
      strip_check   : Boolean;
      unstripped    : out Boolean) return Boolean;

   function  log_linked_libraries
     (id            : builders;
      pkgversion    : String;
      environ       : String) return Boolean;

   procedure stack_linked_libraries
     (id            : builders;
      base          : String;
      filename      : String;
      environ       : String);

   function  detect_leftovers_and_MIA
     (id            : builders;
      action        : String;
      description   : String;
      environ       : String) return Boolean;

end PortScan.Buildcycle;
