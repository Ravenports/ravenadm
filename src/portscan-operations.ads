--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Port_Specification;

private with Parameters;

package PortScan.Operations is

   --  Call before executing sanity check.  It checks the present of build
   --  hooks at the synth_conf location and caches the results.
   procedure initialize_hooks;

    --  Fire off first hook (run_start) as a bulk build starts
   procedure run_start_hook;

   --  For the pkg(8), trigger a success or failure hook based on if it built or not.
   procedure run_hook_after_build (built : Boolean; id : port_id);

   --  Removes ??_history.json files from previous runs
   procedure delete_existing_web_history_files;

   --  Figure package names from portlist and remove all existing packages from that set.
   procedure delete_existing_packages_of_ports_list;

   --  Look in build queue and return the next ignore port in the queue (if it exists).
   function next_ignored_port return port_id;

   --  Returns true if every port in the queue has all of ports listed in the
   --  blocks and blocked_by containers are all also present in the queue
   function integrity_intact return Boolean;

   --  This removes the first reverse dependency port from all_ports that is
   --  found the complete reverse deps list and return the port_id of the
   --  deleted port.  If the list is empty, return port_match_failed instead.
   function skip_next_reverse_dependency (pinnacle : port_id) return port_id;

   --  Exposed for pilot which eliminated ignored ports during the sanity check
   procedure record_history_ignored
     (elapsed   : String;
      bucket    : String;
      origin    : String;
      reason    : String;
      skips     : Natural);

   --  The port build failed, so set all reverse dependences as skipped
   --  Remove the port from the queue when this is done.
   --  Exposed for pilot
   procedure cascade_failed_build (id : port_id; numskipped : out Natural);

   --  removes processed port from the top of ranking queue and returns the port id
   function unlist_first_port return port_id;

   --  Returns True on success; stores value in global external_repository
   function located_external_repository return Boolean;

   --  Returns the value of the stored external repository
   function top_external_repository return String;

   --  If performing a limited build run (likely 99% of the use cases), only
   --  the queued packages will be checked.  The checks are limited to finding
   --  options changes and dependency changes.  Obsolete packages (related or
   --  unrelated to upcoming build) are not removed; this would occur in
   --  clean_repository().  These old packages will not interfere at this step.
   procedure limited_sanity_check
     (repository       : String;
      dry_run          : Boolean;
      rebuild_compiler : Boolean;
      suppress_remote  : Boolean;
      major_release    : String;
      architecture     : supported_arch);

   --  Unconditionally copies web assets to <log directory/report directory
   --  It also provides an initial summary.json data file just the report has something to load
   procedure initialize_web_report (num_builders : builders);

   --  Kicks off curses or sets color support off.  Do it before
   --  calling parallel_bulk_run.
   procedure initialize_display (num_builders : builders);

   --  Kick off bulk run using the given number of builders
   --  The rank_queue and all_ports must be already set up (it's recommended
   --  To eliminate the ignored ports and subsequent skips first.
   procedure parallel_bulk_run (num_builders : builders; sysrootver : sysroot_characteristics);

   --  Explodes the buildsheet after applying directives, and returns True if all the subpackges
   --  were successfully built.  Exposes for use by test mode from pilot
   function build_subpackages
     (builder     : builders;
      sequence_id : port_id;
      sysrootver  : sysroot_characteristics;
      interactive : Boolean := False;
      enterafter  : String := "") return Boolean;

   function skip_verified (id : port_id) return Boolean;

   --  Generic parse/transform routine for a given buildsheet or specification file
   --  With a specification file.
   procedure parse_and_transform_buildsheet
     (specification : in out Port_Specification.Portspecs;
      successful    : out Boolean;
      buildsheet    : String;
      variant       : String;
      portloc       : String;
      excl_targets  : Boolean;
      avoid_dialog  : Boolean;
      for_webpage   : Boolean;
      sysrootver    : sysroot_characteristics);

   --  Using a populated package_list, cross off all package names that are found in the current
   --  all_ports data.  Whatever is left represents obsolete packages which are then removed.
   procedure eliminate_obsolete_packages;

   --  Using a populated package list, print out all subpackages for each package
   procedure list_subpackages_of_queued_ports;

private

   package PM  renames Parameters;

   type hook_type    is (run_start, run_end, pkg_success, pkg_failure, pkg_skipped, pkg_ignored);
   type dim_hooks    is array (hook_type) of Boolean;
   type dim_hooksloc is array (hook_type) of HT.Text;

   type machine_state     is (idle, tasked, busy, done_failure, done_success, shutdown);
   type dim_instruction   is array (builders) of port_id;
   type dim_builder_state is array (builders) of machine_state;

   active_hook   : dim_hooks := (False, False, False, False, False, False);
   hook_location : constant dim_hooksloc :=
                     (HT.SUS (PM.raven_confdir & "/hook_run_start"),
                      HT.SUS (PM.raven_confdir & "/hook_run_end"),
                      HT.SUS (PM.raven_confdir & "/hook_pkg_success"),
                      HT.SUS (PM.raven_confdir & "/hook_pkg_failure"),
                      HT.SUS (PM.raven_confdir & "/hook_pkg_skipped"),
                      HT.SUS (PM.raven_confdir & "/hook_pkg_ignored"));

   --  History log entries average less than 200 bytes.  Allot more than twice this amount.
   kfile_unit_maxsize : constant Positive := 512;

    --  Each history segment is limited to this many log lines
   kfile_units_limit : constant Positive := 500;

   subtype filearch is String (1 .. 11);
   subtype impulse_range is Integer range 1 .. 500;
   subtype kfile_content  is String (1 .. kfile_unit_maxsize * kfile_units_limit);

   type progress_history is
      record
         segment       : Natural := 0;
         segment_count : Natural := 0;
         log_entry     : Natural := 0;
         last_index    : Natural := 0;
         last_written  : Natural := 0;
         content       : kfile_content;
      end record;

   type package_abi is
      record
         calculated_abi      : HT.Text;
         calculated_alt_abi  : HT.Text;
         calc_abi_noarch     : HT.Text;
         calc_alt_abi_noarch : HT.Text;
      end record;

   type subpackage_identifier is
      record
         id         : port_index;
         subpackage : HT.Text;
      end record;

   package subpackage_queue is new CON.Vectors
     (Element_Type => subpackage_identifier,
      Index_Type   => port_index);

   pkgscan_progress : dim_progress := (others => 0);
   pkgscan_total    : Natural := 0;
   history          : progress_history;
   abi_formats      : package_abi;
   curses_support   : Boolean := False;

   external_repository : HT.Text;

   --  Debugging purposes only, can be turned on by environment variable
   debug_dep_check : Boolean := False;
   debug_opt_check : Boolean := False;

   --  Return true if file is executable (platform-specific)
   function file_is_executable (filename : String) return Boolean;

   procedure run_hook (hook : hook_type; envvar_list : String);
   procedure run_package_hook (hook : hook_type; id : port_id);

   function  nv (name, value : String) return String;
   function  nv (name : String; value : Integer) return String;
   procedure assimulate_substring (history : in out progress_history; substring : String);
   procedure handle_first_history_entry;
   procedure check_history_segment_capacity;
   procedure delete_rank (id : port_id);
   function  still_ranked (id : port_id) return Boolean;
   function  rank_arrow (id : port_id) return ranking_crate.Cursor;
   function  get_swap_status return Float;
   function  swapinfo_command return String;
   function  nothing_left (num_builders : builders) return Boolean;
   function  shutdown_recommended (active_builders : Positive) return Boolean;

   procedure write_history_json;

   procedure write_summary_json
     (active            : Boolean;
      states            : dim_builder_state;
      num_builders      : builders;
      num_history_files : Natural);

   procedure record_history_skipped
     (elapsed   : String;
      bucket    : String;
      origin    : String;
      reason    : String);

   procedure record_history_built
     (elapsed   : String;
      slave_id  : builders;
      bucket    : String;
      origin    : String;
      duration  : String);

   procedure record_history_failed
     (elapsed   : String;
      slave_id  : builders;
      bucket    : String;
      origin    : String;
      duration  : String;
      die_phase : String;
      skips     : Natural);

   --  This calculates the ABI for the platform and stores it.  The value is
   --  used by passed_abi_check()
   procedure establish_package_architecture (release : String; architecture : supported_arch);

   --  Use file to determine arch on ELF platforms
   function isolate_arch_from_file_type (fileinfo : String) return filearch;

   --  Use file to dtermine arch on MacOS
   function isolate_arch_from_macho_file (fileinfo : String) return filearch;

   --  This function returns "True" if the scanned dependencies match exactly
   --  what the current ports tree has.
   function passed_dependency_check
     (subpackage   : String;
      query_result : HT.Text;
      id           : port_id) return Boolean;

   --  Turn on option and dependency debug checks programmatically
   procedure activate_debugging_code;

   --  The result of the dependency query giving "id" port_id
   function result_of_dependency_query
     (repository : String;
      id         : port_id;
      subpackage : String) return HT.Text;

   --  Dedicated progress meter for prescanning packages
   function package_scan_progress return String;

   --  For each package in the query, check the ABI and options (this is the
   --  only time they are checked).  If those pass, query the dependencies,
   --  store the result, and check them.  Set the "deletion" flag as needed.
   --  The dependency check is NOT performed yet.
   procedure initial_package_scan (repository : String; id : port_id; subpackage : String);

   --  Same as above, but for packages in the external repository
   procedure remote_package_scan (id : port_id; subpackage : String);

   --  Using the same make_queue as was used to scan the ports, use tasks (up to 32) to do the
   --  initial scanning of the ports, including getting the pkg dependency query.
   procedure parallel_package_scan
     (repository    : String;
      remote_scan   : Boolean;
      show_progress : Boolean);

   --  The port build succeeded, so remove the "blocked_by" designation
   --  for all the immediate reverse dependencies.
   --  Remove the port from the queue when this is done.
   procedure cascade_successful_build (id : port_id);

   --  This function returns "True" if the scanned package has the expected
   --  package ABI, e.g. dragonfly:4.6:x86:64, freebsd:10:amd64
   function passed_abi_check
     (repository       : String;
      id               : port_id;
      subpackage       : String;
      skip_exist_check : Boolean := False) return Boolean;

   --  This function returns "True" if the scanned options exactly match
   --  the options in the already-built package.  Usually it's already known
   --  that a package exists before the function is called, but an existence
   --  check will be performed just in case (failure returns "False")
   function passed_option_check
     (repository       : String;
      id               : port_id;
      subpackage       : String;
      skip_exist_check : Boolean := False) return Boolean;

   --  Before starting to build a port, lock it.  This is required for
   --  parallel building.
   procedure lock_package (id : port_id);

   --  Returns the highly priority buildable port
   function top_buildable_port return port_id;

   --  removes processed port from the ranking queue.
   procedure unlist_port (id : port_id);

end PortScan.Operations;
