--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Display;

private with Parameters;

package PortScan.Operations is

   package DPY renames Display;

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
      origin    : String;
      reason    : String;
      skips     : Natural);

   --  removes processed port from the ranking queue.
   procedure unlist_port (id : port_id);

   --  After the initial queue is created, and before the limited sanity
   --  check, we go through each port and check if it has cached options.
   --  If it does, then it's checked for validity.  If it has too many or
   --  too few options, or an option's name doesn't match, the port is
   --  printed to stdout.  The rest of the ports are checked, but at that
   --  point the function has failed.
   function limited_cached_options_check return Boolean;

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
     (repository      : String;
      dry_run         : Boolean;
      suppress_remote : Boolean);

   function skip_verified (id : port_id) return Boolean;

private

   package PM  renames Parameters;

   type hook_type    is (run_start, run_end, pkg_success, pkg_failure, pkg_skipped, pkg_ignored);
   type dim_hooks    is array (hook_type) of Boolean;
   type dim_hooksloc is array (hook_type) of HT.Text;

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

   type package_abi is record
      calculated_abi      : HT.Text;
      calculated_alt_abi  : HT.Text;
      calc_abi_noarch     : HT.Text;
      calc_alt_abi_noarch : HT.Text;
   end record;

   history     : progress_history;
   abi_formats : package_abi;

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

   procedure write_history_json;

   procedure record_history_skipped
     (elapsed   : String;
      origin    : String;
      reason    : String);

   --  This calculates the ABI for the platform and stores it.  The value is
   --  used by passed_abi_check()
   procedure establish_package_architecture;

   function isolate_arch_from_file_type (fileinfo : String) return filearch;

   --  This function returns "True" if the scanned dependencies match exactly
   --  what the current ports tree has.
   function passed_dependency_check (query_result : HT.Text; id : port_id) return Boolean;

   --  Turn on option and dependency debug checks programmatically
   procedure activate_debugging_code;

   --  The result of the dependency query giving "id" port_id
   function result_of_dependency_query
     (repository : String;
      id         : port_id;
      subpackage : String) return HT.Text;

   --  Using the same make_queue as was used to scan the ports, use tasks (up to 32) to do the
   --  initial scanning of the ports, including getting the pkg dependency query.
   procedure parallel_package_scan
     (repository    : String;
      remote_scan   : Boolean;
      show_progress : Boolean);

   --  The port build failed, so set all reverse dependences as skipped
   --  Remove the port from the queue when this is done.
   procedure cascade_failed_build (id : port_id; numskipped : out Natural);

   --  The port build succeeded, so remove the "blocked_by" designation
   --  for all the immediate reverse dependencies.
   --  Remove the port from the queue when this is done.
   procedure cascade_successful_build (id : port_id);

end PortScan.Operations;
