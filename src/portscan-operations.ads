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

private

   package PM  renames Parameters;

   type hook_type         is (run_start, run_end, pkg_success, pkg_failure,
                              pkg_skipped, pkg_ignored);

   type dim_hooks         is array (hook_type) of Boolean;
   type dim_hooksloc      is array (hook_type) of HT.Text;

   active_hook     : dim_hooks := (False, False, False, False, False, False);
   hook_location   : constant dim_hooksloc :=
                     (HT.SUS (PM.raven_confdir & "/hook_run_start"),
                      HT.SUS (PM.raven_confdir & "/hook_run_end"),
                      HT.SUS (PM.raven_confdir & "/hook_pkg_success"),
                      HT.SUS (PM.raven_confdir & "/hook_pkg_failure"),
                      HT.SUS (PM.raven_confdir & "/hook_pkg_skipped"),
                      HT.SUS (PM.raven_confdir & "/hook_pkg_ignored"));

   --  Return true if file is executable (platform-specific)
   function file_is_executable (filename : String) return Boolean;

   procedure run_hook (hook : hook_type; envvar_list : String);
   procedure run_package_hook (hook : hook_type; id : port_id);

end PortScan.Operations;
