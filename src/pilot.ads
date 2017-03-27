--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions; use Definitions;

private with HelperText;
private with PortScan;
private with Ada.Containers.Vectors;

package Pilot is

   procedure display_usage;
   procedure react_to_unknown_first_level_command (argument : String);
   procedure react_to_unknown_second_level_command (level1, level2 : String);
   procedure dump_ravensource (optional_directory : String);
   --  These are just a placeholders
   procedure generate_makefile (optional_directory : String;
                                optional_variant : String);
   procedure generate_buildsheet (sourcedir    : String;
                                  save_command : String);

   --  Return True when TERM is defined in environment (required)
   function TERM_defined_in_environment return Boolean;

   --  The current working directory will cause ravenadm to fail
   --  Known issue for when cwd is within <system root>/usr/local
   function launch_clash_detected return Boolean;

   --  Returns True if the root user didn't execute ravenadm.
   function insufficient_privileges return Boolean;

   --  Returns True if a pidfile is found and it's a valid ravenadm process
   function already_running return Boolean;

   --  Create a pidfile on major actions and remove it when complete.
   procedure create_pidfile;
   procedure destroy_pidfile;

      --  Checks if things are mounted from aborted previous run.
   --  The action upon "True" would be to try to clean them up (else abort)
   function previous_run_mounts_detected return Boolean;

   --  Checks if work directories are left over from aborted previous run
   --  The action upon "True" would be to remove them completely
   function previous_realfs_work_detected return Boolean;

   --  Returns True if all the old mounts were unmounted without issue.
   --  If not, it will emit messages so ravenadm can just eject directly.
   function old_mounts_successfully_removed return Boolean;

   --  Returns True if all the old SL*_(work|localbase) directories
   --  were removed without any problems
   function old_realfs_work_successfully_removed return Boolean;

   --  libexec/ravenexec is required, make sure it's installed!
   function ravenexec_missing return Boolean;

   --  pass-through for configure package
   procedure launch_configure_menu;

   --  Returns the conspiracy location for candidate.  The wording changes bases on if the
   --  buildsheet exists or not.
   procedure locate (candidate : String);

   --  Scan sysroot to get exact OS versions, return False if anything fails
   function slave_platform_determined return Boolean;

   --  Iterate through stack of individual build requests and scan each one.
   --  If any scan fails, return False.
   function scan_stack_of_single_ports (always_build : Boolean) return Boolean;

   --  Runs post-scan sanity check
   --  If successful, then scans for all "ignored" ports, failing them
   --  For each ignored port, cascade the failures (designated "skipped" ports)
   --  Starts the build log documenting all this.
   --  Return True if no problems are encountered.
   function sanity_check_then_prefail
     (delete_first : Boolean := False;
      dry_run      : Boolean := False) return Boolean;

   --  Everything is fine so kick of the parallel builders.  They will
   --  continue until everything is complete.
   procedure perform_bulk_run (testmode : Boolean);

   --  Returns "True" when all the port origins
   --  (command line or inside file) are verified and arguments are correct.
   function store_origins (start_from : Positive) return Boolean;

   --  Sends a basic template for writing specifications to standard out.
   procedure print_spec_template (save_command : String);

   --  Explodes current directory specfication into scan slave and generates a distinfo
   --  file which is copied back to current directory.
   procedure generate_distinfo;

   --  This procedure removes the single CLI listed port from the queue,
   --  executes "perform_bulk_run" and then builds the final port, but
   --  breaks into the jail at the specified (by ENTERAFTER env) point
   --  The test mode is always "True" so that's not an argument.
   procedure bulk_run_then_interact_with_final_port;

   --  If ENTERAFTER is defined to valid phase and there's only one port
   --  given on the command line, then return True
   function interact_with_single_builder return Boolean;

private

   package HT renames HelperText;
   package CON renames Ada.Containers;

   subtype logname_field is String (1 .. 19);
   type dim_logname  is array (count_type) of logname_field;
   type verdiff is (newbuild, rebuild, change);

   specfile   : constant String := "specification";
   errprefix  : constant String := "Error : ";
   pidfile    : constant String := "/var/run/ravenadm.pid";
   bailing    : constant String := "  (ravenadm must exit)";
   shutreq    : constant String := "Graceful shutdown requested, exiting ...";
   brkname    : constant String := "ENTERAFTER";

   scan_slave : constant builders := 9;
   ss_base    : constant String   := "/SL09";

   sysrootver : PortScan.sysroot_characteristics;

   logname    : constant dim_logname := ("00_last_results.log",
                                         "01_success_list.log",
                                         "02_failure_list.log",
                                         "03_ignored_list.log",
                                         "04_skipped_list.log");

   procedure DNE (filename : String);

   --  Returns True if given port-variant name is valid (existing buildsheet and variant
   --  exists on that buildsheet.  Unkindness directory takes precedence over conspiracy.
   function valid_origin
     (port_variant : String;
      bad_namebase : out Boolean;
      bad_format   : out Boolean;
      assume_std   : out Boolean) return Boolean;

end Pilot;
