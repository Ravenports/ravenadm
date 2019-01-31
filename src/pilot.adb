--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

--  To enable ncurses support, use sed to change Options_Dialog_Console => Options_Dialog

with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Calendar;
with Ada.Text_IO;
with Ada.Exceptions;
with Specification_Parser;
with File_Operations;
with Information;
with Parameters;
with Replicant;
with Configure;
with Utilities;
with Signals;
with Unix;
with Port_Specification.Buildsheet;
with Port_Specification.Makefile;
with Port_Specification.Transform;
with Port_Specification.Web;
with PortScan.Operations;
with PortScan.Buildcycle;
with PortScan.Scan;
with PortScan.Log;
with Package_Manifests;
with Options_Dialog_Console;
with Ravenports;
with Repository;

package body Pilot is

   package UTL renames Utilities;
   package REP renames Replicant;
   package PM  renames Parameters;
   package NFO renames Information;
   package FOP renames File_Operations;
   package PAR renames Specification_Parser;
   package PSB renames Port_Specification.Buildsheet;
   package PSM renames Port_Specification.Makefile;
   package PST renames Port_Specification.Transform;
   package WEB renames Port_Specification.Web;
   package OPS renames PortScan.Operations;
   package CYC renames PortScan.Buildcycle;
   package SCN renames PortScan.Scan;
   package LOG renames PortScan.Log;
   package MAN renames Package_Manifests;
   package OPT renames Options_Dialog_Console;
   package LAT renames Ada.Characters.Latin_1;
   package CLI renames Ada.Command_Line;
   package DIR renames Ada.Directories;
   package CAL renames Ada.Calendar;
   package TIO renames Ada.Text_IO;
   package AS  renames Ada.Strings;
   package EX  renames Ada.Exceptions;

   --------------------------------------------------------------------------------------------
   --  display_usage
   --------------------------------------------------------------------------------------------
   procedure display_usage is
   begin
      NFO.display_usage;
   end display_usage;

   --------------------------------------------------------------------------------------------
   --  react_to_unknown_first_level_command
   --------------------------------------------------------------------------------------------
   procedure react_to_unknown_first_level_command (argument : String) is
   begin
      NFO.display_unknown_command (argument);
   end react_to_unknown_first_level_command;


   --------------------------------------------------------------------------------------------
   --  react_to_unknown_second_level_command
   --------------------------------------------------------------------------------------------
   procedure react_to_unknown_second_level_command (level1, level2 : String) is
   begin
      NFO.display_unknown_command (level1, level2);
   end react_to_unknown_second_level_command;


   --------------------------------------------------------------------------------------------
   --  show_short_help
   --------------------------------------------------------------------------------------------
   procedure show_short_help is
   begin
      NFO.short_help_screen;
   end show_short_help;


   --------------------------------------------------------------------------------------------
   --  launch_man_page
   --------------------------------------------------------------------------------------------
   procedure launch_man_page (level2 : String)
   is
      function man_command return String;

      result   : Boolean;
      level2ok : Boolean := False;
      man_page : constant String :=
        host_localbase & "/share/man/man8/ravenadm-" & level2 & ".8.gz";

      function man_command return String is
      begin
         case platform_type is
            when sunos  => return host_localbase & "/bin/man 8 ravenadm-" & level2;
            when others => return "/usr/bin/man " & man_page;
         end case;
      end man_command;

   begin
      if
        level2 = "dev" or else
        level2 = "build" or else
        level2 = "build-everything" or else
        level2 = "force" or else
        level2 = "test" or else
        level2 = "test-everything" or else
        level2 = "status" or else
        level2 = "status-everything" or else
        level2 = "configure" or else
        level2 = "locate" or else
        level2 = "purge-distfiles" or else
        level2 = "purge-logs" or else
        level2 = "set-options" or else
        level2 = "check-ports" or else
        level2 = "update-ports" or else
        level2 = "generate-repository" or else
        level2 = "generate-website" or else
        level2 = "subpackages"
      then
         if DIR.Exists (man_page) then
            result := Unix.external_command (man_command);
         else
            TIO.Put_Line (errprefix & "the " & level2 & " man page is missing");
         end if;
      else
         TIO.Put_Line ("'" & level2 & "' is not a valid command (so no help is available)");
      end if;
   end launch_man_page;


   --------------------------------------------------------------------------------------------
   --  dump_ravensource
   --------------------------------------------------------------------------------------------
   procedure dump_ravensource (optional_directory : String)
   is
      directory_specified : constant Boolean := (optional_directory /= "");
      successful : Boolean;

      specification : Port_Specification.Portspecs;
   begin
      if directory_specified then
         declare
            filename : String := optional_directory & "/" & specfile;
         begin
            if DIR.Exists (filename) then
               PAR.parse_specification_file (dossier         => filename,
                                             specification   => specification,
                                             opsys_focus     => platform_type,
                                             arch_focus      => sysrootver.arch,
                                             success         => successful,
                                             stop_at_targets => False);
            else
               DNE (filename);
               return;
            end if;
         end;
      else
         if DIR.Exists (specfile) then
            PAR.parse_specification_file (dossier         => specfile,
                                          specification   => specification,
                                          opsys_focus     => platform_type,
                                          arch_focus      => sysrootver.arch,
                                          success         => successful,
                                          stop_at_targets => False);
         else
            DNE (specfile);
            return;
         end if;
      end if;
      if successful then
         specification.dump_specification;
      else
         TIO.Put_Line (errprefix & "Failed to parse " & specfile);
         TIO.Put_Line (PAR.get_parse_error);
      end if;
   end dump_ravensource;


   --------------------------------------------------------------------------------------------
   --  list_subpackages
   --------------------------------------------------------------------------------------------
   procedure list_subpackages
   is
      successful : Boolean := SCN.scan_provided_list_of_ports (False, sysrootver);
   begin
      if successful then
         OPS.list_subpackages_of_queued_ports;
      end if;
   end list_subpackages;


   --------------------------------------------------------------------------------------------
   --  generate_makefile
   --------------------------------------------------------------------------------------------
   procedure generate_makefile (optional_directory : String;
                                optional_variant : String)
   is
      function get_variant return String;

      directory_specified : constant Boolean := (optional_directory /= "");
      successful : Boolean;

      specification : Port_Specification.Portspecs;
      dossier_text  : HT.Text;

      function get_variant return String is
      begin
         if optional_variant = "" then
            return variant_standard;
         else
            return optional_variant;
         end if;
      end get_variant;

      selected_variant : constant String := get_variant;

   begin
      if directory_specified then
         dossier_text := HT.SUS (optional_directory & "/" & specfile);
      else
         dossier_text := HT.SUS (specfile);
      end if;
      if DIR.Exists (HT.USS (dossier_text)) then
         OPS.parse_and_transform_buildsheet (specification => specification,
                                             successful    => successful,
                                             buildsheet    => HT.USS (dossier_text),
                                             variant       => selected_variant,
                                             portloc       => "",
                                             excl_targets  => False,
                                             avoid_dialog  => True,
                                             for_webpage   => False,
                                             sysrootver    => sysrootver);
      else
         DNE (HT.USS (dossier_text));
         return;
      end if;

      if successful then

         if not specification.post_transform_option_group_defaults_passes then
            return;
         end if;

         PSM.generator (specs         => specification,
                        variant       => selected_variant,
                        opsys         => platform_type,
                        arch          => sysrootver.arch,
                        output_file   => "");
      else
         TIO.Put_Line (errprefix & "Failed to parse " & specfile);
         TIO.Put_Line (PAR.get_parse_error);
      end if;
   end generate_makefile;


   --------------------------------------------------------------------------------------------
   --  generate_webpage
   --------------------------------------------------------------------------------------------
   procedure generate_webpage  (required_namebase : String;
                                optional_variant : String)
   is
      function get_variant return String;

      successful : Boolean;

      specification : Port_Specification.Portspecs;
      bogustime     : constant CAL.Time := CAL.Clock;
      dossier       : constant String := HT.USS (PM.configuration.dir_conspiracy) & "/bucket_" &
                      UTL.bucket (required_namebase) & "/" & required_namebase;

      function get_variant return String is
      begin
         if optional_variant = "" then
            return variant_standard;
         else
            return optional_variant;
         end if;
      end get_variant;
   begin
      if DIR.Exists (dossier) then
         REP.launch_workzone;
         OPS.parse_and_transform_buildsheet (specification => specification,
                                             successful    => successful,
                                             buildsheet    => dossier,
                                             variant       => get_variant,
                                             portloc       => REP.get_workzone_path,
                                             excl_targets  => False,
                                             avoid_dialog  => True,
                                             for_webpage   => True,
                                             sysrootver    => sysrootver);
      else
         DNE (dossier);
         return;
      end if;

      if successful then
         WEB.produce_page (specs   => specification,
                           variant => get_variant,
                           dossier => TIO.Standard_Output,
                           portdir => REP.get_workzone_path,
                           blocked => "",
                           created => bogustime,
                           changed => bogustime,
                           devscan => True);
      else
         TIO.Put_Line (errprefix & "Failed to parse " & dossier);
         TIO.Put_Line (PAR.get_parse_error);
      end if;
      REP.destroy_workzone;
   end generate_webpage;


   --------------------------------------------------------------------------------------------
   --  generate_buildsheet
   --------------------------------------------------------------------------------------------
   procedure generate_buildsheet (sourcedir    : String;
                                  save_command : String)
   is
      save_it       : Boolean := False;
      successful    : Boolean;
      ravensrcdir   : constant String := Unix.true_path (sourcedir);
      filename      : constant String := ravensrcdir & "/" & specfile;
      specification : Port_Specification.Portspecs;
   begin
      if save_command = "save" then
         --  We don't want custom ports to be saved into the conspiracy directory.
         --  These source ports are parsed every time (not through a build sheet).
         --  However, we want to preserve the ability to construct and display a build sheet,
         --  but just not save it.  If it's a custom port, tell user that saving is a no-no
         if not HT.equivalent (PM.configuration.dir_unkindness, PM.no_unkindness) then
            if HT.leads (ravensrcdir, HT.USS (PM.configuration.dir_unkindness)) then
               TIO.Put_Line ("Custom port buildsheets must not be saved.");
               TIO.Put_Line ("Don't worry, ravenadm will compile them automatically as needed.");
               return;
            end if;
         end if;
         save_it := True;
      elsif save_command /= "" then
         TIO.Put_Line (errprefix & "fourth argument can only be 'save'");
         return;
      end if;
      if ravensrcdir = "" then
         TIO.Put_Line (errprefix & "not a valid directory: " & sourcedir);
         return;
      end if;

      if DIR.Exists (filename) then
         PAR.parse_specification_file (dossier         => filename,
                                       specification   => specification,
                                       opsys_focus     => platform_type,  --  unused
                                       arch_focus      => sysrootver.arch,
                                       success         => successful,
                                       stop_at_targets => False);
      else
         DNE (filename);
         return;
      end if;

      if not successful then
         TIO.Put_Line (errprefix & "Failed to parse " & specfile);
         TIO.Put_Line (PAR.get_parse_error);
         return;
      end if;

      PST.set_option_defaults
        (specs         => specification,
         variant       => specification.get_list_item (Port_Specification.sp_variants, 1),
         opsys         => platform_type,
         arch_standard => sysrootver.arch,
         osrelease     => HT.USS (sysrootver.release));

      if not specification.post_transform_option_group_defaults_passes then
         successful := False;
         return;
      end if;

      declare
         namebase    : String := specification.get_namebase;
         output_file : String := HT.USS (PM.configuration.dir_conspiracy) & "/bucket_" &
                                 UTL.bucket (palabra => namebase) & "/" & namebase;
      begin
         if save_it then
            FOP.mkdirp_from_filename (output_file);
            PSB.generator (specs       => specification,
                           ravensrcdir => ravensrcdir,
                           output_file => output_file);
            TIO.Put_Line (namebase & " buildsheet created at:");
            TIO.Put_Line (output_file);
         else
            PSB.generator (specs       => specification,
                           ravensrcdir => ravensrcdir,
                           output_file => "");
         end if;
      end;

   end generate_buildsheet;


   --------------------------------------------------------------------------------------------
   --  DNE
   --------------------------------------------------------------------------------------------
   procedure DNE (filename : String) is
   begin
      TIO.Put_Line (errprefix & "File " & LAT.Quotation & filename & LAT.Quotation &
                      " does not exist.");
   end DNE;


   --------------------------------------------------------------------------------------------
   --  TERM_defined_in_environment
   --------------------------------------------------------------------------------------------
   function TERM_defined_in_environment return Boolean
   is
      defined : constant Boolean := Unix.env_variable_defined ("TERM");
   begin
      if not defined then
         TIO.Put_Line ("Please define TERM in environment first and retry.");
      end if;
      return defined;
   end TERM_defined_in_environment;


   --------------------------------------------------------------------------------------------
   --  launch_clash_detected
   --------------------------------------------------------------------------------------------
   function launch_clash_detected return Boolean
   is
      violation  : Boolean;
      cwd        : constant String := DIR.Current_Directory;
      sysroot    : constant String := HT.USS (PM.configuration.dir_sysroot);
      portsdir   : constant String := HT.USS (PM.configuration.dir_conspiracy);
      distfiles  : constant String := HT.USS (PM.configuration.dir_distfiles);
      packages   : constant String := HT.USS (PM.configuration.dir_packages);
      ccache     : constant String := HT.USS (PM.configuration.dir_ccache);
      buildbase  : constant String := HT.USS (PM.configuration.dir_buildbase) & "/";
   begin
      case platform_type is
         when macos | openbsd =>
            violation :=
              HT.leads (cwd, sysroot & "/System") or else
              HT.leads (cwd, distfiles) or else
              HT.leads (cwd, packages) or else
              HT.leads (cwd, ccache) or else
              HT.leads (cwd, buildbase);
         when others =>
            violation :=
              HT.leads (cwd, sysroot) or else
              HT.leads (cwd, portsdir) or else
              HT.leads (cwd, distfiles) or else
              HT.leads (cwd, packages) or else
              HT.leads (cwd, ccache) or else
              HT.leads (cwd, buildbase);
      end case;
      if violation then
         TIO.Put_Line ("Please change the current directory; " &
                         "ravenadm is unable to launch from here.");
      end if;
      return violation;
   end launch_clash_detected;


   --------------------------------------------------------------------------------------------
   --  insufficient_privileges
   --------------------------------------------------------------------------------------------
   function insufficient_privileges return Boolean
   is
      function id_command return String;
      function id_command return String is
      begin
         case platform_type is
            when sunos  => return "/usr/xpg4/bin/id -u";
            when others => return "/usr/bin/id -u";
         end case;
      end id_command;

      status  : Integer;
      command : constant String := id_command;
      result  : String := HT.USS (Unix.piped_command (command, status));
   begin
      if status /= 0 then
         TIO.Put_Line ("command '" & command & "' failed.  Output=" & result);
         return True;
      end if;
      declare
         resint : constant Integer := Integer'Value (HT.first_line (result));
      begin
         if resint = 0 then
            return False;
         else
            --  Regular user called this
            return True;
         end if;
      end;
   end insufficient_privileges;


   --------------------------------------------------------------------------------------------
   --  already_running
   --------------------------------------------------------------------------------------------
   function already_running return Boolean
   is
      procedure remove_stale_pidfile;
      procedure remove_stale_pidfile is
      begin
         DIR.Delete_File (pidfile);
      exception
         when others =>
            --  silently ignore failure.  Likely ravenadm was launched by regular
            --  user after event that left root-created stale pid file.  We can
            --  clean it up the next time root executes this command
            null;
      end remove_stale_pidfile;
   begin
      if DIR.Exists (pidfile) then
         declare
            textpid : constant String := FOP.head_n1 (pidfile);
            command : constant String := "/bin/ps -p " & textpid;
            pid     : Integer;
            comres  : HT.Text;
            status  : Integer;
         begin
            --  test if valid by converting it (exception if fails)
            pid := Integer'Value (textpid);

            --  exception raised by line below if pid not found.
            comres := Unix.piped_command (command, status);
            if status = 0 and then
              HT.contains (comres, "ravenadm")
            then
               TIO.Put_Line ("ravenadm is already running on this system.");
               return True;
            else
               --  pidfile is obsolete, remove it.
               remove_stale_pidfile;
               return False;
            end if;
         exception
            when others =>
               --  pidfile contains garbage, remove it
               remove_stale_pidfile;
               return False;
         end;
      end if;
      return False;
   end already_running;


   --------------------------------------------------------------------------------------------
   --  create_pidfile
   --------------------------------------------------------------------------------------------
   procedure create_pidfile is
   begin
      FOP.create_pidfile (pidfile);
   end create_pidfile;


   --------------------------------------------------------------------------------------------
   --  destroy_pidfile
   --------------------------------------------------------------------------------------------
   procedure destroy_pidfile is
   begin
      FOP.destroy_pidfile (pidfile);
   end destroy_pidfile;


   --------------------------------------------------------------------------------------------
   --  previous_run_mounts_detected
   --------------------------------------------------------------------------------------------
   function previous_run_mounts_detected return Boolean is
   begin
      if REP.ravenadm_mounts_exist then
         TIO.Put_Line ("Builder mounts detected; attempting to remove them automatically ...");
         return True;
      else
         return False;
      end if;
   end previous_run_mounts_detected;


   --------------------------------------------------------------------------------------------
   --  previous_realfs_work_detected
   --------------------------------------------------------------------------------------------
   function previous_realfs_work_detected return Boolean is
   begin
      if REP.disk_workareas_exist then
         TIO.Put_Line ("Old work directories detected; " &
                         "attempting to remove them automatically ...");
         return True;
      else
         return False;
      end if;
   end previous_realfs_work_detected;


   --------------------------------------------------------------------------------------------
   --  old_mounts_successfully_removed
   --------------------------------------------------------------------------------------------
   function old_mounts_successfully_removed return Boolean is
   begin
      if REP.clear_existing_mounts then
         TIO.Put_Line ("Dismounting successful!");
         return True;
      end if;
      TIO.Put_Line ("The attempt failed.  Check for stuck or ongoing processes and kill them.");
      TIO.Put_Line ("After that try running ravenadm again or just manually unmount everything");
      TIO.Put_Line ("that is still attached to " & HT.USS (PM.configuration.dir_buildbase));
      return False;
   end old_mounts_successfully_removed;


   --------------------------------------------------------------------------------------------
   --  old_realfs_work_successfully_removed
   --------------------------------------------------------------------------------------------
   function old_realfs_work_successfully_removed return Boolean is
   begin
      if REP.clear_existing_workareas then
         TIO.Put_Line ("Directory removal successful!");
         return True;
      end if;
      TIO.Put_Line ("The attempt to remove the work directories located at ");
      TIO.Put_Line (HT.USS (PM.configuration.dir_buildbase) & " failed.");
      TIO.Put_Line ("Please remove them manually before continuing.");
      return False;
   end old_realfs_work_successfully_removed;


   --------------------------------------------------------------------------------------------
   --  ravenexec_missing
   --------------------------------------------------------------------------------------------
   function ravenexec_missing return Boolean is
   begin
      if DIR.Exists (ravenexec) then
         return False;
      end if;
      TIO.Put_Line (ravenexec & " missing!" & bailing);
      return True;
   end ravenexec_missing;


   --------------------------------------------------------------------------------------------
   --  launch_configure_menu
   --------------------------------------------------------------------------------------------
   procedure launch_configure_menu is
   begin
      Configure.launch_configure_menu;
   end launch_configure_menu;


   --------------------------------------------------------------------------------------------
   --  locate
   --------------------------------------------------------------------------------------------
   procedure locate (candidate : String)
   is
      suffix    : String := "/bucket_" & UTL.bucket (candidate) & LAT.Solidus & candidate;
      should_be : String := HT.USS (PM.configuration.dir_conspiracy) & suffix;
      customloc : String := HT.USS (PM.configuration.dir_unkindness) & suffix;
   begin
      if candidate = "" then
         TIO.Put_Line ("The locate command requires the port's name base as an argument");
         return;
      end if;
      if DIR.Exists (should_be) then
         TIO.Put_Line ("Found at " & should_be);
      elsif not HT.equivalent (PM.configuration.dir_unkindness, PM.no_unkindness) and then
        DIR.Exists (customloc)
      then
         TIO.Put_Line ("Custom port found at " & customloc);
      else
         TIO.Put_Line ("Does not exist at " & should_be);
      end if;
   end locate;


   --------------------------------------------------------------------------------------------
   --  locate
   --------------------------------------------------------------------------------------------
   function slave_platform_determined return Boolean
   is
      base : String := HT.USS (PM.configuration.dir_sysroot) & "/usr/share/";
      F1   : String := base & "OSRELEASE";
      F2   : String := base & "OSMAJOR";
      F3   : String := base & "OSVERSION";
      F4   : String := base & "STDARCH";
   begin
      if not DIR.Exists (F1) or else
        not DIR.Exists (F2) or else
        not DIR.Exists (F3) or else
        not DIR.Exists (F4)
      then
         TIO.Put_Line ("Platform type could not be determined (sysroot F1-F4 missing)");
         return False;
      end if;
      sysrootver.release := HT.SUS (FOP.head_n1 (F1));
      sysrootver.major   := HT.SUS (FOP.head_n1 (F2));
      sysrootver.version := HT.SUS (FOP.head_n1 (F3));
      declare
         candidate : String := FOP.head_n1 (F4);
      begin
         if UTL.valid_cpu_arch (candidate) then
            sysrootver.arch := UTL.convert_cpu_arch (candidate);
         else
            TIO.Put_Line ("Platform type could not be determined (STDARCH conversion failed)");
            return False;
         end if;
      end;
      return True;
   end slave_platform_determined;


   --------------------------------------------------------------------------------------------
   --  fully_scan_ports_tree
   --------------------------------------------------------------------------------------------
   function fully_scan_ports_tree return Boolean
   is
      successful : Boolean;
   begin
      if not unkindness_index_current then
         return False;
      end if;
      successful := SCN.scan_entire_ports_tree (sysrootver);
      if successful then
         SCN.set_build_priority;
         if PortScan.queue_is_empty then
            successful := False;
            TIO.Put_Line ("There are no valid ports to build." & bailing);
         end if;
      else
         if Signals.graceful_shutdown_requested then
            TIO.Put_Line (shutreq);
         else
            TIO.Put_Line ("Failed to scan ports tree " & bailing);
         end if;
      end if;
      return successful;
   end fully_scan_ports_tree;


   --------------------------------------------------------------------------------------------
   --  scan_stack_of_single_ports
   --------------------------------------------------------------------------------------------
   function scan_stack_of_single_ports (always_build : Boolean) return Boolean
   is
      successful : Boolean;
   begin
      --  unkindness index generated at store_origins routine (can't get this far if failed)
      successful := SCN.scan_provided_list_of_ports (always_build, sysrootver);
      if successful then
         SCN.set_build_priority;
         if PortScan.queue_is_empty then
            successful := False;
            TIO.Put_Line ("There are no valid ports to build." & bailing);
         end if;
      else
         if Signals.graceful_shutdown_requested then
            TIO.Put_Line (shutreq);
         end if;
      end if;
      return successful;
   end scan_stack_of_single_ports;


   --------------------------------------------------------------------------------------------
   --  sanity_check_then_prefail
   --------------------------------------------------------------------------------------------
   function sanity_check_then_prefail
     (delete_first : Boolean := False;
      dry_run      : Boolean := False) return Boolean
   is
      ptid              : PortScan.port_id;
      num_skipped       : Natural;
      block_remote      : Boolean := True;
      explicit_ravensys : Boolean := False;
      update_external_repo : constant String := host_pkg8 & " update --quiet --repository ";
      no_packages : constant String := "No prebuilt packages will be used as a result.";

   begin
      LOG.set_overall_start_time (CAL.Clock);

      if delete_first and then not dry_run then
         OPS.delete_existing_packages_of_ports_list;
      end if;

      if PM.configuration.defer_prebuilt then
         --  Before any remote operations, find the external repo
         if OPS.located_external_repository then
            block_remote := False;
            --  We're going to use prebuilt packages if available, so let's
            --  prepare for that case by updating the external repository
            TIO.Put ("Stand by, updating external repository catalogs ... ");
            if Unix.external_command (update_external_repo & OPS.top_external_repository) then
               TIO.Put_Line ("done.");
            else
               TIO.Put_Line ("Failed!");
               TIO.Put_Line ("The external repository could not be updated.");
               TIO.Put_Line (no_packages);
               block_remote := True;
            end if;
         else
            TIO.Put_Line ("The external repository does not seem to be configured.");
            TIO.Put_Line (no_packages);
         end if;
      end if;

      if delete_first then
         explicit_ravensys := PortScan.jail_env_port_specified;
      end if;

      OPS.run_start_hook;
      OPS.limited_sanity_check (repository       => HT.USS (PM.configuration.dir_repository),
                                dry_run          => dry_run,
                                rebuild_compiler => explicit_ravensys,
                                suppress_remote  => block_remote,
                                major_release    => HT.USS (sysrootver.major),
                                architecture     => sysrootver.arch);
      LOG.set_build_counters (PortScan.queue_length, 0, 0, 0, 0);
      if dry_run then
         return True;
      end if;

      if Signals.graceful_shutdown_requested then
         TIO.Put_Line (shutreq);
         return False;
      end if;

      OPS.delete_existing_web_history_files;

      LOG.start_logging (PortScan.total);
      LOG.start_logging (PortScan.ignored);
      LOG.start_logging (PortScan.skipped);
      LOG.start_logging (PortScan.success);
      LOG.start_logging (PortScan.failure);

      loop
         ptid := OPS.next_ignored_port;
         exit when not PortScan.valid_port_id (ptid);
         exit when Signals.graceful_shutdown_requested;
         LOG.increment_build_counter (PortScan.ignored);
         LOG.scribe (PortScan.total, LOG.elapsed_now & " " & PortScan.get_port_variant (ptid) &
                     " has been ignored: " & PortScan.ignore_reason (ptid), False);
         LOG.scribe (PortScan.ignored, LOG.elapsed_now & " " & PortScan.get_port_variant (ptid) &
                         "   ## reason: " & PortScan.ignore_reason (ptid), False);
         OPS.cascade_failed_build (id         => ptid,
                                   numskipped => num_skipped);
         OPS.record_history_ignored (elapsed   => LOG.elapsed_now,
                                     bucket    => PortScan.get_bucket (ptid),
                                     origin    => PortScan.get_port_variant (ptid),
                                     reason    => PortScan.ignore_reason (ptid),
                                     skips     => num_skipped);
         LOG.increment_build_counter (PortScan.skipped, num_skipped);
      end loop;
      LOG.stop_logging (PortScan.ignored);
      LOG.scribe (PortScan.total, LOG.elapsed_now & " Sanity check complete. "
                  & "Ports remaining to build:" & PortScan.queue_length'Img, True);
      if Signals.graceful_shutdown_requested then
         TIO.Put_Line (shutreq);
      else
         if OPS.integrity_intact then
            return True;
         end if;
      end if;
      --  If here, we either got control-C or failed integrity check
      if not Signals.graceful_shutdown_requested then
         TIO.Put_Line ("Queue integrity lost! " & bailing);
      end if;
      LOG.stop_logging (PortScan.total);
      LOG.stop_logging (PortScan.skipped);
      LOG.stop_logging (PortScan.success);
      LOG.stop_logging (PortScan.failure);
      return False;
   end sanity_check_then_prefail;


   --------------------------------------------------------------------------------------------
   --  perform_bulk_run
   --------------------------------------------------------------------------------------------
   procedure perform_bulk_run (testmode : Boolean)
   is
      num_builders : constant builders := PM.configuration.num_builders;
      show_tally   : Boolean := True;
   begin
      if PortScan.queue_is_empty then
         TIO.Put_Line ("After inspection, it has been determined that there are no packages that");
         TIO.Put_Line ("require rebuilding; the task is therefore complete.");
         show_tally := False;
      else
         REP.initialize (testmode);
         CYC.initialize (testmode);
         OPS.initialize_web_report (num_builders);
         OPS.initialize_display (num_builders);
         OPS.parallel_bulk_run (num_builders, sysrootver);
         REP.finalize;
      end if;
      LOG.set_overall_complete (CAL.Clock);
      LOG.stop_logging (PortScan.total);
      LOG.stop_logging (PortScan.success);
      LOG.stop_logging (PortScan.failure);
      LOG.stop_logging (PortScan.skipped);
      if show_tally then
         TIO.Put_Line (LAT.LF & LAT.LF);
         TIO.Put_Line ("The task is complete.  Final tally:");
         TIO.Put_Line ("Initial queue size:" & LOG.port_counter_value (PortScan.total)'Img);
         TIO.Put_Line ("    packages built:" & LOG.port_counter_value (PortScan.success)'Img);
         TIO.Put_Line ("           ignored:" & LOG.port_counter_value (PortScan.ignored)'Img);
         TIO.Put_Line ("           skipped:" & LOG.port_counter_value (PortScan.skipped)'Img);
         TIO.Put_Line ("            failed:" & LOG.port_counter_value (PortScan.failure)'Img);
         TIO.Put_Line ("");
         TIO.Put_Line (LOG.bulk_run_duration);
         TIO.Put_Line ("The build logs can be found at: " &
                         HT.USS (PM.configuration.dir_logs) & "/logs");
      end if;
   end perform_bulk_run;


   --------------------------------------------------------------------------------------------
   --  store_origins
   --------------------------------------------------------------------------------------------
   function store_origins (start_from : Positive) return Boolean
   is
      --  format is <namebase>:<variant>
   begin
      if CLI.Argument_Count < 2 then
         TIO.Put_Line ("This command requires a list of one or more port origins.");
         return False;
      end if;
      if not unkindness_index_current then
         TIO.Put_Line ("Failed to generate unkindness index.");
         return False;
      end if;
      all_stdvar := True;
      if CLI.Argument_Count = 2 then
         --  Check if this is a file
         declare
            potential_file : String renames CLI.Argument (2);
            use type DIR.File_Kind;
         begin
            if DIR.Exists (potential_file) and then
              DIR.Kind (potential_file) = DIR.Ordinary_File
            then
               return valid_origin_file (potential_file);
            end if;
         end;
      end if;
      for k in start_from .. CLI.Argument_Count loop
         declare
            Argk : constant String := CLI.Argument (k);
            bad_namebase : Boolean;
            bad_format   : Boolean;
            add_standard : Boolean;
            is_stdvar    : Boolean;
         begin
            if valid_origin (Argk, bad_namebase, bad_format, add_standard, is_stdvar) then
               if add_standard then
                  PortScan.insert_into_portlist (Argk & LAT.Colon & variant_standard);
               else
                  PortScan.insert_into_portlist (Argk);
               end if;
               if not is_stdvar then
                  all_stdvar := False;
               end if;
            else
               if bad_format then
                  TIO.Put_Line (badformat & "'" & Argk & "'");
               elsif bad_namebase then
                  TIO.Put_Line (badname & "'" & Argk & "'");
               else
                  if HT.contains (Argk, ":") then
                     TIO.Put_Line (badvariant & "'" & Argk & "'");
                  else
                     TIO.Put_Line (badvariant & "(:standard)'" & Argk & "'");
                  end if;
               end if;
               return False;
            end if;
         end;
      end loop;
      return True;
   end store_origins;


   --------------------------------------------------------------------------------------------
   --  valid_origin
   --------------------------------------------------------------------------------------------
   function valid_origin
     (port_variant : String;
      bad_namebase : out Boolean;
      bad_format   : out Boolean;
      assume_std   : out Boolean;
      known_std    : out Boolean) return Boolean
   is
      function variant_valid (fileloc : String; variant : String) return Boolean;

      numcolons  : constant Natural := HT.count_char (port_variant, LAT.Colon);
      num_spaces : constant Natural := HT.count_char (port_variant, LAT.Space);

      function variant_valid (fileloc : String; variant : String) return Boolean
      is
         contents    : String := FOP.get_file_contents (fileloc);
         variants    : constant String := "VARIANTS=" & LAT.HT & LAT.HT;
         single_LF   : constant String (1 .. 1) := (1 => LAT.LF);
         variantsvar : Natural := AS.Fixed.Index (contents, variants);
      begin
         if variantsvar = 0 then
            return False;
         end if;
         declare
            nextlf : Natural := AS.Fixed.Index (Source  => contents,
                                                Pattern => single_LF,
                                                From    => variantsvar);
            special : String :=
              HT.strip_excessive_spaces (contents (variantsvar + variants'Length .. nextlf - 1));
         begin
            if nextlf = 0 then
               return False;
            end if;
            return
              special = variant or else
              (special'First + variant'Length <= special'Last and then
               special (special'First .. special'First + variant'Length) = variant & " ") or else
              (special'Last > variant'Length and then
               special (special'Last - variant'Length .. special'Last) = " " & variant) or else
              AS.Fixed.Index (special, " " & variant & " ") /= 0;
         end;
      end variant_valid;
   begin
      bad_namebase := True;
      assume_std   := False;
      if num_spaces > 0 or else numcolons > 1 then
         bad_format := True;
         return False;
      end if;
      bad_format := False;

      if numcolons = 1 then
         declare
            namebase   : String := HT.part_1 (port_variant, ":");
            variant    : String := HT.part_2 (port_variant, ":");
            bsheetname : String := "/bucket_" & UTL.bucket (namebase) & "/" & namebase;
            bsheetc    : String := HT.USS (PM.configuration.dir_conspiracy) & bsheetname;
            bsheetu    : String := HT.USS (PM.configuration.dir_profile) &
                                   "/unkindness" & bsheetname;
         begin
            known_std := (variant = variant_standard);
            if DIR.Exists (bsheetu) then
               bad_namebase := False;
               return variant_valid (bsheetu, variant);
            elsif  DIR.Exists (bsheetc) then
               bad_namebase := False;
               return variant_valid (bsheetc, variant);
            else
               return False;
            end if;
         end;
      else
         declare
            bsheetname : String := "/bucket_" & UTL.bucket (port_variant) & "/" & port_variant;
            bsheetc    : String := HT.USS (PM.configuration.dir_conspiracy) & bsheetname;
            bsheetu    : String := HT.USS (PM.configuration.dir_profile) &
                                   "/unkindness" & bsheetname;
         begin
            assume_std := True;
            known_std  := True;
            if DIR.Exists (bsheetu) then
               bad_namebase := False;
               return variant_valid (bsheetu, variant_standard);
            elsif  DIR.Exists (bsheetc) then
               bad_namebase := False;
               return variant_valid (bsheetc, variant_standard);
            else
               return False;
            end if;
         end;
      end if;
   end valid_origin;


   --------------------------------------------------------------------------------------------
   --  valid_origin_file
   --------------------------------------------------------------------------------------------
   function valid_origin_file (regular_file : String) return Boolean
   is
      origin_list : constant String := FOP.get_file_contents (regular_file);
      markers     : HT.Line_Markers;
      good        : Boolean := True;
      total       : Natural := 0;
   begin
      HT.initialize_markers (origin_list, markers);
      loop
         exit when not HT.next_line_present (origin_list, markers);
         declare
            line         : constant String := HT.extract_line (origin_list, markers);
            bad_namebase : Boolean;
            bad_format   : Boolean;
            add_standard : Boolean;
            is_stdvar    : Boolean;
         begin
            if not HT.IsBlank (line) then
               if valid_origin (line, bad_namebase, bad_format, add_standard, is_stdvar) then
                  if add_standard then
                     PortScan.insert_into_portlist (line & LAT.Colon & variant_standard);
                  else
                     PortScan.insert_into_portlist (line);
                  end if;
                  if not is_stdvar then
                     all_stdvar := False;
                  end if;
                  total := total + 1;
               else
                  if bad_format then
                     TIO.Put_Line (badformat & "'" & line & "'");
                  elsif bad_namebase then
                     TIO.Put_Line (badname & "'" & line & "'");
                  else
                     TIO.Put_Line (badvariant & "'" & line & "'");
                  end if;
                  good := False;
                  exit;
               end if;
            end if;
         end;
      end loop;
      return (total > 0) and then good;
   end valid_origin_file;


   --------------------------------------------------------------------------------------------
   --  print_spec_template
   --------------------------------------------------------------------------------------------
   procedure print_spec_template (save_command : String)
   is
      save_here : constant Boolean := (save_command = "save");
   begin
      PSB.print_specification_template (save_here);
   end print_spec_template;


   --------------------------------------------------------------------------------------------
   --  generate_distinfo
   --------------------------------------------------------------------------------------------
   procedure generate_distinfo
   is
      portloc  : String := HT.USS (PM.configuration.dir_buildbase) & ss_base & "/port";
      makefile : String := portloc & "/Makefile";
      successful    : Boolean;
      specification : Port_Specification.Portspecs;
   begin
      if not DIR.Exists (specfile) then
         TIO.Put_Line ("No specification file found in current directory.");
         return;
      end if;
      REP.initialize (testmode  => False);
      REP.launch_slave (scan_slave);
      PAR.parse_specification_file (dossier         => specfile,
                                    specification   => specification,
                                    opsys_focus     => platform_type,
                                    arch_focus      => x86_64,
                                    success         => successful,
                                    stop_at_targets => True,
                                    extraction_dir  => portloc);
      if not successful then
         TIO.Put_Line (errprefix & "Failed to parse " & specfile);
         TIO.Put_Line (PAR.get_parse_error);
         goto endzone;
      end if;
      declare
         variant : String := specification.get_list_item (Port_Specification.sp_variants, 1);
      begin
         PSM.generator (specs       => specification,
                        variant     => variant,
                        opsys       => platform_type,
                        arch        => x86_64,
                        output_file => makefile);
      end;

      CYC.run_makesum (scan_slave);
      <<endzone>>
      REP.destroy_slave (scan_slave);
      REP.finalize;
   end generate_distinfo;


   --------------------------------------------------------------------------------------------
   --  bulk_run_then_interact_with_final_port
   --------------------------------------------------------------------------------------------
   procedure bulk_run_then_interact_with_final_port
   is
      brkphase : constant String := Unix.env_variable_value (brkname);
      buildres : Boolean;
      ptid     : PortScan.port_id := OPS.unlist_first_port;
      pvname   : constant String  := PortScan.get_port_variant (ptid);
      use_proc : constant Boolean := PortScan.requires_procfs (ptid);
   begin
      if not PortScan.valid_port_id (ptid) then
         TIO.Put_Line ("Failed to remove first port from list." & bailing);
         return;
      end if;
      perform_bulk_run (testmode => True);
      if Signals.graceful_shutdown_requested then
         return;
      end if;
      if LOG.port_counter_value (PortScan.ignored) > 0 or else
        LOG.port_counter_value (PortScan.skipped) > 0 or else
        LOG.port_counter_value (PortScan.failure) > 0
      then
         TIO.Put_Line ("It appears a prerequisite failed, so the interactive build of");
         TIO.Put_Line (pvname & " has been cancelled.");
         return;
      end if;
      TIO.Put_Line ("Starting interactive build of " & pvname);
      TIO.Put_Line ("Stand by, building up to the point requested ...");

      REP.initialize (testmode => True);
      CYC.initialize (test_mode => True);
      REP.launch_slave (scan_slave, use_proc);
      Unix.cone_of_silence (deploy => False);

      buildres := OPS.build_subpackages (builder     => scan_slave,
                                         sequence_id => ptid,
                                         sysrootver  => sysrootver,
                                         interactive => True,
                                         enterafter  => brkphase);

      REP.destroy_slave (scan_slave, use_proc);
      REP.finalize;
   end bulk_run_then_interact_with_final_port;


   --------------------------------------------------------------------------------------------
   --  interact_with_single_builder
   --------------------------------------------------------------------------------------------
   function interact_with_single_builder return Boolean
   is
      EA_defined : constant Boolean := Unix.env_variable_defined (brkname);
   begin
      if PortScan.build_request_length /= 1 then
         return False;
      end if;
      if not EA_defined then
         return False;
      end if;
      return CYC.valid_test_phase (Unix.env_variable_value (brkname));
   end interact_with_single_builder;


   --------------------------------------------------------------------------------------------
   --  install_compiler_packages
   --------------------------------------------------------------------------------------------
   function install_compiler_packages return Boolean
   is
      function get_package_name (subpackage : String; use_prev, xz_pkg : Boolean) return String;
      function package_copy (subpackage : String) return Boolean;
      function dupe_archive (origin, destino : String) return Boolean;

      binutils : constant String := "binutils";

      function get_package_name (subpackage : String; use_prev, xz_pkg : Boolean) return String
      is
         function pkg_format return String;
         function pkg_name (vsn_binutils, vsn_compiler : String) return String;

         function pkg_format return String is
         begin
            if xz_pkg then
               return ".txz";
            else
               return ".tzst";
            end if;
         end pkg_format;

         function pkg_name (vsn_binutils, vsn_compiler : String) return String is
         begin
            if subpackage = binutils then
               return "binutils-single-ravensys-" & vsn_binutils & pkg_format;
            else
               return default_compiler & LAT.Hyphen & subpackage & LAT.Hyphen &
                 variant_standard & LAT.Hyphen & vsn_compiler & pkg_format;
            end if;
         end pkg_name;
      begin
         if use_prev then
            return pkg_name (previous_binutils, previous_compiler);
         else
            return pkg_name (binutils_version, compiler_version);
         end if;
      end get_package_name;

      function dupe_archive (origin, destino : String) return Boolean is
      begin
         DIR.Copy_File (Source_Name => origin, Target_Name => destino);
         return True;
      exception
         when others =>
            TIO.Put_Line ("Failed to copy " & origin & " to " & destino);
            return False;
      end dupe_archive;

      function package_copy (subpackage : String) return Boolean
      is
         pkgname   : constant String := get_package_name (subpackage, False, False);
         tool_path : constant String := HT.USS (PM.configuration.dir_toolchain) & "/share/";
         src_path  : constant String := tool_path & pkgname;
         dest_dir  : constant String := HT.USS (PM.configuration.dir_repository);
         dest_path : constant String := dest_dir & LAT.Solidus & pkgname;
      begin
         begin
            if not DIR.Exists (dest_dir) then
               DIR.Create_Directory (dest_dir);
            end if;
         exception
            when DIR.Use_Error =>
               TIO.Put_Line ("Failed to create " & dest_dir & " (repository directory)");
               return False;
            when issue : others =>
               TIO.Put_Line ("install_compiler_packages error: " &
                               EX.Exception_Information (issue));
               return False;
         end;
         if DIR.Exists (dest_path) then
            return True;
         else
            if DIR.Exists (src_path) then
               return dupe_archive (origin => src_path, destino => dest_path);
            else
               --  We didn't find the current binutils or compiler in the system root storage.
               --  It's likely that we're in a transition with a new version of binutils or
               --  gcc available, and a new system root needs to be generated.  Assuming this,
               --  try to copy the previously known compiler/binutils under the new name so
               --  that package building doesn't break.
               --  Also try the "current" binutils but with the old ".txz" format.
               declare
                  old_pkg  : constant String := get_package_name (subpackage, True, False);
                  old_path : constant String := tool_path & old_pkg;
               begin
                  if DIR.Exists (old_path) then
                     return dupe_archive (origin => old_path, destino => dest_path);
                  else
                     declare
                        txz_pkg  : constant String := get_package_name (subpackage, False, True);
                        txz_path : constant String := tool_path & txz_pkg;
                     begin
                        if DIR.Exists (txz_path) then
                           return dupe_archive (origin => txz_path, destino => dest_path);
                        else
                           TIO.Put_Line ("None of the following compiler packages were found:");
                           TIO.Put_Line (src_path);
                           TIO.Put_Line (old_path);
                           TIO.Put_Line (txz_path);
                           return False;
                        end if;
                     end;
                  end if;
               end;
            end if;
         end if;
      exception
         when issue : others =>
            TIO.Put_Line ("install_compiler_packages error: " & EX.Exception_Information (issue));
            return False;
      end package_copy;
   begin
      return
        package_copy (binutils) and then
        package_copy ("ada_run") and then
        package_copy ("compilers") and then
        package_copy ("complete") and then
        package_copy ("cxx_run") and then
        package_copy ("fortran_run") and then
        package_copy ("infopages") and then
        package_copy ("libs");
   end install_compiler_packages;


   --------------------------------------------------------------------------------------------
   --  generate_ports_index
   --------------------------------------------------------------------------------------------
   procedure generate_ports_index is
   begin
      PortScan.Scan.generate_conspiracy_index (sysrootver);
   end generate_ports_index;


   --------------------------------------------------------------------------------------------
   --  display_results_of_dry_run
   --------------------------------------------------------------------------------------------
   procedure display_results_of_dry_run is
   begin
      PortScan.Scan.display_results_of_dry_run;
   end display_results_of_dry_run;


   --------------------------------------------------------------------------------------------
   --  purge_distfiles
   --------------------------------------------------------------------------------------------
   procedure purge_distfiles is
   begin
      if unkindness_index_current and then
        PortScan.Scan.gather_distfile_set (sysrootver)
      then
         PortScan.Scan.purge_obsolete_distfiles;
      end if;
   end purge_distfiles;


   --------------------------------------------------------------------------------------------
   --  purge_logs
   --------------------------------------------------------------------------------------------
   procedure purge_logs is
   begin
      PortScan.Scan.gather_list_of_build_logs;
      PortScan.Scan.eliminate_current_logs (main_tree => True);
      PortScan.Scan.eliminate_current_logs (main_tree => False);
      PortScan.Scan.remove_obsolete_logs;
   end purge_logs;


   --------------------------------------------------------------------------------------------
   --  change_options
   --------------------------------------------------------------------------------------------
   procedure change_options
   is
      number_ports : constant Natural := PortScan.build_request_length;
      issues : HT.Text;
   begin
      --  First confirm all given origins are the standard variants
      if not all_stdvar then
         TIO.Put_Line ("User error: Only standard variants of ports have configurable options");
         return;
      end if;
      for x in 1 .. number_ports loop
         declare
            specification : Port_Specification.Portspecs;
            successful    : Boolean;
            buildsheet    : constant String := PortScan.get_buildsheet_from_origin_list (x);
         begin
            OPS.parse_and_transform_buildsheet (specification => specification,
                                                successful    => successful,
                                                buildsheet    => buildsheet,
                                                variant       => variant_standard,
                                                portloc       => "",
                                                excl_targets  => True,
                                                avoid_dialog  => True,
                                                for_webpage   => False,
                                                sysrootver    => sysrootver);
            if not specification.standard_options_present then
               HT.SU.Append (issues, "User error: The " & specification.get_namebase &
                               " has no options to configure." & LAT.LF);
            else
               exit when not OPT.launch_dialog (specification);
            end if;
         end;
      end loop;
      if not HT.IsBlank (issues) then
         TIO.Put (HT.USS (issues));
      end if;
   end change_options;


   --------------------------------------------------------------------------------------------
   --  check_ravenports_version
   --------------------------------------------------------------------------------------------
   procedure check_ravenports_version is
   begin
      Ravenports.check_version_available;
   end check_ravenports_version;


   --------------------------------------------------------------------------------------------
   --  update_to_latest_ravenports
   --------------------------------------------------------------------------------------------
   procedure update_to_latest_ravenports is
   begin
      Ravenports.retrieve_latest_ravenports;
   end update_to_latest_ravenports;


   --------------------------------------------------------------------------------------------
   --  generate_repository
   --------------------------------------------------------------------------------------------
   procedure generate_repository is
   begin
      if fully_scan_ports_tree then
         Repository.rebuild_local_respository (remove_invalid_packages => True);
      end if;
   end generate_repository;


   --------------------------------------------------------------------------------------------
   --  generate_repository
   --------------------------------------------------------------------------------------------
   procedure check_that_ravenadm_is_modern_enough
   is
      raverreq : constant String := HT.USS (PM.configuration.dir_conspiracy) & "/Mk/Misc/raverreq";
   begin
      if not DIR.Exists (raverreq) then
         return;
      end if;
      declare
         filecon : constant String := FOP.get_file_contents (raverreq);
         ravenadm_ver : constant Integer :=
           Integer'Value (raven_version_major) * 100 +
           Integer'Value (raven_version_minor);
         required_ver : constant Integer := Integer'Value (HT.first_line (filecon));
         stars : constant String (1 .. 51) := (others => '*');
         Ch : Character;
      begin
         if ravenadm_ver < required_ver then
            TIO.Put_Line
              (LAT.LF & stars & LAT.LF &
                 "*** Please upgrade ravenadm as soon as possible ***" &
                 LAT.LF & stars & LAT.LF & LAT.LF &
                 "Either build and install ravenadm, or upgrade the ravenports package" & LAT.LF &
                 "This version of ravenadm will not recognize all directives in some ports.");
            if Unix.env_variable_defined ("TERM") then
               TIO.Put ("Press any key to acknowledge:");
               Ada.Text_IO.Get_Immediate (Ch);
               TIO.Put_Line ("");
            end if;
         end if;
      end;
   exception
      when others =>
         TIO.Put_Line ("check_that_ravenadm_is_modern_enough: exception");

   end check_that_ravenadm_is_modern_enough;


   --------------------------------------------------------------------------------------------
   --  generate_website
   --------------------------------------------------------------------------------------------
   procedure generate_website
   is
      www_site : constant String := HT.USS (PM.configuration.dir_profile) & "/www";
   begin
      if not DIR.Exists (www_site) then
         DIR.Create_Path (www_site);
      end if;
      if SCN.generate_entire_website (www_site, sysrootver) then
         TIO.Put_Line ("The web site generation is complete.");
      else
         TIO.Put_Line ("The web site generation was not entirely successful.");
      end if;
   end generate_website;


   --------------------------------------------------------------------------------------------
   --  regenerate_patches
   --------------------------------------------------------------------------------------------
   procedure regenerate_patches (optional_directory : String;
                                 optional_variant : String)
   is
      function get_variant return String;
      function get_buildsheet return String;

      directory_specified : constant Boolean := (optional_directory /= "");
      successful : Boolean;

      portloc       : String := HT.USS (PM.configuration.dir_buildbase) & ss_base & "/port";
      specification : Port_Specification.Portspecs;
      spec_found    : Boolean := False;

      function get_variant return String is
      begin
         if optional_variant = "" then
            return variant_standard;
         else
            return optional_variant;
         end if;
      end get_variant;

      function get_buildsheet return String
      is
         dossier_text : HT.Text;
         presuccess   : Boolean;
         prespec      : Port_Specification.Portspecs;
      begin
         if directory_specified then
            dossier_text := HT.SUS (optional_directory & "/" & specfile);
         else
            dossier_text := HT.SUS (specfile);
         end if;
         if DIR.Exists (HT.USS (dossier_text)) then
            PAR.parse_specification_file (dossier         => HT.USS (dossier_text),
                                          specification   => prespec,
                                          opsys_focus     => platform_type,
                                          arch_focus      => x86_64,
                                          success         => presuccess,
                                          stop_at_targets => True);
            if presuccess then
               declare
                  namebase  : String := prespec.get_namebase;
                  buckname  : String := "/bucket_" & UTL.bucket (namebase) & "/" & namebase;
                  savedspec : String := HT.USS (PM.configuration.dir_conspiracy) & buckname;
                  custspec  : String := HT.USS (PM.configuration.dir_profile) &
                                        "/unkindness" & buckname;
               begin
                  if DIR.Exists (custspec) then
                     spec_found := True;
                     return custspec;
                  else
                     if DIR.Exists (savedspec) then
                        spec_found := True;
                        return savedspec;
                     else
                        DNE (savedspec);
                     end if;
                  end if;
               end;
            end if;
         end if;
         return "unused";
      end get_buildsheet;

      selected_variant : constant String := get_variant;
      dossier          : constant String := get_buildsheet;

   begin
      if spec_found then
         REP.initialize (testmode  => False);
         REP.launch_slave (scan_slave);
         OPS.parse_and_transform_buildsheet (specification => specification,
                                             successful    => successful,
                                             buildsheet    => dossier,
                                             variant       => selected_variant,
                                             portloc       => portloc,
                                             excl_targets  => False,
                                             avoid_dialog  => True,
                                             for_webpage   => False,
                                             sysrootver    => sysrootver);
         CYC.run_patch_regen (id => scan_slave, sourceloc => optional_directory);
         REP.destroy_slave (scan_slave);
         REP.finalize;
      end if;
   end regenerate_patches;


   --------------------------------------------------------------------------------------------
   --  resort_manifests
   --------------------------------------------------------------------------------------------
   procedure resort_manifests (sourcedir : String)
   is
      function assume_dot (source : String) return String;
      function assume_dot (source : String) return String is
      begin
         if source = "" then
            return ".";
         else
            return source;
         end if;
      end assume_dot;
      portsrc     : constant String := Unix.true_path (assume_dot (sourcedir));
      manifestdir : constant String := portsrc & "/manifests";
   begin
      if not DIR.Exists (manifestdir) then
         TIO.Put_Line ("Sort ignored: No manifests directory detected");
         return;
      end if;
      declare
         use type DIR.File_Kind;
      begin
         if not (DIR.Kind (manifestdir) = DIR.Directory) then
            TIO.Put_Line ("Manifest sort failed because " & manifestdir & " is not a directory");
            return;
         end if;
      end;
      declare
         search : DIR.Search_Type;
         dirent : DIR.Directory_Entry_Type;
         filter : constant DIR.Filter_Type := (DIR.Ordinary_File => True, others => False);
      begin
         DIR.Start_Search (Search    => search,
                           Directory => manifestdir,
                           Pattern   => "plist.*",
                           Filter    => filter);
         while DIR.More_Entries (search) loop
            DIR.Get_Next_Entry (search, dirent);
            declare
               sname : constant String := DIR.Simple_Name (dirent);
               plist : constant String := manifestdir & "/" & sname;
            begin
               MAN.sort_manifest (MAN.Filename (plist));
               TIO.Put_Line ("Sort complete: " & sname);
            exception
               when others =>
                  TIO.Put_Line ("Failed to sort " & plist & " manifest");
            end;
         end loop;
         DIR.End_Search (search);
      end;
   end resort_manifests;


   --------------------------------------------------------------------------------------------
   --  unkindness_index_current
   --------------------------------------------------------------------------------------------
   function unkindness_index_current return Boolean
   is
      require_new_index : Boolean;
   begin
      require_new_index := SCN.unkindness_index_required;
      if require_new_index then
         return SCN.generate_unkindness_index (sysrootver);
      else
         return True;
      end if;
   end unkindness_index_current;


   --------------------------------------------------------------------------------------------
   --  show_config_value
   --------------------------------------------------------------------------------------------
   procedure show_config_value (AQvalue : String)
   is
      errmsg : constant String := "Configuration info command requires 'A' .. 'Q' argument";
      option : Character;
   begin
      if HT.IsBlank (AQvalue) then
         TIO.Put_Line (errmsg);
      else
         option := AQvalue (AQvalue'First);
         case option is
            when 'A' .. 'Z' => Configure.print_configuration_value (option);
            when others => TIO.Put_Line (errmsg);
         end case;
      end if;
   end show_config_value;

end Pilot;
