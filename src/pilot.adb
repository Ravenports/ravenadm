--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Calendar;
with Ada.Text_IO;
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
with PortScan.Operations;
with PortScan.Buildcycle;
with PortScan.Scan;
with PortScan.Log;

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
   package OPS renames PortScan.Operations;
   package CYC renames PortScan.Buildcycle;
   package SCN renames PortScan.Scan;
   package LOG renames PortScan.Log;
   package LAT renames Ada.Characters.Latin_1;
   package CLI renames Ada.Command_Line;
   package DIR renames Ada.Directories;
   package CAL renames Ada.Calendar;
   package TIO renames Ada.Text_IO;
   package AS  renames Ada.Strings;

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
   --  generate_makefile
   --------------------------------------------------------------------------------------------
   procedure generate_makefile (optional_directory : String;
                                optional_variant : String)
   is
      function get_variant return String;

      directory_specified : constant Boolean := (optional_directory /= "");
      successful : Boolean;

      specification : Port_Specification.Portspecs;

      function get_variant return String is
      begin
         if optional_variant = "" then
            return variant_standard;
         else
            return optional_variant;
         end if;
      end get_variant;

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
         PST.set_option_defaults (specs         => specification,
                                  variant       => get_variant,
                                  opsys         => platform_type,
                                  arch_standard => sysrootver.arch,
                                  osrelease     => HT.USS (sysrootver.release));
         --  TODO: apply options to settings
         PST.set_option_to_default_values (specs => specification);
         PST.set_outstanding_ignore (specs         => specification,
                                     variant       => get_variant,
                                     opsys         => platform_type,
                                     arch_standard => sysrootver.arch,
                                     osrelease     => HT.USS (sysrootver.release),
                                     osmajor       => HT.USS (sysrootver.major));
         PST.apply_directives (specs         => specification,
                               variant       => get_variant,
                               arch_standard => sysrootver.arch,
                               osmajor       => HT.USS (sysrootver.major));
         PSM.generator (specs         => specification,
                        variant       => get_variant,
                        opsys         => platform_type,
                        arch          => sysrootver.arch,
                        output_file   => "");
      else
         TIO.Put_Line (errprefix & "Failed to parse " & specfile);
         TIO.Put_Line (PAR.get_parse_error);
      end if;
   end generate_makefile;


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
      cwd        : constant String := DIR.Current_Directory;
      sysroot    : constant String := HT.USS (PM.configuration.dir_sysroot);
      portsdir   : constant String := HT.USS (PM.configuration.dir_conspiracy);
      distfiles  : constant String := HT.USS (PM.configuration.dir_distfiles);
      packages   : constant String := HT.USS (PM.configuration.dir_packages);
      logs       : constant String := HT.USS (PM.configuration.dir_logs);
      ccache     : constant String := HT.USS (PM.configuration.dir_ccache);
      buildbase  : constant String := HT.USS (PM.configuration.dir_buildbase) & "/";

   begin
      if HT.leads (cwd, sysroot) or else
        HT.leads (cwd, portsdir) or else
        HT.leads (cwd, distfiles) or else
        HT.leads (cwd, packages) or else
        HT.leads (cwd, logs) or else
        HT.leads (cwd, ccache) or else
        HT.leads (cwd, buildbase)
      then
         TIO.Put_Line ("Please change the current directory; " &
                         "ravenadm is unable to launch from here.");
         return True;
      else
         return False;
      end if;
   end launch_clash_detected;


   --------------------------------------------------------------------------------------------
   --  insufficient_privileges
   --------------------------------------------------------------------------------------------
   function insufficient_privileges return Boolean
   is
      status  : Integer;
      command : constant String := "/usr/bin/id -u";
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
            TIO.Put_Line ("Only the root user can execute ravenadm.");
            return True;
         end if;
      end;
   end insufficient_privileges;


   --------------------------------------------------------------------------------------------
   --  already_running
   --------------------------------------------------------------------------------------------
   function already_running return Boolean is
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
               DIR.Delete_File (pidfile);
               return False;
            end if;
         exception
            when others =>
               --  pidfile contains garbage, remove it
               DIR.Delete_File (pidfile);
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
   function ravenexec_missing return Boolean
   is
      ravenexec : constant String := host_localbase & "/libexec/ravenexec";
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
      should_be : String := HT.USS (PM.configuration.dir_conspiracy) & "/bucket_" &
        UTL.bucket (candidate) & LAT.Solidus & candidate;
   begin
      if candidate = "" then
         TIO.Put_Line ("The locate command requires the port's name base as an argument");
         return;
      end if;
      if DIR.Exists (should_be) then
         TIO.Put_Line ("Found at " & should_be);
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
   --  scan_stack_of_single_ports
   --------------------------------------------------------------------------------------------
   function scan_stack_of_single_ports (always_build : Boolean) return Boolean
   is
      successful : Boolean := SCN.scan_provided_list_of_ports (always_build, sysrootver);
   begin
      if successful then
         SCN.set_build_priority;
         if PortScan.queue_is_empty then
            successful := False;
            TIO.Put_Line ("There are no valid ports to build." & bailing);
         end if;
      end if;

      <<clean_exit>>
      if Signals.graceful_shutdown_requested then
         successful := False;
         TIO.Put_Line (shutreq);
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
      ptid : PortScan.port_id;
      num_skipped : Natural;
      block_remote : Boolean := True;
      update_external_repo : constant String := host_pkg8 & " update --quiet --repository ";
      no_packages : constant String := "No prebuilt packages will be used as a result.";

   begin
      LOG.set_overall_start_time (CAL.Clock);

      if delete_first and then not dry_run then
         OPS.delete_existing_packages_of_ports_list;
      end if;

      if not OPS.limited_cached_options_check then
         --  Error messages emitted by function
         return False;
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

      OPS.run_start_hook;
      OPS.limited_sanity_check (repository       => HT.USS (PM.configuration.dir_repository),
                                dry_run          => dry_run,
                                rebuild_compiler => delete_first,
                                suppress_remote  => block_remote);
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
                         PortScan.ignore_reason (ptid), False);
         OPS.cascade_failed_build (id         => ptid,
                                   numskipped => num_skipped);
         OPS.record_history_ignored (elapsed   => LOG.elapsed_now,
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
      badformat  : constant String := "Invalid format: ";
      badname    : constant String := "Invalid port namebase: ";
      badvariant : constant String := "Invalid port variant: ";
   begin
      for k in start_from .. CLI.Argument_Count loop
         declare
            Argk : constant String := CLI.Argument (k);
            bad_namebase : Boolean;
            bad_format   : Boolean;
            add_standard : Boolean;
         begin
            if valid_origin (Argk, bad_namebase, bad_format, add_standard) then
               if add_standard then
                  PortScan.insert_into_portlist (Argk & LAT.Colon & variant_standard);
               else
                  PortScan.insert_into_portlist (Argk);
               end if;
            else
               if bad_format then
                  TIO.Put_Line (badformat & "'" & Argk & "'");
               elsif bad_namebase then
                  TIO.Put_Line (badname & "'" & Argk & "'");
               else
                  TIO.Put_Line (badvariant & "'" & Argk & "'");
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
      assume_std   : out Boolean) return Boolean
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
              special (special'First .. special'First + variant'Length) = variant & " " or else
              special (special'Last - variant'Length .. special'Last) = " " & variant or else
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
            bsheetu    : String := HT.USS (PM.configuration.dir_unkindness) & bsheetname;
         begin
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
            bsheetu    : String := HT.USS (PM.configuration.dir_unkindness) & bsheetname;
         begin
            assume_std := True;
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
      specfile : constant String := "specification";
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
      function package_copy (subpackage : String) return Boolean;
      function package_copy (subpackage : String) return Boolean
      is
         pkgname   : constant String := default_compiler & LAT.Hyphen & subpackage & LAT.Hyphen &
                     variant_standard & LAT.Hyphen & compiler_version & arc_ext;
         src_path  : constant String := HT.USS (PM.configuration.dir_sysroot) &
                     "/usr/share/compiler-packages/" & pkgname;
         dest_dir  : constant String := HT.USS (PM.configuration.dir_repository);
         dest_path : constant String := dest_dir & LAT.Solidus & pkgname;
      begin
         if not DIR.Exists (dest_path) then
            if DIR.Exists (src_path) then
               if DIR.Exists (dest_dir) then
                  DIR.Copy_File (Source_Name => src_path, Target_Name => dest_path);
               else
                  TIO.Put_Line ("Package directory " & dest_path & " does not exist");
                  return False;
               end if;
            else
               TIO.Put_Line ("Compiler package " & src_path & " does not exist");
               return False;
            end if;
         end if;
         return True;
      exception
         when others =>
            TIO.Put_Line ("Failed to copy " & src_path & " to " & dest_path);
            return False;
      end package_copy;
   begin
      return package_copy ("ada_run") and then
        package_copy ("compilers") and then
        package_copy ("complete") and then
        package_copy ("cxx_run") and then
        package_copy ("fortran_run") and then
        package_copy ("infopages") and then
        package_copy ("libs");
   end install_compiler_packages;

end Pilot;
