--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Directories;
with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Specification_Parser;
with File_Operations;
with Information;
with HelperText;
with Parameters;
with Replicant;
with Unix;
with Port_Specification.Buildsheet;
with Port_Specification.Makefile;
with Port_Specification.Transform;

with Definitions; use Definitions;

package body Pilot is

   package REP renames Replicant;
   package HT  renames HelperText;
   package PM  renames Parameters;
   package NFO renames Information;
   package FOP renames File_Operations;
   package PAR renames Specification_Parser;
   package PSB renames Port_Specification.Buildsheet;
   package PSM renames Port_Specification.Makefile;
   package PST renames Port_Specification.Transform;
   package LAT renames Ada.Characters.Latin_1;
   package DIR renames Ada.Directories;
   package TIO renames Ada.Text_IO;

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

      use_opsys : supported_opsys := dragonfly;
   begin
      if directory_specified then
         declare
            filename : String := optional_directory & "/" & specfile;
         begin
            if DIR.Exists (filename) then
               PAR.parse_specification_file (dossier         => filename,
                                             opsys_focus     => use_opsys,
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
                                          opsys_focus     => use_opsys,
                                          success         => successful,
                                          stop_at_targets => False);
         else
            DNE (specfile);
            return;
         end if;
      end if;
      if successful then
         PAR.specification.dump_specification;
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

      function get_variant return String is
      begin
         if optional_variant = "" then
            return variant_standard;
         else
            return optional_variant;
         end if;
      end get_variant;

      use_opsys : supported_opsys := dragonfly;
   begin
      if directory_specified then
         declare
            filename : String := optional_directory & "/" & specfile;
         begin
            if DIR.Exists (filename) then
               PAR.parse_specification_file (dossier         => filename,
                                             opsys_focus     => use_opsys,
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
                                          opsys_focus     => use_opsys,
                                          success         => successful,
                                          stop_at_targets => False);
         else
            DNE (specfile);
            return;
         end if;
      end if;
      if successful then
         PST.set_option_defaults (specs         => PAR.specification,
                                  variant       => get_variant,
                                  opsys         => use_opsys,
                                  arch_standard => x86_64,
                                  osrelease     => "4.7");
         PST.set_option_to_default_values (specs => PAR.specification);
         PST.set_outstanding_ignore (specs         => PAR.specification,
                                     variant       => get_variant,
                                     opsys         => use_opsys,
                                     arch_standard => x86_64,
                                     osrelease     => "4.7",
                                     osmajor       => "4.7");
         PST.apply_directives (specs => PAR.specification);
         PSM.generator (specs         => PAR.specification,
                        variant       => get_variant,
                        opsys         => use_opsys,
                        output_file   => "");
      else
         TIO.Put_Line (errprefix & "Failed to parse " & specfile);
         TIO.Put_Line (PAR.get_parse_error);
      end if;
   end generate_makefile;


   --------------------------------------------------------------------------------------------
   --  generate_buildsheet
   --------------------------------------------------------------------------------------------
   procedure generate_buildsheet (optional_directory : String)
   is
      directory_specified : constant Boolean := (optional_directory /= "");
      successful : Boolean;

      use_opsys : supported_opsys := dragonfly;
   begin
      if directory_specified then
         declare
            filename : String := optional_directory & "/" & specfile;
         begin
            if DIR.Exists (filename) then
               PAR.parse_specification_file (dossier         => filename,
                                             opsys_focus     => use_opsys,
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
                                          opsys_focus     => use_opsys,
                                          success         => successful,
                                          stop_at_targets => False);
         else
            DNE (specfile);
            return;
         end if;
      end if;
      if successful then
         PSB.generator (specs       => PAR.specification,
                        ravensrcdir => optional_directory,
                        output_file => "");
      else
         TIO.Put_Line (errprefix & "Failed to parse " & specfile);
         TIO.Put_Line (PAR.get_parse_error);
      end if;
   end generate_buildsheet;


   --------------------------------------------------------------------------------------------
   --  explode_buildsheet
   --------------------------------------------------------------------------------------------
   procedure explode_buildsheet (extract_to_directory : String;
                                 optional_variant : String)
   is
      function get_variant return String;

      successful : Boolean;

      function get_variant return String is
      begin
         if optional_variant = "" then
            return variant_standard;
         else
            return optional_variant;
         end if;
      end get_variant;

      use_opsys : supported_opsys := dragonfly;
      specfile : constant String := "buildsheet";
   begin
      if extract_to_directory = "" then
         TIO.Put_Line (errprefix & "extraction sheet can't be empty");
         return;
      end if;

      if DIR.Exists (specfile) then
         PAR.parse_specification_file (dossier         => specfile,
                                       opsys_focus     => use_opsys,
                                       success         => successful,
                                       stop_at_targets => False,
                                       extraction_dir  => extract_to_directory);
      else
         DNE (specfile);
         return;
      end if;

      if successful then
         PST.set_option_defaults (specs         => PAR.specification,
                                  variant       => get_variant,
                                  opsys         => use_opsys,
                                  arch_standard => x86_64,
                                  osrelease     => "4.7");
         PST.set_option_to_default_values (specs => PAR.specification);
         PST.set_outstanding_ignore (specs         => PAR.specification,
                                     variant       => get_variant,
                                     opsys         => use_opsys,
                                     arch_standard => x86_64,
                                     osrelease     => "4.7",
                                     osmajor       => "4.7");
         PST.apply_directives (specs => PAR.specification);
         PSM.generator (specs         => PAR.specification,
                        variant       => get_variant,
                        opsys         => use_opsys,
                        output_file   => extract_to_directory & "/Makefile");
      else
         TIO.Put_Line (errprefix & "Failed to parse " & specfile);
         TIO.Put_Line (PAR.get_parse_error);
      end if;
   end explode_buildsheet;


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
      sysroot    : constant String := HT.USS (PM.configuration.dir_system);
      portsdir   : constant String := HT.USS (PM.configuration.dir_conspiracy);
      distfiles  : constant String := HT.USS (PM.configuration.dir_distfiles);
      packages   : constant String := HT.USS (PM.configuration.dir_packages);
      logs       : constant String := HT.USS (PM.configuration.dir_logs);
      ccache     : constant String := HT.USS (PM.configuration.dir_ccache);
      repository : constant String := HT.USS (PM.configuration.dir_repository);
      buildbase  : constant String := HT.USS (PM.configuration.dir_buildbase) & "/";

   begin
      if HT.leads (cwd, sysroot) or else
        HT.leads (cwd, portsdir) or else
        HT.leads (cwd, distfiles) or else
        HT.leads (cwd, packages) or else
        HT.leads (cwd, logs) or else
        HT.leads (cwd, ccache) or else
        HT.leads (cwd, repository) or else
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
      TIO.Put_Line (HT.USS (PM.configuration.dir_buildbase) & "failed.");
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

end Pilot;
