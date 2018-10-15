--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: License.txt

with Ada.Command_Line;
with Ada.Text_IO;
with Parameters;
with Pilot;
with Unix;

procedure Ravenadm is

   package CLI renames Ada.Command_Line;
   package TIO renames Ada.Text_IO;

   type mandate_type is (unset, help, dev, build, build_everything, force, test, test_everything,
                         status, status_everything, configure, locate, purge, changeopts,
                         checkports, portsnap, repository, list_subpackages, website);
   type dev_mandate  is (unset, dump, makefile, distinfo, buildsheet, template, genindex, web,
                         repatch, sort_plist, confinfo);

   procedure scan_first_command_word;
   function scan_dev_command_word return dev_mandate;
   function get_arg (arg_number : Positive) return String;

   mandate    : mandate_type := unset;
   low_rights : Boolean := False;
   reg_user   : Boolean;
   reg_error  : constant String := "This command requires root permissions to execute.";

   procedure scan_first_command_word
   is
      first  : constant String := CLI.Argument (1);
   begin
      if first = "help" then
         mandate := help;
      elsif first = "dev" then
         mandate := dev;
      elsif first = "build" then
         mandate := build;
      elsif first = "build-everything" then
         mandate := build_everything;
      elsif first = "force" then
         mandate := force;
      elsif first = "test" then
         mandate := test;
      elsif first = "test-everything" then
         mandate := test_everything;
      elsif first = "status" then
         mandate := status;
      elsif first = "status-everything" then
         mandate := status_everything;
      elsif first = "configure" then
         mandate := configure;
      elsif first = "locate" then
         mandate := locate;
      elsif first = "purge-distfiles" then
         mandate := purge;
      elsif first = "set-options" then
         mandate := changeopts;
      elsif first = "check-ports" then
         mandate := checkports;
      elsif first = "update-ports" then
         mandate := portsnap;
      elsif first = "generate-repository" then
         mandate := repository;
      elsif first = "subpackages" then
         mandate := list_subpackages;
      elsif first = "generate-website" then
         mandate := website;
      end if;
   end scan_first_command_word;

   function scan_dev_command_word return dev_mandate
   is
      --  Check argument count before calling
      second : constant String := CLI.Argument (2);
   begin
      if second = "dump" then
         return dump;
      elsif second = "makefile" then
         return makefile;
      elsif second = "distinfo" then
         return distinfo;
      elsif second = "buildsheet" then
         return buildsheet;
      elsif second = "template" then
         return template;
      elsif second = "generate-index" then
         return genindex;
      elsif second = "web" then
         return web;
      elsif second = "repatch" then
         return repatch;
      elsif second = "sort" then
         return sort_plist;
      elsif second = "info" then
         return confinfo;
      else
         return unset;
      end if;
   end scan_dev_command_word;

   function get_arg (arg_number : Positive) return String is
   begin
      if CLI.Argument_Count >= arg_number then
         return CLI.Argument (arg_number);
      else
         return "";
      end if;
   end get_arg;

begin

   if CLI.Argument_Count = 0 then
      Pilot.display_usage;
      return;
   end if;

   scan_first_command_word;

   if mandate = unset then
      Pilot.react_to_unknown_first_level_command (CLI.Argument (1));
      return;
   end if;

   --------------------------------------------------------------------------------------------
   --  Validation block start
   --------------------------------------------------------------------------------------------

   if Pilot.already_running then
      return;
   end if;

   reg_user := Pilot.insufficient_privileges;

   if not Parameters.configuration_exists and then reg_user then
      TIO.Put_Line ("No configuration file found.");
      TIO.Put_Line ("Please switch to root permissions and retry the command.");
      return;
   end if;

   if not Parameters.load_configuration then
      return;
   end if;

   case mandate is
      when build | force | build_everything | test | status | status_everything =>
         --  All commands involving replicant slaves
         if Pilot.launch_clash_detected then
            return;
         end if;
      when others => null;
   end case;

   case mandate is
      when build | force | build_everything | test =>
         if Parameters.configuration.avec_ncurses and then
           not Pilot.TERM_defined_in_environment
         then
            return;
         end if;
      when others => null;
   end case;

   case mandate is
      when configure | help => null;
      when portsnap =>
         if not Parameters.all_paths_valid (skip_mk_check => True) then
            return;
         end if;
      when others =>
         if not Parameters.all_paths_valid (skip_mk_check => False) then
            return;
         end if;
   end case;

   case mandate is
      when help | locate | list_subpackages => null;
         low_rights := True;
      when dev =>
         declare
            dev_subcmd : dev_mandate := unset;
         begin
            if CLI.Argument_Count > 1 then
               dev_subcmd := scan_dev_command_word;
            end if;
            case dev_subcmd is
               when template | sort_plist | confinfo | unset =>
                  low_rights := True;
               when dump | makefile | distinfo | buildsheet | genindex | web | repatch =>
                  if reg_user then
                     TIO.Put_Line (reg_error);
                     return;
                  end if;
            end case;
         end;
      when others =>
         if reg_user then
            TIO.Put_Line (reg_error);
            return;
         end if;
   end case;

   if Pilot.previous_run_mounts_detected and then
     not Pilot.old_mounts_successfully_removed
   then
      return;
   end if;

   if Pilot.previous_realfs_work_detected and then
     not Pilot.old_realfs_work_successfully_removed
   then
      return;
   end if;

   if Pilot.ravenexec_missing then
      return;
   end if;

   case mandate is
      when build | force | test | changeopts | list_subpackages =>
         if not Pilot.store_origins (start_from => 2) then
            return;
         end if;
      when status =>
         if CLI.Argument_Count > 1 then
            if not Pilot.store_origins (start_from => 2) then
               return;
            end if;
         end if;
      when others => null;
   end case;

   case mandate is
      when build | build_everything | force |
           test | test_everything |
           status | status_everything =>
         Pilot.check_that_ravenadm_is_modern_enough;
         if not Pilot.slave_platform_determined then
            return;
         end if;
      when others => null;
   end case;

   if not low_rights then
      Pilot.create_pidfile;
      Unix.ignore_background_tty;
   end if;

   if mandate /= configure then
      Unix.cone_of_silence (deploy => True);
   end if;

   --------------------------------------------------------------------------------------------
   --  Validation block end
   --------------------------------------------------------------------------------------------

   case mandate is

      when status =>
         --------------------------------
         --  status command
         --------------------------------
         if CLI.Argument_Count > 1 then
            if Pilot.install_compiler_packages and then
              Pilot.scan_stack_of_single_ports (always_build => False) and then
              Pilot.sanity_check_then_prefail (delete_first => False, dry_run => True)
            then
               Pilot.display_results_of_dry_run;
            end if;
         else
            null;  -- reserved for upgrade_system_everything maybe
         end if;

      when status_everything =>
         --------------------------------
         --  status_everything command
         --------------------------------
         if Pilot.install_compiler_packages and then
           Pilot.fully_scan_ports_tree and then
           Pilot.sanity_check_then_prefail (delete_first => False, dry_run => True)
         then
            Pilot.display_results_of_dry_run;
         end if;

      when build =>
         --------------------------------
         --  build command
         --------------------------------
         if Pilot.install_compiler_packages and then
           Pilot.scan_stack_of_single_ports (always_build => False) and then
           Pilot.sanity_check_then_prefail (delete_first => False, dry_run => False)
         then
            Pilot.perform_bulk_run (testmode => False);
         end if;

      when build_everything =>
         --------------------------------
         --  build-everything command
         --------------------------------
         if Pilot.install_compiler_packages and then
           Pilot.fully_scan_ports_tree and then
           Pilot.sanity_check_then_prefail (delete_first => False, dry_run => False)
         then
            Pilot.perform_bulk_run (testmode => False);
         end if;

      when test_everything =>
         --------------------------------
         --  test-everything command
         --------------------------------
         if Pilot.install_compiler_packages and then
           Pilot.fully_scan_ports_tree and then
           Pilot.sanity_check_then_prefail (delete_first => True, dry_run => False)
         then
            Pilot.perform_bulk_run (testmode => True);
         end if;

      when website =>
         --------------------------------
         --  generate-website command
         --------------------------------
         Pilot.generate_website;

      when force =>
         --------------------------------
         --  force command
         --------------------------------
         if Pilot.install_compiler_packages and then
           Pilot.scan_stack_of_single_ports (always_build => False) and then
           Pilot.sanity_check_then_prefail (delete_first => True, dry_run => False)
         then
            Pilot.perform_bulk_run (testmode => False);
         end if;


      when dev =>
         --------------------------------
         --  dev command
         --------------------------------
         if CLI.Argument_Count > 1 then
            declare
               dev_subcmd : dev_mandate := scan_dev_command_word;
            begin
               case dev_subcmd is
                  when unset =>
                     Pilot.react_to_unknown_second_level_command (CLI.Argument (1),
                                                                  CLI.Argument (2));
                  when dump =>
                     Pilot.dump_ravensource (get_arg (3));
                  when distinfo =>
                     Pilot.generate_distinfo;
                  when buildsheet =>
                     Pilot.generate_buildsheet (get_arg (3), get_arg (4));
                  when makefile =>
                     Pilot.generate_makefile (get_arg (3), get_arg (4));
                  when web =>
                     Pilot.generate_webpage (get_arg (3), get_arg (4));
                  when repatch =>
                     Pilot.regenerate_patches (get_arg (3), get_arg (4));
                  when sort_plist =>
                     Pilot.resort_manifests (get_arg (3));
                  when template =>
                     Pilot.print_spec_template (get_arg (3));
                  when genindex =>
                     Pilot.generate_ports_index;
                  when confinfo =>
                     Pilot.show_config_value (get_arg (3));
               end case;
            end;
         else
            Pilot.react_to_unknown_second_level_command (CLI.Argument (1), "");
         end if;

      when help =>
         --------------------------------
         --  help command
         --------------------------------
         if CLI.Argument_Count > 1 then
            Pilot.launch_man_page (CLI.Argument (2));
         else
            Pilot.show_short_help;
         end if;

      when test =>
         --------------------------------
         --  test command
         --------------------------------
         if Pilot.install_compiler_packages and then
           Pilot.scan_stack_of_single_ports (always_build => True) and then
           Pilot.sanity_check_then_prefail (delete_first => True, dry_run => False)
         then
            if Pilot.interact_with_single_builder then
               Pilot.bulk_run_then_interact_with_final_port;
            else
               Pilot.perform_bulk_run (testmode => True);
            end if;
         end if;

      when configure =>
         --------------------------------
         --  configure
         --------------------------------
         Pilot.launch_configure_menu;

      when locate =>
         --------------------------------
         --  locate
         --------------------------------
         Pilot.locate (get_arg (2));

      when list_subpackages =>
         --------------------------------
         --  subpackages
         --------------------------------
         Pilot.list_subpackages;

      when purge =>
         --------------------------------
         --  purge-distfiles
         --------------------------------
         Pilot.purge_distfiles;

      when changeopts =>
         --------------------------------
         --  set-options
         --------------------------------
         Pilot.change_options;

      when checkports =>
         --------------------------------
         --  check-ports
         --------------------------------
         Pilot.check_ravenports_version;

      when portsnap =>
         --------------------------------
         --  update-ports
         --------------------------------
         Pilot.update_to_latest_ravenports;

      when repository =>
         --------------------------------
         --  generate-repository
         --------------------------------
         Pilot.generate_repository;

      when unset => null;

   end case;

   Unix.cone_of_silence (deploy => False);

   if not low_rights then
      Pilot.destroy_pidfile;
   end if;

end Ravenadm;
