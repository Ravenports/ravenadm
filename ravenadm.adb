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

   type mandate_type is (unset, help, dev, build, force, test, status, configure, locate);
   type dev_mandate  is (unset, dump, makefile, distinfo, buildsheet, template);

   procedure scan_first_command_word;
   function scan_dev_command_word return dev_mandate;
   function get_arg (arg_number : Positive) return String;

   mandate : mandate_type := unset;

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
      elsif first = "force" then
         mandate := force;
      elsif first = "test" then
         mandate := test;
      elsif first = "status" then
         mandate := status;
      elsif first = "configure" then
         mandate := configure;
      elsif first = "locate" then
         mandate := locate;
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

   if not Parameters.load_configuration then
      return;
   end if;

   case mandate is
      when build | test | status =>
         --  All commands involving replicant slaves
         if Pilot.launch_clash_detected then
            return;
         end if;
      when others => null;
   end case;

   case mandate is
      when build | test =>
         if Parameters.configuration.avec_ncurses and then
           not Pilot.TERM_defined_in_environment
         then
            return;
         end if;
      when others => null;
   end case;

   case mandate is
      when configure | help => null;
      when others =>
         if not Parameters.all_paths_valid then
            return;
         end if;
   end case;

   case mandate is
      when help => null;
      when others =>
         if Pilot.insufficient_privileges then
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
      when build | force =>
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

   Pilot.create_pidfile;
   Unix.ignore_background_tty;

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
            if Pilot.scan_stack_of_single_ports (always_build => False) and then
              Pilot.sanity_check_then_prefail (delete_first => False, dry_run => True)
            then
               Pilot.perform_bulk_run (testmode => False);
            end if;
         else
            null;  -- reserved for upgrade_system_everything maybe
         end if;

      when build =>
         --------------------------------
         --  build command
         --------------------------------
         if Pilot.scan_stack_of_single_ports (always_build => False) and then
           Pilot.sanity_check_then_prefail (delete_first => False, dry_run => False)
         then
            Pilot.perform_bulk_run (testmode => False);
         end if;

      when force =>
         --------------------------------
         --  force command
         --------------------------------
         if Pilot.scan_stack_of_single_ports (always_build => False) and then
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
                  when template =>
                     Pilot.print_spec_template;
               end case;
            end;
         else
            Pilot.react_to_unknown_second_level_command (CLI.Argument (1), "");
         end if;

      when help =>
         --------------------------------
         --  help command
         --------------------------------
         null;  --  tbw

      when test =>
         --------------------------------
         --  test command
         --------------------------------
         null;  --  tbw

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

      when unset => null;

   end case;

   Unix.cone_of_silence (deploy => False);
   Pilot.destroy_pidfile;

end Ravenadm;
