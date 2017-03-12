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

   type mandate_type is (unset, help, dev, build, test, status, configure);
   type dev_mandate  is (unset, dump, makefile, distinfo, buildsheet, explode);

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
      elsif first = "test" then
         mandate := test;
      elsif first = "status" then
         mandate := status;
      elsif first = "configure" then
         mandate := configure;
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
      elsif second = "explode" then
         return explode;
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

   if not Pilot.TERM_defined_in_environment then
      return;
   end if;

   if Pilot.launch_clash_detected then
      return;  --  Really only affects commands involving slaves
   end if;

   --  Load configuration here

   if not Parameters.all_paths_valid then
      return;
   end if;


   if Pilot.insufficient_privileges then
      return;
   end if;

   if Pilot.already_running then
      return;
   end if;

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

--     if Pilot.ravenexec_missing then
--        return
--     end if;

      --  TODO: store origins check


   Pilot.create_pidfile;
   Unix.ignore_background_tty;

   if mandate /= configure then
      Unix.cone_of_silence (deploy => True);
   end if;

   --------------------------------------------------------------------------------------------
   --  Validation block end
   --------------------------------------------------------------------------------------------

   case mandate is

      when build =>
         --------------------------------
         --  build command
         --------------------------------
         null;  --  tbw


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
                     null;
                  when buildsheet =>
                     Pilot.generate_buildsheet (get_arg (3));
                  when makefile =>
                     Pilot.generate_makefile (get_arg (3), get_arg (4));
                  when explode =>
                     Pilot.explode_buildsheet (get_arg (3), get_arg (4));
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

      when status =>
         --------------------------------
         --  status command
         --------------------------------
         null; --  tbw

      when test =>
         --------------------------------
         --  test command
         --------------------------------
         null; --  tbw

      when configure =>
         --------------------------------
         --  configure
         --------------------------------
         null; --  tbw

      when unset => null;

   end case;

   Unix.cone_of_silence (deploy => False);
   Pilot.destroy_pidfile;

end Ravenadm;
