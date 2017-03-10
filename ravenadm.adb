--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: License.txt

with Ada.Command_Line;
with Ada.Text_IO;
with Pilot;

procedure Ravenadm is

   package CLI renames Ada.Command_Line;
   package TIO renames Ada.Text_IO;
   package PLT renames Pilot;

   type mandate_type is (unset, help, dev, build, test, status);
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
      PLT.display_usage;
      return;
   end if;

   scan_first_command_word;

   case mandate is
      when unset =>
         PLT.react_to_unknown_first_level_command (CLI.Argument (1));

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
                     PLT.react_to_unknown_second_level_command (CLI.Argument (1),
                                                                CLI.Argument (2));
                  when dump =>
                     PLT.dump_ravensource (get_arg (3));
                  when distinfo =>
                     null;
                  when buildsheet =>
                     PLT.generate_buildsheet (get_arg (3));
                  when makefile =>
                     PLT.generate_makefile (get_arg (3), get_arg (4));
                  when explode =>
                     PLT.explode_buildsheet (get_arg (3), get_arg (4));
               end case;
            end;
         else
            PLT.react_to_unknown_second_level_command (CLI.Argument (1), "");
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
   end case;

end Ravenadm;
