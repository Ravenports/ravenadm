--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions; use Definitions;

with Ada.Integer_Text_IO;
with Ada.Directories;
with Ada.Text_IO;
with HelperText;
with Unix;

package body Configure is

   package INT renames Ada.Integer_Text_IO;
   package DIR renames Ada.Directories;
   package TIO renames Ada.Text_IO;
   package HT  renames HelperText;

   --------------------------------------------------------------------------------------------
   --  launch_configure_menu
   --------------------------------------------------------------------------------------------
   procedure launch_configure_menu
   is
      pristine       : Boolean;
      pristine_def   : Boolean;
      extra_profiles : Boolean;
      answer         : Character;
      ascii          : Natural;
      continue       : Boolean := True;
   begin
      dupe := PM.configuration;
      pristine_def := True;
      loop
         pristine := pristine_def;
         extra_profiles := PM.alternative_profiles_exist;
         clear_screen;
         print_header;
         print_menu (pristine, extra_profiles, pristine_def);

         loop
            TIO.Get_Immediate (answer);
            ascii := Character'Pos (answer);
            case answer is
               when 'A' .. 'J' =>
                  change_directory_option (option (ascii - 64), pristine);
                  exit;
               when 'a' .. 'j' =>
                  change_directory_option (option (ascii - 96), pristine);
                  exit;
               when 'K' .. 'L' =>
                  change_positive_option (option (ascii - 64), pristine);
                  exit;
               when 'k' .. 'l' =>
                  change_positive_option (option (ascii - 96), pristine);
                  exit;
               when 'M' .. 'P' =>
                  change_boolean_option (option (ascii - 64), pristine);
                  exit;
               when 'm' .. 'p' =>
                  change_boolean_option (option (ascii - 96), pristine);
                  exit;
               when 'v' | 'V' =>
                  move_to_defaults_menu (pristine_def);
                  exit;
               when '>' =>
                  switch_profile;
                  exit;
               when '<' =>
                  if extra_profiles then
                     delete_profile;
                     exit;
                  end if;
               when LAT.LF =>
                  if not pristine then
                     PM.configuration := dupe;
                     PM.insert_profile (dupe);
                     PM.rewrite_configuration;
                  end if;
                  continue := False;
                  exit;
               when LAT.ESC =>
                  continue := False;
                  exit;
               when others => null;
            end case;
         end loop;
         exit when not continue;
      end loop;

   end launch_configure_menu;


   --------------------------------------------------------------------------------------------
   --  clear_screen
   --------------------------------------------------------------------------------------------
   procedure clear_screen
   is
      --  Known to work on all platforms
      command : constant String := "/usr/bin/clear";

      success    : Boolean;
      cmd_output : HT.Text;
   begin
      success := Unix.piped_mute_command (command, cmd_output);
      if success then
         --  The output string is what actually clears the screen
         TIO.Put (HT.USS (cmd_output));
      end if;
   end clear_screen;


   --------------------------------------------------------------------------------------------
   --  print_header
   --------------------------------------------------------------------------------------------
   procedure print_header
   is
      dashes : constant String (1 .. 79) := (others => LAT.Equals_Sign);
   begin
      TIO.Put_Line ("ravenadm configuration profile: " & HT.USS (PM.configuration.profile));
      TIO.Put_Line (dashes);
   end print_header;


   --------------------------------------------------------------------------------------------
   --  print_menu
   --------------------------------------------------------------------------------------------
   procedure print_menu
     (pristine       : in out Boolean;
      extra_profiles : Boolean;
      pristine_def   : Boolean) is
   begin
      for line in option'Range loop
         print_opt (line, pristine);
      end loop;
      TIO.Put_Line ("");
      if pristine_def then
         TIO.Put_Line (indent & optX5A);
      else
         TIO.Put_Line (indent & optX5A & "*");
      end if;
      if pristine then
         TIO.Put_Line (indent & optX1B);
         if extra_profiles then
            TIO.Put_Line (indent & optX4B);
         end if;
         TIO.Put_Line (indent & optX3B);
      else
         TIO.Put_Line (indent & optX1A);
         TIO.Put_Line (indent & optX2A);
         TIO.Put_Line (indent & optX3A);
      end if;
   end print_menu;


   --------------------------------------------------------------------------------------------
   --  print_opt
   --------------------------------------------------------------------------------------------
   procedure print_opt (opt : option; pristine : in out Boolean)
   is
      origt : HT.Text;
      nextt : HT.Text;
      orign : builders;
      nextn : builders;
      origb : Boolean;
      nextb : Boolean;
      show  : HT.Text;
      equivalent : Boolean;
   begin
      TIO.Put (indent & descriptions (opt));
      case opt is
         when  1 => nextt := dupe.dir_sysroot;     origt := PM.configuration.dir_sysroot;
         when  2 => nextt := dupe.dir_toolchain;   origt := PM.configuration.dir_toolchain;
         when  3 => nextt := dupe.dir_localbase;   origt := PM.configuration.dir_localbase;
         when  4 => nextt := dupe.dir_conspiracy;  origt := PM.configuration.dir_conspiracy;
         when  5 => nextt := dupe.dir_unkindness;  origt := PM.configuration.dir_unkindness;
         when  6 => nextt := dupe.dir_distfiles;   origt := PM.configuration.dir_distfiles;
         when  7 => nextt := dupe.dir_profile;     origt := PM.configuration.dir_profile;
         when  8 => nextt := dupe.dir_packages;    origt := PM.configuration.dir_packages;
         when  9 => nextt := dupe.dir_ccache;      origt := PM.configuration.dir_ccache;
         when 10 => nextt := dupe.dir_buildbase;   origt := PM.configuration.dir_buildbase;
         when 11 => nextn := dupe.num_builders;    orign := PM.configuration.num_builders;
         when 12 => nextn := dupe.jobs_limit;      orign := PM.configuration.jobs_limit;
         when 13 => nextb := dupe.avoid_tmpfs;     origb := PM.configuration.avoid_tmpfs;
         when 14 => nextb := dupe.record_options;  origb := PM.configuration.record_options;
         when 15 => nextb := dupe.avec_ncurses;    origb := PM.configuration.avec_ncurses;
         when 16 => nextb := dupe.defer_prebuilt;  origb := PM.configuration.defer_prebuilt;
      end case;
      case opt is
         when  1 .. 10 =>
            equivalent := HT.equivalent (origt, nextt);
            show := nextt;
         when 11 .. 12 =>
            equivalent := (orign = nextn);
            show := HT.int2text (Integer (nextn));
         when 13 .. 16 =>
            equivalent := (origb = nextb);
            show := HT.bool2text (nextb);
      end case;
      if equivalent then
         TIO.Put_Line (" " & HT.USS (show));
      else
         TIO.Put_Line ("*" & HT.USS (show));
         pristine := False;
      end if;
   end print_opt;


   --------------------------------------------------------------------------------------------
   --  change_directory_option
   --------------------------------------------------------------------------------------------
   procedure change_directory_option (opt : option; pristine : in out Boolean)
   is
      continue : Boolean := False;
   begin
      loop
         clear_screen;
         print_header;
         print_opt (opt, pristine);
         TIO.Put (LAT.LF & "Set valid path for directory");
         if opt = 5 then
            TIO.Put (" (or 'none' to indicate no custom ports): ");
         elsif opt = 8 then
            TIO.Put (" (or 'none' to disable ccache): ");
         else
            TIO.Put (": ");
         end if;
         declare
            testpath : constant String := TIO.Get_Line;
         begin
            if opt = 3 then
               --  ravenbase doesn't have to exist, but there are limits to what it can be
               if not PM.forbidden_localbase (testpath) then
                  dupe.dir_localbase := HT.SUS (testpath);
                  continue := True;
               end if;
            elsif DIR.Exists (testpath) then
               declare
                  stp : constant String := Unix.true_path (testpath);
                  utp : HT.Text := HT.SUS (stp);
               begin
                  if HT.IsBlank (stp) then
                     raise menu_error
                       with "Does not resolve: " & testpath;
                  else
                     case opt is
                        when  1 => dupe.dir_sysroot    := utp;
                        when  2 => dupe.dir_toolchain  := utp;
                        when  4 => dupe.dir_conspiracy := utp;
                        when  5 => dupe.dir_unkindness := utp;
                        when  6 => dupe.dir_distfiles  := utp;
                        when  7 => dupe.dir_profile    := utp;
                        when  8 => dupe.dir_packages   := utp;
                        when  9 => dupe.dir_ccache     := utp;
                        when 10 => dupe.dir_buildbase  := utp;
                        when others => raise menu_error
                             with "Illegal value : " & opt'Img;
                     end case;
                  end if;
               end;
               continue := True;
            elsif opt = 5 then
               dupe.dir_unkindness := HT.SUS (PM.no_unkindness);
               continue := True;
            elsif opt = 8 then
               dupe.dir_ccache := HT.SUS (PM.no_ccache);
               continue := True;
            end if;
         exception
            when others =>
               continue := True;
         end;
         exit when continue;
      end loop;
   end change_directory_option;


   --------------------------------------------------------------------------------------------
   --  change_boolean_option
   --------------------------------------------------------------------------------------------
   procedure change_boolean_option (opt : option; pristine : in out Boolean)
   is
      new_value : Boolean;
      TF : Character;
   begin
      clear_screen;
      print_header;
      print_opt (opt, pristine);
      TIO.Put (LAT.LF & "Set parameter value (T/F): ");
      loop
         TIO.Get_Immediate (TF);
         case TF is
            when 'T' | 't' =>
               new_value := True;
               exit;
            when 'F' | 'f' =>
               new_value := False;
               exit;
            when others => null;
         end case;
      end loop;
      case opt is
         when 13 => dupe.avoid_tmpfs    := new_value;
         when 14 => dupe.record_options := new_value;
         when 15 => dupe.avec_ncurses   := new_value;
         when 16 => dupe.defer_prebuilt := new_value;
         when others =>
            raise menu_error with "Illegal value : " & opt'Img;
      end case;
   end change_boolean_option;


   --------------------------------------------------------------------------------------------
   --  change_positive_option
   --------------------------------------------------------------------------------------------
   procedure change_positive_option  (opt : option; pristine : in out Boolean)
   is
      function read_positive return Positive;
      function read_positive return Positive
      is
         number : Positive;
      begin
         INT.Get (number);
         return number;
      exception
         when others =>
            TIO.Skip_Line;
            return 100000;
      end read_positive;

      max_value : Positive;
      given_value : Positive;
      continue : Boolean;
   begin
      loop
         clear_screen;
         print_header;
         print_opt (opt, pristine);
         case opt is
            when 11 .. 12 => max_value := Integer (builders'Last);
            when others => raise menu_error with "Illegal value : " & opt'Img;
         end case;
         TIO.Put (LAT.LF & "Set parameter value (1 to" & max_value'Img & "): ");
         continue := True;
         given_value := read_positive;

         if given_value > max_value then
            continue := False;
         else
            case opt is
               when 11 => dupe.num_builders := builders (given_value);
               when 12 => dupe.jobs_limit   := builders (given_value);
               when others => null;
            end case;
            exit;
         end if;
         exit when continue;
      end loop;
   end change_positive_option;


   --------------------------------------------------------------------------------------------
   --  delete_profile
   --------------------------------------------------------------------------------------------
   procedure delete_profile
   is
      function list return Natural;

      all_profiles : String  := PM.list_profiles;
      continue     : Boolean := False;
      max_menu     : Natural;
      number       : Positive;
      actprofile   : Natural := 0;

      function list return Natural
      is
         markers : HT.Line_Markers;
         linenum : Natural := 0;
      begin
         HT.initialize_markers (all_profiles, markers);
         loop
            exit when not HT.next_line_present (all_profiles, markers);
            linenum := linenum + 1;
            declare
               line : constant String := HT.extract_line (all_profiles, markers);
            begin
               if HT.equivalent (PM.configuration.profile, line) then
                  actprofile := linenum;
               else
                  TIO.Put_Line (indent & LAT.Left_Square_Bracket & HT.int2str (linenum) &
                                  "] Delete " & LAT.Quotation & line & LAT.Quotation &
                                  " profile");
               end if;
            end;
         end loop;
         linenum := linenum + 1;
         TIO.Put_Line (indent & LAT.Left_Square_Bracket & HT.int2str (linenum) &
                         "] Do nothing (return to previous screen)");
         return linenum;
      end list;

   begin
      loop
         clear_screen;
         print_header;
         max_menu := list;

         TIO.Put (LAT.LF & "Select profile number (cannot be undone): ");
         begin
            INT.Get (number);
         exception
            when others =>
               TIO.Skip_Line;
               number := 1000;
         end;
         if number = max_menu then
            continue := True;
         elsif number = actprofile then
            null;
         elsif number < max_menu then
            declare
               unwanted : constant String := HT.specific_line (all_profiles, number);
            begin
               PM.delete_profile (profile => unwanted);
            end;
            continue := True;
         end if;
         exit when continue;
      end loop;
   end delete_profile;


   --------------------------------------------------------------------------------------------
   --  switch_profile
   --------------------------------------------------------------------------------------------
   procedure switch_profile
   is
      function list return Natural;

      all_profiles : String  := PM.list_profiles;
      continue     : Boolean := False;
      max_menu     : Natural;
      number       : Positive;

      function list return Natural
      is
         markers : HT.Line_Markers;
         linenum : Natural := 0;
      begin
         HT.initialize_markers (all_profiles, markers);
         loop
            exit when not HT.next_line_present (all_profiles, markers);
            linenum := linenum + 1;
            declare
               line : constant String := HT.extract_line (all_profiles, markers);
            begin
               TIO.Put_Line (indent & LAT.Left_Square_Bracket & HT.int2str (linenum) &
                               "] Switch to " & LAT.Quotation & line & LAT.Quotation &
                               " profile");
            end;
         end loop;
         linenum := linenum + 1;
         TIO.Put_Line (indent & LAT.Left_Square_Bracket & HT.int2str (linenum) &
                         "] Create new profile");
         linenum := linenum + 1;
         TIO.Put_Line (indent & LAT.Left_Square_Bracket & HT.int2str (linenum) &
                         "] Do nothing (return to previous screen)");
         return linenum;
      end list;

   begin
      loop
         clear_screen;
         print_header;
         max_menu := list;
         TIO.Put (LAT.LF & "Select profile number: ");
         begin
            INT.Get (number);
         exception
            when others =>
               TIO.Skip_Line;
               number := 1000;
         end;
         if number = max_menu then
            continue := True;
         elsif number = max_menu - 1 then
            clear_screen;
            print_header;
            TIO.Skip_Line;
            TIO.Put (LAT.LF & "Name of new profile: ");
            declare
               newname : String := TIO.Get_Line;
            begin
               PM.insert_profile (PM.default_profile (newname));
               PM.rewrite_configuration;
               PM.switch_profile (to_profile => newname);
               dupe := PM.configuration;
            exception
               when others => null;
            end;
            continue := True;
         elsif number < max_menu - 1 then
            declare
               nextprofile : String := HT.specific_line (all_profiles, number);
            begin
               PM.switch_profile (to_profile => nextprofile);
               PM.rewrite_configuration;
               dupe := PM.configuration;
            end;
            continue := True;
         end if;
         exit when continue;
      end loop;
   end switch_profile;


   --------------------------------------------------------------------------------------------
   --  print_default
   --------------------------------------------------------------------------------------------
   procedure print_default (def : default; pristine_def : in out Boolean)
   is
      origt : HT.Text;
      nextt : HT.Text;
      show  : HT.Text;
      equivalent : Boolean;
   begin
      TIO.Put (indent & version_desc (def));
      case def is
         when  1 => nextt := dupe.def_firebird;    origt := PM.configuration.def_firebird;
         when  2 => nextt := dupe.def_lua;         origt := PM.configuration.def_lua;
         when  3 => nextt := dupe.def_mysql_group; origt := PM.configuration.def_mysql_group;
         when  4 => nextt := dupe.def_perl;        origt := PM.configuration.def_perl;
         when  5 => nextt := dupe.def_php;         origt := PM.configuration.def_php;
         when  6 => nextt := dupe.def_postgresql;  origt := PM.configuration.def_postgresql;
         when  7 => nextt := dupe.def_python3;     origt := PM.configuration.def_python3;
         when  8 => nextt := dupe.def_ruby;        origt := PM.configuration.def_ruby;
         when  9 => nextt := dupe.def_ssl;         origt := PM.configuration.def_ssl;
         when 10 => nextt := dupe.def_tcl_tk;      origt := PM.configuration.def_tcl_tk;
      end case;
      equivalent := HT.equivalent (origt, nextt);
      show := nextt;
      if equivalent then
         TIO.Put_Line (" " & HT.USS (show));
      else
         TIO.Put_Line ("*" & HT.USS (show));
         pristine_def := False;
      end if;
   end print_default;


   --------------------------------------------------------------------------------------------
   --  move_to_defaults_menu
   --------------------------------------------------------------------------------------------
   procedure move_to_defaults_menu (pristine_def : in out Boolean)
   is
      answer   : Character;
      ascii    : Natural;
   begin
      loop
         pristine_def := True;
         clear_screen;
         print_header;
         for line in default'Range loop
            print_default (line, pristine_def);
         end loop;
         TIO.Put_Line ("");
         TIO.Put_Line (indent & "[RET] Return to main configuration menu");

         loop
            TIO.Get_Immediate (answer);
            ascii := Character'Pos (answer);
            case answer is
               when 'A' | 'a' =>
                  update_version (1, version_A, "Firebird SQL");
               when 'B' | 'b' =>
                  update_version (2, version_B, "Lua");
               when 'C' | 'c' =>
                  update_version (3, version_C, "MySQL group");
               when 'D' | 'd' =>
                  update_version (4, version_D, "Perl");
               when 'E' | 'e' =>
                  update_version (5, version_E, "PHP");
               when 'F' | 'f' =>
                  update_version (6, version_F, "PostgreSQL");
               when 'G' | 'g' =>
                  update_version (7, version_G, "Python 3");
               when 'H' | 'h' =>
                  update_version (8, version_H, "Ruby");
               when 'I' | 'i' =>
                  update_version (9, version_I, "SSL library");
               when 'J' | 'j' =>
                  update_version (10, version_J, "TCL/TK");
               when LAT.LF =>
                  return;
               when others =>
                  null;
            end case;
            case answer is
               when 'A' .. 'J' | 'a' .. 'j' | LAT.LF =>
                  exit;
               when others =>
                  null;
            end case;
         end loop;
      end loop;
   end move_to_defaults_menu;


   --------------------------------------------------------------------------------------------
   --  update_version
   --------------------------------------------------------------------------------------------
   procedure update_version
     (def     : default;
      choices : String;
      label   : String)
   is
      new_value   : HT.Text;
      num_choices : Natural := HT.count_char (choices, LAT.Colon) + 1;
      letter      : Character;
      field_index : Natural;
   begin
      clear_screen;
      print_header;
      TIO.Put_Line ("Options for default version of " & label & LAT.Colon);
      TIO.Put_Line (LAT.LF & indent &
                      "[A] floating (Ravenports choses default, may change over time)");
      for item in 1 .. num_choices loop
         letter := Character'Val (Character'Pos ('A') + item);
         TIO.Put_Line (indent & LAT.Left_Square_Bracket & letter & LAT.Right_Square_Bracket &
                         LAT.Space & HT.specific_field (choices, item, ":"));
      end loop;

      TIO.Put (LAT.LF & "Current setting: ");
      case def is
         when  1 => TIO.Put_Line (HT.USS (dupe.def_firebird));
         when  2 => TIO.Put_Line (HT.USS (dupe.def_lua));
         when  3 => TIO.Put_Line (HT.USS (dupe.def_mysql_group));
         when  4 => TIO.Put_Line (HT.USS (dupe.def_perl));
         when  5 => TIO.Put_Line (HT.USS (dupe.def_php));
         when  6 => TIO.Put_Line (HT.USS (dupe.def_postgresql));
         when  7 => TIO.Put_Line (HT.USS (dupe.def_python3));
         when  8 => TIO.Put_Line (HT.USS (dupe.def_ruby));
         when  9 => TIO.Put_Line (HT.USS (dupe.def_ssl));
         when 10 => TIO.Put_Line (HT.USS (dupe.def_tcl_tk));
      end case;
      TIO.Put (" Change setting: ");
      loop
         TIO.Get_Immediate (letter);
         case letter is
            when 'A' | 'a' =>
               new_value := HT.SUS (ports_default);
               exit;
            when 'B' .. 'Z' =>
               field_index := Character'Pos (letter) - Character'Pos ('A');
               if field_index <= num_choices then
                  new_value := HT.SUS (HT.specific_field (choices, field_index, ":"));
                  exit;
               end if;
            when 'b' .. 'z' =>
               field_index := Character'Pos (letter) - Character'Pos ('a');
               if field_index <= num_choices then
                  new_value := HT.SUS (HT.specific_field (choices, field_index, ":"));
                  exit;
               end if;
            when LAT.LF =>
               return;
            when others => null;
         end case;
      end loop;
      case def is
         when  1 => dupe.def_firebird    := new_value;
         when  2 => dupe.def_lua         := new_value;
         when  3 => dupe.def_mysql_group := new_value;
         when  4 => dupe.def_perl        := new_value;
         when  5 => dupe.def_php         := new_value;
         when  6 => dupe.def_postgresql  := new_value;
         when  7 => dupe.def_python3     := new_value;
         when  8 => dupe.def_ruby        := new_value;
         when  9 => dupe.def_ssl         := new_value;
         when 10 => dupe.def_tcl_tk      := new_value;
      end case;
   end update_version;

end Configure;
