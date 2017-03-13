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
      extra_profiles : Boolean;
      answer         : Character;
      ascii          : Natural;
      continue       : Boolean := True;
   begin
      dupe := PM.configuration;
      loop
         pristine := True;
         extra_profiles := PM.alternative_profiles_exist;
         clear_screen;
         print_header;
         print_menu (pristine, extra_profiles);

          TIO.Put ("Press key of selection: ");
         loop
            TIO.Get_Immediate (answer);
            ascii := Character'Pos (answer);
            case answer is
               when 'A' .. 'I' =>
                  change_directory_option (option (ascii - 64), pristine);
                  exit;
               when 'a' .. 'i' =>
                  change_directory_option (option (ascii - 96), pristine);
                  exit;
               when 'J' .. 'K' =>
                  change_positive_option (option (ascii - 64), pristine);
                  exit;
               when 'j' .. 'k' =>
                  change_positive_option (option (ascii - 96), pristine);
                  exit;
               when 'L' .. 'O' =>
                  change_boolean_option (option (ascii - 64), pristine);
                  exit;
               when 'l' .. 'o' =>
                  change_boolean_option (option (ascii - 96), pristine);
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
   procedure print_menu (pristine : in out Boolean; extra_profiles : Boolean) is
   begin
      for line in option'Range loop
         print_opt (line, pristine);
      end loop;
      TIO.Put_Line ("");
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
      TIO.Put_Line ("");
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
         when  2 => nextt := dupe.dir_localbase;   origt := PM.configuration.dir_localbase;
         when  3 => nextt := dupe.dir_conspiracy;  origt := PM.configuration.dir_conspiracy;
         when  4 => nextt := dupe.dir_unkindness;  origt := PM.configuration.dir_unkindness;
         when  5 => nextt := dupe.dir_distfiles;   origt := PM.configuration.dir_distfiles;
         when  6 => nextt := dupe.dir_packages;    origt := PM.configuration.dir_packages;
         when  7 => nextt := dupe.dir_ccache;      origt := PM.configuration.dir_ccache;
         when  8 => nextt := dupe.dir_buildbase;   origt := PM.configuration.dir_buildbase;
         when  9 => nextt := dupe.dir_logs;        origt := PM.configuration.dir_logs;
         when 10 => nextn := dupe.num_builders;    orign := PM.configuration.num_builders;
         when 11 => nextn := dupe.jobs_limit;      orign := PM.configuration.jobs_limit;
         when 12 => nextb := dupe.avoid_tmpfs;     origb := PM.configuration.avoid_tmpfs;
         when 13 => nextb := dupe.record_options;  origb := PM.configuration.record_options;
         when 14 => nextb := dupe.avec_ncurses;    origb := PM.configuration.avec_ncurses;
         when 15 => nextb := dupe.defer_prebuilt;  origb := PM.configuration.defer_prebuilt;
      end case;
      case opt is
         when 1 .. 9   =>
            equivalent := HT.equivalent (origt, nextt);
            show := nextt;
         when 10 .. 11  =>
            equivalent := (orign = nextn);
            show := HT.int2text (Integer (nextn));
         when 12 .. 15 =>
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
         if opt = 4 then
            TIO.Put (" (or 'none' to indicate no custom ports): ");
         elsif opt = 7 then
            TIO.Put (" (or 'none' to disable ccache): ");
         else
            TIO.Put (": ");
         end if;
         declare
            testpath : constant String := TIO.Get_Line;
         begin
            if DIR.Exists (testpath) then
               declare
                  stp : constant String := Unix.true_path (testpath);
                  utp : HT.Text := HT.SUS (stp);
               begin
                  if HT.IsBlank (stp) then
                     raise menu_error
                       with "Does not resolve: " & testpath;
                  else
                     case opt is
                        when 1 => dupe.dir_sysroot    := utp;
                        when 3 => dupe.dir_conspiracy := utp;
                        when 4 => dupe.dir_unkindness := utp;
                        when 5 => dupe.dir_distfiles  := utp;
                        when 6 => dupe.dir_packages   := utp;
                        when 7 => dupe.dir_ccache     := utp;
                        when 8 => dupe.dir_buildbase  := utp;
                        when 9 => dupe.dir_logs       := utp;
                        when 2 =>
                           if HT.leads (stp, "/usr") and then
                             stp = "/usr/local"
                           then
                              continue := True;
                              dupe.dir_localbase := utp;
                           end if;
                        when others => raise menu_error
                             with "Illegal value : " & opt'Img;
                     end case;
                  end if;
               end;
               if opt /= 2 then
                  continue := True;
               end if;
            elsif opt = 4 then
               dupe.dir_unkindness := HT.SUS (PM.no_unkindness);
               continue := True;
            elsif opt = 7 then
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
         when 12 => dupe.avoid_tmpfs    := new_value;
         when 13 => dupe.record_options := new_value;
         when 14 => dupe.avec_ncurses   := new_value;
         when 15 => dupe.defer_prebuilt := new_value;
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
            when 10 .. 11 => max_value := Integer (builders'Last);
            when others => raise menu_error with "Illegal value : " & opt'Img;
         end case;
         TIO.Put (LAT.LF & "Set parameter value (1 to" & max_value'Img & "): ");
         continue := True;
         given_value := read_positive;

         if given_value > max_value then
            continue := False;
         else
            case opt is
               when 10 => dupe.num_builders := builders (given_value);
               when 11 => dupe.jobs_limit   := builders (given_value);
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

end Configure;
