--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions;  use Definitions;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Directories;
with File_Operations;
with INI_File_Manager;
with Parameters;
with Unix;

package body Options_Dialog is

   package FOP renames File_Operations;
   package IFM renames INI_File_Manager;
   package TIO renames Ada.Text_IO;
   package DIR renames Ada.Directories;
   package EX  renames Ada.Exceptions;

   --------------------------------------------------------------------------------------------
   --  launch_dialog
   --------------------------------------------------------------------------------------------
   function launch_dialog (specification : in out PSP.Portspecs) return Boolean
   is
   begin
      if not Start_Curses_Mode then
         TIO.Put_Line ("Failed to enter curses modes");
         return False;
      end if;
      if not TIC.Has_Colors or else not establish_colors then
         Return_To_Text_Mode;
         TIO.Put_Line ("The TERM environment variable value (" & Unix.env_variable_value ("TERM")
                       & ") does not support colors.");
         return False;
      end if;

      if Natural (TIC.Lines) < 10 then
         Return_To_Text_Mode;
         TIO.Put_Line ("At " & HT.int2str (Natural (TIC.Lines)) &
                         " lines tall, the curses window is too short to function correctly.");
         return False;
      end if;

      begin
         TIC.Set_Echo_Mode (False);
         TIC.Set_Raw_Mode (True);
         TIC.Set_Cbreak_Mode (True);
         TIC.Set_Cursor_Visibility (Visibility => cursor_vis);
      exception
         when issue : TIC.Curses_Exception =>
            TIO.Put_Line ("Unknown curses issues: " & EX.Exception_Message (issue));
            Return_To_Text_Mode;
            return False;
      end;

      if not launch_keymenu_zone or else
        not launch_dialog_zone
      then
         terminate_dialog;
         return False;
      end if;

      setup_parameters (specification);

      draw_static_keymenu;
      handle_user_commands;
      terminate_dialog;

      return True;
   end launch_dialog;


   --------------------------------------------------------------------------------------------
   --  establish_colors
   --------------------------------------------------------------------------------------------
   function establish_colors return Boolean is
   begin
      TIC.Start_Color;
      begin
         TIC.Init_Pair (TIC.Color_Pair  (1), TIC.White,   TIC.Black);
         TIC.Init_Pair (TIC.Color_Pair  (2), TIC.Cyan,    TIC.Black);
         TIC.Init_Pair (TIC.Color_Pair  (3), TIC.Green,   TIC.Black);
         TIC.Init_Pair (TIC.Color_Pair  (4), TIC.Black,   TIC.Black);
         TIC.Init_Pair (TIC.Color_Pair  (5), TIC.Blue,    TIC.Cyan);
         TIC.Init_Pair (TIC.Color_Pair  (6), TIC.Yellow,  TIC.Black);
         TIC.Init_Pair (TIC.Color_Pair  (7), TIC.Magenta, TIC.Black);
         TIC.Init_Pair (TIC.Color_Pair  (8), TIC.Black,   TIC.White);
         TIC.Init_Pair (TIC.Color_Pair  (9), TIC.Magenta, TIC.White);
         TIC.Init_Pair (TIC.Color_Pair (10), TIC.Blue,    TIC.White);
         TIC.Init_Pair (TIC.Color_Pair (11), TIC.Red,     TIC.White);
      exception
         when TIC.Curses_Exception => return False;
      end;

      c_standard     := TIC.Color_Pair (1);
      c_key_desc     := TIC.Color_Pair (2);
      c_title        := TIC.Color_Pair (3);
      c_trimmings    := TIC.Color_Pair (4);
      c_optbox_title := TIC.Color_Pair (5);
      c_group_text   := TIC.Color_Pair (11);
      c_group_trim   := TIC.Color_Pair (8);
      c_letters      := TIC.Color_Pair (9);
      c_options      := TIC.Color_Pair (10);
      c_inv_gray     := TIC.Color_Pair (8);
      c_tick_on      := TIC.Color_Pair (8);
      c_tick_delta   := TIC.Color_Pair (11);
      c_arrow        := TIC.Color_Pair (6);
      return True;

   end establish_colors;


   --------------------------------------------------------------------------------------------
   --  Start_Curses_Mode
   --------------------------------------------------------------------------------------------
   function Start_Curses_Mode return Boolean is
   begin
      TIC.Init_Screen;
      return True;
   exception
      when TIC.Curses_Exception => return False;
   end Start_Curses_Mode;


   --------------------------------------------------------------------------------------------
   --  Return_To_Text_Mode
   --------------------------------------------------------------------------------------------
   procedure Return_To_Text_Mode is
   begin
      TIC.End_Windows;
   exception
      when TIC.Curses_Exception => null;
   end Return_To_Text_Mode;


   --------------------------------------------------------------------------------------------
   --  zone_window
   --------------------------------------------------------------------------------------------
   function zone_window (zone : zones) return TIC.Window is
   begin
      case zone is
         when keymenu => return zone_keymenu;
         when dialog  => return zone_dialog;
      end case;
   end zone_window;


   --------------------------------------------------------------------------------------------
   --  Refresh_Zone
   --------------------------------------------------------------------------------------------
   procedure Refresh_Zone (zone : zones) is
   begin
      TIC.Refresh (Win => zone_window (zone));
   exception
      when TIC.Curses_Exception => null;
   end Refresh_Zone;


   ------------------------------------------------------------------------
   --  Scrawl
   ------------------------------------------------------------------------
   procedure Scrawl (zone        : zones;
                     information : TIC.Attributed_String;
                     at_line     : TIC.Line_Position;
                     at_column   : TIC.Column_Position := 0) is
   begin
      TIC.Add (Win    => zone_window (zone),
               Line   => at_line,
               Column => at_column,
               Str    => information,
               Len    => information'Length);
   exception
      when TIC.Curses_Exception => null;
   end Scrawl;


   --------------------------------------------------------------------------------------------
   --  launch_keymenu_zone
   --------------------------------------------------------------------------------------------
   function launch_keymenu_zone return Boolean is
   begin
      zone_keymenu := TIC.Create (Number_Of_Lines       => 4,
                                  Number_Of_Columns     => app_width,
                                  First_Line_Position   => 0,
                                  First_Column_Position => 0);
      return True;
   exception
      when TIC.Curses_Exception => return False;
   end launch_keymenu_zone;


   --------------------------------------------------------------------------------------------
   --  launch_dialog_zone
   --------------------------------------------------------------------------------------------
   function launch_dialog_zone return Boolean
   is
      --  56 limit comes from 26x3 + 1 (header) + 1x2 (margin) + 1 footer
      --  A .. Z + a .. Z + worst case of 26 2-member groups
   begin
      zone_dialog := TIC.Create (Number_Of_Lines       => dialog_height,
                                 Number_Of_Columns     => app_width,
                                 First_Line_Position   => 4,
                                 First_Column_Position => 0);
      return True;
   exception
      when TIC.Curses_Exception => return False;
   end launch_dialog_zone;


   --------------------------------------------------------------------------------------------
   --  terminate_dialog
   --------------------------------------------------------------------------------------------
   procedure terminate_dialog
   is
      ok : Boolean := True;
   begin
      --  zone_window can't be used because Delete will modify Win variable
      begin
         TIC.Delete (Win => zone_keymenu);
         TIC.Delete (Win => zone_dialog);
      exception
         when TIC.Curses_Exception =>
            ok := False;
      end;
      Return_To_Text_Mode;
      if not ok then
         TIO.Put_Line ("Saw error during termination");
      end if;
   end terminate_dialog;


   --------------------------------------------------------------------------------------------
   --  custom_message
   --------------------------------------------------------------------------------------------
   function custom_message (message   : String;
                            attribute : TIC.Character_Attribute_Set;
                            pen_color : TIC.Color_Pair) return TIC.Attributed_String
   is
      product : TIC.Attributed_String (1 .. message'Length);
      pindex  : Positive := 1;
   begin
      for index in message'Range loop
         product (pindex) := (Attr  => attribute,
                              Color => pen_color,
                              Ch    => message (index));
         pindex := pindex + 1;
      end loop;
      return product;
   end custom_message;


   --------------------------------------------------------------------------------------------
   --  touch_up
   --------------------------------------------------------------------------------------------
   procedure touch_up (ATS        : in out TIC.Attributed_String;
                       From_index : Positive;
                       length     : Positive;
                       attribute  : TIC.Character_Attribute_Set;
                       pen_color  : TIC.Color_Pair)
   is
   begin
      for index in From_index .. From_index - 1 + length loop
         ATS (index).Attr  := attribute;
         ATS (index).Color := pen_color;
      end loop;
   end touch_up;


   --------------------------------------------------------------------------------------------
   --  colorize_groups
   --------------------------------------------------------------------------------------------
   function colorize_groups (textdata : String) return TIC.Attributed_String
   is
      product : TIC.Attributed_String (1 .. textdata'Length);
      pindex  : Positive := 1;
      endmarker : Positive := textdata'Last - 3;
   begin
      if textdata'Length < 8 then
         return product;  --  should never happen
      end if;
      for index in textdata'First .. textdata'First + 2 loop
         product (pindex) := (normal, c_standard, textdata (index));
         pindex := pindex + 1;
      end loop;
      for index in textdata'First + 3 .. endmarker loop
         if textdata (index) = '-' then
            product (pindex) := (normal, c_group_trim, textdata (index));
         else
            product (pindex) := (normal, c_group_text, textdata (index));
         end if;
         pindex := pindex + 1;
      end loop;
      for index in endmarker + 1 .. textdata'Last loop
         product (pindex) := (normal, c_standard, textdata (index));
         pindex := pindex + 1;
      end loop;
      return product;
   end colorize_groups;


   --------------------------------------------------------------------------------------------
   --  colorize_groups
   --------------------------------------------------------------------------------------------
   function colorize_option (textdata : String) return TIC.Attributed_String
   is
      product : TIC.Attributed_String (1 .. textdata'Length);
      pindex  : Positive := 1;
      endmarker : Positive := textdata'Last - 3;
   begin
      if textdata'Length < 8 then
         return product;  --  should never happen
      end if;
      for index in textdata'First .. textdata'First + 2 loop
         product (pindex) := (bright, c_arrow, textdata (index));
         pindex := pindex + 1;
      end loop;
      --  Menu letter, 2 characters
      for index in textdata'First + 3 .. textdata'First + 4 loop
         product (pindex) := (normal, c_letters, textdata (index));
         pindex := pindex + 1;
      end loop;
      --  Tickbox (5 characters)
      for index in textdata'First + 5 .. textdata'First + 9 loop
         product (pindex) := (normal, c_inv_gray, textdata (index));
         pindex := pindex + 1;
      end loop;
      --  Option identifier (14 characters)
      for index in textdata'First + 10 .. textdata'First + 24 loop
         product (pindex) := (normal, c_options, textdata (index));
         pindex := pindex + 1;
      end loop;
      --  Option description (everything else)
      for index in textdata'First + 25 .. endmarker loop
         product (pindex) := (normal, c_inv_gray, textdata (index));
         pindex := pindex + 1;
      end loop;
      for index in endmarker + 1 .. textdata'Last loop
         product (pindex) := (normal, c_standard, textdata (index));
         pindex := pindex + 1;
      end loop;
      return product;
   end colorize_option;


   --------------------------------------------------------------------------------------------
   --  draw_static_keymenu
   --------------------------------------------------------------------------------------------
   procedure draw_static_keymenu
   is
      procedure key_label (S : String; col : TIC.Column_Position; row : TIC.Line_Position);

      subtype full_line is String (1 .. appline_max);
      msg1 : full_line := "       Save option settings        " &
                          "         move highlight arrow               ";
      msg2 : full_line := "       Reset to current values     " &
                          "         Toggle highlighted option setting  ";
      msg3 : full_line := "       Reset to default values     " &
                          "         Toggle associated option setting   ";
      msg4 : full_line := (others => '#');
      line1 : constant appline := custom_message (msg1, bright, c_key_desc);
      line2 : constant appline := custom_message (msg2, bright, c_key_desc);
      line3 : constant appline := custom_message (msg3, bright, c_key_desc);
      line4 : constant appline := custom_message (msg4, bright, c_trimmings);
      title : constant String := title_bar_contents;
      tit_x : constant TIC.Column_Position := index_to_center (title);

      procedure key_label (S : String; col : TIC.Column_Position; row : TIC.Line_Position)
      is
         info : TIC.Attributed_String := custom_message (S, bright, c_standard);
      begin
         Scrawl (keymenu, info, row, col);
      end key_label;
   begin
      Scrawl (keymenu, line1, 0);
      Scrawl (keymenu, line2, 1);
      Scrawl (keymenu, line3, 2);
      Scrawl (keymenu, line4, 3);
      key_label ("F/1:", 2, 0);   key_label ("arrows:", 36, 0);
      key_label ("F/2:", 2, 1);   key_label ("space:",  37, 1);
      key_label ("F/3:", 2, 2);   key_label ("A .. " & last_alphakey & ":", 36, 2);
      Scrawl (keymenu, custom_message (title, bright, c_title), 3, tit_x);
   end draw_static_keymenu;


   --------------------------------------------------------------------------------------------
   --  setup_parameters
   --------------------------------------------------------------------------------------------
   procedure setup_parameters (specification : PSP.Portspecs)
   is
      function str2bool (value : String) return Boolean;
      function button   (linenum : Natural) return String;
      function str2behavior (value : String) return group_type;
      function format (value : String; size : Positive) return String;
      function group_title (value : String; gtype : group_type) return String;

      block   : String := specification.option_block_for_dialog;
      markers : HT.Line_Markers;
      lastgrp : HT.Text;
      linenum : Natural := 0;
      gcount  : Natural := 0;

      function str2bool (value : String) return Boolean is
      begin
         return (value = "1");
      end str2bool;

      function str2behavior (value : String) return group_type is
      begin
         if value = "RESTR" then
            return restrict;
         elsif value = "RADIO" then
            return radio;
         else
            return unlimited;
         end if;
      end str2behavior;

      function button (linenum : Natural) return String
      is
         letter     : Character;
      begin
         if linenum > 26 then
            letter := Character'Val (Character'Pos ('a') - 27 + linenum);
         else
            letter := Character'Val (Character'Pos ('A') - 1 + linenum);
         end if;
         return letter & " ";
      end button;

      function format (value : String; size : Positive) return String
      is
         slate : String (1 .. size) := (others => ' ');
      begin
         if value'Length > size then
            slate := value (value'First .. value'First - 1 + size);
         else
            slate (1 .. value'Length) := value;
         end if;
         return slate;
      end format;

      function group_title (value : String; gtype : group_type) return String is
      begin
         case gtype is
            when radio     => return "[ " & value & " (exactly 1) ]";
            when restrict  => return "[ " & value & " (minimum 1) ]";
            when unlimited => return "[ " & value & " ]";
         end case;
      end group_title;

   begin
      num_std_options := specification.get_list_length (PSP.sp_opts_standard);
      if num_std_options < 27 then
         last_alphakey := Character'Val (Character'Pos ('A') + num_std_options - 1);
      else
         last_alphakey := Character'Val (Character'Pos ('a') + num_std_options - 27);
      end if;
      port_namebase := HT.SUS (specification.get_field_value (PSP.sp_namebase));
      port_sdesc    := HT.SUS (specification.get_tagline (variant_standard));
      port_version  := HT.SUS (specification.get_field_value (PSP.sp_version));

      HT.initialize_markers (block, markers);
      loop
         exit when not HT.next_line_present (block, markers);
         declare
            line   : constant String := HT.extract_line (block, markers);
            grid   : constant String := HT.specific_field (line, 1, ":");
            group  : constant String := HT.specific_field (line, 3, ":");
            gtype  : constant group_type := str2behavior (HT.specific_field (line, 2, ":"));
            title  : constant String := group_title (group, gtype);
            center : constant Natural := ((optentry'Length - title'Length) / 2) + 1;
            cend   : constant Natural := center + title'Length - 1;
         begin
            if not HT.equivalent (lastgrp, grid) then
               --  new group
               num_groups := num_groups + 1;
               linenum := linenum + 1;
               formatted_grps (num_groups).relative_vert := linenum;
               formatted_grps (num_groups).template := (others => '-');
               formatted_grps (num_groups).behavior := gtype;
               formatted_grps (num_groups).template (center .. cend) := title;
               lastgrp := HT.SUS (grid);
               gcount  := gcount + 1;
            end if;
            linenum := linenum + 1;
            num_options := num_options + 1;
            declare
               fopt : optentry_rec renames formatted_opts (num_options);
            begin
               fopt.relative_vert := linenum;
               fopt.default_value := str2bool (HT.specific_field (line, 5, ":"));
               fopt.current_value := str2bool (HT.specific_field (line, 6, ":"));
               fopt.ticked_value  := fopt.current_value;
               fopt.member_group  := gcount;
               fopt.template      := button (num_options) & "[ ] " &
                 format (HT.specific_field (line, 4, ":"), 14) & " " &
                 format (HT.specific_field (line, 7, ":"), 50);
            end;
         end;
      end loop;
   end setup_parameters;


   --------------------------------------------------------------------------------------------
   --  index_to_center
   --------------------------------------------------------------------------------------------
   function index_to_center (display_text : String) return TIC.Column_Position
   is
      --  We want at least "- " to start and " -" to end, so if length of display_text is
      --  greater than appwidth minus 4 set exception.  It should be handled earlier
      max : Natural := Natural (app_width) - 4;
      res : Integer;
   begin
      if display_text'Length > max then
         raise dev_error with "display text is too long";
      end if;

      --  column positions start with "0"
      res := (Natural (app_width) - display_text'Length) / 2;

      return TIC.Column_Position (res);
   end index_to_center;


   --------------------------------------------------------------------------------------------
   --  title_bar_contents
   --------------------------------------------------------------------------------------------
   function title_bar_contents return String
   is
      --  We want at least "- " to start and " -" to end, so the max length is
      --  appwidth minus 4.  We just truncate anything over that.
      max : Natural := Natural (app_width) - 4;
      raw : String := " " & HT.USS (port_sdesc) & " ";
   begin
      if raw'Length < max then
         return raw;
      else
         --  Since sdesc is limited to 50 chars, this should be impossible
         return raw (raw'First .. raw'First + max - 1);
      end if;
   end title_bar_contents;


   --------------------------------------------------------------------------------------------
   --  handle_user_commands
   --------------------------------------------------------------------------------------------
   procedure handle_user_commands
   is
      KeyCode         : TIC.Real_Key_Code;
      Key_Num1        : constant TIC.Key_Code := Character'Pos ('1');
      Key_Num2        : constant TIC.Key_Code := Character'Pos ('2');
      Key_Num3        : constant TIC.Key_Code := Character'Pos ('3');
      Key_Num4        : constant TIC.Key_Code := Character'Pos ('4');
      Key_Space       : constant TIC.Key_Code := Character'Pos (' ');
      Key_Option_01   : constant TIC.Key_Code := Character'Pos ('A');
      Key_Option_26   : constant TIC.Key_Code := Character'Pos ('Z');
      Key_Option_27   : constant TIC.Key_Code := Character'Pos ('a');
      Key_Option_52   : constant TIC.Key_Code := Character'Pos ('z');
      Key_Option_Last : constant TIC.Key_Code := Character'Pos (last_alphakey);
      option_index    : Positive;

      use type TIC.Real_Key_Code;
   begin
      TIC.Set_KeyPad_Mode (Win => zone_keymenu, SwitchOn => True);
      loop
         draw_static_dialog;
         populate_dialog;
         Refresh_Zone (keymenu);
         Refresh_Zone (dialog);
         KeyCode := TIC.Get_Keystroke (zone_keymenu);
         case KeyCode is
            when TIC.Key_Cursor_Up | TIC.Key_Cursor_Left =>
               if arrow_points > 1 then
                  arrow_points := arrow_points - 1;
               end if;
            when TIC.Key_Cursor_Down | TIC.Key_Cursor_Right =>
               if arrow_points < num_std_options then
                  arrow_points := arrow_points + 1;
               end if;
            when TIC.Key_F1 | Key_Num1 =>
               save_options;
               exit;
            when TIC.Key_F2 | Key_Num2 =>
               --  Reset to current
               for x in 1 .. num_std_options loop
                  formatted_opts (x).ticked_value := formatted_opts (x).current_value;
               end loop;
            when TIC.Key_F3 | Key_Num3 =>
               --  Reset to default
               for x in 1 .. num_std_options loop
                  formatted_opts (x).ticked_value := formatted_opts (x).default_value;
               end loop;
            when Key_Option_01 .. Key_Option_26 =>
               if KeyCode <= Key_Option_Last then
                  option_index := Positive (KeyCode - Key_Option_01 + 1);
                  toggle_option (option_index);
               end if;
            when Key_Option_27 .. Key_Option_52 =>
               if num_std_options < 27 then
                  option_index := Positive (KeyCode - Key_Option_27 + 1);
               else
                  option_index := Positive (KeyCode - Key_Option_01 + 1);
               end if;
               toggle_option (option_index);
            when Key_Space =>
               toggle_option (arrow_points);
            when others => null;
         end case;
      end loop;
   end handle_user_commands;


   --------------------------------------------------------------------------------------------
   --  draw_static_dialog
   --------------------------------------------------------------------------------------------
   procedure draw_static_dialog
   is
      function option_content (index : Positive) return String;

      viewheight  : Integer := Integer (TIC.Lines) - 4;
      full_length : Natural := num_groups + num_options + 1;
      S4          : constant String := "    ";
      SARROW      : constant String := " >  ";
      blank_line  : String (1 .. appline_max) := (others => ' ');
      title_line  : String := blank_line;
      title_text  : String := HT.USS (port_namebase) & "-" & HT.USS (port_version);
      tcenter     : Natural := Natural (index_to_center (title_text));
      ATS_BLANK   : appline := custom_message (blank_line, normal, c_standard);
      ATS_FOOTER  : appline := custom_message (blank_line, normal, c_optbox_title);
      ATS_TITLE   : appline;
      scrollable  : Boolean;
      titlerow    : constant Positive := 1;

      function option_content (index : Positive) return String is
      begin
         if index = arrow_points then
            return SARROW & formatted_opts (index).template & S4;
         else
            return S4 & formatted_opts (index).template & S4;
         end if;
      end option_content;

   begin
      offset := 0;
      --  The zero row is alway left blank
      scrollable := (full_length + titlerow > viewheight);
      if scrollable then
         declare
            arrow_line : Positive := formatted_opts (arrow_points).relative_vert;
            magic_line : Integer  := viewheight - 4;
         begin
            --  Arrow can't go below 3 from the bottom
            if arrow_line > magic_line then
               offset := arrow_line - magic_line;
            end if;
         end;
      end if;

      Scrawl (dialog, ATS_BLANK, TIC.Line_Position (0));

      if offset = 0 then
         if title_text'Length > appline_max then
            title_line :=
              title_text (title_text'First .. title_text'First - 1 + Integer (app_width));
         else
            title_line (tcenter  .. tcenter - 1 + title_text'Length) := title_text;
         end if;
         ATS_TITLE := custom_message (title_line, normal, c_optbox_title);
         touch_up (ATS_TITLE, 1, 3, normal, c_standard);
         touch_up (ATS_TITLE, 77, 3, normal, c_standard);
         Scrawl (dialog, ATS_TITLE, TIC.Line_Position (titlerow));
      end if;

      for x in 1 .. num_options loop
         declare
            linepos : Integer := 1 + formatted_opts (x).relative_vert - offset;
            ATS : TIC.Attributed_String := colorize_option (option_content (x));
         begin
            if linepos > titlerow then
               Scrawl (dialog, ATS, TIC.Line_Position (linepos));
            end if;
         end;
      end loop;
      for x in 1 .. num_groups loop
         declare
            linepos : Integer := 1 + formatted_grps (x).relative_vert - offset;
            ATS : TIC.Attributed_String := colorize_groups (S4 & formatted_grps (x).template & S4);
         begin
            if linepos > titlerow then
               Scrawl (dialog, ATS, TIC.Line_Position (linepos));
            end if;
         end;
      end loop;

      touch_up (ATS_FOOTER, 1, 3, normal, c_standard);
      touch_up (ATS_FOOTER, 77, 3, normal, c_standard);
      Scrawl (dialog, ATS_FOOTER, TIC.Line_Position (titlerow + full_length - offset));

      for x in titlerow + full_length - offset + 1 .. viewheight loop
         Scrawl (dialog, ATS_BLANK, TIC.Line_Position (x));
      end loop;
   end draw_static_dialog;


   --------------------------------------------------------------------------------------------
   --  populate_dialog
   --------------------------------------------------------------------------------------------
   procedure populate_dialog
   is
      mark_x_on    : TIC.Attributed_String (1 .. 3) := (1 => (normal, c_inv_gray, '['),
                                                        2 => (normal, c_tick_on, 'x'),
                                                        3 => (normal, c_inv_gray, ']'));
      mark_x_delta : TIC.Attributed_String (1 .. 3) := (1 => (bright, c_tick_delta, '['),
                                                        2 => (normal, c_tick_delta, 'x'),
                                                        3 => (bright, c_tick_delta, ']'));
      mark_blank   : TIC.Attributed_String (1 .. 3) := (1 => (normal, c_inv_gray, '['),
                                                        2 => (normal, c_inv_gray, ' '),
                                                        3 => (normal, c_inv_gray, ']'));
      mark_blank_delta : TIC.Attributed_String (1 .. 3) := (1 => (bright, c_tick_delta, '['),
                                                            2 => (normal, c_inv_gray, ' '),
                                                            3 => (bright, c_tick_delta, ']'));
      fline        : TIC.Line_Position;
      vertical     : Integer;
      changed      : Boolean;
   begin
      for x in 1 .. num_std_options loop
         vertical := formatted_opts (x).relative_vert + 1 - offset;
         if vertical > 1 then
            fline := TIC.Line_Position (vertical);
            changed := (formatted_opts (x).ticked_value /= formatted_opts (x).current_value);
            if formatted_opts (x).ticked_value then
               if changed then
                  Scrawl (dialog, mark_x_delta, fline, 6);
               else
                  Scrawl (dialog, mark_x_on, fline, 6);
               end if;
            else
               if changed then
                  Scrawl (dialog, mark_blank_delta, fline, 6);
               else
                  Scrawl (dialog, mark_blank, fline, 6);
               end if;
            end if;
         end if;
      end loop;
   end populate_dialog;


   --------------------------------------------------------------------------------------------
   --  toggle_option
   --------------------------------------------------------------------------------------------
   procedure toggle_option (option_index : Positive)
   is
      --  RADIO groups have to have at least one option selected.  Selecting an unset member
      --  of a radio group causes the other members to unset (should be only 1).  Likewise
      --  selected a set member normally has no effect.
      --  For a restricted group, the last member cannot be unset.
      gtype   : group_type;
      allowed : Boolean;
   begin
      if formatted_opts (option_index).member_group = 0 then
         --  Ungrouped option, no restrictions
         formatted_opts (option_index).ticked_value :=
           not formatted_opts (option_index).ticked_value;
         return;
      end if;
      gtype := formatted_grps (formatted_opts (option_index).member_group).behavior;
      case gtype is
         when unlimited =>
            formatted_opts (option_index).ticked_value :=
              not formatted_opts (option_index).ticked_value;
         when radio =>
            for x in 1 .. num_std_options loop
               if formatted_opts (x).member_group = formatted_opts (option_index).member_group then
                  formatted_opts (x).ticked_value := (x = option_index);
               end if;
            end loop;
         when restrict =>
            if not formatted_opts (option_index).ticked_value then
               --  It's being set on which is always permitted
               formatted_opts (option_index).ticked_value := True;
            else
               --  It's being turned off, which is only allowed if there's another option
               --  in the group set on.
               allowed := False;
               for x in 1 .. num_std_options loop
                  if formatted_opts (x).member_group = formatted_opts (option_index).member_group
                  then
                     if x /= option_index then
                        if formatted_opts (x).ticked_value then
                           allowed := True;
                        end if;
                     end if;
                  end if;
               end loop;
               if allowed then
                  formatted_opts (option_index).ticked_value := False;
               end if;
            end if;
      end case;
   end toggle_option;


   --------------------------------------------------------------------------------------------
   --  save_options
   --------------------------------------------------------------------------------------------
   procedure save_options
   is
      matches_defaults : Boolean := True;
      section1 : constant String := "parameters";
      section2 : constant String := "options";
      dir_opt  : constant String := HT.USS (Parameters.configuration.dir_options);
      namebase : constant String := HT.USS (port_namebase);
      cookie   : constant String := dir_opt & "/defconf_cookies/" & namebase;
      optfile  : constant String := dir_opt & "/" & namebase;
      optlist  : HT.Text;
   begin
      for x in 1 .. num_std_options loop
         if formatted_opts (x).ticked_value /= formatted_opts (x).default_value then
            matches_defaults := False;
         end if;
      end loop;
      if matches_defaults then
         --  If mode is to record all options, we create an options file every when they
         --  match the defaults, otherwise we remove existing files.
         --  Cookies are not checked under "record_options
         if Parameters.configuration.record_options then
            if DIR.Exists (cookie) then
               DIR.Delete_File (cookie);
            end if;
         else
            if DIR.Exists (optfile) then
               DIR.Delete_File (optfile);
            end if;

            if Parameters.configuration.batch_mode then
               if DIR.Exists (cookie) then
                  DIR.Delete_File (cookie);
               end if;
            else
               if not DIR.Exists (cookie) then
                  FOP.create_cookie (cookie);
               end if;
            end if;
            return;
         end if;
      end if;

      --  Create/overwrite options configure
      IFM.clear_section_data;

      for x in 1 .. num_std_options loop
         declare
            NAME : String := HT.trim (HT.substring (formatted_opts (x).template, 6, 52));
         begin
            IFM.insert_or_update (section => section2,
                                  name    => NAME,
                                  value   => HT.bool2str (formatted_opts (x).ticked_value));
            if HT.IsBlank (optlist) then
               optlist := HT.SUS (NAME);
            else
               HT.SU.Append (optlist, "," & NAME);
            end if;
         end;
      end loop;
      IFM.insert_or_update (section1, "namebase", namebase);
      IFM.insert_or_update (section1, "version",  HT.USS (port_version));
      IFM.insert_or_update (section1, "available", HT.USS (optlist));

      IFM.scribe_file (directory     => dir_opt,
                       filename      => namebase,
                       first_comment => "Option configuration for the " & namebase &
                         " standard variant");
   end save_options;

end Options_Dialog;
