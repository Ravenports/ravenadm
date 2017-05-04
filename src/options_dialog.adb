--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions;  use Definitions;
with Ada.Text_IO;
with Ada.Exceptions;
with Unix;

package body Options_Dialog is

   package TIO renames Ada.Text_IO;
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

      Refresh_Zone (keymenu);
      Refresh_Zone (dialog);

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
         TIC.Init_Pair (TIC.Color_Pair (1), TIC.White,   TIC.Black);
         TIC.Init_Pair (TIC.Color_Pair (2), TIC.Cyan,    TIC.Black);
         TIC.Init_Pair (TIC.Color_Pair (3), TIC.Green,   TIC.Black);
         TIC.Init_Pair (TIC.Color_Pair (4), TIC.Black,   TIC.Black);

--         TIC.Init_Pair (TIC.Color_Pair (3), TIC.Red,     TIC.Black);
--         TIC.Init_Pair (TIC.Color_Pair (4), TIC.Yellow,  TIC.Black);
--         TIC.Init_Pair (TIC.Color_Pair (7), TIC.Blue,    TIC.Black);
--         TIC.Init_Pair (TIC.Color_Pair (8), TIC.Magenta, TIC.Black);
--         TIC.Init_Pair (TIC.Color_Pair (9), TIC.Blue,    TIC.White);
      exception
         when TIC.Curses_Exception => return False;
      end;

      c_standard    := TIC.Color_Pair (1);
      c_key_desc    := TIC.Color_Pair (2);
      c_title       := TIC.Color_Pair (3);
      c_trimmings   := TIC.Color_Pair (4);


--
--        c_slave  (1).palette   := TIC.Color_Pair (1);  --  white / Black
--        c_slave  (1).attribute := bright;
--
--        c_slave  (2).palette   := TIC.Color_Pair (2);  --  light green / Black
--        c_slave  (2).attribute := bright;
--
--        c_slave  (3).palette   := TIC.Color_Pair (4);  --  yellow / Black
--        c_slave  (3).attribute := bright;
--
--        c_slave  (4).palette   := TIC.Color_Pair (8);  --  light magenta / Black
--        c_slave  (4).attribute := bright;
--
--        c_slave  (5).palette   := TIC.Color_Pair (3);  --  light red / Black
--        c_slave  (5).attribute := bright;
--
--        c_slave  (6).palette   := TIC.Color_Pair (7);  --  light blue / Black
--        c_slave  (6).attribute := bright;
--
--        c_slave  (7).palette   := TIC.Color_Pair (6);  --  light cyan / Black
--        c_slave  (7).attribute := bright;
--
--        c_slave  (8).palette   := TIC.Color_Pair (5);  --  dark grey / Black
--        c_slave  (8).attribute := bright;
--
--        c_slave  (9).palette   := TIC.Color_Pair (1);  --  light grey / Black
--        c_slave  (9).attribute := normal;
--
--        c_slave (10).palette   := TIC.Color_Pair (2);  --  light green / Black
--        c_slave (10).attribute := normal;
--
--        c_slave (11).palette   := TIC.Color_Pair (4);  --  brown / Black
--        c_slave (11).attribute := normal;
--
--        c_slave (12).palette   := TIC.Color_Pair (8);  --  dark magenta / Black
--        c_slave (12).attribute := normal;
--
--        c_slave (13).palette   := TIC.Color_Pair (3);  --  dark red / Black
--        c_slave (13).attribute := normal;
--
--        c_slave (14).palette   := TIC.Color_Pair (7);  --  dark blue / Black
--        c_slave (14).attribute := normal;
--
--        c_slave (15).palette   := TIC.Color_Pair (6);  --  dark cyan / Black
--        c_slave (15).attribute := normal;
--
--        c_slave (16).palette   := TIC.Color_Pair (9);  --  white / dark blue
--        c_slave (16).attribute := normal;

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
      --  55 limit comes from 26x2 + 1 (header) + 1x2 (margin)
   begin
      zone_dialog := TIC.Create (Number_Of_Lines       => 55,
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
   --  draw_static_keymenu
   --------------------------------------------------------------------------------------------
   procedure draw_static_keymenu
   is
      procedure key_label (S : String; col : TIC.Column_Position; row : TIC.Line_Position);

      subtype full_line is String (1 .. appline_max);
      msg1 : full_line := "       Save option settings        " &
                          "         move highlight bar                 ";
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
   procedure setup_parameters (specification : PSP.Portspecs) is
   begin
      num_std_options := specification.get_list_length (PSP.sp_opts_standard);
      if num_std_options < 27 then
         last_alphakey := Character'Val (Character'Pos ('A') + num_std_options - 1);
      else
         last_alphakey := Character'Val (Character'Pos ('a') + num_std_options - 27);
      end if;
      port_namebase := HT.SUS (specification.get_field_value (PSP.sp_namebase));
      port_sdesc    := HT.SUS (specification.get_tagline (variant_standard));
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
      KeyCode       : TIC.Real_Key_Code;
      Key_Num1      : constant TIC.Key_Code := Character'Pos ('1');
      Key_Num2      : constant TIC.Key_Code := Character'Pos ('2');
      Key_Num3      : constant TIC.Key_Code := Character'Pos ('3');
      Key_Num4      : constant TIC.Key_Code := Character'Pos ('4');
      Key_Space     : constant TIC.Key_Code := Character'Pos (' ');
      Key_Option_01 : constant TIC.Key_Code := Character'Pos ('A');
      Key_Option_26 : constant TIC.Key_Code := Character'Pos ('Z');
      Key_Option_27 : constant TIC.Key_Code := Character'Pos ('a');
      Key_Option_52 : constant TIC.Key_Code := Character'Pos ('z');
      Key_Option_Last : constant TIC.Key_Code := Character'Pos (last_alphakey);

      use type TIC.Real_Key_Code;
   begin
      loop
         KeyCode := TIC.Get_Keystroke (zone_keymenu);
         case KeyCode is
            when TIC.Key_Cursor_Up | TIC.Key_Cursor_Left =>
               null;
            when TIC.Key_Cursor_Down | TIC.Key_Cursor_Right =>
               null;
            when TIC.Key_F1 | Key_Num1 =>
               --  save options
               exit;
            when TIC.Key_F2 | Key_Num2 => null;
            when TIC.Key_F3 | Key_Num3 => null;
            when Key_Option_01 .. Key_Option_26 =>
               if KeyCode <= Key_Option_Last then
                  --  do something
                  null;
               end if;
            when Key_Option_27 .. Key_Option_52 =>
               if num_std_options < 27 then
                  --  Treat lower case as equivalent to upper case
                  null;
               else
                  --  Upper and lower case are distinct
                  null;
               end if;
            when others => null;
         end case;
      end loop;
   end handle_user_commands;

end Options_Dialog;
