--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Terminal_Interface.Curses;
with Port_Specification;
with HelperText;

package Options_Dialog is

   package TIC renames Terminal_Interface.Curses;
   package PSP renames Port_Specification;
   package HT  renames HelperText;

   --  Initialize the curses screen.
   --  Returns False if no color support (curses not used at all)
   function launch_dialog (specification : in out PSP.Portspecs) return Boolean;

   dev_error : exception;

private

   appline_max : constant Positive := 79;

   type zones is (keymenu, dialog);
   subtype appline is TIC.Attributed_String (1 .. appline_max);

   type palette_rec is
      record
         palette   : TIC.Color_Pair;
         attribute : TIC.Character_Attribute_Set;
      end record;

   cursor_vis    : TIC.Cursor_Visibility := TIC.Invisible;

   normal        : constant TIC.Character_Attribute_Set :=
                            (others => False);
   bright        : constant TIC.Character_Attribute_Set :=
                            (Bold_Character => True, others => False);
   dimmed        : constant TIC.Character_Attribute_Set :=
                            (Dim_Character => True, others => False);
   app_width     : constant TIC.Column_Count := 80;

   zone_keymenu  : TIC.Window;
   zone_dialog   : TIC.Window;

   c_standard    : TIC.Color_Pair;
   c_key_desc    : TIC.Color_Pair;
   c_title       : TIC.Color_Pair;
   c_trimmings   : TIC.Color_Pair;

   last_alphakey   : Character := 'A';
   num_std_options : Natural;
   port_namebase   : HT.Text;
   port_sdesc      : HT.Text;


   function establish_colors return Boolean;
   function Start_Curses_Mode return Boolean;
   function launch_keymenu_zone return Boolean;
   function launch_dialog_zone return Boolean;
   function zone_window (zone : zones) return TIC.Window;
   function index_to_center (display_text : String) return TIC.Column_Position;
   function title_bar_contents return String;
   procedure Refresh_Zone (zone : zones);
   procedure Return_To_Text_Mode;
   procedure draw_static_keymenu;
   procedure terminate_dialog;
   procedure setup_parameters (specification : PSP.Portspecs);
   procedure handle_user_commands;

   procedure Scrawl
     (zone        : zones;
      information : TIC.Attributed_String;
      at_line     : TIC.Line_Position;
      at_column   : TIC.Column_Position := 0);

   function custom_message (message   : String;
                            attribute : TIC.Character_Attribute_Set;
                            pen_color : TIC.Color_Pair) return TIC.Attributed_String;

end Options_Dialog;
