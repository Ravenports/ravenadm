--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions;  use Definitions;
with Terminal_Interface.Curses;

package Display is

   package TIC renames Terminal_Interface.Curses;

   subtype history_origin  is String (1 .. 45);
   subtype history_elapsed is String (1 .. 8);
   subtype history_action  is String (1 .. 8);
   subtype fivelong        is String (1 .. 5);
   subtype fld_phase       is String (1 .. 12);
   subtype fld_origin      is String (1 .. 40);
   subtype fld_lines       is String (1 .. 7);
   subtype fld_slavid      is String (1 .. 2);
   type history_rec is
      record
         id          : builders;
         slavid      : String (1 .. 2);
         run_elapsed : history_elapsed;
         action      : history_action;
         pkg_elapsed : history_elapsed;
         origin      : history_origin;
         established : Boolean := False;
      end record;

   type summary_rec is
      record
         Initially : Natural;
         Built     : Natural;
         Failed    : Natural;
         Ignored   : Natural;
         Skipped   : Natural;
         elapsed   : history_elapsed;
         impulse   : Natural;
         pkg_hour  : Natural;
         load      : Float;
         swap      : Float;
      end record;

   type builder_rec is
      record
         id        : builders;
         shutdown  : Boolean;
         idle      : Boolean;
         slavid    : fld_slavid;
         Elapsed   : history_elapsed;
         LLines    : fld_lines;
         phase     : fld_phase;
         origin    : fld_origin;
      end record;

   action_shutdown : constant history_action := "shutdown";
   action_skipped  : constant history_action := "skipped ";
   action_ignored  : constant history_action := "ignored ";
   action_success  : constant history_action := "success ";
   action_failure  : constant history_action := "failure ";

   --  Initialize the curses screen.
   --  Returns False if no color support (curses not used at all)
   function launch_monitor (num_builders : builders) return Boolean;

   --  The build is done, return to the console
   procedure terminate_monitor;

   --  prints the summary header
   procedure summarize (data : summary_rec);

   --  Updates the status of a builder (contained in builder_rec)
   procedure update_builder (BR : builder_rec);

   --  After all the update_builder calls, call refresh to implement
   procedure refresh_builder_window;

   --  After all the history inserts, call refresh to implement
   procedure refresh_history_window;

   --  Insert history as builder finishes (shutdown, success, failure);
   procedure insert_history (HR : history_rec);

   --  Clears and redraws the static portion of builder and summary zones
   --  (Realized when the regular zones are refreshed)
   procedure set_full_redraw_next_update;

   --  Expose helper function that formats float values for www report
   function fmtpc (f : Float; percent : Boolean) return fivelong;

   --  Expose helper function that formats load values for www report
   function fmtload (f : Float) return fivelong;

private

   type palette_rec is
      record
         palette   : TIC.Color_Pair;
         attribute : TIC.Character_Attribute_Set;
      end record;

   type builder_palette is array (builders) of palette_rec;
   type cyclic_range is range 1 .. 50;
   type dim_history is array (cyclic_range) of history_rec;
   type zones is (summary, builder, action);
   subtype appline is TIC.Attributed_String (1 .. 79);

   history       : dim_history;
   history_arrow : cyclic_range := cyclic_range'Last;
   builders_used : Integer;

   app_width     : constant TIC.Column_Count := 80;
   historyheight : TIC.Line_Position;
   zone_summary  : TIC.Window;
   zone_builders : TIC.Window;
   zone_actions  : TIC.Window;
   viewheight    : TIC.Line_Count;

   c_standard    : TIC.Color_Pair;
   c_slave       : builder_palette;
   c_success     : TIC.Color_Pair;
   c_failure     : TIC.Color_Pair;
   c_ignored     : TIC.Color_Pair;
   c_skipped     : TIC.Color_Pair;
   c_sumlabel    : TIC.Color_Pair;
   c_dashes      : TIC.Color_Pair;
   c_tableheader : TIC.Color_Pair;
   c_elapsed     : TIC.Color_Pair;
   c_origin      : TIC.Color_Pair;
   c_bldphase    : TIC.Color_Pair;
   c_shutdown    : TIC.Color_Pair;
   c_advisory    : TIC.Color_Pair;

   cursor_vis    : TIC.Cursor_Visibility := TIC.Invisible;

   normal        : constant TIC.Character_Attribute_Set :=
                            (others => False);
   bright        : constant TIC.Character_Attribute_Set :=
                            (Bold_Character => True, others => False);
   dimmed        : constant TIC.Character_Attribute_Set :=
                            (Dim_Character => True, others => False);

   function launch_summary_zone  return Boolean;
   function launch_builders_zone return Boolean;
   function launch_actions_zone  return Boolean;

   function inc (X : TIC.Line_Position; by : Integer) return TIC.Line_Position;
   function zone_window (zone : zones) return TIC.Window;
   function Start_Curses_Mode return Boolean;
   function establish_colors return Boolean;
   function blank_line return appline;
   function shutdown_message return appline;
   function emphasis       (dimmed    : Boolean) return TIC.Character_Attribute_Set;
   function custom_message (message   : String;
                            attribute : TIC.Character_Attribute_Set;
                            pen_color : TIC.Color_Pair) return TIC.Attributed_String;

   procedure draw_static_summary_zone;
   procedure draw_static_builders_zone;

   procedure Scrawl
     (zone        : zones;
      information : TIC.Attributed_String;
      at_line     : TIC.Line_Position;
      at_column   : TIC.Column_Position := 0);

   procedure Return_To_Text_Mode;
   procedure Refresh_Zone (zone : zones);

   procedure log_non_curses_exception (message_text : String);

end Display;
