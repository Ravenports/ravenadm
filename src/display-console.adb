--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package body Display.Console is

   --  These are dummy routines to match active ones in Display.Curses
   --  This module is only used to support building ravenadm without curses support

   function launch_monitor (num_builders : builders) return Boolean is
   begin
      pragma Unreferenced (num_builders);
      return False;
   end launch_monitor;

   procedure terminate_monitor is
   begin
      null;
   end terminate_monitor;

   procedure summarize (data : summary_rec) is
   begin
      pragma Unreferenced (data);
      null;
   end summarize;

   procedure update_builder (BR : builder_rec) is
   begin
      pragma Unreferenced (BR);
      null;
   end update_builder;

   procedure set_full_redraw_next_update is
   begin
      null;
   end set_full_redraw_next_update;

   procedure refresh_builder_window is
   begin
      null;
   end refresh_builder_window;

   procedure refresh_history_window is
   begin
      null;
   end refresh_history_window;

end Display.Console;
