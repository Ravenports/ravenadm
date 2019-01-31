--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Display.Console is

   --  These are dummy routines to match active ones in Display.Curses
   --  This module is only used to support building ravenadm without curses support

   function launch_monitor (num_builders : builders) return Boolean;
   procedure terminate_monitor;
   procedure summarize (data : summary_rec);
   procedure update_builder (BR : builder_rec);
   procedure set_full_redraw_next_update;
   procedure refresh_builder_window;
   procedure refresh_history_window;

end Display.Console;
