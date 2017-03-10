--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Text_IO;
with Ada.Characters.Latin_1;

package body Signals is

   package TIO renames Ada.Text_IO;
   package LAT renames Ada.Characters.Latin_1;

   -----------------------------------
   --  graceful_shutdown_requested  --
   -----------------------------------
   function graceful_shutdown_requested return Boolean
   is
      caught_char : Character;
      got_one     : Boolean;
   begin
      TIO.Get_Immediate (Item => caught_char, Available => got_one);
      --  Although the variable is called control_c, it's Control-Q that
      --  we are catching.  It was the ESCAPE key after control-C, but that
      --  one was getting triggered by terminal ANSI codes.  Control-Q
      --  requires the IXON TTY flag off (disabled output flow control).
      if got_one and then caught_char = LAT.DC1 then
         control_q_break := True;
      end if;
      return control_q_break;
   exception
      when others => return control_q_break;
   end graceful_shutdown_requested;

end Signals;
