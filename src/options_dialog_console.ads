--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

--  This is the console version of Options_Dialog
--  In effect, options selection is not possible without ncurses

with Port_Specification;

package Options_Dialog_Console is

   package PSP renames Port_Specification;

   function launch_dialog (specification : in out PSP.Portspecs) return Boolean;

end Options_Dialog_Console;
