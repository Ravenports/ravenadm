--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

private with Ada.Characters.Latin_1;
private with Definitions;

package Information is

   procedure display_usage;
   procedure display_unknown_command (first_word : String);
   procedure display_unknown_command (first_word, second_word : String);

private

   use Definitions;
   package LAT renames Ada.Characters.Latin_1;

   suggestion : constant String := "Perhaps try " & LAT.Quotation & raven_tool & LAT.Space &
                                   "help" & LAT.Quotation & "?";

   procedure print_header;

end Information;
