--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Text_IO;

package body Information is

   package TIO renames Ada.Text_IO;

   --------------------------------------------------------------------------------------------
   --  display_usage
   --------------------------------------------------------------------------------------------
   procedure display_usage is
   begin
      print_header;
      TIO.Put_Line ("Type " & LAT.Quotation & raven_tool & " help" & LAT.Quotation &
                      " for detailed usage information." & LAT.LF);
   end display_usage;


   --------------------------------------------------------------------------------------------
   --  display_unknown_command #1
   --------------------------------------------------------------------------------------------
   procedure display_unknown_command (first_word : String) is
   begin
      print_header;
      TIO.Put_Line ("Command unrecognized: " & LAT.Quotation & raven_tool & LAT.Space &
                      first_word & LAT.Quotation);
      TIO.Put_Line (suggestion);
   end display_unknown_command;


   --------------------------------------------------------------------------------------------
   --  display_unknown_command #2
   --------------------------------------------------------------------------------------------
   procedure display_unknown_command (first_word, second_word : String) is
   begin
      print_header;
      if second_word = "" then
         TIO.Put_Line ("The " &  LAT.Quotation & raven_tool & LAT.Space & first_word &
                         LAT.Quotation & " command requires a subcommand.");
      else
         TIO.Put_Line ("The " & LAT.Quotation & raven_tool & LAT.Space & first_word &
                         LAT.Quotation & " subcommand " & LAT.Quotation & second_word &
                         LAT.Quotation & " is unrecognized.");
      end if;
      TIO.Put_Line (suggestion);
   end display_unknown_command;


   --------------------------------------------------------------------------------------------
   --  print_header
   --------------------------------------------------------------------------------------------
   procedure print_header
   is
      version   : String := "  v" & raven_version_major & "." & raven_version_minor;
      copyright : String := "Copyright (C) " & copyright_years & " John R. Marino";
      tagline   : String := "Ravenports Universal Package System Administration Tool " & version;
      dashes    : String (1 .. tagline'Length + 4) := (others => LAT.Equals_Sign);
      gap       : String (1 .. (dashes'Length - copyright'Length) / 2) := (others => LAT.Space);
   begin
      TIO.Put_Line (LAT.LF & dashes);
      TIO.Put_Line ("  " & tagline);
      TIO.Put_Line (dashes);
      TIO.Put_Line (gap & copyright & LAT.LF);
   end print_header;

end Information;
