--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Directories;
with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Information;
with Specification_Parser;

package body Pilot is

   package NFO renames Information;
   package PAR renames Specification_Parser;
   package LAT renames Ada.Characters.Latin_1;
   package DIR renames Ada.Directories;
   package TIO renames Ada.Text_IO;

   --------------------------------------------------------------------------------------------
   --  display_usage
   --------------------------------------------------------------------------------------------
   procedure display_usage is
   begin
      NFO.display_usage;
   end display_usage;

   --------------------------------------------------------------------------------------------
   --  react_to_unknown_first_level_command
   --------------------------------------------------------------------------------------------
   procedure react_to_unknown_first_level_command (argument : String) is
   begin
      NFO.display_unknown_command (argument);
   end react_to_unknown_first_level_command;


   --------------------------------------------------------------------------------------------
   --  react_to_unknown_second_level_command
   --------------------------------------------------------------------------------------------
   procedure react_to_unknown_second_level_command (level1, level2 : String) is
   begin
      NFO.display_unknown_command (level1, level2);
   end react_to_unknown_second_level_command;


   --------------------------------------------------------------------------------------------
   --  dump_ravensource
   --------------------------------------------------------------------------------------------
   procedure dump_ravensource (optional_directory : String)
   is
      procedure DNE (filename : String);

      directory_specified : constant Boolean := (optional_directory /= "");
      specfile   : constant String := "specification";
      errprefix  : constant String := "Error : ";
      successful : Boolean;

      procedure DNE (filename : String) is
      begin
         TIO.Put_Line (errprefix & "File " & LAT.Quotation & filename & LAT.Quotation &
                         " does not exist.");
      end DNE;
   begin
      if directory_specified then
         declare
            filename : String := optional_directory & "/" & specfile;
         begin
            if DIR.Exists (filename) then
               PAR.parse_specification_file (filename, successful);
            else
               DNE (filename);
               return;
            end if;
         end;
      else
         if DIR.Exists (specfile) then
            PAR.parse_specification_file (specfile, successful);
         else
            DNE (specfile);
            return;
         end if;
      end if;
      if successful then
         PAR.specification.dump_specification;
      else
         TIO.Put_Line (errprefix & "Failed to parse " & specfile);
         TIO.Put_Line (PAR.get_parse_error);
      end if;
   end dump_ravensource;

end Pilot;
