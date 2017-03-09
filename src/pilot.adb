--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Directories;
with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Information;
with Specification_Parser;
with Port_Specification.Makefile;
with Port_Specification.Transform;

with Definitions; use Definitions;

package body Pilot is

   package NFO renames Information;
   package PAR renames Specification_Parser;
   package PSM renames Port_Specification.Makefile;
   package PST renames Port_Specification.Transform;
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
               PAR.parse_specification_file (filename, successful, False);
            else
               DNE (filename);
               return;
            end if;
         end;
      else
         if DIR.Exists (specfile) then
            PAR.parse_specification_file (specfile, successful, False);
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


   --------------------------------------------------------------------------------------------
   --  generate_makefile
   --------------------------------------------------------------------------------------------
   procedure generate_makefile (optional_directory : String;
                                optional_variant : String)
   is
      procedure DNE (filename : String);
      function get_variant return String;

      directory_specified : constant Boolean := (optional_directory /= "");
      specfile   : constant String := "specification";
      errprefix  : constant String := "Error : ";
      successful : Boolean;

      procedure DNE (filename : String) is
      begin
         TIO.Put_Line (errprefix & "File " & LAT.Quotation & filename & LAT.Quotation &
                         " does not exist.");
      end DNE;

      function get_variant return String is
      begin
         if optional_variant = "" then
            return variant_standard;
         else
            return optional_variant;
         end if;
      end get_variant;
   begin
      if directory_specified then
         declare
            filename : String := optional_directory & "/" & specfile;
         begin
            if DIR.Exists (filename) then
               PAR.parse_specification_file (filename, successful, False);
            else
               DNE (filename);
               return;
            end if;
         end;
      else
         if DIR.Exists (specfile) then
            PAR.parse_specification_file (specfile, successful, False);
         else
            DNE (specfile);
            return;
         end if;
      end if;
      if successful then
         PST.set_option_defaults (specs         => PAR.specification,
                                  variant       => get_variant,
                                  opsys         => dragonfly,
                                  arch_standard => x86_64,
                                  osrelease     => "4.7");
         PST.set_option_to_default_values (specs => PAR.specification);
         PST.set_outstanding_ignore (specs         => PAR.specification,
                                     variant       => get_variant,
                                     opsys         => dragonfly,
                                     arch_standard => x86_64,
                                     osrelease     => "4.7",
                                     osmajor       => "4.7");
         PST.apply_directives (specs => PAR.specification);
         PSM.generator (specs         => PAR.specification,
                        variant       => get_variant,
                        opsys         => dragonfly,
                        output_file   => "");
      else
         TIO.Put_Line (errprefix & "Failed to parse " & specfile);
         TIO.Put_Line (PAR.get_parse_error);
      end if;
   end generate_makefile;

end Pilot;
