--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Pilot is

   procedure display_usage;
   procedure react_to_unknown_first_level_command (argument : String);
   procedure react_to_unknown_second_level_command (level1, level2 : String);
   procedure dump_ravensource (optional_directory : String);
   --  These are just a placeholders
   procedure generate_makefile (optional_directory : String;
                                optional_variant : String);
   procedure generate_buildsheet (optional_directory : String);

private

   specfile  : constant String := "specification";
   errprefix : constant String := "Error : ";

   procedure DNE (filename : String);

end Pilot;
