--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Pilot is

   procedure display_usage;
   procedure react_to_unknown_first_level_command (argument : String);
   procedure react_to_unknown_second_level_command (level1, level2 : String);
   procedure dump_ravensource (optional_directory : String);

end Pilot;
