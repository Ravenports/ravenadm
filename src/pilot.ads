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

   procedure explode_buildsheet (extract_to_directory : String;
                                 optional_variant : String);

   --  Return True when TERM is defined in environment (required)
   function TERM_defined_in_environment return Boolean;

   --  The current working directory will cause ravenadm to fail
   --  Known issue for when cwd is within <system root>/usr/local
   function launch_clash_detected return Boolean;

   --  Returns True if the root user didn't execute ravenadm.
   function insufficient_privileges return Boolean;

   --  Returns True if a pidfile is found and it's a valid ravenadm process
   function already_running return Boolean;

   --  Create a pidfile on major actions and remove it when complete.
   procedure create_pidfile;
   procedure destroy_pidfile;

private

   specfile  : constant String := "specification";
   errprefix : constant String := "Error : ";
   pidfile   : constant String := "/var/run/ravenadm.pid";

   procedure DNE (filename : String);

end Pilot;
