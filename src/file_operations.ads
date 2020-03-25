--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package File_Operations is

   file_handling    : exception;

   --  Generic function to scan a text file and convert to a string
   function get_file_contents (dossier : String) return String;

   --  Generic procedure to convert a string to a new text file
   --  exception thrown on failures
   procedure dump_contents_to_file
     (contents : String;
      dossier  : String);

   --  If relative_filename contains no path separator ("/") do nothing
   --  Otherwise test the existence of extraction_directory / subdirectory and if it
   --  does not exist, create it.
   procedure create_subdirectory
     (extraction_directory : String;
      subdirectory         : String);

   --  generic function to return first line of file.
   function head_n1 (filename : String) return String;

   --  Create a pidfile on major actions and remove it when complete.
   procedure create_pidfile (pidfile : String);
   procedure destroy_pidfile (pidfile : String);

   --  Equivalent to "mkdir -p <path/to/final/directory>
   --  May throw exeption
   procedure mkdirp_from_filename (filename : String);

   --  Adds the contents of "another_file" to the existing contents of basefile.
   --  If "basefile" does not exist, just copy it (directory must already exist in that case)
   procedure concatenate_file (basefile : String; another_file : String);

   --  Creates a zero-length file
   procedure create_cookie (fullpath : String);

   --  Replace pattern-matching files in target directory from the source directory
   procedure replace_directory_contents
     (source_directory : String;
      target_directory : String;
      pattern          : String);

   --  If runpath does not contain the string "$ORIGIN" then runpath is returned
   --  Otherwise, the $ORIGIN text is replaced by the base directory of the filename
   --             and the resulting text is returned.
   function convert_ORIGIN_in_runpath (filename : String; runpath : String) return String;

private

   --  helper for create_pidfile
   function Get_PID return Integer;
   pragma Import (C, Get_PID, "getpid");

end File_Operations;
