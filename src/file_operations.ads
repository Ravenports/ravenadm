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
      relative_filename    : String);

end File_Operations;
