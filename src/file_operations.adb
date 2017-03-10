--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Directories;
with Ada.Direct_IO;

package body File_Operations is

   package DIR renames Ada.Directories;

   --------------------------------------------------------------------------------------------
   --  get_file_contents
   --------------------------------------------------------------------------------------------
   function get_file_contents (dossier : String) return String
   is
      File_Size : Natural := Natural (DIR.Size (dossier));

      subtype File_String    is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);

      file_handle : File_String_IO.File_Type;
      contents    : File_String;
   begin
      File_String_IO.Open  (File => file_handle,
                            Mode => File_String_IO.In_File,
                            Name => dossier);
      File_String_IO.Read  (File => file_handle,
                            Item => contents);
      File_String_IO.Close (file_handle);
      return contents;
   exception
      when others =>
         if File_String_IO.Is_Open (file_handle) then
            File_String_IO.Close (file_handle);
         end if;
         raise file_handling;
   end get_file_contents;


   --------------------------------------------------------------------------------------------
   --  dump_contents_to_file
   --------------------------------------------------------------------------------------------
   procedure dump_contents_to_file
     (contents : String;
      dossier  : String)
   is
      File_Size : Natural := contents'Length;

      subtype File_String    is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);

      file_handle : File_String_IO.File_Type;
   begin
      File_String_IO.Create (File => file_handle,
                             Mode => File_String_IO.Out_File,
                             Name => dossier);
      File_String_IO.Write  (File => file_handle,
                             Item => contents);
      File_String_IO.Close  (file_handle);
   exception
      when others =>
         if File_String_IO.Is_Open (file_handle) then
            File_String_IO.Close (file_handle);
         end if;
         raise file_handling;
   end dump_contents_to_file;


   --------------------------------------------------------------------------------------------
   --  create_subdirectory
   --------------------------------------------------------------------------------------------
   procedure create_subdirectory
     (extraction_directory : String;
      subdirectory         : String)
   is
      abspath : String := extraction_directory & "/" & subdirectory;
   begin
      if subdirectory = "" then
         return;
      end if;
      if not DIR.Exists (abspath) then
         DIR.Create_Directory (abspath);
      end if;
   end create_subdirectory;

end File_Operations;
