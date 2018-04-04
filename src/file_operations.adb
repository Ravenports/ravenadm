--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Directories;
with Ada.Direct_IO;
with Ada.Text_IO;
with HelperText;

package body File_Operations is

   package DIR renames Ada.Directories;
   package TIO renames Ada.Text_IO;
   package HT  renames HelperText;

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
      attempts    : Natural := 0;
   begin
      if File_Size = 0 then
         return "";
      end if;
      --  The introduction of variants causes a buildsheet to be scanned once per variant.
      --  It's possible (even common) for simultaneous requests to scan the same buildsheet to
      --  occur.  Thus, if the file is already open, wait and try again (up to 5 times)
      loop
         begin
            File_String_IO.Open  (File => file_handle,
                                  Mode => File_String_IO.In_File,
                                  Name => dossier);
            exit;
         exception
            when File_String_IO.Status_Error | File_String_IO.Use_Error =>
               if attempts = 5 then
                  raise file_handling with "get_file_contents: remained open: " & dossier;
               end if;
               attempts := attempts + 1;
               delay 0.1;
         end;
      end loop;
      File_String_IO.Read  (File => file_handle, Item => contents);
      File_String_IO.Close (file_handle);
      return contents;
   exception
      when others =>
         if File_String_IO.Is_Open (file_handle) then
            File_String_IO.Close (file_handle);
         end if;
         raise file_handling with "get_file_contents: " & dossier;
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
      mkdirp_from_filename (dossier);
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


   --------------------------------------------------------------------------------------------
   --  head_n1
   --------------------------------------------------------------------------------------------
   function head_n1 (filename : String) return String
   is
      handle : TIO.File_Type;
   begin
      TIO.Open (File => handle, Mode => TIO.In_File, Name => filename);
      if TIO.End_Of_File (handle) then
         TIO.Close (handle);
         return "";
      end if;

      declare
         line : constant String := TIO.Get_Line (handle);
      begin
         TIO.Close (handle);
         return line;
      end;
   end head_n1;


   --------------------------------------------------------------------------------------------
   --  create_pidfile
   --------------------------------------------------------------------------------------------
   procedure create_pidfile (pidfile : String)
   is
      pidtext : constant String := HT.int2str (Get_PID);
      handle  : TIO.File_Type;
   begin
      TIO.Create (File => handle, Mode => TIO.Out_File, Name => pidfile);
      TIO.Put_Line (handle, pidtext);
      TIO.Close (handle);
   end create_pidfile;


   --------------------------------------------------------------------------------------------
   --  destroy_pidfile
   --------------------------------------------------------------------------------------------
   procedure destroy_pidfile (pidfile : String) is
   begin
      if DIR.Exists (pidfile) then
         DIR.Delete_File (pidfile);
      end if;
   exception
      when others => null;
   end destroy_pidfile;


   --------------------------------------------------------------------------------------------
   --  mkdirp_from_filename
   --------------------------------------------------------------------------------------------
   procedure mkdirp_from_filename (filename : String)
   is
      condir : String := DIR.Containing_Directory (Name => filename);
   begin
      DIR.Create_Path (New_Directory => condir);
   end mkdirp_from_filename;


   --------------------------------------------------------------------------------------------
   --  concatenate_file
   --------------------------------------------------------------------------------------------
   procedure concatenate_file (basefile : String; another_file : String)
   is
      handle : TIO.File_Type;
   begin
      if not DIR.Exists (another_file) then
         raise file_handling with "concatenate_file: new_file does not exist => " & another_file;
      end if;
      if DIR.Exists (basefile) then
         TIO.Open (File => handle,
                   Mode => TIO.Append_File,
                   Name => basefile);
         declare
            contents : constant String := get_file_contents (another_file);
         begin
            TIO.Put (handle, contents);
         end;
         TIO.Close (handle);
      else
         DIR.Copy_File (Source_Name => another_file,
                        Target_Name => basefile);
      end if;
   exception
      when others =>
         if TIO.Is_Open (handle) then
            TIO.Close (handle);
         end if;
         raise file_handling;
   end concatenate_file;


   --------------------------------------------------------------------------------------------
   --  create_cookie
   --------------------------------------------------------------------------------------------
   procedure create_cookie (fullpath : String)
   is
      subtype File_String    is String (1 .. 1);
      package File_String_IO is new Ada.Direct_IO (File_String);

      file_handle : File_String_IO.File_Type;
   begin
      mkdirp_from_filename (fullpath);
      File_String_IO.Create (File => file_handle,
                             Mode => File_String_IO.Out_File,
                             Name => fullpath);
      File_String_IO.Close  (file_handle);
   exception
      when others =>
         raise file_handling;
   end create_cookie;


   --------------------------------------------------------------------------------------------
   --  replace_directory_contents
   --------------------------------------------------------------------------------------------
   procedure replace_directory_contents
     (source_directory : String;
      target_directory : String;
      pattern          : String)
   is
      search : DIR.Search_Type;
      dirent : DIR.Directory_Entry_Type;
      filter : constant DIR.Filter_Type := (DIR.Ordinary_File => True, others => False);
   begin
      if not DIR.Exists (target_directory) then
         return;
      end if;
      DIR.Start_Search (Search    => search,
                        Directory => target_directory,
                        Pattern   => pattern,
                        Filter    => filter);
      while DIR.More_Entries (search) loop
         DIR.Get_Next_Entry (search, dirent);
         declare
            SN      : constant String := DIR.Simple_Name (dirent);
            oldfile : constant String := target_directory & "/" & SN;
            newfile : constant String := source_directory & "/" & SN;
         begin
            if DIR.Exists (newfile) then
               DIR.Copy_File (Source_Name => newfile, Target_Name => oldfile);
            end if;
         exception
            when others =>
               TIO.Put_Line ("Failed to copy " & newfile & " over to " & oldfile);
         end;
      end loop;
      DIR.End_Search (search);
   end replace_directory_contents;


   --------------------------------------------------------------------------------------------
   --  convert_ORIGIN_in_runpath
   --------------------------------------------------------------------------------------------
   function convert_ORIGIN_in_runpath (filename : String; runpath : String) return String
   is
      ORIGIN : constant String := "$ORIGIN";
   begin
      if not HT.contains (runpath, ORIGIN) then
         return runpath;
      end if;
      declare
         basedir     : constant String := HT.head (filename, "/");
         new_runpath : HT.Text := HT.replace_substring (US         => HT.SUS (runpath),
                                                        old_string => ORIGIN,
                                                        new_string => basedir);
      begin
         return HT.USS (new_runpath);
      end;
   end convert_ORIGIN_in_runpath;

end File_Operations;
