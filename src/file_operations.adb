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
      File_Size : constant Natural := Natural (DIR.Size (dossier));
   begin
      if File_Size = 0 then
         return "";
      end if;

      declare
         subtype File_String    is String (1 .. File_Size);
         package File_String_IO is new Ada.Direct_IO (File_String);

         file_handle : File_String_IO.File_Type;
         contents    : File_String;
         attempts    : Natural := 0;
      begin

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
                     raise file_handling with "get_file_contents: failed open: " & dossier;
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
            raise file_handling with "get_file_contents(" & dossier & ") failed";
      end;
   exception
      when Storage_Error =>
         raise file_handling with "get_file_contents(" & dossier & ") failed to allocate memory";
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
      when Storage_Error =>
         if File_String_IO.Is_Open (file_handle) then
            File_String_IO.Close (file_handle);
         end if;
         raise file_handling
           with "dump_contents_to_file(" & dossier & ") failed to allocate memory";
      when others =>
         if File_String_IO.Is_Open (file_handle) then
            File_String_IO.Close (file_handle);
         end if;
         raise file_handling
           with "dump_contents_to_file(" & dossier & ") error";
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
         new_runpath : HT.Text := HT.SUS (runpath);
      begin
         loop
            new_runpath := HT.replace_substring (US         => new_runpath,
                                                 old_string => ORIGIN,
                                                 new_string => basedir);
            exit when not HT.contains (new_runpath, ORIGIN);
         end loop;
         return HT.USS (new_runpath);
      end;
   end convert_ORIGIN_in_runpath;


   --------------------------------
   --  update_latest_log_length  --
   --------------------------------
   --  procedure update_latest_log_length
   --    (handle     : in out SIO.File_Type;
   --     num_lines  : in out Natural;
   --     log_offset : in out SIO.Positive_Count)
   --  is
   --     buffer    : Ada.Streams.Stream_Element_Array (1 .. 4096);
   --     last_read : Ada.Streams.Stream_Element_Offset;
   --     linefeed  : constant Ada.Streams.Stream_Element := 10;
   --
   --     use type Ada.Streams.Stream_Element;
   --     use type Ada.Streams.Stream_Element_Offset;
   --     use type SIO.Count;
   --  begin
   --     loop
   --        SIO.Read (File => handle,
   --                  Item => buffer,
   --                  Last => last_read,
   --                  From => log_offset);
   --        for x in 1 .. last_read loop
   --           if buffer (x) = linefeed then
   --              num_lines := num_lines + 1;
   --           end if;
   --        end loop;
   --        log_offset := log_offset + SIO.Positive_Count (last_read);
   --        exit when last_read < buffer'Last;
   --     end loop;
   --  end update_latest_log_length;


   --------------------
   --  lines_in_log  --
   --------------------
   function lines_in_log (filename : String) return Natural
   is
      buffer     : Ada.Streams.Stream_Element_Array (1 .. 4096);
      last_read  : Ada.Streams.Stream_Element_Offset;
      linefeed   : constant Ada.Streams.Stream_Element := 10;
      sio_handle : SIO.File_Type;
      shared     : constant String := "shared=yes";
      num_lines  : Natural := 1;

      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Offset;
      use type SIO.Count;
   begin
      begin
         SIO.Open (File => sio_handle,
                   Mode => SIO.In_File,
                   Name => filename,
                   Form => shared);
      exception
         when error : others =>
            return 0;
      end;

      loop
         SIO.Read (File => sio_handle,
                   Item => buffer,
                   Last => last_read);
         for x in 1 .. last_read loop
            if buffer (x) = linefeed then
               num_lines := num_lines + 1;
            end if;
         end loop;
         exit when last_read < buffer'Last;
      end loop;

      SIO.Close (sio_handle);
      return num_lines;
   end lines_in_log;


   ------------------------------------
   --  reset_distfiles_working_area  --
   ------------------------------------
   procedure reset_distfiles_working_area (distfiles_directory : String)
   is
      transient : constant String := distfiles_directory & "/transient";
   begin
      if not DIR.Exists (transient) then
         return;
      end if;
      case DIR.Kind (transient) is
         when DIR.Directory => null;
         when others => return;
      end case;
      declare
         search : DIR.Search_Type;
         dirent : DIR.Directory_Entry_Type;
         filter : constant DIR.Filter_Type := (DIR.Ordinary_File => True, others => False);
      begin
         DIR.Start_Search (Search    => search,
                           Directory => transient,
                           Pattern   => "",
                           Filter    => filter);
         while DIR.More_Entries (search) loop
            DIR.Get_Next_Entry (search, dirent);
            begin
               DIR.Delete_File (DIR.Full_Name (dirent));
            exception
               when others =>
                  TIO.Put_Line ("Failed to delete " & DIR.Full_Name (dirent));
            end;
         end loop;
         DIR.End_Search (search);
      end;
   end reset_distfiles_working_area;

end File_Operations;
