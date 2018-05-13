--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Text_IO;
with File_Operations;
with HelperText;

package body Package_Manifests
is
   package DIR renames Ada.Directories;
   package CON renames Ada.Containers;
   package TIO renames Ada.Text_IO;
   package FOP renames File_Operations;
   package HT  renames HelperText;

   --------------------------------------------------------------------------------------------
   --  compress_manifest #1
   --------------------------------------------------------------------------------------------
   procedure compress_manifest
     (old_manifest : Filename;
      new_manifest : Filename)
   is
      compressed_contents : constant String := compress_manifest (old_manifest);
   begin
      FOP.dump_contents_to_file (compressed_contents, String (new_manifest));
   exception
      when file_handling => raise compress_issue;
   end compress_manifest;


   --------------------------------------------------------------------------------------------
   --  compress_manifest #2
   --------------------------------------------------------------------------------------------
   function compress_manifest (manifest : Filename) return String
   is
      package crate is new CON.Hashed_Maps
        (Key_Type        => HT.Text,
         Element_Type    => Natural,
         Hash            => HT.hash,
         Equivalent_Keys => HT.equivalent);

      function next_line return Boolean;
      function last_slash return Natural;
      procedure increment (Key : HT.Text; Element : in out Natural);
      procedure save_line (line : String);

      back_marker   : Natural;
      back_marker2  : Natural := 0;
      front_marker  : Natural;
      front_marker2 : Natural := 0;
      contents      : String := FOP.get_file_contents (String (manifest));
      canvas        : String (1 .. contents'Length);
      dircount      : crate.Map;
      new_key       : HT.Text;
      last_key      : HT.Text;
      spos          : Natural;
      empty_plist   : constant String := "@comment manifest is empty, mistake??";

      function next_line return Boolean is
      begin
         if front_marker > 1 and then
           front_marker + 2 > contents'Last then
            --  lines are at least one character long
            return False;
         end if;

         --  Any manifest with 2 consecutive line feeds will break the logic, so just
         --  truncate immediately if as soon as this is detected
         if contents (front_marker) = ASCII.LF then
            return False;
         end if;

         if front_marker > contents'First then
            back_marker  := front_marker + 2;
            front_marker := back_marker;
         end if;
         loop
            exit when front_marker = contents'Last;
            exit when contents (front_marker + 1) = ASCII.LF;
            front_marker := front_marker + 1;
         end loop;
         return True;
      end next_line;

      function last_slash return Natural
      is
         marker : Natural := front_marker;
      begin
         if contents (front_marker) = '/' then
            return 0;  --  Unexpected, probably plist error
         end if;
         loop
            exit when marker < back_marker;
            marker := marker - 1;
            if contents (marker) = '/' then
               return marker;
            end if;
         end loop;
         return 0;
      end last_slash;

      procedure increment (Key : HT.Text; Element : in out Natural) is
      begin
         Element := Element + 1;
      end increment;

      procedure save_line (line : String) is
      begin
         if line'Length > 0 then
            back_marker2 := front_marker2 + 1;
            front_marker2 := front_marker2 + line'Length;
            canvas (back_marker2 .. front_marker2) := line;
            front_marker2 := front_marker2 + 1;
            canvas (front_marker2) := ASCII.LF;
         end if;
      end save_line;

   begin
      if contents'Length < 2 then
         return empty_plist;
      end if;
      back_marker  := contents'First;
      front_marker := contents'First;
      loop
         exit when not next_line;
         if not (contents (back_marker) = ASCII.At_Sign) then
            spos := last_slash;
            if spos > 1 then
               new_key := HT.SUS (contents (back_marker .. spos - 1));
               if dircount.Contains (new_key) then
                  dircount.Update_Element (Position => dircount.Find (new_key),
                                           Process => increment'Access);
               else
                  dircount.Insert (new_key, 1);
               end if;
            end if;
         end if;
      end loop;
      back_marker  := contents'First;
      front_marker := contents'First;
      loop
         exit when not next_line;
         if contents (back_marker) = ASCII.At_Sign then
            save_line (contents (back_marker .. front_marker));
         else
            spos := last_slash;
            if spos > 1 then
               new_key := HT.SUS (contents (back_marker .. spos - 1));
               if crate.Element (dircount.Find (new_key)) = 1 then
                  --  Directory only has one file, so just them together
                  save_line (contents (back_marker .. front_marker));
               else
                  if not HT.equivalent (last_key, new_key) then
                     declare
                        folder : String := contents (back_marker .. spos - 1) & "/";
                     begin
                        save_line (folder);
                     end;
                  end if;
                  declare
                     single_file : String := " " & contents (spos + 1 .. front_marker);
                  begin
                     save_line (single_file);
                  end;
                  last_key := new_key;
               end if;
            else
               --  Likely a file in $PREFIX directory
               --  Otherwise, something unexpected so just keep it in any case
               save_line (contents (back_marker .. front_marker));
            end if;
         end if;
      end loop;
      if front_marker2 = 0 then
         return empty_plist;
      else
         return canvas (canvas'First .. front_marker2);
      end if;
   exception
      when file_handling => raise compress_issue;
   end compress_manifest;


   --------------------------------------------------------------------------------------------
   --  decompress_manifest_file
   --------------------------------------------------------------------------------------------
   procedure decompress_manifest_file
     (compressed_file : Filename;
      save_to_file    : Filename)
   is
      compressed_contents : constant String := FOP.get_file_contents (String (compressed_file));
   begin
      decompress_manifest (compressed_string => compressed_contents,
                           save_to_file      => save_to_file);
   exception
      when file_handling => raise decompress_issue;
   end decompress_manifest_file;


   --------------------------------------------------------------------------------------------
   --  decompress_manifest
   --------------------------------------------------------------------------------------------
   procedure decompress_manifest
     (compressed_string : String;
      save_to_file      : Filename)
   is
      --  Like the compress routine, decompress requires two passes.
      --  Pass 1 to calculate how long the result will be and
      --  Pass 2 to write the result after allocation

      function next_line return Boolean;

      back_marker   : Natural;
      front_marker  : Natural;
      line_size     : Natural;
      out_size      : Natural := 0;
      folder_size   : Natural := 0;

      function next_line return Boolean is
      begin
         if front_marker + 2 > compressed_string'Last then
            return False;
         end if;
         if front_marker > compressed_string'First then
            back_marker  := front_marker + 2;
            front_marker := back_marker;
         end if;
         loop
            exit when front_marker = compressed_string'Last;
            exit when compressed_string (front_marker + 1) = ASCII.LF;
            front_marker := front_marker + 1;
         end loop;
         return True;
      end next_line;
   begin
      if compressed_string'Length < 2 then
         raise decompress_issue;
      end if;
      back_marker  := compressed_string'First;
      front_marker := compressed_string'First;
      loop
         exit when not next_line;
         line_size := (front_marker - back_marker) + 2;  --  includes LF
         if compressed_string (back_marker) = ASCII.At_Sign then
            out_size := out_size + line_size;
         else
            if compressed_string (back_marker) = ' ' then
               if folder_size = 0 then
                  raise decompress_issue;
               end if;
               out_size := out_size + folder_size + line_size - 1;
            else
               if compressed_string (front_marker) = '/' then
                  --  Folder, don't count yet, but note size minus LF
                  folder_size := line_size - 1;
               else
                  --  Should be folder/file single entry; take it all
                  out_size := out_size + line_size;
               end if;
            end if;
         end if;
      end loop;

      back_marker  := compressed_string'First;
      front_marker := compressed_string'First;
      declare
         procedure save_line (line : String);

         result        : String (1 .. out_size);
         back_marker2  : Natural := 0;
         front_marker2 : Natural := 0;
         last_folder   : HT.Text;

         procedure save_line (line : String) is
         begin
            if line'Length > 0 then
               back_marker2 := front_marker2 + 1;
               front_marker2 := front_marker2 + line'Length;
               result (back_marker2 .. front_marker2) := line;
               front_marker2 := front_marker2 + 1;
               result (front_marker2) := ASCII.LF;
            end if;
         end save_line;
      begin
         loop
            exit when not next_line;
            if compressed_string (back_marker) = ASCII.At_Sign then
               save_line (compressed_string (back_marker .. front_marker));
            else
               if compressed_string (back_marker) = ' ' then
                  save_line (HT.USS (last_folder) &
                               compressed_string (back_marker + 1 .. front_marker));
               else
                  if compressed_string (front_marker) = '/' then
                     last_folder := HT.SUS (compressed_string (back_marker .. front_marker));
                  else
                     save_line (compressed_string (back_marker .. front_marker));
                  end if;
               end if;
            end if;
         end loop;

         FOP.dump_contents_to_file (contents => result,
                                    dossier  => String (save_to_file));
      exception
         when file_handling => raise decompress_issue;
      end;

   end decompress_manifest;


   --------------------------------------------------------------------------------------------
   --  sort_manifest
   --------------------------------------------------------------------------------------------
   procedure sort_manifest (manifest : Filename)
   is
      temp_file : Filename := manifest & ".sorted";
      succeeded : constant Boolean := save_sorted_manifest (manifest, temp_file);
   begin
      if not succeeded then
         raise sorting_issue;
      end if;
      DIR.Delete_File (String (manifest));
      DIR.Rename (String (temp_file), String (manifest));
   exception
      when others =>
         raise sorting_issue;
   end sort_manifest;


   --------------------------------------------------------------------------------------------
   --  save_sorted_manifest
   --------------------------------------------------------------------------------------------
   function save_sorted_manifest
     (old_manifest    : Filename;
      sorted_manifest : Filename) return Boolean
   is
      type manifest_file is
         record
            mode     : HT.Text;
            owner    : HT.Text;
            group    : HT.Text;
         end record;

      --  Key is the filename
      package file_crate is new CON.Hashed_Maps
        (Key_Type        => HT.Text,
         Element_Type    => manifest_file,
         Hash            => HT.hash,
         Equivalent_Keys => HT.equivalent);

      type manifest_directory is
         record
            folder : HT.Text;
            files  : file_crate.Map;
         end record;

      --  Key is the directory name (folder).  The manifest.folder is a copy of the key.
      package folder_crate is new CON.Hashed_Maps
        (Key_Type        => HT.Text,
         Element_Type    => manifest_directory,
         Hash            => HT.hash,
         Equivalent_Keys => HT.equivalent);

      package keyword_crate is new CON.Vectors
        (Element_Type => HT.Text,
         Index_Type   => Positive,
         "="          => HT.SU."=");

      package keyword_sorter is new keyword_crate.Generic_Sorting ("<" => HT.SU."<");

      function is_root_file (teststring : String) return Boolean;
      function file_perms   (teststring : String) return manifest_file;
      function just_file    (teststring : String) return String;
      function perms_string (perms : manifest_file) return String;

      procedure add_list_root (filename : String; perms : manifest_file);
      procedure add_to_folder (filename : String; perms : manifest_file);
      procedure add_keyword   (keyword_line : String);
      procedure write_keywords (top : Boolean);
      procedure write_root_files;
      procedure write_folders;

      store_keywords : keyword_crate.Vector;
      store_folders  : folder_crate.Map;
      store_root     : file_crate.Map;
      store_order    : keyword_crate.Vector;
      current_mode   : HT.Text := HT.blank;
      current_owner  : HT.Text := HT.blank;
      current_group  : HT.Text := HT.blank;
      new_manifest   : TIO.File_Type;


      --   ------------------------------------------------
      function is_root_file (teststring : String) return Boolean is
      begin
         if HT.leads (teststring, "@(") then
            declare
               filepart : String := HT.trim (HT.part_2 (teststring, ")"));
            begin
               return not HT.contains (filepart, "/");
            end;
         else
            return not HT.contains (teststring, "/");
         end if;
      end is_root_file;
      --   ------------------------------------------------


      --   ------------------------------------------------
      function just_file (teststring : String) return String is
      begin
         if HT.leads (teststring, "@(") then
            return HT.trim (HT.part_2 (teststring, ")"));
         else
            return HT.trim (teststring);
         end if;
      end just_file;
      --   ------------------------------------------------


      --   ------------------------------------------------
      function file_perms (teststring : String) return manifest_file
      is
         result : manifest_file := (current_mode, current_owner, current_group);
      begin
         if HT.leads (teststring, "@(") then
            declare
               guts : String := HT.part_1 (HT.part_2 (teststring, "@("), ")");
            begin
               if not HT.IsBlank (HT.specific_field (guts, 1, ",")) then
                  result.owner  := HT.SUS (HT.specific_field (guts, 1, ","));
               end if;
               if not HT.IsBlank (HT.specific_field (guts, 2, ",")) then
                  result.group := HT.SUS (HT.specific_field (guts, 2, ","));
               end if;
               if not HT.IsBlank (HT.specific_field (guts, 3, ",")) then
                  result.mode := HT.SUS (HT.specific_field (guts, 3, ","));
               end if;
            end;
         end if;
         return result;
      end file_perms;
      --   ------------------------------------------------


      --   ------------------------------------------------
      procedure add_list_root (filename : String; perms : manifest_file)
      is
         filename_text : HT.Text := HT.SUS (filename);
      begin
         if not store_root.Contains (filename_text) then
            store_root.Insert (filename_text, perms);
         end if;
      end add_list_root;
      --   ------------------------------------------------


      --   ------------------------------------------------
      procedure add_keyword (keyword_line : String) is
         keytext : HT.Text := HT.SUS (keyword_line);
      begin
         if not store_keywords.Contains (keytext) then
            store_keywords.Append (keytext);
         end if;
      end add_keyword;
      --   ------------------------------------------------


      --   ------------------------------------------------
      procedure add_to_folder (filename : String; perms : manifest_file)
      is
         procedure add_file (Key : HT.Text; Element : in out manifest_directory);

         caboose  : HT.Text := HT.SUS (HT.tail (filename, "/"));
         train    : HT.Text := HT.SUS (HT.head (filename, "/"));

         procedure add_file (Key : HT.Text; Element : in out manifest_directory) is
         begin
            Element.files.Insert (caboose, perms);
         end add_file;
      begin
         if store_folders.Contains (train) then
            --  Folder has already been registered
            store_folders.Update_Element (Position => store_folders.Find (train),
                                          Process  => add_file'Access);
         else
            --  Folder needs to be registered with a single file.
            declare
               first_file : manifest_directory;
            begin
               first_file.folder := train;
               first_file.files.Insert (caboose, perms);
               store_folders.Insert (train, first_file);
            end;
            store_order.Append (train);
         end if;
      end add_to_folder;
      --   ------------------------------------------------


      --   ------------------------------------------------
      procedure write_keywords (top : Boolean)
      is
         procedure write_keyword (position : keyword_crate.Cursor);
         procedure write_keyword (position : keyword_crate.Cursor)
         is
            line  : String := HT.USS (keyword_crate.Element (position));
            kword : String := HT.part_1 (line, " ");
         begin
            if kword = "@comment" or else
              kword = "@info" or else
              kword = "@sample" or else
              kword = "@shell"
            then
               if top then
                  TIO.Put_Line (new_manifest, line);
               end if;
            else
               if not top then
                  TIO.Put_Line (new_manifest, line);
               end if;
            end if;
         end write_keyword;
      begin
         store_keywords.Iterate (write_keyword'Access);
      end write_keywords;
      --   ------------------------------------------------


      --   ------------------------------------------------
      procedure write_root_files
      is
         procedure extract_name (position : file_crate.Cursor);
         procedure write_name (position : keyword_crate.Cursor);

         root_words : keyword_crate.Vector;

         procedure extract_name (position : file_crate.Cursor)
         is
            filename : HT.Text renames file_crate.Key (position);
         begin
            root_words.Append (filename);
         end extract_name;

         procedure write_name (position : keyword_crate.Cursor)
         is
            filename : String := HT.USS (keyword_crate.Element (position));
            perms    : manifest_file := store_root.Element (HT.SUS (filename));
         begin
            TIO.Put_Line (new_manifest, perms_string (perms) & filename);
         end write_name;
      begin
         store_root.Iterate (extract_name'Access);
         keyword_sorter.Sort (root_words);
         root_words.Iterate (write_name'Access);
      end write_root_files;
      --   ------------------------------------------------


      --   ------------------------------------------------
      function perms_string (perms : manifest_file) return String is
      begin
         if HT.IsBlank (perms.mode) and then
           HT.IsBlank (perms.owner) and then
           HT.IsBlank (perms.group)
         then
            return "";
         else
            return "@(" & HT.USS (perms.owner) &
              "," & HT.USS (perms.group) &
              "," & HT.USS (perms.mode) & ") ";
         end if;
      end perms_string;
      --   ------------------------------------------------


      --   ------------------------------------------------
      procedure write_folders
      is
         procedure write_folder_contents (position : keyword_crate.Cursor);

         procedure write_folder_contents (position : keyword_crate.Cursor)
         is
            procedure extract_name (file_position : file_crate.Cursor);
            procedure write_name (word_position : keyword_crate.Cursor);

            folder : HT.Text renames keyword_crate.Element (position);
            folder_files : keyword_crate.Vector;

            procedure extract_name (file_position : file_crate.Cursor)
            is
               filename : HT.Text renames file_crate.Key (file_position);
            begin
               folder_files.Append (filename);
            end extract_name;

            procedure write_name (word_position : keyword_crate.Cursor)
            is
               filename : String := HT.USS (keyword_crate.Element (word_position));
               fullpath : String := HT.USS (folder) & "/" & filename;
               perms    : manifest_file :=
                 store_folders.Element (folder).files.Element (HT.SUS (filename));
            begin
               TIO.Put_Line (new_manifest, perms_string (perms) & fullpath);
            end write_name;
         begin
            store_folders.Element (folder).files.Iterate (extract_name'Access);
            keyword_sorter.Sort (folder_files);
            folder_files.Iterate (write_name'Access);
         end write_folder_contents;
      begin
         store_order.Iterate (write_folder_contents'Access);
      end write_folders;
      --   ------------------------------------------------


      contents : String := FOP.get_file_contents (String (old_manifest));

   begin
      --  Go through each line and put in the correct storage buckets
      declare
         markers : HT.Line_Markers;
      begin
         HT.initialize_markers (contents, markers);
         loop
            exit when not HT.next_line_present (contents, markers);
            declare
               line  : constant String := HT.extract_line (contents, markers);
               perms : manifest_file;
            begin
               if HT.leads (line, "@mode") then
                  if HT.trim (line) = "@mode" then
                     current_mode := HT.blank;
                  else
                     current_mode := HT.SUS (HT.trim (HT.part_2 (line, " ")));
                  end if;
               elsif HT.leads (line, "@owner") then
                  if HT.trim (line) = "@owner" then
                     current_owner := HT.blank;
                  else
                     current_owner := HT.SUS (HT.trim (HT.part_2 (line, " ")));
                  end if;
               elsif HT.leads (line, "@group") then
                  if HT.trim (line) = "@group" then
                     current_group := HT.blank;
                  else
                     current_group := HT.SUS (HT.trim (HT.part_2 (line, " ")));
                  end if;
               elsif HT.leads (line, "@(") then
                  perms := file_perms (line);
                  if is_root_file (line) then
                     add_list_root (just_file (line), perms);
                  else
                     add_to_folder (just_file (line), perms);
                  end if;
               elsif HT.leads (line, "@") then
                  add_keyword (HT.trim (line));
               else
                  perms := file_perms (line);
                  if is_root_file (line) then
                     add_list_root (HT.trim (line), perms);
                  else
                     add_to_folder (HT.trim (line), perms);
                  end if;
               end if;
            end;
         end loop;
      end;

      --  Sort keywords and folders
      keyword_sorter.Sort (store_keywords);
      keyword_sorter.Sort (store_order);

      --  manifest order:
      --  some keywords (@comment, @info, @sample, @shell)
      --  root files (rare, most plists don't have these, probably variables)
      --  folders (all files in one folder listed before folder changes)
      --  remaining keywords commands (@dir first)

      TIO.Create (File => new_manifest, Mode => TIO.Out_File, Name => String (sorted_manifest));
      write_keywords (top => True);
      write_root_files;
      write_folders;
      write_keywords (top => False);
      TIO.Close (new_manifest);

      return True;
   exception
      when others =>
         return False;
   end save_sorted_manifest;

end Package_Manifests;
