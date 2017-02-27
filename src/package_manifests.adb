with Ada.Containers.Hashed_Maps;
with File_Operations;
with HelperText;

package body Package_Manifests
is
   package CON renames Ada.Containers;
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

      function next_line return Boolean is
      begin
         if front_marker + 2 > contents'Last then
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
         return "";
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
      return canvas (canvas'First .. front_marker2);
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

end Package_Manifests;
