--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with File_Operations;
with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Text_IO;

package body INI_File_Manager is

   package LAT renames Ada.Characters.Latin_1;
   package DIR renames Ada.Directories;
   package TIO renames Ada.Text_IO;
   package FOP renames File_Operations;

   --------------------------------------------------------------------------------------------
   --  scribe_file
   --------------------------------------------------------------------------------------------
   procedure scribe_file (directory, filename, first_comment : String)
   is
      package sorter is new string_crate.Generic_Sorting ("<" => HT.SU."<");

      procedure write_section     (section_name : HT.Text);
      procedure write_section     (position : string_crate.Cursor);
      procedure save_section_name (position : list_crate.Cursor);

      fullpath : String := directory & "/" & filename;
      ini_file : TIO.File_Type;
      sections : string_crate.Vector;

      procedure write_section (section_name : HT.Text)
      is
         procedure write_item (position : nvpair_crate.Cursor);

         section : String := HT.USS (section_name);

         procedure write_item (position : nvpair_crate.Cursor)
         is
            name  : String := HT.USS (nvpair_crate.Key (position));
            value : String := HT.USS (nvpair_crate.Element (position));
         begin
            TIO.Put_Line (name & "= " & value);
         end write_item;
      begin
         TIO.Put_Line (ini_file, LAT.LF & LAT.Left_Square_Bracket & section &
                         LAT.Right_Square_Bracket);
         INI_sections.Element (section_name).list.Iterate (write_item'Access);
      end write_section;

      procedure save_section_name (position : list_crate.Cursor) is
      begin
         sections.Append (list_crate.Element (position).section);
      end save_section_name;

      procedure write_section (position : string_crate.Cursor)
      is
         section_name : HT.Text := string_crate.Element (position);
      begin
         write_section (section_name);
      end write_section;
   begin
      INI_sections.Iterate (save_section_name'Access);
      sorter.Sort (Container => sections);

      FOP.mkdirp_from_filename (fullpath);
      TIO.Create (File => ini_file,
                  Mode => TIO.Out_File,
                  Name => fullpath);

      TIO.Put_Line (ini_file, "; " & first_comment);
      TIO.Put_Line (ini_file, "; Take care when hand editing!");

      sections.Iterate (write_section'Access);
      TIO.Close (ini_file);
   exception
      when others =>
         if TIO.Is_Open (ini_file) then
            TIO.Close (ini_file);
         end if;
         raise file_operation_failed;
   end scribe_file;


   --------------------------------------------------------------------------------------------
   --  Delete_Section
   --------------------------------------------------------------------------------------------
   procedure delete_section (section : String)
   is
      section_text : HT.Text := HT.SUS (section);
   begin
      if INI_sections.Contains (section_text) then
         INI_sections.Delete (section_text);
      end if;
   end delete_section;


   --------------------------------------------------------------------------------------------
   --  delete_nv_pair
   --------------------------------------------------------------------------------------------
   procedure delete_nv_pair (section, name : String)
   is
      procedure delete_nvpair (Key : HT.Text; Element : in out group_list);

      section_text : HT.Text := HT.SUS (section);
      name_text    : HT.Text := HT.SUS (name);

      procedure delete_nvpair (Key : HT.Text; Element : in out group_list) is
      begin
         Element.list.Delete (name_text);
      end delete_nvpair;
   begin
      if INI_sections.Contains (section_text) then
         if INI_sections.Element (section_text).list.Contains (name_text) then
            INI_sections.Update_Element (Position => INI_sections.Find (section_text),
                                         Process  => delete_nvpair'Access);
         end if;
      end if;
   end delete_nv_pair;


   --------------------------------------------------------------------------------------------
   --  Insert_or_Update
   --------------------------------------------------------------------------------------------
   procedure insert_or_update (section, name, value : String)
   is
      procedure upsert (Key : HT.Text; Element : in out group_list);
      procedure update (Key : HT.Text; Element : in out HT.Text);

      section_text : HT.Text := HT.SUS (section);
      name_text    : HT.Text := HT.SUS (name);
      value_text   : HT.Text := HT.SUS (value);
      initial_rec  : group_list;

      procedure update (Key : HT.Text; Element : in out HT.Text) is
      begin
         Element := value_text;
      end update;

      procedure upsert (Key : HT.Text; Element : in out group_list) is
      begin
         if Element.list.Contains (name_text) then
            Element.list.Update_Element (Position => Element.list.Find (name_text),
                                         Process  => update'Access);
         else
            Element.list.Insert (name_text, value_text);
         end if;
      end upsert;
   begin
      if INI_sections.Contains (section_text) then
         INI_sections.Update_Element (Position => INI_sections.Find (section_text),
                                      Process  => upsert'Access);
      else
         initial_rec.section := section_text;
         initial_rec.index   := 1;
         initial_rec.list.Insert (name_text, value_text);
         initial_rec.cursor  := nvpair_crate.First (initial_rec.list);
         INI_sections.Insert (section_text, initial_rec);
      end if;
   end insert_or_update;


   --------------------------------------------------------------------------------------------
   --  section_count
   --------------------------------------------------------------------------------------------
   function section_count return Natural is
   begin
      return Natural (INI_sections.Length);
   end section_count;


   --------------------------------------------------------------------------------------------
   --  field_count
   --------------------------------------------------------------------------------------------
   function field_count (section : String) return Natural
   is
      section_text : HT.Text := HT.SUS (section);
   begin
      if INI_sections.Contains (section_text) then
         return Natural (INI_sections.Element (section_text).list.Length);
      else
         return 0;
      end if;
   end field_count;


   --------------------------------------------------------------------------------------------
   --  section_reset
   --------------------------------------------------------------------------------------------
   procedure section_list_reset (section : String)
   is
      procedure reset_cursor (Key : HT.Text; Element : in out group_list);

      section_text : HT.Text := HT.SUS (section);
      first_position : nvpair_crate.Cursor;

      procedure reset_cursor (Key : HT.Text; Element : in out group_list) is
      begin
         Element.cursor := first_position;
         Element.index  := 1;
      end reset_cursor;
   begin
      if INI_sections.Contains (section_text) then
         first_position := nvpair_crate.First (INI_sections.Element (section_text).list);
         INI_sections.Update_Element (Position => INI_sections.Find (section_text),
                                      Process  => reset_cursor'Access);
      end if;
   end section_list_reset;


   --------------------------------------------------------------------------------------------
   --  show_name
   --------------------------------------------------------------------------------------------
   function show_name (section : String) return String
   is
      section_text : HT.Text := HT.SUS (section);
      position     : nvpair_crate.Cursor;
   begin
      if INI_sections.Contains (section_text) then
         position := INI_sections.Element (section_text).cursor;
         if nvpair_crate.Has_Element (position) then
            return HT.USS (nvpair_crate.Element (position));
         end if;
      end if;
      return "";
   end show_name;


   --------------------------------------------------------------------------------------------
   --  show_value #1
   --------------------------------------------------------------------------------------------
   function show_value (section : String) return String
   is
      section_text : HT.Text := HT.SUS (section);
      position     : nvpair_crate.Cursor;
   begin
      if INI_sections.Contains (section_text) then
         position := INI_sections.Element (section_text).cursor;
         if nvpair_crate.Has_Element (position) then
            return HT.USS (nvpair_crate.Key (position));
         end if;
      end if;
      return "";
   end show_value;


   --------------------------------------------------------------------------------------------
   --  show_value #2
   --------------------------------------------------------------------------------------------
   function show_value (section, name : String) return String
   is
      section_text : HT.Text := HT.SUS (section);
      name_text    : HT.Text := HT.SUS (name);
   begin
      if INI_sections.Contains (section_text) then
         if INI_sections.Element (section_text).list.Contains (name_text) then
            return HT.USS (INI_sections.Element (section_text).list.Element (name_text));
         end if;
      end if;
      return "";
   end show_value;


   --------------------------------------------------------------------------------------------
   --  advance_section_list
   --------------------------------------------------------------------------------------------
   function advance_section_list (section : String) return Boolean
   is
      procedure advance (Key : HT.Text; Element : in out group_list);

      section_text : HT.Text := HT.SUS (section);

      procedure advance (Key : HT.Text; Element : in out group_list) is
      begin
         nvpair_crate.Next (Element.cursor);
         Element.index := Element.index + 1;
      end advance;
   begin
      if INI_sections.Contains (section_text) then
         if INI_sections.Element (section_text).index < field_count (section) then
            INI_sections.Update_Element (Position => INI_sections.Find (section_text),
                                         Process  => advance'Access);
            return True;
         end if;
      end if;
      return False;
   end advance_section_list;


   --------------------------------------------------------------------------------------------
   --  section_name
   --------------------------------------------------------------------------------------------
   function section_name (index : Positive) return String
   is
      position : list_crate.Cursor;
      tracker  : Positive := 1;
   begin
      if not INI_sections.Is_Empty and then
        Natural (INI_sections.Length) <= index
      then
         position := list_crate.First (INI_sections);
         loop
            exit when tracker = index;
            tracker := tracker + 1;
            list_crate.Next (position);
         end loop;
         return HT.USS (list_crate.Element (position).section);
      end if;
      return "";
   end section_name;


   --------------------------------------------------------------------------------------------
   --  scan_file
   --------------------------------------------------------------------------------------------
   procedure scan_file (directory, filename : String)
   is
      procedure insert (Key : HT.Text; Element : in out group_list);

      fullpath : String := directory & "/" & filename;
      name_text  : HT.Text;
      value_text : HT.Text;

      procedure insert (Key : HT.Text; Element : in out group_list) is
      begin
         Element.list.Insert (name_text, value_text);
      end insert;
   begin
      if not DIR.Exists (fullpath) then
         raise ini_file_nonexistent;
      end if;
      declare
         contents     : String := FOP.get_file_contents (fullpath);
         markers      : HT.Line_Markers;
         last_section : HT.Text := HT.blank;
         linenum      : Natural := 0;
      begin
         INI_sections.Clear;
         HT.initialize_markers (contents, markers);
         loop
            exit when not HT.next_line_present (contents, markers);
            linenum := linenum + 1;
            declare
               line : constant String := HT.trim (HT.extract_line (contents, markers));
               LN   : constant String := "Line" & linenum'Img & ": ";
            begin
               if not (line = "") and then line (line'First) /= LAT.Semicolon then
                  if line (line'First) = LAT.Left_Square_Bracket then
                     if line (line'Last) = LAT.Right_Square_Bracket then
                        last_section := HT.SUS (HT.partial_search (line, 1, "]"));
                        if HT.SU.Length (last_section) /= line'Length - 2 then
                           raise bad_ini_format with LN & "heading contains ']'";
                        end if;
                        if INI_sections.Contains (last_section) then
                           raise bad_ini_format with LN & "duplicate heading found";
                        end if;
                        declare
                           initial_rec  : group_list;
                        begin
                           initial_rec.section := last_section;
                           initial_rec.index   := 0;
                           INI_sections.Insert (last_section, initial_rec);
                        end;
                     else
                        raise bad_ini_format with LN & "heading not terminated with ']'";
                     end if;
                  end if;
                  if not HT.contains (line, "=") then
                     raise bad_ini_format with LN & "missing '=', so not a name-value pair";
                  end if;
                  if HT.equivalent (last_section, HT.blank) then
                     raise bad_ini_format with LN & "name-value pair found before section set";
                  end if;

                  name_text := HT.SUS (HT.trim (HT.part_1 (line, "=")));
                  value_text := HT.SUS (HT.trim (HT.part_2 (line, "=")));
                  if INI_sections.Element (last_section).list.Contains (name_text) then
                     raise bad_ini_format with LN & "duplicate key '" & HT.USS (name_text)
                       & "' found in section '" & HT.USS (last_section) & "'";
                  else
                     INI_sections.Update_Element (Position => INI_sections.Find (last_section),
                                                  Process  => insert'Access);
                  end if;
               end if;
            end;
         end loop;
      exception
         when others =>
            raise file_operation_failed;
      end;
   end scan_file;


   --------------------------------------------------------------------------------------------
   --  section_exists
   --------------------------------------------------------------------------------------------
   function section_exists (section : String) return Boolean
   is
      section_text : HT.Text := HT.SUS (section);
   begin
      return INI_sections.Contains (section_text);
   end section_exists;

end INI_File_Manager;
