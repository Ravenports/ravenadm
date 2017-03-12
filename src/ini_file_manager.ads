--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Vectors;
private with HelperText;

package INI_File_Manager is

   directory_nonexistent : exception;
   file_operation_failed : exception;

   --  Create new INI File or overwrite existing one.
   --  Throws exception if writing failed
   --  It will attempt to create the directory (and throw exception if that fails)
   --  Sections are sorted before writing
   procedure scribe_file (directory, filename, first_comment : String);

   --  If section exists, remove from internal storage.
   --  Typical use: read INI File, remove section, Scribe_File
   procedure delete_section (section : String);

   --  If the section doesn't exist, create it in memory.
   --  If the nvpair already exists overwrite value, otherwise insert it.
   procedure insert_or_update (section, name, value : String);

   --  If the section exists and the field within the section exists, remove it from memory
   procedure delete_nv_pair (section, name : String);

   --  Scans an existing INI file.
   --  Throws exception if directory does not exist, or if reading file fails
   procedure scan_file (directory, filename : String);

   --  Returns number of sections present
   function section_count return Natural;

   --  Return name of section given its index
   function section_name (index : Positive) return String;

   --  Return number of nvpairs in a given section
   function field_count (section : String) return Natural;

   --  Call before iterating through section
   procedure section_list_reset (section : String);

   --  returns the name part of NV pair of current cursor
   function show_name (section : String) return String;

   --  returns the value part of the NV pair of current cursor
   function show_value (section : String) return String;

   --  Attempt to advance cursor of section list.  If it's in the last position, return False.
   function advance_section_list (section : String) return Boolean;

   --  Return value of field identified by "name" within given section
   function show_value (section, name : String) return String;

private

   package HT  renames HelperText;
   package CON renames Ada.Containers;

   package nvpair_crate is new CON.Hashed_Maps
     (Key_Type        => HT.Text,
      Element_Type    => HT.Text,
      Hash            => HT.hash,
      Equivalent_Keys => HT.equivalent,
      "="             => HT.SU."=");

   type group_list is
      record
         section : HT.Text;
         list    : nvpair_crate.Map;
         index   : Natural;
         cursor  : nvpair_crate.Cursor;
      end record;

   package list_crate is new CON.Hashed_Maps
     (Key_Type        => HT.Text,
      Element_Type    => group_list,
      Hash            => HT.hash,
      Equivalent_Keys => HT.equivalent);

   package string_crate is new CON.Vectors
     (Element_Type => HT.Text,
      Index_Type   => Positive,
      "="          => HT.SU."=");

   INI_sections : list_crate.Map;

end INI_File_Manager;
