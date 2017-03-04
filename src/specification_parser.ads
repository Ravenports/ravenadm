--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Port_Specification;

private with HelperText;
private with Ada.Containers.Hashed_Maps;

package Specification_Parser is

   package PSP renames Port_Specification;

   specification : PSP.Portspecs;

   --  Parse the port specification file and extract the data into the specification record.
   procedure parse_specification_file
     (dossier : String;
      success : out Boolean);

   --  If the parse procedure fails, this function returns the associated error message
   function get_parse_error return String;

private

   package HT  renames HelperText;
   package CON renames Ada.Containers;

   package def_crate is new CON.Hashed_Maps
        (Key_Type        => HT.Text,
         Element_Type    => HT.Text,
         Hash            => HT.hash,
         Equivalent_Keys => HT.equivalent,
         "="             => HT.SU."=");

   type spec_array   is (not_array, def, sdesc, sites, distfile, spkgs, vopts,
                         ext_head, ext_tail);
   type spec_singlet is (not_singlet, namebase, version, revision, epoch, keywords, variants,
                         contacts, dl_groups, dist_subdir, df_index, opt_avail, exc_opsys,
                         inc_opsys, exc_arch, ext_only, ext_zip, ext_7z, ext_lha, ext_dirty,
                         distname, skip_build, destdir_env, build_wrksrc, makefile, destdirname);
   type type_category is (cat_none, cat_array, cat_singlet);

   last_parse_error   : HT.Text;
   spec_definitions   : def_crate.Map;

   missing_definition : exception;
   bad_modifier       : exception;
   expansion_too_long : exception;
   mistabbed          : exception;
   integer_expected   : exception;
   extra_spaces       : exception;
   duplicate_key      : exception;
   generic_format     : exception;

   --  This looks for the pattern ${something}.  If not found, the original value is returned.
   --  Otherwise it looks up "something".  If that's not a definition, the missing_definition
   --  exception is thrown, otherwise it's expanded.  If the $something contains a modifier
   --  (column followed by code) and that modifier is unknown or misused, the bad_modifier
   --  exception is thrown.  Upon cycle, repeat until no more patterns found, then return
   --  final expanded value.  If the length of the expanded value exceeds 512 bytes, the
   --  expansion_too_long exception is thrown.
   function expand_value (value : String) return String;

   --  If the line represents a recognized array type, indicate which one,
   --  otherwise return "not_array"
   function determine_array (line : String) return spec_array;

   --  If the line represents a recognized singlet type, indicate which one,
   --  otherwise return "not_singlet"
   function determine_singlet (line : String) return spec_singlet;

   --  Returns everything following the tab(s) until end of line.  If last tab doesn't align
   --  text with column 24, the mistabbed exception is thrown.
   function retrieve_single_value (line : String) return String;

   --  Calls retrieve_single_value and tries to convert to a natural number.
   function retrieve_single_integer (line : String) return Natural;

   --  Returns the key for array item definition lines.
   function retrieve_key (line : String; previous_index : HT.Text) return HT.Text;

   --  Line may contain spaces, and each space is considered a single item on a list.
   --  This iterates through the value with space delimiters.
   procedure build_list (field : PSP.spec_field; line : String);

   --  Line may contain spaces and they are considered part of an entire string
   procedure build_string (field : PSP.spec_field; line : String);

   --  For boolean variables, ensure "yes" was defined and pass to specification record.
   procedure set_boolean (field : PSP.spec_field; line : String);

   --  Pass integer variables to specification record
   procedure set_natural (field : PSP.spec_field; line : String);

   --  Line may contain spaces, and each space is considered a single item on a list.
   --  This iterates through the value with space delimiters to build a group list.
   procedure build_group_list
     (field : PSP.spec_field;
      key   : String;
      value : String);

   --  Return true if all final validity checks pass
   function late_validity_check_error return HT.Text;

end Specification_Parser;
