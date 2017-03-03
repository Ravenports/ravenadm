--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

private with HelperText;
private with Ada.Containers.Vectors;
private with Ada.Containers.Hashed_Maps;

package Port_Specification is

   type Portspecs is tagged private;

   misordered      : exception;
   contains_spaces : exception;
   wrong_type      : exception;
   wrong_value     : exception;
   dupe_spec_key   : exception;
   dupe_list_value : exception;
   missing_group   : exception;

   type spec_field is (sp_namebase, sp_version, sp_revision, sp_epoch, sp_keywords,
                       sp_variants, sp_taglines, sp_contacts, sp_dl_groups, sp_dl_sites,
                       sp_distfiles, sp_distsubdir, sp_df_index, sp_subpackages,
                       sp_opts_avail);

   --  Initialize specification data
   procedure initialize (specs : out Portspecs);

   --  Generic function to set single string types.
   --  Throws misordered exception if set too early (or late depending on perspective)
   --  Throws contains spaces exception if space found anywhere in string
   --  Throws wrong_type exception if field isn't a single string type.
   procedure set_single_string
     (specs : in out Portspecs;
      field : spec_field;
      value : String);

   --  Generic function to populate lists
   --  Throws misordered exception if set out of order.
   --  Throws contains spaces exception if space found anywhere in string
   --  Throws wrong_type exception if field isn't a list type.
   procedure append_list
     (specs : in out Portspecs;
      field : spec_field;
      value : String);

   --  Generic function to set integers
   --  Throws misordered exception if set out of order.
   --  Throws wrong_type exception if field isn't a natural integer type
   procedure set_natural_integer
     (specs : in out Portspecs;
      field : spec_field;
      value : Natural);

   --  Generic function to populate arrays
   --  Throws misordered exception if set out of order.
   --  Throws contains spaces exception if space found anywhere in string
   --  Throws wrong_type exception if field isn't a list type.
   --  Throws duplicate exception if key has already been seen.
   procedure append_array
     (specs : in out Portspecs;
      field : spec_field;
      key   : String;
      value : String;
      allow_spaces : Boolean);

   --  Generic function to establish groups of string arrays.
   --  Throws misordered exception if set out of order.
   --  Throws contains spaces exception if space found anywhere in string
   --  Throws wrong_type exception if field isn't a list type.
   --  Throws duplicate exception if key has already been seen.
   procedure establish_group
     (specs : in out Portspecs;
      field : spec_field;
      group : String);

   --  Return True if provided variant is known
   function variant_exists (specs : Portspecs; variant : String) return Boolean;

   --  Generic function to determine if group exists, returns True if so
   function group_exists
     (specs : Portspecs;
      field : spec_field;
      group : String) return Boolean;

   --  Developer routine which shows contents of specification
   procedure dump_specification (specs : Portspecs);

private

   package HT  renames HelperText;
   package CON renames Ada.Containers;

   type spec_order is (so_initialized, so_namebase, so_version, so_revision, so_epoch,
                       so_keywords, so_variants, so_taglines, so_contacts, so_dl_groups,
                       so_dl_sites, so_distfiles, so_distsubdir, so_df_index,
                       so_subpackages, so_opts_avail);

   package string_crate is new CON.Vectors
     (Element_Type => HT.Text,
      Index_Type   => Positive,
      "="          => HT.SU."=");

   package def_crate is new CON.Hashed_Maps
        (Key_Type        => HT.Text,
         Element_Type    => HT.Text,
         Hash            => HT.hash,
         Equivalent_Keys => HT.equivalent,
         "="             => HT.SU."=");

   type group_list is
      record
         group : HT.Text;
         list  : string_crate.Vector;
      end record;

   package list_crate is new CON.Hashed_Maps
        (Key_Type        => HT.Text,
         Element_Type    => group_list,
         Hash            => HT.hash,
         Equivalent_Keys => HT.equivalent);

   type Portspecs is tagged
      record
         namebase    : HT.Text;
         version     : HT.Text;
         revision    : Natural;
         epoch       : Natural;
         keywords    : string_crate.Vector;
         variants    : string_crate.Vector;
         taglines    : def_crate.Map;
         contacts    : string_crate.Vector;
         dl_sites    : list_crate.Map;
         distfiles   : string_crate.Vector;
         dist_subdir : HT.Text;
         df_index    : string_crate.Vector;
         subpackages : list_crate.Map;
         ops_avail   : string_crate.Vector;
         last_set    : spec_order;
      end record;

   --  Compares given keyword against known values
   function keyword_is_valid (keyword : String) return Boolean;

   --  Returns true if there is a short description defined for each variant.
   function all_taglines_defined (specs : Portspecs) return Boolean;

end Port_Specification;
