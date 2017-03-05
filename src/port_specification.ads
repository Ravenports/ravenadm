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
   invalid_option  : exception;

   type spec_field is (sp_namebase, sp_version, sp_revision, sp_epoch, sp_keywords,
                       sp_variants, sp_taglines, sp_homepage, sp_contacts, sp_dl_groups,
                       sp_dl_sites, sp_distfiles, sp_distsubdir, sp_df_index, sp_subpackages,
                       sp_opts_avail, sp_opts_standard, sp_vopts, sp_exc_opsys, sp_inc_opsys,
                       sp_exc_arch, sp_ext_only, sp_ext_zip, sp_ext_7z, sp_ext_lha, sp_ext_head,
                       sp_ext_tail, sp_ext_dirty, sp_distname, sp_skip_build, sp_single_job,
                       sp_destdir_env, sp_destdirname, sp_build_wrksrc, sp_makefile,
                       sp_make_args, sp_make_env, sp_build_target, sp_cflags, sp_cxxflags,
                       sp_cppflags, sp_ldflags, sp_makefile_targets);

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

   --  Generic function to set boolean values
   --  Throws wrong_type exception if field isn't a boolean type
   procedure set_boolean
     (specs : in out Portspecs;
      field : spec_field;
      value : Boolean);

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

   --  Return True if provided option name is known
   function option_exists (specs : Portspecs; option : String) return Boolean;

   --  Given the provided option name, return True if setting is "ON" and False otherwise
   --  If option name is not valid, raise invalid option
   function option_current_setting (specs : Portspecs; option : String) return Boolean;

   --  Generic function to determine if group exists, returns True if so
   function group_exists
     (specs : Portspecs;
      field : spec_field;
      group : String) return Boolean;

   --  Developer routine which shows contents of specification
   procedure dump_specification (specs : Portspecs);

   --  Iterate through all non-standard variants to check if all options are accounted for.
   --  Return blank string if all of them pass or the name of the first variant that doesn't
   --  concatenated with the missing option.
   function check_variants (specs : Portspecs) return String;

   --  Perform any post-parsing adjustments necessary
   procedure adjust_defaults_port_parse (specs : in out Portspecs);

private

   package HT  renames HelperText;
   package CON renames Ada.Containers;

   type spec_order is (so_initialized, so_namebase, so_version, so_revision, so_epoch,
                       so_keywords, so_variants, so_taglines, so_homepage, so_contacts,
                       so_dl_groups, so_dl_sites, so_distfiles, so_distsubdir, so_df_index,
                       so_subpackages, so_opts_avail, so_opts_std, so_vopts);

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
         namebase      : HT.Text;
         version       : HT.Text;
         revision      : Natural;
         epoch         : Natural;
         keywords      : string_crate.Vector;
         variants      : string_crate.Vector;
         taglines      : def_crate.Map;
         homepage      : HT.Text;
         contacts      : string_crate.Vector;
         dl_sites      : list_crate.Map;
         distfiles     : string_crate.Vector;
         dist_subdir   : HT.Text;
         df_index      : string_crate.Vector;
         subpackages   : list_crate.Map;
         ops_avail     : string_crate.Vector;
         ops_standard  : string_crate.Vector;
         last_set      : spec_order;
         variantopts   : list_crate.Map;
         exc_opsys     : string_crate.Vector;
         inc_opsys     : string_crate.Vector;
         exc_arch      : string_crate.Vector;
         extract_only  : string_crate.Vector;
         extract_zip   : string_crate.Vector;
         extract_lha   : string_crate.Vector;
         extract_7z    : string_crate.Vector;
         extract_dirty : string_crate.Vector;
         extract_head  : list_crate.Map;
         extract_tail  : list_crate.Map;
         distname      : HT.Text;
         --  configure placeholder
         skip_build    : Boolean;
         destdir_env   : Boolean;
         single_job    : Boolean;
         build_wrksrc  : HT.Text;
         makefile      : HT.Text;
         destdirname   : HT.Text;
         make_env      : string_crate.Vector;
         make_args     : string_crate.Vector;
         build_target  : string_crate.Vector;
         cflags        : string_crate.Vector;
         cxxflags      : string_crate.Vector;
         cppflags      : string_crate.Vector;
         ldflags       : string_crate.Vector;

         make_targets  : list_crate.Map;
      end record;

   --  Compares given keyword against known values
   function keyword_is_valid (keyword : String) return Boolean;

   --  Returns true if there is a short description defined for each variant.
   function all_taglines_defined (specs : Portspecs) return Boolean;

   --  Return true if the given string is a recogized opsys
   function lower_opsys_is_valid (test_opsys : String) return Boolean;

   --  Return true if given string is a recognized architecture
   function arch_is_valid (test_arch : String) return Boolean;

   --  Returns true if given string can convert to an integer between 1 and
   --  distfiles count.
   function dist_index_is_valid (specs : Portspecs; test_index : String) return Boolean;

   --  Returns true if space exists outside of quotation marks
   function contains_nonquoted_spaces (word : String) return Boolean;

end Port_Specification;
