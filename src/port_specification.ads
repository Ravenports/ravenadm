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
                       sp_cppflags, sp_ldflags, sp_makefile_targets, sp_skip_install,
                       sp_opt_level, sp_options_on, sp_broken, sp_opt_helper, sp_patchfiles,
                       sp_uses, sp_sub_list, sp_sub_files, sp_config_args, sp_config_env,
                       sp_build_deps, sp_lib_deps, sp_run_deps, sp_cmake_args, sp_qmake_args,
                       sp_info, sp_install_tgt, sp_patch_strip, sp_pfiles_strip,
                       sp_patch_wrksrc, sp_extra_patches, sp_apply_f10_fix, sp_must_config,
                       sp_config_wrksrc, sp_config_script, sp_gnu_cfg_prefix, sp_cfg_outsrc,
                       sp_config_target, sp_deprecated, sp_expiration, sp_install_wrksrc,
                       sp_plist_sub);

   type spec_option  is (not_helper_format, not_supported_helper, broken_on, build_depends_on,
                         build_target_on, cflags_on, cmake_args_off, cmake_args_on,
                         cmake_bool_f_both, cmake_bool_t_both, configure_args_off,
                         configure_args_on, configure_enable_both, configure_env_on,
                         configure_with_both, cppflags_on, cxxflags_on, df_index_on,
                         extra_patches_on, extract_only_on, gh_account_on, gh_project_on,
                         gh_subdir_on, gh_tagname_on, gh_tuple_on, implies_on, info_on,
                         install_target_on, keywords_on, ldflags_on, lib_depends_on,
                         make_args_on, make_env_on, patchfiles_on, plist_sub_on, prevents_on,
                         qmake_off, qmake_on, run_depends_on, sub_files_on, sub_list_on,
                         test_target_on, uses_on);

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

   --  Generic function to populate option helper
   --  Throws misordered exception if called before standard options
   --  Throws contains spaces exception if spaces aren't permitted but found
   --  Throws wrong_type exception if field isn't supported
   --  Throws wrong_value exception if option doesn't exist (caller should check first)
   procedure build_option_helper
     (specs  : in out Portspecs;
      field  : spec_option;
      option : String;
      value  : String);

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

   --  Return False if deprecation set without expiration or vice versa.
   function deprecation_valid (specs : Portspecs) return Boolean;

   --  Perform any post-parsing adjustments necessary
   procedure adjust_defaults_port_parse (specs : in out Portspecs);

   --  Returns true if indicated option helper is empty
   function option_helper_unset
     (specs  : Portspecs;
      field  : spec_option;
      option : String) return Boolean;

   --  After parsing, this is used to return the port name
   function get_namebase (specs  : Portspecs) return String;

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

   type Option_Helper is
      record
         option_name           : HT.Text;
         currently_set_ON      : Boolean := False;
         set_ON_by_default     : Boolean := False;
         BROKEN_ON             : HT.Text;
         BUILD_DEPENDS_ON      : string_crate.Vector;
         BUILD_TARGET_ON       : string_crate.Vector;
         CFLAGS_ON             : string_crate.Vector;
         CMAKE_ARGS_OFF        : string_crate.Vector;
         CMAKE_ARGS_ON         : string_crate.Vector;
         CMAKE_BOOL_F_BOTH     : string_crate.Vector;
         CMAKE_BOOL_T_BOTH     : string_crate.Vector;
         CONFIGURE_ARGS_ON     : string_crate.Vector;
         CONFIGURE_ARGS_OFF    : string_crate.Vector;
         CONFIGURE_ENABLE_BOTH : string_crate.Vector;
         CONFIGURE_ENV_ON      : string_crate.Vector;
         CONFIGURE_WITH_BOTH   : string_crate.Vector;
         CPPFLAGS_ON           : string_crate.Vector;
         CXXFLAGS_ON           : string_crate.Vector;
         DF_INDEX_ON           : string_crate.Vector;
         EXTRA_PATCHES_ON      : string_crate.Vector;
         EXTRACT_ONLY_ON       : string_crate.Vector;
         GH_ACCOUNT_ON         : string_crate.Vector;
         GH_PROJECT_ON         : string_crate.Vector;
         GH_SUBDIR_ON          : string_crate.Vector;
         GH_TAGNAME_ON         : string_crate.Vector;
         GH_TUPLE_ON           : string_crate.Vector;
         IMPLIES_ON            : string_crate.Vector;
         INFO_ON               : string_crate.Vector;
         INSTALL_TARGET_ON     : string_crate.Vector;
         KEYWORDS_ON           : string_crate.Vector;
         LDFLAGS_ON            : string_crate.Vector;
         LIB_DEPENDS_ON        : string_crate.Vector;
         MAKE_ARGS_ON          : string_crate.Vector;
         MAKE_ENV_ON           : string_crate.Vector;
         PATCHFILES_ON         : string_crate.Vector;
         PLIST_SUB_ON          : string_crate.Vector;
         PREVENTS_ON           : string_crate.Vector;
         QMAKE_OFF             : string_crate.Vector;
         QMAKE_ON              : string_crate.Vector;
         RUN_DEPENDS_ON        : string_crate.Vector;
         SUB_FILES_ON          : string_crate.Vector;
         SUB_LIST_ON           : string_crate.Vector;
         TEST_TARGET_ON        : string_crate.Vector;
         USES_ON               : string_crate.Vector;
      end record;

   package option_crate is new CON.Hashed_Maps
        (Key_Type        => HT.Text,
         Element_Type    => Option_Helper,
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
         ops_helpers   : option_crate.Map;
         last_set      : spec_order;
         variantopts   : list_crate.Map;
         options_on    : list_crate.Map;
         broken        : list_crate.Map;
         exc_opsys     : string_crate.Vector;
         inc_opsys     : string_crate.Vector;
         exc_arch      : string_crate.Vector;
         deprecated    : HT.Text;
         expire_date   : HT.Text;
         uses          : string_crate.Vector;
         sub_list      : string_crate.Vector;
         sub_files     : string_crate.Vector;
         extract_only  : string_crate.Vector;
         extract_zip   : string_crate.Vector;
         extract_lha   : string_crate.Vector;
         extract_7z    : string_crate.Vector;
         extract_dirty : string_crate.Vector;
         extract_head  : list_crate.Map;
         extract_tail  : list_crate.Map;
         distname      : HT.Text;

         patchfiles    : string_crate.Vector;
         extra_patches : string_crate.Vector;
         patch_strip   : string_crate.Vector;
         pfiles_strip  : string_crate.Vector;
         patch_wrksrc  : HT.Text;

         config_args   : string_crate.Vector;
         config_env    : string_crate.Vector;
         config_must   : HT.Text;
         config_prefix : HT.Text;
         config_script : HT.Text;
         config_target : HT.Text;
         config_wrksrc : HT.Text;
         config_outsrc : Boolean;
         apply_f10_fix : Boolean;

         skip_build    : Boolean;
         skip_install  : Boolean;
         destdir_env   : Boolean;
         single_job    : Boolean;
         build_wrksrc  : HT.Text;
         makefile      : HT.Text;
         destdirname   : HT.Text;
         make_env      : string_crate.Vector;
         make_args     : string_crate.Vector;
         build_target  : string_crate.Vector;
         build_deps    : string_crate.Vector;
         lib_deps      : string_crate.Vector;
         run_deps      : string_crate.Vector;
         cflags        : string_crate.Vector;
         cxxflags      : string_crate.Vector;
         cppflags      : string_crate.Vector;
         ldflags       : string_crate.Vector;
         optimizer_lvl : Natural;
         cmake_args    : string_crate.Vector;
         qmake_args    : string_crate.Vector;
         info          : string_crate.Vector;
         install_tgt   : string_crate.Vector;
         install_wrksrc : HT.Text;
         plist_sub     : string_crate.Vector;

         make_targets  : list_crate.Map;
      end record;

   --  Compares given keyword against known values
   function keyword_is_valid (keyword : String) return Boolean;

   --  Returns true if there is a short description defined for each variant.
   function all_taglines_defined (specs : Portspecs) return Boolean;

   --  Returns true if given string can convert to an integer between 1 and
   --  distfiles count.
   function dist_index_is_valid (specs : Portspecs; test_index : String) return Boolean;

   --  Returns true if space exists outside of quotation marks
   function contains_nonquoted_spaces (word : String) return Boolean;

   --  OPT_ON can only match existing option names exactly, or
   --  have "/" separator with digits and full_stop only or
   --  have above with "/" followed by one or more valid arch separated by "|" or
   --  same as above except nothing between the two "/" separators
   function valid_OPT_ON_value (specs : Portspecs;
                                key   : String;
                                word  : String) return Boolean;

   --  Return True if same option is already defined in all.
   function option_present_in_OPT_ON_all (specs : Portspecs;
                                          option_name : String) return Boolean;

   --  Return True if in format YYYY-MM-DD and YYYY > 2016 and MM is 01..12 and DD is 01..31
   --  and it succesfully converts to a date.
   function ISO8601_format (value : String) return Boolean;

end Port_Specification;
