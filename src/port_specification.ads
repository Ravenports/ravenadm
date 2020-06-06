--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

--  GCC 6.0 only
--  pragma Suppress (Tampering_Check);

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
   missing_extract : exception;
   missing_require : exception;

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
                       sp_build_deps, sp_buildrun_deps, sp_run_deps, sp_cmake_args, sp_qmake_args,
                       sp_info, sp_install_tgt, sp_patch_strip, sp_pfiles_strip,
                       sp_patch_wrksrc, sp_extra_patches, sp_must_config,
                       sp_config_wrksrc, sp_config_script, sp_gnu_cfg_prefix, sp_cfg_outsrc,
                       sp_config_target, sp_deprecated, sp_expiration, sp_install_wrksrc,
                       sp_plist_sub, sp_prefix, sp_licenses, sp_users, sp_groups, sp_catchall,
                       sp_notes, sp_inst_tchain, sp_var_opsys, sp_var_arch, sp_lic_name,
                       sp_lic_file, sp_lic_scheme, sp_skip_ccache, sp_test_tgt, sp_exrun,
                       sp_mandirs, sp_rpath_warning, sp_debugging, sp_broken_ssl, sp_test_args,
                       sp_gnome, sp_rcscript, sp_ug_pkg, sp_broken_mysql, sp_broken_pgsql,
                       sp_og_radio, sp_og_unlimited, sp_og_restrict, sp_opt_descr, sp_opt_group,
                       sp_ext_deb, sp_os_bdep, sp_os_rdep, sp_os_brdep, sp_test_env, sp_generated,
                       sp_xorg, sp_sdl, sp_phpext, sp_job_limit, sp_soversion, sp_os_uses,
                       sp_lic_terms, sp_lic_awk, sp_lic_src, sp_repsucks, sp_killdog,
                       sp_cgo_conf, sp_cgo_build, sp_cgo_inst, sp_cgo_cargs, sp_cgo_bargs,
                       sp_cgo_iargs, sp_cgo_feat, sp_verbatim);

   type spec_option  is (not_helper_format, not_supported_helper, broken_on, buildrun_depends_off,
                         buildrun_depends_on, build_depends_off, build_depends_on,
                         build_target_off, build_target_on, cflags_off, cflags_on,
                         cmake_args_off, cmake_args_on, cmake_bool_f_both, cmake_bool_t_both,
                         configure_args_off, configure_args_on,
                         configure_enable_both, configure_env_off, configure_env_on,
                         configure_with_both, cppflags_off, cppflags_on,
                         cxxflags_off, cxxflags_on, df_index_off, df_index_on,
                         extract_only_off, extract_only_on, extra_patches_off, extra_patches_on,
                         implies_on, info_off, info_on,
                         install_target_off, install_target_on, keywords_off, keywords_on,
                         ldflags_off, ldflags_on, make_args_off, make_args_on,
                         make_env_off, make_env_on, patchfiles_off, patchfiles_on,
                         plist_sub_off, plist_sub_on, prevents_on,
                         qmake_args_off, qmake_args_on, run_depends_off, run_depends_on,
                         sub_files_off, sub_files_on, sub_list_off, sub_list_on,
                         test_target_off, test_target_on,
                         uses_off, uses_on, makefile_off, makefile_on, description,
                         only_for_opsys_on, xorg_comp_off, xorg_comp_on,
                         gnome_comp_off, gnome_comp_on, php_ext_off, php_ext_on);

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
   function get_namebase (specs : Portspecs) return String;

   --  Generic retrieve data function
   function get_field_value (specs : Portspecs; field : spec_field) return String;

   --  Specialized variant-specific list esp. for package manifest
   function get_options_list (specs : Portspecs; variant : String) return String;

   --  Retrieve the tagline on a given variant
   function get_tagline (specs : Portspecs; variant : String) return String;

   --  Calculate the surprisingly complex pkgversion string
   function calculate_pkgversion (specs : Portspecs) return String;

   --  Return count on variants list
   function get_number_of_variants (specs : Portspecs) return Natural;

   --  Return the list length of the data indicated by field
   function get_list_length (specs : Portspecs; field : spec_field) return Natural;

   --  Return item given by number when the list is indicated by the field
   function get_list_item (specs : Portspecs; field : spec_field; item : Natural) return String;

   --  Return number of subpackage for a given variant
   function get_subpackage_length (specs : Portspecs; variant : String) return Natural;

   --  Return subpackage given a variant and index
   function get_subpackage_item
     (specs   : Portspecs;
      variant : String;
      item    : Natural) return String;

   --  Return number of extra runtime dependences on a named subpackage
   function get_number_extra_run (specs : Portspecs; subpackage : String) return Natural;

   --  Return extra runtime specification of a named subpackage given an index
   function get_extra_runtime
     (specs      : Portspecs;
      subpackage : String;
      item       : Natural) return String;

   --  Return aggregate and formatted reason(s) for ignoring the port.
   function aggregated_ignore_reason (specs : Portspecs) return String;

   --  Returns a formatted block of lines to represent the current option settings
   function options_summary (specs : Portspecs; variant : String) return String;

   --  Returns True if one or more variants have no defined subpackages.
   function missing_subpackage_definition (specs : Portspecs) return Boolean;

   --  Return string block (delimited by LF) of unique build + buildrun + run depends (optional)
   --  If limit_to_run is true, only run dependencies are returned
   function combined_dependency_origins
     (specs        : Portspecs;
      include_run  : Boolean;
      limit_to_run : Boolean) return String;

   --  Runs through specs to ensure all license framework information is present.
   function post_parse_license_check_passes (specs : Portspecs) return Boolean;

   --  Ensures USERGROUP_SPKG is set if USERS or GROUP is set.
   function post_parse_usergroup_check_passes (specs : Portspecs) return Boolean;

   --  Return "single", "dual" or "multi";
   function get_license_scheme (specs : Portspecs) return String;

   --  Return True if rpath check failures need to break the build.
   function rpath_check_errors_are_fatal (specs : Portspecs) return Boolean;

   --  Return True if debugging is set on.
   function debugging_is_on (specs : Portspecs) return Boolean;

   --  Returns the key of the last catchall insertion
   function last_catchall_key (specs : Portspecs) return String;

   --  Returns true if all the options have a description
   --  It also outputs to standard out which ones fail
   function post_parse_opt_desc_check_passes (specs : Portspecs) return Boolean;

   --  Returns true if all the option groups have at least 2 members
   --  It also outputs to standard out which groups have only one member
   function post_parse_option_group_size_passes (specs : Portspecs) return Boolean;

   --  Checks radio and restricted groups.  Radio groups have to have exactly one option
   --  set by (by default) and restricted groups need at least one.
   function post_transform_option_group_defaults_passes (specs : Portspecs) return Boolean;

   --  Return "joined" table of group + options
   function option_block_for_dialog (specs : Portspecs) return String;

   --  Return true if options_avail is not "none"
   function global_options_present (specs : Portspecs) return Boolean;

   --  Return true if ops_standard is not "none"
   function standard_options_present (specs : Portspecs) return Boolean;

   --  Return True if the port is generated
   function port_is_generated (specs : Portspecs) return Boolean;

   --  If catchall FPC_EQUIVALENT is defined, return its value, otherwise return "N/A".
   function equivalent_fpc_port (specs : Portspecs) return String;

   --  Returns True if given dependency is present as run_depends or buildrun_depends
   function run_dependency (specs : Portspecs; dependency : String) return Boolean;

   --  Used for json-repology report only (returns full download URL (1) given distfile number)
   function get_repology_distfile (specs : Portspecs; item : Natural) return String;

   --  Format contacts with html (span, mailto)
   function get_web_contacts (specs : Portspecs; subject : String) return String;

   --  Provides json-formatted contacts
   --  If contact is "nobody" then it returns a blank string
   function get_json_contacts (specs : Portspecs) return String;

   --  Ensure opsys dependencies are not applied (only for web page generation)
   procedure do_not_apply_opsys_dependencies (specs : in out Portspecs);

   --  Return true if broken_all key present in the broken array
   function broken_all_set (specs : Portspecs) return Boolean;

   --  Return true if BLOCK_WATCHDOG set by specification
   function watchdog_disabled (specs : Portspecs) return Boolean;

   --  store error seen during specification parsing
   procedure set_parse_error (specs : in out Portspecs; error : String);

   --  Retrieve parse error
   function get_parse_error (specs : Portspecs) return String;

   --  Insert new variable definition
   procedure define (specs : in out Portspecs; variable : String; value : String);

   --  Returns true if variable already defined
   function definition_exists (specs : Portspecs; variable : String) return Boolean;

   --  Returns value of defined variable
   function definition (specs : Portspecs; variable : String) return String;

   --  Return true if no definition are defined
   function no_definitions (specs : Portspecs) return Boolean;

   --  Detects SSL variant override by checking module arguments
   function get_ssl_variant (specs : Portspecs; normal_variant : String) return String;

private

   package HT  renames HelperText;
   package CON renames Ada.Containers;

   type spec_order is (so_initialized, so_namebase, so_version, so_revision, so_epoch,
                       so_keywords, so_variants, so_taglines, so_homepage, so_contacts,
                       so_dl_groups, so_dl_sites, so_distfiles, so_distsubdir, so_df_index,
                       so_subpackages, so_opts_avail, so_opts_std, so_vopts);

   type license_type is
     (AGPLv3, AGPLv3x, APACHE10, APACHE11, APACHE20, ART10, ART20, ARTPERL10,
      BSD2CLAUSE, BSD3CLAUSE, BSD4CLAUSE, BSDGROUP,
      CUSTOM1, CUSTOM2, CUSTOM3, CUSTOM4,
      GPLv1, GPLv1x, GPLv2, GPLv2x, GPLv3, GPLv3x,
      GPLv3RLE, GPLv3RLEx, GMGPL, GMGPL3, INVALID, ISCL,
      LGPL20, LGPL20x, LGPL21, LGPL21x, LGPL3, LGPL3x,
      MIT, MPL, POSTGRESQL, PSFL, PUBDOM, OPENSSL, RUBY, ZLIB, HPND, AFL, CDDL, GFDL,
      CC0_10, CC_30, CC_40, CC_NC_30, CC_NC_40,
      CC_NCND_30, CC_NCND_40, CC_NCSA_30, CC_NCSA_40,
      CC_ND_30, CC_ND_40, CC_SA_30, CC_SA_40);

   type described_option_set is
     (AALIB, ALSA, ASM, COLORD, CUPS, DBUS, DEBUG, DOCS, FIREBIRD, ICONV, IDN, IPV4, IPV6, JAVA,
      LANG_CN, LANG_KO, LANG_RU,
      LDAP, LDAPS, MYSQL, NAS, NLS, OPENGL, OSS, PERL528, PERL530, PGSQL, PNG, PULSEAUDIO,
      PY27, PY37, PY38,
      READLINE, RUBY25, RUBY26, RUBY27,
      SNDIO, SOUND, SQLITE, STATIC, SYSLOG, TCL, TCLTK, THREADS, X11, ZLIB,
      OPT_NOT_DEFINED);

   type gnome_type is
     (atk, cairo, glib, gtk2, gtk3, gtksourceview3, gdkpixbuf, intltool, introspection, pango,
      pygobject, libcroco, libglade, libgsf, librsvg, libxml2, libxslt,
      dconf, gconf, libidl, orbit2, vte, libxmlxx2, libsigcxx2, glibmm, cairomm, atkmm,
      pangomm, gtkmm24, gtkmm30,
      invalid_component);

   type xorg_type is
     (xorgproto, fontcacheproto, printproto, xtransproto,
      dmx, fontenc, fontutil, ice, pciaccess, pixman, sm, x11, xau, xaw, xcb,
      xcb_util, xcb_util_cursor, xcb_util_image, xcb_util_keysyms, xcb_util_wm, xcb_util_xrm,
      xcb_render_util, xcomposite, xcursor, xdamage, xdmcp, xext, xfixes, xfont, xfont2,
      xfontcache, xft, xi, xinerama, xkbfile, xmu, xpm, xprop, xrandr, xrender, xres,
      xscrnsaver, xset, xshmfence, xt, xtst, xv, xvmc, xxf86dga, xxf86vm, xbitmaps,
      invalid_component);

   type sdl_type is
     (sdl1, sdl2, gfx1, gfx2, image1, image2, mixer1, mixer2, net1, net2, ttf1, ttf2,
      invalid_component);

   type phpext_type is
     (bcmath, bitset, bz2, calendar, ctype, curl, dba, dom, enchant, exif, fileinfo, filter,
      ftp, gd, gettext, gmp, hash, iconv, igbinary, imap, interbase, intl, jsonext, ldap, mbstring,
      mcrypt, memcache, memcached, mysqli, odbc, opcache, openssl, pcntl, pdf, pdo,
      pdo_dblib, pdo_firebird, pdo_mysql, pdo_odbc, pdo_pgsql, pdo_sqlite, pgsql, phar, posix,
      pspell, radius, readline, recode, redis, session, shmop, simplexml, snmp, soap, sockets,
      sqlite3, sysvmsg, sysvsem, sysvshm, tidy, tokenizer, wddx, xml, xmlreader, xmlrpc,
      xmlwriter, xsl, zip, zlib, ffi, sodium, invalid_extension);

   package string_crate is new CON.Vectors
     (Element_Type => HT.Text,
      Index_Type   => Positive,
      "="          => HT.SU."=");

   package sorter is new string_crate.Generic_Sorting ("<" => HT.SU."<");

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
         option_description    : HT.Text;
         currently_set_ON      : Boolean := False;
         set_ON_by_default     : Boolean := False;
         standard_option       : Boolean := False;
         BROKEN_ON             : HT.Text;
         BUILDRUN_DEPENDS_OFF  : string_crate.Vector;
         BUILDRUN_DEPENDS_ON   : string_crate.Vector;
         BUILD_DEPENDS_OFF     : string_crate.Vector;
         BUILD_DEPENDS_ON      : string_crate.Vector;
         BUILD_TARGET_OFF      : string_crate.Vector;
         BUILD_TARGET_ON       : string_crate.Vector;
         CFLAGS_OFF            : string_crate.Vector;
         CFLAGS_ON             : string_crate.Vector;
         CMAKE_ARGS_OFF        : string_crate.Vector;
         CMAKE_ARGS_ON         : string_crate.Vector;
         CMAKE_BOOL_F_BOTH     : string_crate.Vector;
         CMAKE_BOOL_T_BOTH     : string_crate.Vector;
         CONFIGURE_ARGS_OFF    : string_crate.Vector;
         CONFIGURE_ARGS_ON     : string_crate.Vector;
         CONFIGURE_ENABLE_BOTH : string_crate.Vector;
         CONFIGURE_ENV_OFF     : string_crate.Vector;
         CONFIGURE_ENV_ON      : string_crate.Vector;
         CONFIGURE_WITH_BOTH   : string_crate.Vector;
         CPPFLAGS_OFF          : string_crate.Vector;
         CPPFLAGS_ON           : string_crate.Vector;
         CXXFLAGS_OFF          : string_crate.Vector;
         CXXFLAGS_ON           : string_crate.Vector;
         DF_INDEX_OFF          : string_crate.Vector;
         DF_INDEX_ON           : string_crate.Vector;
         EXTRACT_ONLY_OFF      : string_crate.Vector;
         EXTRACT_ONLY_ON       : string_crate.Vector;
         EXTRA_PATCHES_OFF     : string_crate.Vector;
         EXTRA_PATCHES_ON      : string_crate.Vector;
         IMPLIES_ON            : string_crate.Vector;
         INFO_OFF              : string_crate.Vector;
         INFO_ON               : string_crate.Vector;
         INSTALL_TARGET_OFF    : string_crate.Vector;
         INSTALL_TARGET_ON     : string_crate.Vector;
         KEYWORDS_OFF          : string_crate.Vector;
         KEYWORDS_ON           : string_crate.Vector;
         LDFLAGS_OFF           : string_crate.Vector;
         LDFLAGS_ON            : string_crate.Vector;
         MAKEFILE_OFF          : string_crate.Vector;
         MAKEFILE_ON           : string_crate.Vector;
         MAKE_ARGS_OFF         : string_crate.Vector;
         MAKE_ARGS_ON          : string_crate.Vector;
         MAKE_ENV_OFF          : string_crate.Vector;
         MAKE_ENV_ON           : string_crate.Vector;
         ONLY_FOR_OPSYS_ON     : string_crate.Vector;
         PATCHFILES_OFF        : string_crate.Vector;
         PATCHFILES_ON         : string_crate.Vector;
         PLIST_SUB_OFF         : string_crate.Vector;
         PLIST_SUB_ON          : string_crate.Vector;
         PREVENTS_ON           : string_crate.Vector;
         QMAKE_ARGS_OFF        : string_crate.Vector;
         QMAKE_ARGS_ON         : string_crate.Vector;
         RUN_DEPENDS_OFF       : string_crate.Vector;
         RUN_DEPENDS_ON        : string_crate.Vector;
         SUB_FILES_OFF         : string_crate.Vector;
         SUB_FILES_ON          : string_crate.Vector;
         SUB_LIST_OFF          : string_crate.Vector;
         SUB_LIST_ON           : string_crate.Vector;
         TEST_TARGET_OFF       : string_crate.Vector;
         TEST_TARGET_ON        : string_crate.Vector;
         USES_OFF              : string_crate.Vector;
         USES_ON               : string_crate.Vector;
         XORG_COMPONENTS_OFF   : string_crate.Vector;
         XORG_COMPONENTS_ON    : string_crate.Vector;
         GNOME_COMPONENTS_OFF  : string_crate.Vector;
         GNOME_COMPONENTS_ON   : string_crate.Vector;
         PHP_EXTENSIONS_OFF    : string_crate.Vector;
         PHP_EXTENSIONS_ON     : string_crate.Vector;
      end record;

   package option_crate is new CON.Hashed_Maps
        (Key_Type        => HT.Text,
         Element_Type    => Option_Helper,
         Hash            => HT.hash,
         Equivalent_Keys => HT.equivalent);

   type Portspecs is tagged
      record
         definitions    : def_crate.Map;

         namebase       : HT.Text;
         version        : HT.Text;
         revision       : Natural;
         epoch          : Natural;
         job_limit      : Natural;
         keywords       : string_crate.Vector;
         variants       : string_crate.Vector;
         taglines       : def_crate.Map;
         homepage       : HT.Text;
         contacts       : string_crate.Vector;
         dl_sites       : list_crate.Map;
         distfiles      : string_crate.Vector;
         dist_subdir    : HT.Text;
         df_index       : string_crate.Vector;
         subpackages    : list_crate.Map;
         ops_avail      : string_crate.Vector;
         ops_standard   : string_crate.Vector;
         ops_helpers    : option_crate.Map;
         last_set       : spec_order;
         variantopts    : list_crate.Map;
         options_on     : list_crate.Map;
         broken         : list_crate.Map;
         exc_opsys      : string_crate.Vector;
         inc_opsys      : string_crate.Vector;
         exc_arch       : string_crate.Vector;
         deprecated     : HT.Text;
         expire_date    : HT.Text;
         uses           : string_crate.Vector;
         uses_base      : string_crate.Vector;
         sub_list       : string_crate.Vector;
         sub_files      : string_crate.Vector;
         extract_only   : string_crate.Vector;
         extract_zip    : string_crate.Vector;
         extract_lha    : string_crate.Vector;
         extract_7z     : string_crate.Vector;
         extract_deb    : string_crate.Vector;
         extract_dirty  : string_crate.Vector;
         extract_head   : list_crate.Map;
         extract_tail   : list_crate.Map;
         distname       : HT.Text;

         patchfiles     : string_crate.Vector;
         extra_patches  : string_crate.Vector;
         patch_strip    : string_crate.Vector;
         pfiles_strip   : string_crate.Vector;
         patch_wrksrc   : HT.Text;

         config_args    : string_crate.Vector;
         config_env     : string_crate.Vector;
         config_must    : HT.Text;
         config_prefix  : HT.Text;
         config_script  : HT.Text;
         config_target  : HT.Text;
         config_wrksrc  : HT.Text;
         config_outsrc  : Boolean;
         skip_build     : Boolean;
         skip_install   : Boolean;
         skip_ccache    : Boolean;
         destdir_env    : Boolean;
         single_job     : Boolean;
         shift_install  : Boolean;
         fatal_rpath    : Boolean;
         debugging_on   : Boolean;
         generated      : Boolean;
         opt_df_index   : Boolean;
         skip_opsys_dep : Boolean;
         repology_sucks : Boolean;
         kill_watchdog  : Boolean;
         prefix         : HT.Text;
         build_wrksrc   : HT.Text;
         makefile       : HT.Text;
         destdirname    : HT.Text;
         make_env       : string_crate.Vector;
         make_args      : string_crate.Vector;
         build_target   : string_crate.Vector;
         build_deps     : string_crate.Vector;
         buildrun_deps  : string_crate.Vector;
         run_deps       : string_crate.Vector;
         opsys_b_deps   : list_crate.Map;
         opsys_r_deps   : list_crate.Map;
         opsys_br_deps  : list_crate.Map;
         opsys_c_uses   : list_crate.Map;
         cflags         : string_crate.Vector;
         cxxflags       : string_crate.Vector;
         cppflags       : string_crate.Vector;
         ldflags        : string_crate.Vector;
         optimizer_lvl  : Natural;
         cmake_args     : string_crate.Vector;
         qmake_args     : string_crate.Vector;
         gnome_comps    : string_crate.Vector;
         xorg_comps     : string_crate.Vector;
         sdl_comps      : string_crate.Vector;
         php_extensions : string_crate.Vector;
         info           : string_crate.Vector;
         install_tgt    : string_crate.Vector;
         test_tgt       : string_crate.Vector;
         test_args      : string_crate.Vector;
         test_env       : string_crate.Vector;
         install_wrksrc : HT.Text;
         plist_sub      : string_crate.Vector;

         make_targets   : list_crate.Map;
         licenses       : string_crate.Vector;
         lic_names      : string_crate.Vector;
         lic_files      : string_crate.Vector;
         lic_terms      : string_crate.Vector;
         lic_awk        : string_crate.Vector;
         lic_source     : string_crate.Vector;
         lic_scheme     : HT.Text;
         usergroup_pkg  : HT.Text;
         users          : string_crate.Vector;
         groups         : string_crate.Vector;
         mandirs        : string_crate.Vector;
         mk_verbatim    : string_crate.Vector;
         subr_scripts   : string_crate.Vector;
         broken_ssl     : string_crate.Vector;
         broken_mysql   : string_crate.Vector;
         broken_pgsql   : string_crate.Vector;
         catch_all      : list_crate.Map;
         pkg_notes      : def_crate.Map;
         var_opsys      : list_crate.Map;
         var_arch       : list_crate.Map;
         extra_rundeps  : list_crate.Map;
         last_catchkey  : HT.Text;
         soversion      : HT.Text;
         used_python    : HT.Text;
         used_perl      : HT.Text;
         used_ruby      : HT.Text;
         used_lua       : HT.Text;
         parse_error    : HT.Text;

         cgo_skip_conf  : Boolean;
         cgo_skip_build : Boolean;
         cgo_skip_inst  : Boolean;
         cgo_cargolock  : HT.Text;
         cgo_cargotoml  : HT.Text;
         cgo_cargo_bin  : HT.Text;
         cgo_target_dir : HT.Text;
         cgo_vendor_dir : HT.Text;
         cgo_build_args : string_crate.Vector;
         cgo_conf_args  : string_crate.Vector;
         cgo_inst_args  : string_crate.Vector;
         cgo_features   : string_crate.Vector;

         opt_radio      : string_crate.Vector;
         opt_restrict   : string_crate.Vector;
         opt_unlimited  : string_crate.Vector;
         optgroup_desc  : list_crate.Map;
         optgroups      : list_crate.Map;
      end record;

   --  Ordinal type representing numbers that have subpackage arguments
   type smodules is range 1 .. 5;

   --  Returns the name of the module associated with the smodules index
   function base_module (index : smodules) return String;

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

   --  checks for exactly two colons
   --  checks the three components are not empty strings
   --  Does not do existence checks on namebase, variants or subpackages.
   function valid_dependency_format (value : String) return Boolean;

   --  If illegal characters in the namebase are detected, return True.
   function invalid_namebase (value : String; allow_comma : Boolean) return Boolean;

   --  Returns true if value is a known USES module.
   function valid_uses_module (value : String) return Boolean;

   --  Returns true if value is a known mysql group setting
   function valid_broken_mysql_value (value : String) return Boolean;

   --  Returns true if value is a known postgresql setting
   function valid_broken_pgsql_value (value : String) return Boolean;

   --  Return true if INFO appendum is valid (compared against existing entries)
   --  Specifically it's checking the subdirectory (if it exists) to make sure it matches
   --  previous entries.  It will define INFO_SUBDIR in catchall (once)
   function valid_info_page (specs : in out Portspecs; value : String) return Boolean;

   --  Checks against a list of known licenses or CUSTOM(1,2,3,4)
   function determine_license (value : String) return license_type;

   --  Returns enumeration of described option or OPT_NOT_FOUND if the option isn't described
   function described_option (value : String) return described_option_set;

   --  Returns true if subpackage exists in any variant.
   function subpackage_exists (specs : Portspecs; subpackage : String) return Boolean;

   --  Returns True if given_module matches the base_module exists, but it doesn't have
   --  an argument or the argument doesn't match a known subpackage.
   function module_subpackage_failed
     (specs        : Portspecs;
      base_module  : String;
      given_module : String) return Boolean;

   --  Checks against known list of gnome components and identifies it
   function determine_gnome_component (component : String) return gnome_type;

   --  Checks against known list of xorg components and identifies it
   function determine_xorg_component (component : String) return xorg_type;

   --  Checks against known list of SDL components and identifies it
   function determine_sdl_component (component : String) return sdl_type;

   --  Checks against known list of PHP extensions and identifies it
   function determine_php_extension (component : String) return phpext_type;

   --  Given a string XXXX/account:project:tag(:directory) return a standard
   --  distribution file name.
   function generate_github_distfile (download_site : String) return String;

   --  Like generate_github_distfile, but doesn't filter/modify out leading v's and plus signs
   function generate_gitlab_distfile (download_site : String) return String;

   --  Given a string XXXX/project:version return a standard
   function generate_crates_distfile (download_site : String) return String;

   --  Returns True if a given option already present in radio, restricted or unlimited group
   function option_already_in_group (specs : Portspecs; option_name : String) return Boolean;

   --  Given an option enumeration, return the default option description
   function default_description (option : described_option_set) return String;

   --  Split distfile translation to separate function (needed more than once)
   --  It converts "generated" to a filename mainly
   function translate_distfile (specs : Portspecs; distfile : String) return String;

   --  Give full download URL (one) for each distfile.
   --  Represent macros with "mirror://" prefix
   function repology_distfile (specs : Portspecs; distfile : String) return String;

   --  split out info entry validation (returns non-black on failed check)
   function info_page_check_message (specs : Portspecs; value : String) return String;

   --  Return True if uses module is fully specified (mainly for compiler modules)
   function extra_uses_modules_sanity_check_passes
     (specs  : Portspecs;
      module : String;
      errmsg : out HT.Text) return Boolean;

end Port_Specification;
