--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions; use Definitions;

package Port_Specification.Transform is

   --  Given:
   --     variant
   --     standard arch
   --     opsys (operating system)
   --     osrelease (string)
   --     osmajor   (string)
   --     osversion (string)
   --     and the current option settings (if variant is not "standard"):
   --  Apply all the changes dictated by option helpers and the IGNORE calculation
   procedure apply_directives
     (specs         : in out Portspecs;
      variant       : String;
      arch_standard : supported_arch;
      osmajor       : String);

   --  For non-standard variants, set options defaults as directed by VOPTS
   --  For standard variant, set options default by OPT_ON values
   procedure set_option_defaults
     (specs         : in out Portspecs;
      variant       : String;
      opsys         : supported_opsys;
      arch_standard : supported_arch;
      os_major      : String);

   --  Update current value of given option
   procedure define_option_setting (specs : in out Portspecs; option : String; value : Boolean);

   procedure set_option_to_default_values (specs : in out Portspecs);

   procedure set_outstanding_ignore
     (specs         : in out Portspecs;
      variant       : String;
      opsys         : supported_opsys;
      arch_standard : supported_arch;
      osrelease     : String;
      osmajor       : String);

   procedure shift_extra_patches
     (specs         : Portspecs;
      extract_dir   : String);

private

   BUILD    : constant String := "build";
   BUILDRUN : constant String := "buildrun";
   RUN      : constant String := "run";

   PYTHONPRI : constant String := "python312";
   PYTHONALT : constant String := "python313";
   PY312     : constant String := "v12";
   PY313     : constant String := "v13";

   GETTEXT  : constant String := "gettext";
   TCL86    : constant String := generic_triplet ("tcl86", "tools");
   TCL90    : constant String := generic_triplet ("tcl90", "tools");
   RUBY32   : constant String := primary_triplet ("ruby32");
   RUBY33   : constant String := primary_triplet ("ruby33");
   RUBY34   : constant String := primary_triplet ("ruby34");
   NINJA    : constant String := single_triplet ("ninja");
   GNOMELIB : constant String := primary_triplet ("glib");
   GLIBDEV  : constant String := dev_triplet ("glib");
   GTDEV    : constant String := dev_triplet (GETTEXT);
   GTLIB    : constant String := primary_triplet (GETTEXT);
   GTBTOOLS : constant String := generic_triplet (GETTEXT, "bldtools");
   GTSOLINX : constant String := generic_triplet (GETTEXT, "solinks");
   GTTOOLS  : constant String := generic_triplet (GETTEXT, "tools");

   --  Returns true if all '0' .. '9', and also single '.' if it's not in first or last place.
   function release_format (candidate : String) return Boolean;

   --  Given X, X.Y or X.YY, returns X*100, X*100+Y or X*100+YY
   function centurian_release (release : String) return Natural;

   --  Implement less-than and greater-than OS Major comparision
   function LTE (gen_release, spec_release : String) return Boolean;
   function GTE (gen_release, spec_release : String) return Boolean;

   procedure apply_cpe_module
     (specs         : in out Portspecs;
      arch_standard : supported_arch;
      osmajor       : String);

   procedure apply_scons_module     (specs : in out Portspecs);
   procedure apply_gmake_module     (specs : in out Portspecs);
   procedure apply_libtool_module   (specs : in out Portspecs);
   procedure apply_libiconv_module  (specs : in out Portspecs);
   procedure apply_info_presence    (specs : in out Portspecs);
   procedure apply_pkgconfig_module (specs : in out Portspecs);
   procedure apply_gprbuild_module  (specs : in out Portspecs);
   procedure apply_ncurses_module   (specs : in out Portspecs);
   procedure apply_bdb_module       (specs : in out Portspecs);
   procedure apply_perl_module      (specs : in out Portspecs);
   procedure apply_bison_module     (specs : in out Portspecs);
   procedure apply_makeinfo_module  (specs : in out Portspecs);
   procedure apply_readline_module  (specs : in out Portspecs);
   procedure apply_ssl_module       (specs : in out Portspecs);
   procedure apply_python_module    (specs : in out Portspecs);
   procedure apply_lua_module       (specs : in out Portspecs);
   procedure apply_tcl_module       (specs : in out Portspecs);
   procedure apply_cargo_module     (specs : in out Portspecs);
   procedure apply_fonts_module     (specs : in out Portspecs);
   procedure apply_cmake_module     (specs : in out Portspecs);
   procedure apply_meson_module     (specs : in out Portspecs);
   procedure apply_ninja_module     (specs : in out Portspecs);
   procedure apply_mysql_module     (specs : in out Portspecs);
   procedure apply_pgsql_module     (specs : in out Portspecs);
   procedure apply_sqlite_module    (specs : in out Portspecs);
   procedure apply_gtkdoc_module    (specs : in out Portspecs);
   procedure apply_display_module   (specs : in out Portspecs);
   procedure apply_schemas_module   (specs : in out Portspecs);
   procedure apply_autoconf_module  (specs : in out Portspecs);
   procedure apply_execinfo_module  (specs : in out Portspecs);
   procedure apply_freetype_module  (specs : in out Portspecs);
   procedure apply_clang_module     (specs : in out Portspecs);
   procedure apply_expat_module     (specs : in out Portspecs);
   procedure apply_cran_module      (specs : in out Portspecs);
   procedure apply_mold_module      (specs : in out Portspecs);
   procedure apply_curl_module      (specs : in out Portspecs);
   procedure apply_zlib_module      (specs : in out Portspecs);
   procedure apply_zstd_module      (specs : in out Portspecs);
   procedure apply_mesa_module      (specs : in out Portspecs);
   procedure apply_jpeg_module      (specs : in out Portspecs);
   procedure apply_tiff_module      (specs : in out Portspecs);
   procedure apply_ruby_module      (specs : in out Portspecs);
   procedure apply_pcre_module      (specs : in out Portspecs);
   procedure apply_pcre2_module     (specs : in out Portspecs);
   procedure apply_php_module       (specs : in out Portspecs);
   procedure apply_png_module       (specs : in out Portspecs);
   procedure apply_gif_module       (specs : in out Portspecs);
   procedure apply_lz4_module       (specs : in out Portspecs);
   procedure apply_lzo_module       (specs : in out Portspecs);
   procedure apply_gem_module       (specs : in out Portspecs);
   procedure apply_bz2_module       (specs : in out Portspecs);
   procedure apply_bsd_module       (specs : in out Portspecs);
   procedure apply_xz_module        (specs : in out Portspecs);
   procedure add_primdev_submodule  (specs      : in out Portspecs;
                                     namebase   : String);
   procedure generic_dev_require    (specs      : in out Portspecs;
                                     module     : String;
                                     dependency : String);
   procedure generic_3B_module      (specs      : in out Portspecs;
                                     module     : String;
                                     dependency : String);
   procedure generic_build_module   (specs      : in out Portspecs;
                                     module     : String;
                                     dependency : String);
   procedure generic_library_module (specs      : in out Portspecs;
                                     module     : String;
                                     dependency : String);
   procedure generic_devlib_module  (specs      : in out Portspecs;
                                     module     : String;
                                     depprefix  : String);
   procedure generic_run_module     (specs      : in out Portspecs;
                                     module     : String;
                                     dependency : String);
   procedure apply_gcc_run_module   (specs   : in out Portspecs;
                                     variant : String;
                                     module  : String;
                                     gccsubpackage : String);
   procedure apply_gcc_full_set     (specs : in out Portspecs; variant : String);
   procedure add_exrun_cclibs       (specs : in out Portspecs; variant : String);

   procedure apply_fontconfig_module      (specs : in out Portspecs);
   procedure apply_desktop_utils_module   (specs : in out Portspecs);
   procedure apply_gnome_icons_module     (specs : in out Portspecs);
   procedure apply_mime_info_module       (specs : in out Portspecs);
   procedure apply_gettext_module         (specs : in out Portspecs);
   procedure apply_extraction_deps        (specs : in out Portspecs);
   procedure apply_opsys_dependencies     (specs : in out Portspecs);

   procedure apply_gnome_components_dependencies   (specs : in out Portspecs);
   procedure apply_xorg_components_dependencies    (specs : in out Portspecs);
   procedure apply_sdl_components_dependencies     (specs : in out Portspecs);
   procedure apply_php_extension_dependencies      (specs : in out Portspecs);
   procedure apply_default_version_transformations (specs : in out Portspecs);
   procedure apply_curly_bracket_conversions       (specs : in out Portspecs);
   procedure apply_cbc_string_crate (crate : in out string_crate.Vector);
   procedure convert_exrun_versions (specs : in out Portspecs; variant : String);

   function argument_present (specs : Portspecs; module, argument : String) return Boolean;
   function no_arguments_present (specs : Portspecs; module : String) return Boolean;
   function get_argument (specs : Portspecs; module : String) return String;

   procedure add_build_depends    (specs : in out Portspecs; dependency : String);
   procedure add_buildrun_depends (specs : in out Portspecs; dependency : String);
   procedure add_run_depends      (specs : in out Portspecs; dependency : String);
   procedure add_exrun_depends    (specs : in out Portspecs; dependency, subpackage : String);

   --  Convert e.g. python_default to py3X depending on current defaults.
   --  True for all defaults as they get formed
   function transform_defaults (dep, pyx, plx, lux, rbx : String) return String;

   --  Returns mysql namebase based on mysql configuration setting
   function determine_mysql_namebase return String;

   --  Returns XXXX based on pgsql configuration setting
   function determine_pgsql_namebase return String;

end Port_Specification.Transform;
