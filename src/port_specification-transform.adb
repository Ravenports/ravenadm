--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Utilities;
with Ada.Characters.Latin_1;
with Ada.Directories;
with Parameters;
with Unix;

package body Port_Specification.Transform is

   package UTL renames Utilities;
   package LAT renames Ada.Characters.Latin_1;
   package DIR renames Ada.Directories;

   --------------------------------------------------------------------------------------------
   --  apply_directives
   --------------------------------------------------------------------------------------------
   procedure apply_directives
     (specs         : in out Portspecs;
      variant       : String;
      arch_standard : supported_arch;
      osmajor       : String)
   is
      procedure copy_option_over (position : option_crate.Cursor);

      procedure copy_option_over (position : option_crate.Cursor)
      is
         procedure augment (field : spec_option; directive : string_crate.Vector);
         procedure grow_plist_sub (name : String; currently_ON : Boolean);
         procedure handle_broken;

         rec : Option_Helper renames option_crate.Element (position);

         procedure augment (field : spec_option; directive : string_crate.Vector)
         is
            procedure transfer (position : string_crate.Cursor);
            procedure transfer (position : string_crate.Cursor)
            is
               item    : HT.Text renames string_crate.Element (position);
               itemstr : String := HT.USS (item);
               special : HT.Text;
            begin
               if rec.currently_set_ON then
                  case field is
                     when buildrun_depends_on => specs.buildrun_deps.Append (item);
                     when build_depends_on    => specs.build_deps.Append (item);
                     when build_target_on     => specs.build_target.Append (item);
                     when cflags_on           => specs.cflags.Append (item);
                     when cmake_args_on       => specs.cmake_args.Append (item);
                     when configure_args_on   => specs.config_args.Append (item);
                     when configure_env_on    => specs.config_env.Append (item);
                     when cppflags_on         => specs.cppflags.Append (item);
                     when cxxflags_on         => specs.cxxflags.Append (item);
                     when df_index_on         => specs.df_index.Append (item);
                     when extract_only_on     => specs.extract_only.Append (item);
                     when extra_patches_on    => specs.extra_patches.Append (item);
                     when gnome_comp_on       => specs.gnome_comps.Append (item);
                     when info_on             => specs.info.Append (item);
                     when install_target_on   => specs.install_tgt.Append (item);
                     when keywords_on         => specs.keywords.Append (item);
                     when ldflags_on          => specs.ldflags.Append (item);
                     when make_args_on        => specs.make_args.Append (item);
                     when make_env_on         => specs.make_env.Append (item);
                     when patchfiles_on       => specs.patchfiles.Append (item);
                     when plist_sub_on        => specs.plist_sub.Append (item);
                     when php_ext_on          => specs.php_extensions.Append (item);
                     when qmake_args_on       => specs.qmake_args.Append (item);
                     when run_depends_on      => specs.run_deps.Append (item);
                     when sub_files_on        => specs.sub_files.Append (item);
                     when sub_list_on         => specs.sub_list.Append (item);
                     when test_target_on      => specs.test_tgt.Append (item);
                     when makefile_on         => specs.mk_verbatim.Append (item);
                     when only_for_opsys_on   => specs.inc_opsys.Append (item);
                     when xorg_comp_on        => specs.xorg_comps.Append (item);
                     when uses_on             =>
                        declare
                           stripped      : String  := HT.part_1 (itemstr, ":");
                           text_stripped : HT.Text := HT.SUS (stripped);
                        begin
                           specs.uses.Append (item);
                           if not specs.uses_base.Contains (text_stripped) then
                              specs.uses_base.Append (text_stripped);
                           end if;
                        end;
                     when cmake_bool_f_both =>
                        special := HT.SUS ("-D" & itemstr & ":BOOL=false");
                        specs.cmake_args.Append (special);
                     when cmake_bool_t_both =>
                        special := HT.SUS ("-D" & itemstr & ":BOOL=true");
                        specs.cmake_args.Append (special);
                     when configure_enable_both =>
                        special := HT.SUS ("--enable-"  & itemstr);
                        specs.config_args.Append (special);
                     when configure_with_both =>
                        special := HT.SUS ("--with-" & itemstr);
                        specs.config_args.Append (special);
                     when others =>
                        null;
                  end case;
               else
                  case field is
                     when buildrun_depends_off => specs.buildrun_deps.Append (item);
                     when build_depends_off    => specs.build_deps.Append (item);
                     when build_target_off     => specs.build_target.Append (item);
                     when run_depends_off      => specs.run_deps.Append (item);
                     when cmake_args_off       => specs.cmake_args.Append (item);
                     when cflags_off           => specs.cflags.Append (item);
                     when cppflags_off         => specs.cppflags.Append (item);
                     when cxxflags_off         => specs.cxxflags.Append (item);
                     when ldflags_off          => specs.ldflags.Append (item);
                     when configure_args_off   => specs.config_args.Append (item);
                     when configure_env_off    => specs.config_env.Append (item);
                     when df_index_off         => specs.df_index.Append (item);
                     when extract_only_off     => specs.extract_only.Append (item);
                     when extra_patches_off    => specs.extra_patches.Append (item);
                     when gnome_comp_off       => specs.gnome_comps.Append (item);
                     when info_off             => specs.info.Append (item);
                     when install_target_off   => specs.install_tgt.Append (item);
                     when keywords_off         => specs.keywords.Append (item);
                     when makefile_off         => specs.mk_verbatim.Append (item);
                     when make_args_off        => specs.make_args.Append (item);
                     when make_env_off         => specs.make_env.Append (item);
                     when patchfiles_off       => specs.patchfiles.Append (item);
                     when plist_sub_off        => specs.plist_sub.Append (item);
                     when php_ext_off          => specs.php_extensions.Append (item);
                     when qmake_args_off       => specs.qmake_args.Append (item);
                     when sub_files_off        => specs.sub_files.Append (item);
                     when sub_list_off         => specs.sub_list.Append (item);
                     when test_target_off      => specs.test_tgt.Append (item);
                     when xorg_comp_off        => specs.xorg_comps.Append (item);
                     when cmake_bool_f_both =>
                        special := HT.SUS ("-D" & itemstr & ":BOOL=true");
                        specs.cmake_args.Append (special);
                     when cmake_bool_t_both =>
                        special := HT.SUS ("-D" & itemstr & ":BOOL=false");
                        specs.cmake_args.Append (special);
                     when configure_enable_both =>
                        special := HT.SUS ("--disable-"  & itemstr);
                        specs.config_args.Append (special);
                     when configure_with_both =>
                        special := HT.SUS ("--without-" & itemstr);
                        specs.config_args.Append (special);
                     when uses_off            =>
                        declare
                           stripped      : String  := HT.part_1 (itemstr, ":");
                           text_stripped : HT.Text := HT.SUS (stripped);
                        begin
                           specs.uses.Append (item);
                           if not specs.uses_base.Contains (text_stripped) then
                              specs.uses_base.Append (text_stripped);
                           end if;
                        end;
                     when others =>
                        null;
                  end case;
               end if;

            end transfer;
         begin
            directive.Iterate (Process => transfer'Access);
         end augment;

         procedure grow_plist_sub (name : String; currently_ON : Boolean)
         is
            comment : constant String := LAT.Quotation & "@comment " & LAT.Quotation;
            WON     : constant String := "-ON=";
            WOFF    : constant String := "-OFF=";
         begin
            if currently_ON then
               specs.plist_sub.Append (HT.SUS (name & WON));
               specs.plist_sub.Append (HT.SUS (name & WOFF & comment));
            else
               specs.plist_sub.Append (HT.SUS (name & WOFF));
               specs.plist_sub.Append (HT.SUS (name & WON & comment));
            end if;
         end grow_plist_sub;

         procedure handle_broken
         is
            procedure grow (Key : HT.Text; Element : in out group_list);

            index : HT.Text := HT.SUS (broken_all);

            procedure grow (Key : HT.Text; Element : in out group_list) is
            begin
               Element.list.Append (rec.BROKEN_ON);
            end grow;
         begin
            if not HT.IsBlank (rec.BROKEN_ON) then

               if not specs.broken.Contains (index) then
                  specs.establish_group (sp_broken, broken_all);
               end if;

               specs.broken.Update_Element (Position => specs.broken.Find (index),
                                            Process  => grow'Access);
            end if;
         end handle_broken;
      begin
         if HT.equivalent (rec.option_name, options_none) then
            return;
         end if;

         if rec.currently_set_ON then

            handle_broken;
            augment (buildrun_depends_on,  rec.BUILDRUN_DEPENDS_ON);
            augment (build_depends_on,     rec.BUILD_DEPENDS_ON);
            augment (build_target_on,      rec.BUILD_TARGET_ON);
            augment (cflags_off,           rec.CFLAGS_OFF);
            augment (cflags_on,            rec.CFLAGS_ON);
            augment (cmake_args_on,        rec.CMAKE_ARGS_ON);
            augment (configure_args_on,    rec.CONFIGURE_ARGS_ON);
            augment (configure_env_on,     rec.CONFIGURE_ENV_ON);
            augment (cppflags_on,          rec.CPPFLAGS_ON);
            augment (cxxflags_on,          rec.CXXFLAGS_ON);
            augment (df_index_on,          rec.DF_INDEX_ON);
            augment (extract_only_on,      rec.EXTRACT_ONLY_ON);
            augment (extra_patches_on,     rec.EXTRA_PATCHES_ON);
            augment (gnome_comp_on,        rec.GNOME_COMPONENTS_ON);
            augment (info_on,              rec.INFO_ON);
            augment (install_target_on,    rec.INSTALL_TARGET_ON);
            augment (keywords_on,          rec.KEYWORDS_ON);
            augment (ldflags_on,           rec.LDFLAGS_ON);
            augment (makefile_on,          rec.MAKEFILE_ON);
            augment (make_args_on,         rec.MAKE_ARGS_ON);
            augment (make_env_on,          rec.MAKE_ENV_ON);
            augment (only_for_opsys_on,    rec.ONLY_FOR_OPSYS_ON);
            augment (patchfiles_on,        rec.PATCHFILES_ON);
            augment (plist_sub_on,         rec.PLIST_SUB_ON);
            augment (php_ext_on,           rec.PHP_EXTENSIONS_ON);
            augment (qmake_args_on,        rec.QMAKE_ARGS_ON);
            augment (run_depends_on,       rec.RUN_DEPENDS_ON);
            augment (sub_files_on,         rec.SUB_FILES_ON);
            augment (sub_list_on,          rec.SUB_LIST_ON);
            augment (test_target_on,       rec.TEST_TARGET_ON);
            augment (uses_on,              rec.USES_ON);
            augment (xorg_comp_on,         rec.XORG_COMPONENTS_ON);
         else
            augment (buildrun_depends_off, rec.BUILDRUN_DEPENDS_OFF);
            augment (build_depends_off,    rec.BUILD_DEPENDS_OFF);
            augment (build_target_off,     rec.BUILD_TARGET_OFF);
            augment (cflags_off,           rec.CFLAGS_OFF);
            augment (cppflags_off,         rec.CPPFLAGS_OFF);
            augment (cxxflags_off,         rec.CXXFLAGS_OFF);
            augment (ldflags_off,          rec.LDFLAGS_OFF);
            augment (cmake_args_off,       rec.CMAKE_ARGS_OFF);
            augment (configure_args_off,   rec.CONFIGURE_ARGS_OFF);
            augment (configure_env_off,    rec.CONFIGURE_ENV_OFF);
            augment (df_index_off,         rec.DF_INDEX_OFF);
            augment (extract_only_off,     rec.EXTRACT_ONLY_OFF);
            augment (extra_patches_off,    rec.EXTRA_PATCHES_OFF);
            augment (gnome_comp_off,       rec.GNOME_COMPONENTS_OFF);
            augment (info_off,             rec.INFO_OFF);
            augment (install_target_off,   rec.INSTALL_TARGET_OFF);
            augment (keywords_off,         rec.KEYWORDS_OFF);
            augment (makefile_off,         rec.MAKEFILE_OFF);
            augment (make_args_off,        rec.MAKE_ARGS_OFF);
            augment (make_env_off,         rec.MAKE_ENV_OFF);
            augment (patchfiles_off,       rec.PATCHFILES_OFF);
            augment (plist_sub_off,        rec.PLIST_SUB_OFF);
            augment (php_ext_off,          rec.PHP_EXTENSIONS_OFF);
            augment (qmake_args_off,       rec.QMAKE_ARGS_OFF);
            augment (run_depends_off,      rec.RUN_DEPENDS_OFF);
            augment (sub_files_off,        rec.SUB_FILES_OFF);
            augment (sub_list_off,         rec.SUB_LIST_OFF);
            augment (test_target_off,      rec.TEST_TARGET_OFF);
            augment (uses_off,             rec.USES_OFF);
            augment (xorg_comp_off,        rec.XORG_COMPONENTS_OFF);
         end if;
         augment (cmake_bool_f_both,      rec.CMAKE_BOOL_F_BOTH);
         augment (cmake_bool_t_both,      rec.CMAKE_BOOL_T_BOTH);
         augment (configure_enable_both,  rec.CONFIGURE_ENABLE_BOTH);
         augment (configure_with_both,    rec.CONFIGURE_WITH_BOTH);

         grow_plist_sub (HT.USS (rec.option_name), rec.currently_set_ON);

      end copy_option_over;

      skip_compiler_packages : constant Boolean :=
        Unix.env_variable_defined ("SKIPCCRUN") or else
        DIR.Exists (HT.USS (Parameters.configuration.dir_sysroot) & "/usr/share/GENESIS");

   begin
      specs.ops_helpers.Iterate (Process => copy_option_over'Access);
      apply_extraction_deps (specs);
      apply_opsys_dependencies (specs);
      apply_cpe_module (specs, arch_standard, osmajor);
      apply_gmake_module (specs);
      apply_scons_module (specs);
      apply_makeinfo_module (specs);
      apply_readline_module (specs);
      apply_libiconv_module (specs);
      apply_libtool_module (specs);
      apply_pkgconfig_module (specs);
      apply_gprbuild_module (specs);
      apply_ncurses_module (specs);
      apply_gettext_module (specs);
      apply_info_presence (specs);
      apply_gnome_icons_module (specs);
      apply_mime_info_module (specs);
      apply_autoconf_module (specs);
      apply_execinfo_module (specs);
      apply_freetype_module (specs);
      apply_display_module (specs);
      apply_sqlite_module (specs);
      apply_cmake_module (specs);
      apply_perl_module (specs);
      apply_bdb_module (specs);
      apply_ssl_module (specs);
      apply_clang_module (specs);
      apply_bison_module (specs);
      apply_mysql_module (specs);
      apply_pgsql_module (specs);
      apply_fonts_module (specs);
      apply_pcre2_module (specs);
      apply_mold_module (specs);
      apply_cran_module (specs);
      apply_ruby_module (specs);
      apply_zlib_module (specs);
      apply_zstd_module (specs);
      apply_mesa_module (specs);
      apply_jpeg_module (specs);
      apply_tiff_module (specs);
      apply_pcre_module (specs);
      apply_curl_module (specs);
      apply_lua_module (specs);
      apply_tcl_module (specs);
      apply_php_module (specs);
      apply_png_module (specs);
      apply_gif_module (specs);
      apply_gem_module (specs);
      apply_lz4_module (specs);
      apply_lzo_module (specs);
      apply_bz2_module (specs);
      apply_bsd_module (specs);
      apply_xz_module  (specs);
      apply_expat_module (specs);
      apply_cargo_module (specs);
      apply_gtkdoc_module (specs);
      apply_schemas_module (specs);
      apply_fontconfig_module (specs);
      apply_desktop_utils_module (specs);
      apply_gnome_components_dependencies (specs);
      apply_sdl_components_dependencies (specs);
      apply_xorg_components_dependencies (specs);
      apply_php_extension_dependencies (specs);
      apply_python_module (specs);
      apply_meson_module (specs);  --  requires python_used, so must follow apply_python_module()
      apply_ninja_module (specs);  --  requires python_used, so much follow apply_python_module()
      if not skip_compiler_packages then
         apply_gcc_run_module (specs, variant, "ada", "ada_run");
         apply_gcc_run_module (specs, variant, "c++", "cxx_run");
         apply_gcc_run_module (specs, variant, "fortran", "fortran_run");
         apply_gcc_run_module (specs, variant, "cclibs", "libs");
         if platform_type = macos or else platform_type = sunos
         then
            --  Solaris 10 doesn't use dl_iterate_phdr, so many packages have executables that
            --  requires libgcc_s.so.  Rather than specify potentially hundreds of C_USES
            --  keywords, just make ravensys-gcc:libs:std a run depends of every package (including
            --  gcc6, gcc7, gcc8 and later).
            --  However, Solaris 11 / OmniOS does use dl_iterate_phdr so drop sunos condition
            --  Later: did not work, gcc may need to be told about the dl_iterate_phdr
            --  TODO: Check and used 14.3.0 patch rev1 or later
            add_exrun_cclibs (specs, variant);
         end if;
         apply_gcc_full_set (specs, variant);
      end if;
      apply_curly_bracket_conversions (specs);
      apply_default_version_transformations (specs);
      convert_exrun_versions (specs, variant);
   end apply_directives;


   --------------------------------------------------------------------------------------------
   --  set_option_defaults
   --------------------------------------------------------------------------------------------
   procedure set_option_defaults
     (specs         : in out Portspecs;
      variant       : String;
      opsys         : supported_opsys;
      arch_standard : supported_arch;
      os_major      : String)
   is
      procedure vopt_set   (position : string_crate.Cursor);
      procedure varstd_set (position : string_crate.Cursor);
      procedure opsys_set  (position : string_crate.Cursor);
      procedure set_on     (Key : HT.Text; Element : in out Option_Helper);
      function for_this_architecture (arch_list : String) return Boolean;

      variant_text : HT.Text := HT.SUS (variant);
      all_text     : HT.Text := HT.SUS (options_all);
      arch_text    : HT.Text := HT.SUS (UTL.cpu_arch (arch_standard));
      opsys_text   : HT.Text := HT.SUS (UTL.lower_opsys (opsys));

      function for_this_architecture (arch_list : String) return Boolean
      is
         num_pipes : constant Natural := HT.count_char (arch_list, LAT.Vertical_Line);
         this_arch : constant String := UTL.cpu_arch (arch_standard);
      begin
         if num_pipes = 0 then
            return arch_list = this_arch;
         else
            for candidate in 1 .. num_pipes + 1 loop
               if HT.specific_field (arch_list, candidate, "|") = this_arch then
                  return True;
               end if;
            end loop;
            return False;
         end if;
      end for_this_architecture;

      procedure set_on (Key : HT.Text; Element : in out Option_Helper) is
      begin
         Element.set_ON_by_default := True;
      end set_on;

      procedure vopt_set (position : string_crate.Cursor)
      is
         --  ignore =OFF versions (defaults already to False, only consider =ON
         item : String := HT.USS (string_crate.Element (position));
      begin
         if HT.trails (item, "=ON") then
            declare
               option_name : String  := HT.partial_search (item, 0, "=");
               option_text : HT.Text := HT.SUS (option_name);
            begin
               if specs.ops_helpers.Contains (option_text) then
                  specs.ops_helpers.Update_Element
                    (Position => specs.ops_helpers.Find (option_text),
                     Process  => set_on'Access);
               end if;
            end;
         end if;
      end vopt_set;

      procedure varstd_set (position : string_crate.Cursor)
      is
         option_name : String := HT.USS (string_crate.Element (position));
         option_text : HT.Text := HT.SUS (option_name);
      begin
         if specs.ops_helpers.Contains (option_text) then
            specs.ops_helpers.Update_Element
              (Position => specs.ops_helpers.Find (option_text),
               Process  => set_on'Access);
         end if;
      end varstd_set;

      procedure opsys_set  (position : string_crate.Cursor)
      is
         --  Assume option name is valid
         option_name : String := HT.USS (string_crate.Element (position));
         option_text : HT.Text;
         num_slash   : Natural := HT.count_char (option_name, LAT.Solidus);
      begin
         if num_slash > 0 then
            if num_slash = 1 then
               declare
                  --  e.g. OPT_NAME/MAJOR
                  opt_name   : HT.Text := HT.SUS (HT.part_1 (option_name, "/"));
                  spec_major : String  := HT.part_2 (option_name, "/");
               begin
                  if spec_major = os_major then
                     option_text := opt_name;
                  end if;
               end;
            else
               declare
                  --  e.g OPT_NAME//ARCH  or OPT_NAME/MAJOR/ARCH   or OPT_NAME/x/ARCH1|ARCH2|ARCH3
                  opt_name     : HT.Text := HT.SUS (HT.part_1 (option_name, "/"));
                  temp_P2      : String  := HT.part_2 (option_name, "/");
                  spec_major   : String  := HT.part_1 (temp_P2);
                  arch_str     : String  := HT.part_2 (temp_P2);
                  meets_ver    : Boolean := spec_major = "" or else spec_major = os_major;
                  meets_arch   : Boolean := for_this_architecture (arch_str);
               begin
                  if meets_ver and then meets_arch then
                     option_text := opt_name;
                  end if;
               end;
            end if;
         else
            option_text := HT.SUS (option_name);
         end if;

         if specs.ops_helpers.Contains (option_text) then
            specs.ops_helpers.Update_Element
              (Position => specs.ops_helpers.Find (option_text),
               Process  => set_on'Access);
         end if;

      end opsys_set;
   begin
      if variant = variant_standard then
         if specs.options_on.Contains (all_text) then
            specs.options_on.Element (all_text).list.Iterate (Process => varstd_set'Access);
         end if;
         if specs.options_on.Contains (arch_text) then
            specs.options_on.Element (arch_text).list.Iterate (Process => varstd_set'Access);
         end if;
         if not specs.skip_opsys_dep then
            if specs.options_on.Contains (opsys_text) then
               specs.options_on.Element (opsys_text).list.Iterate (Process => opsys_set'Access);
            end if;
         end if;
      else
         if not specs.variantopts.Contains (variant_text) then
            --  Variant does not exist, silently exit
            return;
         end if;
         specs.variantopts.Element (variant_text).list.Iterate (Process => vopt_set'Access);
      end if;
   end set_option_defaults;


   --------------------------------------------------------------------------------------------
   --  set_option_to_default_values
   --------------------------------------------------------------------------------------------
   procedure set_option_to_default_values (specs : in out Portspecs)
   is
      procedure copy_setting (position : option_crate.Cursor);
      procedure copy_setting (position : option_crate.Cursor)
      is
         procedure set_option (Key : HT.Text; Element : in out Option_Helper);
         procedure set_option (Key : HT.Text; Element : in out Option_Helper) is
         begin
            Element.currently_set_ON := Element.set_ON_by_default;
         end set_option;
      begin
         specs.ops_helpers.Update_Element (Position => position, Process => set_option'Access);
      end copy_setting;
   begin
      specs.ops_helpers.Iterate (Process => copy_setting'Access);
   end set_option_to_default_values;


   --------------------------------------------------------------------------------------------
   --  define_option_setting
   --------------------------------------------------------------------------------------------
   procedure define_option_setting (specs : in out Portspecs; option : String; value : Boolean)
   is
      procedure set_option (Key : HT.Text; Element : in out Option_Helper);

      optname_text : HT.Text := HT.SUS (option);

      procedure set_option (Key : HT.Text; Element : in out Option_Helper) is
      begin
         Element.currently_set_ON := value;
      end set_option;
   begin
      if specs.ops_helpers.Contains (optname_text) then
         specs.ops_helpers.Update_Element (Position => specs.ops_helpers.Find (optname_text),
                                           Process  => set_option'Access);
      end if;
   end define_option_setting;


   --------------------------------------------------------------------------------------------
   --  release_format
   --------------------------------------------------------------------------------------------
   function release_format (candidate : String) return Boolean
   is
      fullstop : Boolean := False;
   begin
      for X in candidate'Range loop
         case candidate (X) is
            when '.' =>
               if fullstop then
                  return False;
               end if;
               if X = candidate'First or else
                 X = candidate'Last
               then
                  return False;
               end if;
               fullstop := True;
            when '0' .. '9' => null;
            when others     => return False;
         end case;
      end loop;
      return True;
   end release_format;


   --------------------------------------------------------------------------------------------
   --  centurian_release
   --------------------------------------------------------------------------------------------
   function centurian_release (release : String) return Natural
   is
      --  Requires release is validated by release_format()
      X  : String := HT.part_1 (release, ".");
      Y  : String := HT.part_2 (release, ".");
      RX : Natural := Integer'Value (X) * 100;
      RY : Natural := 0;
   begin
      if Y = "" then
        RY := Integer'Value (Y);
      end if;
      return (RX + RY);
   end centurian_release;


   --------------------------------------------------------------------------------------------
   --  LTE
   --------------------------------------------------------------------------------------------
   function LTE (gen_release, spec_release : String) return Boolean
   is
      GR : Natural := 999900;
      SR : Natural := 0;
   begin
      if release_format (gen_release) then
         GR := centurian_release (gen_release);
      end if;
      if release_format (spec_release) then
         SR := centurian_release (spec_release);
      end if;
      return (GR <= SR);
   end LTE;


   --------------------------------------------------------------------------------------------
   --  GTE
   --------------------------------------------------------------------------------------------
   function GTE (gen_release, spec_release : String) return Boolean
   is
      GR : Natural := 0;
      SR : Natural := 999900;
   begin
      if release_format (gen_release) then
         GR := centurian_release (spec_release);
      end if;
      if release_format (spec_release) then
         SR := centurian_release (spec_release);
      end if;
      return (GR >= SR);
   end GTE;


   --------------------------------------------------------------------------------------------
   --  set_outstanding_ignore
   --------------------------------------------------------------------------------------------
   procedure set_outstanding_ignore
     (specs         : in out Portspecs;
      variant       : String;
      opsys         : supported_opsys;
      arch_standard : supported_arch;
      osrelease     : String;
      osmajor       : String)
   is
      --  Copy relevant arch broken to "all" index
      --  Copy relevant opsys broken to "all" index (may be funtion of release or arch)
      procedure check (position : list_crate.Cursor);
      procedure check_ignore;

      cpu_ia64  : constant String := UTL.cpu_arch (x86_64) & "_";
      cpu_ia32  : constant String := UTL.cpu_arch (i386) & "_";
      cpu_armv8 : constant String := UTL.cpu_arch (aarch64) & "_";
      separator : constant String := ": ";
      broken_all_index : constant HT.Text := HT.SUS (broken_all);
      local_broken : group_list;

      procedure check (position : list_crate.Cursor)
      is
         procedure check_list (position : string_crate.Cursor);

         original_key : constant String  := HT.USS (list_crate.Element (position).group);

         procedure check_list (position : string_crate.Cursor)
         is
            reason : constant String := HT.USS (string_crate.Element (position));
            used   : Boolean := False;
            split  : Boolean := True;
         begin
            if UTL.valid_cpu_arch (original_key) then
               if original_key = UTL.cpu_arch (arch_standard) then
                  used  := True;
                  split := False;
               end if;
            elsif UTL.valid_lower_opsys (original_key) then
               if original_key = UTL.lower_opsys (opsys) then
                  if HT.leads (reason, "REL_") then
                     used := (HT.partial_search (reason, 4, separator) = osmajor);
                  elsif HT.leads (reason, "GTE_") then
                     used := GTE (gen_release  => osrelease,
                                  spec_release => HT.partial_search (reason, 4, separator));
                  elsif HT.leads (reason, "LTE_") then
                     used := LTE (gen_release  => osmajor,
                                  spec_release => HT.partial_search (reason, 4, separator));
                  elsif HT.leads (reason, cpu_ia64) then
                     used := (HT.partial_search (reason, cpu_ia64'Length, separator) = osmajor);
                  elsif HT.leads (reason, cpu_ia32) then
                     used := (HT.partial_search (reason, cpu_ia32'Length, separator) = osmajor);
                  elsif HT.leads (reason, cpu_armv8) then
                     used := (HT.partial_search (reason, cpu_armv8'Length, separator) = osmajor);
                  else
                     used  := True;
                     split := False;
                  end if;
               end if;
            else
               used := True;
               split := False;
            end if;

            if used then
               if split then
                  local_broken.list.Append (HT.SUS (HT.part_2 (reason, ": ")));
               else
                  local_broken.list.Append (HT.SUS (reason));
               end if;
            end if;
         end check_list;

      begin
         if not specs.skip_opsys_dep then
            list_crate.Element (position).list.Iterate (Process => check_list'Access);
         end if;
      end check;

      procedure check_ignore
      is
         reason : HT.Text;
         LIST_SSL_FAILURE   : constant String := "Does not build with SSL default '";
         LIST_MYSQL_FAILURE : constant String := "Does not build with MySQL default '";
         LIST_PGSQL_FAILURE : constant String := "Does not build with PGSQL default '";
      begin
         --  Handle BROKEN by Operating System
         if not specs.skip_opsys_dep then
            if specs.exc_opsys.Contains (HT.SUS (UTL.lower_opsys (opsys))) or else
              (not specs.inc_opsys.Is_Empty and then
                 not specs.inc_opsys.Contains (HT.SUS (UTL.lower_opsys (opsys))))
            then
               reason := HT.SUS ("Specification excludes " & UTL.mixed_opsys (opsys) & " OS");
               local_broken.list.Append (reason);
            end if;
            if specs.exc_arch.Contains (HT.SUS (UTL.cpu_arch (arch_standard))) then
               reason := HT.SUS ("Specification excludes " & UTL.cpu_arch (arch_standard) &
                                   " architecture");
               local_broken.list.Append (reason);
            end if;
         end if;

         --  Handle BROKEN_SSL directive
         if HT.equivalent (Parameters.configuration.def_ssl, ports_default) then
            if specs.broken_ssl.Contains (HT.SUS (default_ssl)) then
               reason := HT.SUS (LIST_SSL_FAILURE & default_ssl & "'");
               local_broken.list.Append (reason);
            end if;
         else
            if specs.broken_ssl.Contains (Parameters.configuration.def_ssl) then
               reason := HT.SUS (LIST_SSL_FAILURE &
                                   HT.USS (Parameters.configuration.def_ssl) & "'");
               local_broken.list.Append (reason);
            end if;
         end if;

         --  Handle BROKEN_MYSQL directive
         if HT.equivalent (Parameters.configuration.def_mysql_group, ports_default) then
            if specs.broken_mysql.Contains (HT.SUS (default_mysql)) then
               reason := HT.SUS (LIST_MYSQL_FAILURE & default_mysql & "'");
               local_broken.list.Append (reason);
            end if;
         else
            if specs.broken_mysql.Contains (Parameters.configuration.def_mysql_group) then
               reason := HT.SUS (LIST_MYSQL_FAILURE &
                                   HT.USS (Parameters.configuration.def_mysql_group) & "'");
               local_broken.list.Append (reason);
            end if;
         end if;

         --  Handle BROKEN_PGSQL directive
         if HT.equivalent (Parameters.configuration.def_postgresql, ports_default) then
            if specs.broken_pgsql.Contains (HT.SUS (default_pgsql)) then
               reason := HT.SUS (LIST_PGSQL_FAILURE & default_pgsql & "'");
               local_broken.list.Append (reason);
            end if;
         else
            if specs.broken_pgsql.Contains (Parameters.configuration.def_postgresql) then
               reason := HT.SUS (LIST_PGSQL_FAILURE &
                                   HT.USS (Parameters.configuration.def_postgresql) & "'");
               local_broken.list.Append (reason);
            end if;
         end if;

         --  Handle BROKEN on Kaiju port where builders > 1 or TMPFS set.
         if specs.kaiju then
            if Parameters.configuration.num_builders > 1 then
               reason := HT.SUS ("Monster port builder limit is 1, but this profile uses" &
                                   Parameters.configuration.num_builders'Img & " builders.");
               local_broken.list.Append (reason);
            end if;
         end if;

      end check_ignore;

   begin
      local_broken.group := broken_all_index;
      specs.broken.Iterate (Process => check'Access);
      check_ignore;
      specs.broken.Clear;
      specs.broken.Insert (broken_all_index, local_broken);
   end set_outstanding_ignore;


   --------------------------------------------------------------------------------------------
   --  apply_cpe_module
   --------------------------------------------------------------------------------------------
   procedure apply_cpe_module
     (specs         : in out Portspecs;
      arch_standard : supported_arch;
      osmajor       : String)
   is
      function retrieve (key : String; default_value : String) return String;
      function arch_default return String;
      function other_default return String;

      text_cpe : HT.Text := HT.SUS ("cpe");

      function retrieve (key : String; default_value : String) return String
      is
         key_text : HT.Text := HT.SUS (key);
      begin
         if specs.catch_all.Contains (key_text) then
            return HT.USS (specs.catch_all.Element (key_text).list.First_Element);
         else
            return default_value;
         end if;
      end retrieve;

      function arch_default return String is
      begin
         case arch_standard is
            when x86_64  => return "x64";
            when i386    => return "x86";
            when aarch64 => return "aarch64";
         end case;
      end arch_default;

      function other_default return String is
      begin
         if specs.revision = 0 then
            return "";
         else
            return LAT.Colon & HT.int2str (specs.revision);
         end if;
      end other_default;

      procedure add_cve_note (position : string_crate.Cursor) is
      begin
         specs.pkg_notes.Insert (string_crate.Element (position), HT.SUS ("vulnerability_patched"));
      end add_cve_note;
   begin
      if not specs.uses_base.Contains (text_cpe) then
         return;
      end if;
      declare
         cpe_product : String := retrieve ("CPE_PRODUCT", HT.lowercase (specs.get_namebase));
         cpe_vendor  : String := retrieve ("CPE_VENDOR", cpe_product);
         cpe_version : String := retrieve ("CPE_VERSION", HT.lowercase (HT.USS (specs.version)));
         cpe_tgt_sw  : String := retrieve ("CPE_TARGET_SW",
                                           UTL.lower_opsys (platform_type) & osmajor);
         default_note : String := "cpe:2.3" & LAT.Colon &
           retrieve ("CPE_PART", "a")       & LAT.Colon &
           cpe_vendor                       & LAT.Colon &
           cpe_product                      & LAT.Colon &
           cpe_version                      & LAT.Colon &
           retrieve ("CPE_UPDATE", "")      & LAT.Colon &
           retrieve ("CPE_EDITION", "")     & LAT.Colon &
           retrieve ("CPE_LANG", "")        & LAT.Colon &
           retrieve ("CPE_SW_EDITION", "")  & LAT.Colon &
           cpe_tgt_sw                       & LAT.Colon &
           retrieve ("CPE_TARGET_HW", arch_default) &
           retrieve ("CPE_OTHER", other_default);
      begin
         specs.pkg_notes.Insert (text_cpe, HT.SUS (retrieve ("CPE_STR", default_note)));
      end;
      specs.fixed_cve.Iterate (add_cve_note'Access);
   end apply_cpe_module;


   --------------------------------------------------------------------------------------------
   --  add_primdev_submodule
   --------------------------------------------------------------------------------------------
   procedure add_primdev_submodule  (specs      : in out Portspecs;
                                     namebase   : String)
   is
   begin
      add_build_depends (specs, dev_triplet (namebase));
      add_buildrun_depends (specs, primary_triplet (namebase));
   end add_primdev_submodule;

   --------------------------------------------------------------------------------------------
   --  generic_build_module
   --------------------------------------------------------------------------------------------
   procedure generic_build_module
     (specs      : in out Portspecs;
      module     : String;
      dependency : String)
   is
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_build_depends (specs, dependency);
      end if;
   end generic_build_module;


   --------------------------------------------------------------------------------------------
   --  generic_library_module
   --------------------------------------------------------------------------------------------
   procedure generic_library_module
     (specs      : in out Portspecs;
      module     : String;
      dependency : String)
   is
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_buildrun_depends (specs, dependency);
      end if;
   end generic_library_module;


   --------------------------------------------------------------------------------------------
   --  generic_devlib_module
   --------------------------------------------------------------------------------------------
   procedure generic_devlib_module
     (specs     : in out Portspecs;
      module    : String;
      depprefix : String)
   is
      dev_dependency     : constant String := dev_triplet (depprefix);
      primary_dependency : constant String := primary_triplet (depprefix);
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_build_depends (specs, dev_dependency);
         if not argument_present (specs, module, BUILD) then
            add_buildrun_depends (specs, primary_dependency);
         end if;
      end if;
   end generic_devlib_module;


   --------------------------------------------------------------------------------------------
   --  generic_run_module
   --------------------------------------------------------------------------------------------
   procedure generic_run_module
     (specs      : in out Portspecs;
      module     : String;
      dependency : String)
   is
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_run_depends (specs, dependency);
      end if;
   end generic_run_module;


   --------------------------------------------------------------------------------------------
   --  apply_scons_module
   --------------------------------------------------------------------------------------------
   procedure apply_scons_module (specs : in out Portspecs)
   is
      module     : constant String := "scons";
      dependency : constant String := single_triplet (module);
   begin
      generic_build_module (specs, module, dependency);
   end apply_scons_module;


   --------------------------------------------------------------------------------------------
   --  apply_mold_module
   --------------------------------------------------------------------------------------------
   procedure apply_mold_module (specs : in out Portspecs)
   is
      module     : constant String := "mold";
      dependency : constant String := primary_triplet (module);
   begin
      generic_build_module (specs, module, dependency);
   end apply_mold_module;


   --------------------------------------------------------------------------------------------
   --  apply_gmake_module
   --------------------------------------------------------------------------------------------
   procedure apply_gmake_module (specs : in out Portspecs)
   is
      module     : constant String := "gmake";
      dependency : constant String := primary_triplet (module);
   begin
      generic_build_module (specs, module, dependency);
   end apply_gmake_module;


   --------------------------------------------------------------------------------------------
   --  apply_cargo_module
   --------------------------------------------------------------------------------------------
   procedure apply_cargo_module (specs : in out Portspecs)
   is
      module     : constant String := "cargo";
      dependency : constant String := single_triplet ("rust");
   begin
      generic_build_module (specs, module, dependency);
   end apply_cargo_module;


   --------------------------------------------------------------------------------------------
   --  apply_ninja_module
   --------------------------------------------------------------------------------------------
   procedure apply_ninja_module (specs : in out Portspecs)
   is
      module    : constant String := "ninja";
      pyx       : constant String := HT.USS (specs.used_python);
      ninja_pkg : constant String := "python-ninja:single:" & pyx;
   begin
      generic_build_module (specs, module, ninja_pkg);
   end apply_ninja_module;


   --------------------------------------------------------------------------------------------
   --  apply_meson_module
   --------------------------------------------------------------------------------------------
   procedure apply_meson_module (specs : in out Portspecs)
   is
      module     : constant String := "meson";
      pyx        : constant String := HT.USS (specs.used_python);
      meson_pkg  : constant String := "meson:single:" & pyx;
   begin
      generic_build_module (specs, module, meson_pkg);
   end apply_meson_module;


   --------------------------------------------------------------------------------------------
   --  apply_zlib_module
   --------------------------------------------------------------------------------------------
   procedure apply_zlib_module (specs : in out Portspecs)
   is
      zlib : String := "zlib";
   begin
      generic_devlib_module (specs, zlib, zlib);
   end apply_zlib_module;


   --------------------------------------------------------------------------------------------
   --  apply_mesa_module
   --------------------------------------------------------------------------------------------
   procedure apply_mesa_module (specs : in out Portspecs)
   is
      mesa : constant String := "mesa";
   begin
      generic_devlib_module (specs, mesa, mesa);
   end apply_mesa_module;


   --------------------------------------------------------------------------------------------
   --  apply_makeinfo_module
   --------------------------------------------------------------------------------------------
   procedure apply_makeinfo_module  (specs : in out Portspecs)
   is
      module     : constant String := "makeinfo";
      dependency : constant String := primary_triplet ("texinfo");
   begin
      generic_build_module (specs, module, dependency);
   end apply_makeinfo_module;


   --------------------------------------------------------------------------------------------
   --  apply_readline_module
   --------------------------------------------------------------------------------------------
   procedure apply_readline_module  (specs : in out Portspecs)
   is
      module : constant String := "readline";
   begin
      generic_devlib_module (specs, module, module);
   end apply_readline_module;


   --------------------------------------------------------------------------------------------
   --  apply_sqlite_module
   --------------------------------------------------------------------------------------------
   procedure apply_sqlite_module  (specs : in out Portspecs)
   is
      module : constant String := "sqlite";
   begin
      generic_devlib_module (specs, module, module);
   end apply_sqlite_module;


   --------------------------------------------------------------------------------------------
   --  apply_execinfo_module
   --------------------------------------------------------------------------------------------
   procedure apply_execinfo_module  (specs : in out Portspecs)
   is
      module : constant String := "execinfo";
   begin
      if platform_type /= macos then
         --  MacOS has Mach-O formatted files while libexecinfo only
         --  functions under the ELF format, so it's both unbuildable and unusable on mac.
         generic_devlib_module (specs, module, "libexecinfo");
      end if;
   end apply_execinfo_module;


   --------------------------------------------------------------------------------------------
   --  apply_cmake_module
   --------------------------------------------------------------------------------------------
   procedure apply_cmake_module (specs : in out Portspecs)
   is
      module     : constant String := "cmake";
      dependency : constant String := primary_triplet (module);
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         if argument_present (specs, module, RUN) then
            add_buildrun_depends (specs, dependency);
         else
            add_build_depends (specs, dependency);
         end if;
      end if;
   end apply_cmake_module;


   --------------------------------------------------------------------------------------------
   --  apply_clang_module
   --------------------------------------------------------------------------------------------
   procedure apply_clang_module (specs : in out Portspecs)
   is
      module     : constant String := "clang";
      dependency : constant String := generic_triplet (module, spkg_complete);
   begin
      generic_build_module (specs, module, dependency);
   end apply_clang_module;


   --------------------------------------------------------------------------------------------
   --  apply_gtkdoc_module
   --------------------------------------------------------------------------------------------
   procedure apply_gtkdoc_module (specs : in out Portspecs)
   is
      module     : constant String := "gtk-doc";
      dependency : constant String := single_triplet (module);
   begin
      generic_build_module (specs, module, dependency);
   end apply_gtkdoc_module;


   --------------------------------------------------------------------------------------------
   --  apply_cran_module
   --------------------------------------------------------------------------------------------
   procedure apply_cran_module (specs : in out Portspecs)
   is
      module    : constant String := "cran";
      cran_main : constant String := primary_triplet ("R");
   begin
      generic_build_module (specs, module, cran_main);
      generic_build_module (specs, module, dev_triplet ("icu"));
      generic_run_module   (specs, module, cran_main);
      generic_run_module   (specs, module, generic_triplet ("R", spkg_nls));
   end apply_cran_module;


   --------------------------------------------------------------------------------------------
   --  apply_gcc_run_module
   --------------------------------------------------------------------------------------------
   procedure apply_gcc_run_module (specs : in out Portspecs;
                                   variant : String;
                                   module  : String;
                                   gccsubpackage : String)
   is
      procedure scan (position : string_crate.Cursor);

      dependency : constant String := generic_triplet (ports_compiler, gccsubpackage);
      cc_libs    : constant String := generic_triplet (ports_compiler, "libs");

      procedure scan (position : string_crate.Cursor)
      is
         subpackage : constant String := HT.USS (string_crate.Element (position));
      begin
         if argument_present (specs, module, subpackage) then
            add_exrun_depends (specs, dependency, subpackage);
            if gccsubpackage = "cxx_run" or else
              gccsubpackage = "fortran_run" or else
              gccsubpackage = "ada_run"
            then
               add_exrun_depends (specs, cc_libs, subpackage);
            end if;
         end if;
      end scan;
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         specs.subpackages.Element (HT.SUS (variant)).list.Iterate (scan'Access);
      end if;
   end apply_gcc_run_module;


   --------------------------------------------------------------------------------------------
   --  apply_gcc_full_set
   --------------------------------------------------------------------------------------------
   procedure apply_gcc_full_set (specs : in out Portspecs; variant : String)
   is
      module : constant String := "compiler";

      procedure scan (position : string_crate.Cursor)
      is
         subpkg : constant String := HT.USS (string_crate.Element (position));
      begin
         if argument_present (specs, module, subpkg) then
            add_exrun_depends (specs, generic_triplet (ports_compiler, "libs"), subpkg);
            add_exrun_depends (specs, generic_triplet (ports_compiler, "ada_run"), subpkg);
            add_exrun_depends (specs, generic_triplet (ports_compiler, "cxx_run"), subpkg);
            add_exrun_depends (specs, generic_triplet (ports_compiler, "fortran_run"), subpkg);
            add_exrun_depends (specs, generic_triplet (ports_compiler, "infopages"), subpkg);
            add_exrun_depends (specs, generic_triplet (ports_compiler, "compilers"), subpkg);
            add_exrun_depends (specs, generic_triplet (ports_compiler, "set"), subpkg);
            add_exrun_depends (specs, single_triplet ("ravensys-binutils"), subpkg);
         end if;
      end scan;
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         specs.subpackages.Element (HT.SUS (variant)).list.Iterate (scan'Access);
      end if;
   end apply_gcc_full_set;


   --------------------------------------------------------------------------------------------
   --  add_exrun_cclibs
   --------------------------------------------------------------------------------------------
   procedure add_exrun_cclibs (specs : in out Portspecs; variant : String)
   is
      prime_pkg     : HT.Text;
      primary_found : Boolean := False;
   begin
      --  Calculate "primary" package
      for item in Positive range 1 .. specs.get_subpackage_length (variant) loop
         declare
            subpackage : String := specs.get_subpackage_item (variant, item);
         begin
            if not primary_found and then
              subpackage /= spkg_complete and then
              subpackage /= spkg_examples and then
              subpackage /= spkg_nls and then
              subpackage /= spkg_info and then
              subpackage /= spkg_man and then
              subpackage /= spkg_dev and then
              subpackage /= spkg_docs
            then
               prime_pkg := HT.SUS (subpackage);
               primary_found := True;
            end if;
         end;
      end loop;

      --  Abort if primary package was never determined.  Spec file is wrong?
      if not primary_found then
         return;
      end if;

      declare
         dependency : constant String := generic_triplet (ports_compiler, "libs");
      begin
         --  Check if cclibs:<subpackage> has already been set, and abort if so
         if specs.extra_rundeps.Contains (prime_pkg) and then
           specs.extra_rundeps.Element (prime_pkg).list.Contains (HT.SUS (dependency))
         then
            return;
         end if;

         --  set cclibs:<primary subpackage>
         add_exrun_depends (specs, dependency, HT.USS (prime_pkg));
      end;

   end add_exrun_cclibs;


   --------------------------------------------------------------------------------------------
   --  apply_mysql_module
   --------------------------------------------------------------------------------------------
   procedure apply_mysql_module (specs : in out Portspecs)
   is
      module : constant String := "mysql";

      db_namebase    : constant String := determine_mysql_namebase;
      dev_package    : constant String := dev_triplet (db_namebase);
      server_package : constant String := generic_triplet (db_namebase, "server");
      client_package : constant String := generic_triplet (db_namebase, "client");
      only_build     : constant Boolean := argument_present (specs, module, "build");
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_build_depends (specs, dev_package);
         if only_build then
            add_build_depends (specs, client_package);
         else
            add_buildrun_depends (specs, client_package);
         end if;
         if argument_present (specs, module, "server") then
            if only_build then
               add_build_depends (specs, server_package);
            else
               add_buildrun_depends (specs, server_package);
            end if;
         end if;
      end if;
   end apply_mysql_module;


   --------------------------------------------------------------------------------------------
   --  apply_pgsql_module
   --------------------------------------------------------------------------------------------
   procedure apply_pgsql_module (specs : in out Portspecs)
   is
      procedure set_dependency (subpackage : String);
      procedure set_dependency_on_subpackage (subpackage : String);

      module : constant String := "pgsql";
      namebase : String := determine_pgsql_namebase;
      build_only : Boolean := argument_present (specs, module, BUILD);

      procedure set_dependency (subpackage : String)
      is
         dependency : constant String := generic_triplet (namebase, subpackage);
      begin
         if build_only then
            add_build_depends (specs, dependency);
         else
            add_buildrun_depends (specs, dependency);
         end if;
      end set_dependency;

      procedure set_dependency_on_subpackage (subpackage : String) is
      begin
         if argument_present (specs, module, subpackage) then
            set_dependency (subpackage);
         end if;
      end set_dependency_on_subpackage;
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_build_depends (specs, dev_triplet (namebase));
         if no_arguments_present (specs, module) then
            set_dependency ("client");
         elsif argument_present (specs, module, "all") then
            --  ignore everything else
            set_dependency (spkg_complete);
         else
            set_dependency_on_subpackage ("server");
            set_dependency_on_subpackage ("plperl");
            set_dependency_on_subpackage ("plpython");
            set_dependency_on_subpackage ("pltcl");
            set_dependency_on_subpackage ("contrib");
            if not argument_present (specs, module, "server") then
               set_dependency_on_subpackage ("client");
            end if;
         end if;
      end if;
   end apply_pgsql_module;


   --------------------------------------------------------------------------------------------
   --  apply_ncurses_module
   --------------------------------------------------------------------------------------------
   procedure apply_ncurses_module (specs : in out Portspecs)
   is
      ncurses : constant String := "ncurses";
   begin
      generic_devlib_module (specs, ncurses, ncurses);
      if specs.uses_base.Contains (HT.SUS (ncurses)) then
         add_buildrun_depends (specs, generic_triplet ("ncurses", "terminfo"));
      end if;
   end apply_ncurses_module;


   --------------------------------------------------------------------------------------------
   --  apply_libtool_module
   --------------------------------------------------------------------------------------------
   procedure apply_libtool_module (specs : in out Portspecs)
   is
      module     : constant String := "libtool";
      dependency : constant String := primary_triplet (module);
   begin
      if specs.uses_base.Contains (HT.SUS (module)) and then
        argument_present (specs, module, BUILD)
      then
         add_build_depends (specs, dependency);
      end if;
   end apply_libtool_module;


   --------------------------------------------------------------------------------------------
   --  apply_libiconv_module
   --------------------------------------------------------------------------------------------
   procedure apply_libiconv_module (specs : in out Portspecs)
   is
      module     : constant String := "iconv";
      dep_prefix : constant String := "libiconv";
      intl_h     : constant String := HT.USS (Parameters.configuration.dir_sysroot) &
                                      "/usr/include/libintl.h";
      glibc      : constant Boolean := DIR.Exists (intl_h);
   begin
      if not glibc or else argument_present (specs, module, "standalone") then
         generic_devlib_module (specs, module, dep_prefix);
      end if;
   end apply_libiconv_module;


   --------------------------------------------------------------------------------------------
   --  apply_curl_module
   --------------------------------------------------------------------------------------------
   procedure apply_curl_module (specs : in out Portspecs)
   is
      curl : constant String := "curl";
   begin
      generic_devlib_module (specs, curl, curl);
   end apply_curl_module;


   --------------------------------------------------------------------------------------------
   --  apply_zstd_module
   --------------------------------------------------------------------------------------------
   procedure apply_zstd_module (specs : in out Portspecs)
   is
      zstd : constant String := "zstd";
   begin
      generic_devlib_module (specs, zstd, zstd);
   end apply_zstd_module;


   --------------------------------------------------------------------------------------------
   --  apply_lz4_module
   --------------------------------------------------------------------------------------------
   procedure apply_lz4_module (specs : in out Portspecs)
   is
      lz4 : constant String := "lz4";
   begin
      generic_devlib_module (specs, lz4, lz4);
   end apply_lz4_module;


   --------------------------------------------------------------------------------------------
   --  apply_lzo_module
   --------------------------------------------------------------------------------------------
   procedure apply_lzo_module (specs : in out Portspecs)
   is
      lzo : constant String := "lzo";
   begin
      generic_devlib_module (specs, lzo, lzo);
   end apply_lzo_module;


   --------------------------------------------------------------------------------------------
   --  apply_bz2_module
   --------------------------------------------------------------------------------------------
   procedure apply_bz2_module (specs : in out Portspecs)
   is
      module     : constant String := "bz2";
      dep_prefix : constant String := "bzip2";
   begin
      generic_devlib_module (specs, module, dep_prefix);
   end apply_bz2_module;


   --------------------------------------------------------------------------------------------
   --  apply_expat_module
   --------------------------------------------------------------------------------------------
   procedure apply_expat_module (specs : in out Portspecs)
   is
      expat : constant String := "expat";
   begin
      generic_devlib_module (specs, expat, expat);
   end apply_expat_module;


   --------------------------------------------------------------------------------------------
   --  apply_pcre_module
   --------------------------------------------------------------------------------------------
   procedure apply_pcre_module (specs : in out Portspecs)
   is
      pcre : constant String := "pcre";
   begin
      generic_devlib_module (specs, pcre, pcre);
   end apply_pcre_module;


   --------------------------------------------------------------------------------------------
   --  apply_pcre2_module
   --------------------------------------------------------------------------------------------
   procedure apply_pcre2_module (specs : in out Portspecs)
   is
      pcre2 : constant String := "pcre2";
   begin
      generic_devlib_module (specs, pcre2, pcre2);
   end apply_pcre2_module;


   --------------------------------------------------------------------------------------------
   --  apply_freetype_module
   --------------------------------------------------------------------------------------------
   procedure apply_freetype_module (specs : in out Portspecs)
   is
      freetype : constant String := "freetype";
   begin
      generic_devlib_module (specs, freetype, freetype);
   end apply_freetype_module;


   --------------------------------------------------------------------------------------------
   --  apply_bsd_module
   --------------------------------------------------------------------------------------------
   procedure apply_bsd_module (specs : in out Portspecs)
   is
      bsd_module : constant String := "bsd";
   begin
      if not specs.uses_base.Contains (HT.SUS (bsd_module)) then
         return;
      end if;

      case platform_type is
         when dragonfly | freebsd | openbsd | netbsd | midnightbsd =>
            if argument_present (specs, bsd_module, "epoll") then
               generic_devlib_module (specs, bsd_module, "libepoll-shim");
            end if;
            if argument_present (specs, bsd_module, "inotify") then
               generic_devlib_module (specs, bsd_module, "libinotify");
            end if;
            if argument_present (specs, bsd_module, "udev") then
               generic_devlib_module (specs, bsd_module, "libudev-devd");
            end if;
            if argument_present (specs, bsd_module, "gudev") then
               generic_devlib_module (specs, bsd_module, "libgudev-devd");
            end if;
         when others =>
            if argument_present (specs, bsd_module, "gudev") then
               generic_devlib_module (specs, bsd_module, "libgudev");
            end if;
      end case;
   end apply_bsd_module;


   --------------------------------------------------------------------------------------------
   --  apply_info_presence
   --------------------------------------------------------------------------------------------
   procedure apply_info_presence (specs : in out Portspecs)
   is
      procedure scan (position : string_crate.Cursor);

      dependency : constant String := single_triplet ("indexinfo");

      procedure scan (position : string_crate.Cursor)
      is
         subpkg_infopage : constant String := HT.USS (string_crate.Element (position));
         subpackage : constant String := HT.part_1 (subpkg_infopage, ":");
      begin
         add_exrun_depends (specs, dependency, subpackage);
      end scan;
   begin
      specs.info.Iterate (scan'Access);
   end apply_info_presence;


   --------------------------------------------------------------------------------------------
   --  apply_pkgconfig_module
   --------------------------------------------------------------------------------------------
   procedure apply_pkgconfig_module (specs : in out Portspecs)
   is
      module     : constant String := "pkgconfig";
      dependency : constant String := primary_triplet ("pkgconf");
   begin
      if not specs.uses_base.Contains (HT.SUS (module)) then
         return;
      end if;

      if no_arguments_present (specs, module) or else
        argument_present (specs, module, BUILD)
      then
         add_build_depends (specs, dependency);
      else
         if argument_present (specs, module, BUILDRUN) then
            add_buildrun_depends (specs, dependency);
         elsif argument_present (specs, module, RUN) then
            add_run_depends (specs, dependency);
         end if;
      end if;
   end apply_pkgconfig_module;


   --------------------------------------------------------------------------------------------
   --  apply_autoconf_module
   --------------------------------------------------------------------------------------------
   procedure apply_autoconf_module  (specs : in out Portspecs)
   is
      module   : constant String := "autoreconf";
      AUTOCONF : constant String := primary_triplet ("autoconf");
      AUTOMAKE : constant String := primary_triplet ("automake");
      LIBTOOL  : constant String := primary_triplet ("libtool");
   begin
      if not specs.uses_base.Contains (HT.SUS (module)) then
         return;
      end if;
      add_build_depends (specs, AUTOCONF);
      add_build_depends (specs, AUTOMAKE);
      if not argument_present (specs, module, BUILD) then
         add_build_depends (specs, LIBTOOL);
      end if;
   end apply_autoconf_module;


   --------------------------------------------------------------------------------------------
   --  apply_gprbuild_module
   --------------------------------------------------------------------------------------------
   procedure apply_gprbuild_module (specs : in out Portspecs)
   is
      module     : constant String := "gprbuild";
      dependency : constant String := primary_triplet (module);
   begin
      if not specs.uses_base.Contains (HT.SUS (module)) then
         return;
      end if;

      if no_arguments_present (specs, module) or else
        argument_present (specs, module, BUILD)
      then
         add_build_depends (specs, dependency);
      else
         if argument_present (specs, module, BUILDRUN) then
            add_buildrun_depends (specs, dependency);
         elsif argument_present (specs, module, RUN) then
            add_run_depends (specs, dependency);
         end if;
      end if;
   end apply_gprbuild_module;


   --------------------------------------------------------------------------------------------
   --  apply_display_module
   --------------------------------------------------------------------------------------------
   procedure apply_display_module (specs : in out Portspecs)
   is
      module : String := "display";
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_build_depends (specs, "xorg-server:single:virtual");
         add_build_depends (specs, single_triplet ("xorg-misc-bitmap-fonts"));
         add_build_depends (specs, single_triplet ("xorg-font-alias"));
         add_build_depends (specs, primary_triplet ("daemonize"));
      end if;
   end apply_display_module;


   --------------------------------------------------------------------------------------------
   --  apply_gnome_icons_module
   --------------------------------------------------------------------------------------------
   procedure apply_gnome_icons_module (specs : in out Portspecs)
   is
      module     : constant String := "gnome-icons";
      dependency : constant String := generic_triplet ("gtk3", "icon_cache");
      subpackage : constant String := get_argument (specs, module);
   begin
      --  argument already validated to be exactly one subpackage.
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_exrun_depends (specs, dependency, subpackage);
      end if;
   end apply_gnome_icons_module;


   --------------------------------------------------------------------------------------------
   --  apply_mime_info_module
   --------------------------------------------------------------------------------------------
   procedure apply_mime_info_module (specs : in out Portspecs)
   is
      module     : constant String := "mime-info";
      dependency : constant String := primary_triplet ("shared-mime-info");
      subpackage : constant String := get_argument (specs, module);
   begin
      --  argument already validated to be exactly one subpackage.
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_exrun_depends (specs, dependency, subpackage);
      end if;
   end apply_mime_info_module;


   --------------------------------------------------------------------------------------------
   --  apply_schemas_module
   --------------------------------------------------------------------------------------------
   procedure apply_schemas_module (specs : in out Portspecs)
   is
      module     : constant String := "schemas";
      dependency : String renames GNOMELIB;
      subpackage : constant String := get_argument (specs, module);
   begin
      --  argument already validated to be exactly one subpackage.
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_exrun_depends (specs, dependency, subpackage);
      end if;
   end apply_schemas_module;


   --------------------------------------------------------------------------------------------
   --  apply_desktop_utils_module
   --------------------------------------------------------------------------------------------
   procedure apply_desktop_utils_module (specs : in out Portspecs)
   is
      module     : constant String := "desktop-utils";
      dependency : constant String := primary_triplet ("desktop-file-utils");
      subpackage : constant String := get_argument (specs, module);
   begin
      --  argument already validated to be exactly one subpackage.
      if specs.uses_base.Contains (HT.SUS (module)) then
         generic_build_module (specs, module, dependency);
         add_exrun_depends (specs, dependency, subpackage);
      end if;
   end apply_desktop_utils_module;


   --------------------------------------------------------------------------------------------
   --  apply_gettext_runtime_module
   --------------------------------------------------------------------------------------------
   procedure apply_gettext_module (specs : in out Portspecs)
   is
      ASPRINT : constant String := "asprintf";
      ACLOCAL : constant String := "aclocal";
      intl_h  : constant String := HT.USS (Parameters.configuration.dir_sysroot) &
                                   "/usr/include/libintl.h";
   begin
      if specs.uses_base.Contains (HT.SUS (GETTEXT)) then
         declare
            glibc : constant Boolean := DIR.Exists (intl_h);
         begin
            if glibc then
               add_build_depends (specs, generic_triplet (GETTEXT, ACLOCAL));
            else
               add_build_depends (specs, GTDEV);
            end if;
            add_build_depends (specs, GTBTOOLS);
            add_build_depends (specs, GTTOOLS);
            if argument_present (specs, GETTEXT, ASPRINT) then
               add_build_depends (specs, generic_triplet (GETTEXT, ASPRINT & "dev"));
               add_buildrun_depends (specs, generic_triplet (GETTEXT, ASPRINT));
            end if;
            if not argument_present (specs, GETTEXT, BUILD) then
               add_build_depends (specs, GTSOLINX);
               if not glibc then
                  add_buildrun_depends (specs, GTLIB);
               end if;
            end if;
         end;
      end if;
   end apply_gettext_module;


   --------------------------------------------------------------------------------------------
   --  apply_python_module
   --------------------------------------------------------------------------------------------
   procedure apply_python_module (specs : in out Portspecs)
   is
      module        : constant String := "python";
      autopython    : constant String := single_triplet ("autoselect-python");
      sqlite_module : constant String := "sqlite";
      PYVARIANT     : String := PY313;  -- DEFAULT

      use_pip    : Boolean := False;
      use_setup  : Boolean := False;
      use_pep517 : Boolean := False;
      use_sqlite : Boolean := False;
      build_only : Boolean := False;
      default_py : Boolean := True;

      procedure set_base_python_modules
      is
         function dev_spkg return String is
         begin
            if default_py then
               return dev_triplet (PYTHONPRI);
            end if;
            return dev_triplet (PYTHONALT);
         end dev_spkg;

         function primary_spkg return String is
         begin
            if default_py then
               return primary_triplet (PYTHONPRI);
            end if;
            return primary_triplet (PYTHONALT);
         end primary_spkg;

         function sqlite_spkg return String is
         begin
            if default_py then
               return generic_triplet (PYTHONPRI, sqlite_module);
            end if;
            return generic_triplet (PYTHONALT, sqlite_module);
         end sqlite_spkg;
      begin
         add_build_depends (specs, dev_spkg);
         if build_only then
            add_build_depends (specs, primary_spkg);
            if use_sqlite then
               add_build_depends (specs, sqlite_spkg);
            end if;
         else
            add_buildrun_depends (specs, primary_spkg);
            if use_sqlite then
               add_buildrun_depends (specs, sqlite_spkg);
            end if;
         end if;
      end set_base_python_modules;

      procedure set_build_tools (py_variant : String)
      is
         pair_pip     : constant String := "python-pip:single:";
         pair_sutools : constant String := "python-setuptools:single:";
      begin
         if use_pip then
            add_build_depends (specs, pair_pip & py_variant);
         end if;
         if use_setup then
            add_build_depends (specs, pair_sutools & py_variant);
         end if;
         if use_pep517 then
            add_build_depends (specs, pair_pip & py_variant);
            add_build_depends (specs, pair_sutools & py_variant);
            add_build_depends (specs, "python-wheel:single:" & py_variant);
            add_build_depends (specs, "python-build:single:" & py_variant);
            add_build_depends (specs, "python-installer:single:" & py_variant);
         end if;
         specs.used_python := HT.SUS (py_variant);
      end set_build_tools;

   begin
      if not specs.uses_base.Contains (HT.SUS (module)) then
         specs.used_python := HT.SUS (PYVARIANT);
         return;
      end if;

      --  When changing python defaults, don't forget to alter convert_exrun_versions() too.

      if argument_present (specs, module, "pep517") then
         use_pep517 := True;
      elsif argument_present (specs, module, "wheel") then
         use_pip := True;
      elsif argument_present (specs, module, "sutools") then
         use_setup := True;
      end if;

      if argument_present (specs, module, sqlite_module) then
         use_sqlite := True;
      end if;

      if argument_present (specs, module, PY314) then
         PYVARIANT  := PY314;
         default_py := False;
      end if;

      if argument_present (specs, module, "build") then
         build_only := True;
      end if;

      set_base_python_modules;
      set_build_tools (PYVARIANT);
      add_build_depends (specs, autopython);
   end apply_python_module;


   --------------------------------------------------------------------------------------------
   --  apply_ruby_module
   --------------------------------------------------------------------------------------------
   procedure apply_ruby_module (specs : in out Portspecs)
   is
      module     : constant String := "ruby";
      v32        : constant String := "v32";
      v33        : constant String := "v33";
      v34        : constant String := "v34";
   begin
      if not specs.uses_base.Contains (HT.SUS (module)) or else
        argument_present (specs, module, "interp")
      then
         return;
      end if;

      if argument_present (specs, module, "build") then
         if argument_present (specs, module, v32) then
            add_build_depends (specs, RUBY32);
            specs.used_ruby := HT.SUS (v32);
         elsif argument_present (specs, module, v34) then
            add_build_depends (specs, RUBY34);
            specs.used_ruby := HT.SUS (v34);
         else -- default to current default
            add_build_depends (specs, RUBY33);
            specs.used_ruby := HT.SUS (v33);
         end if;
      else
         if argument_present (specs, module, v32) then
            add_buildrun_depends (specs, RUBY32);
            specs.used_ruby := HT.SUS (v32);
         elsif argument_present (specs, module, v34) then
            add_buildrun_depends (specs, RUBY34);
            specs.used_ruby := HT.SUS (v34);
         else -- default to current default
            add_buildrun_depends (specs, RUBY33);
            specs.used_ruby := HT.SUS (v33);
         end if;
      end if;
   end apply_ruby_module;


   --------------------------------------------------------------------------------------------
   --  apply_fonts_module
   --------------------------------------------------------------------------------------------
   procedure apply_fonts_module (specs : in out Portspecs)
   is
      module      : constant String := "fonts";
      fontconfig  : constant String := "fontconfig";
      mkfontscale : constant String := primary_triplet ("xorg-mkfontscale");
   begin
      if not specs.uses_base.Contains (HT.SUS (module)) then
         return;
      end if;
      generic_devlib_module (specs, fontconfig, fontconfig);
      if no_arguments_present (specs, module) or else
        argument_present (specs, module, "fontsdir")
      then
         add_buildrun_depends (specs, mkfontscale);
      end if;
   end apply_fonts_module;


   --------------------------------------------------------------------------------------------
   --  apply_bison_module
   --------------------------------------------------------------------------------------------
   procedure apply_bison_module (specs : in out Portspecs)
   is
      module     : constant String := "bison";
      dependency : constant String := primary_triplet (module);
   begin
      generic_3B_module (specs, module, dependency);
   end apply_bison_module;


   --------------------------------------------------------------------------------------------
   --  generate_dev_depend
   --------------------------------------------------------------------------------------------
   procedure generic_dev_require
     (specs      : in out Portspecs;
      module     : String;
      dependency : String)
   is
      dev_present  : Boolean;
   begin
      if not specs.uses_base.Contains (HT.SUS (module)) then
         return;
      end if;
      dev_present := no_arguments_present (specs, module) or else
        argument_present (specs, module, BUILD) or else
        argument_present (specs, module, BUILDRUN);

      if dev_present then
         add_build_depends (specs, dependency);
      end if;
   end generic_dev_require;


   --------------------------------------------------------------------------------------------
   --  generic_3B_module
   --------------------------------------------------------------------------------------------
   procedure generic_3B_module
     (specs      : in out Portspecs;
      module     : String;
      dependency : String)
   is
      hit_run    : Boolean;
      hit_build  : Boolean;
      hit_both   : Boolean;
   begin
      if not specs.uses_base.Contains (HT.SUS (module)) then
         return;
      end if;
      if no_arguments_present (specs, module) then
         hit_build := True;
         hit_both  := False;
         hit_run   := False;
      else
         hit_build := argument_present (specs, module, BUILD);
         hit_both  := argument_present (specs, module, BUILDRUN);
         hit_run   := argument_present (specs, module, RUN);

         if not (hit_build or else hit_both or else hit_run) then
            hit_build := True;
         end if;
      end if;

      if hit_both or else (hit_build and hit_run) then
         add_buildrun_depends (specs, dependency);
      elsif hit_build then
         add_build_depends (specs, dependency);
      else
         add_run_depends (specs, dependency);
      end if;
   end generic_3B_module;


   --------------------------------------------------------------------------------------------
   --  apply_jpeg_module
   --------------------------------------------------------------------------------------------
   procedure apply_jpeg_module (specs : in out Portspecs)
   is
      module     : constant String := "jpeg";
      dependency : constant String := "jpeg-turbo";
   begin
      generic_devlib_module (specs, module, dependency);
   end apply_jpeg_module;


   --------------------------------------------------------------------------------------------
   --  apply_png_module
   --------------------------------------------------------------------------------------------
   procedure apply_png_module (specs : in out Portspecs)
   is
      module : constant String := "png";
   begin
      generic_devlib_module (specs, module, module);
   end apply_png_module;


   --------------------------------------------------------------------------------------------
   --  apply_gif_module
   --------------------------------------------------------------------------------------------
   procedure apply_gif_module (specs : in out Portspecs)
   is
      module : constant String := "gif";
   begin
      generic_devlib_module (specs, module, "giflib");
   end apply_gif_module;


   --------------------------------------------------------------------------------------------
   --  apply_tiff_module
   --------------------------------------------------------------------------------------------
   procedure apply_tiff_module (specs : in out Portspecs)
   is
      module : constant String := "tiff";
   begin
      generic_devlib_module (specs, module, module);
   end apply_tiff_module;


   --------------------------------------------------------------------------------------------
   --  apply_gem_module
   --------------------------------------------------------------------------------------------
   procedure apply_gem_module (specs : in out Portspecs)
   is
      module : constant String := "gem";
      defver : constant String (1 .. 2) :=
               default_ruby (default_ruby'First) & default_ruby (default_ruby'Last);
      flavor : String := "v" & defver;
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         if not no_arguments_present (specs, module) then
            if argument_present (specs, module, "v34") then
               flavor := "v34";
            elsif argument_present (specs, module, "v32") then
               flavor := "v32";
            elsif argument_present (specs, module, "v33") then
               flavor := "v33";
            end if;
         end if;
         add_buildrun_depends (specs, "ruby-rubygems:single:" & flavor);
      end if;
   end apply_gem_module;


   --------------------------------------------------------------------------------------------
   --  apply_php_module
   --------------------------------------------------------------------------------------------
   procedure apply_php_module (specs : in out Portspecs)
   is
      module      : constant String := "php";
      --  This defver works until PHP 10 is released
      defver : String (1 .. 2) := default_php (default_php'First) & default_php (default_php'Last);
      flavor : String := "php" & defver;
      hit_build   : Boolean := False;
      hit_phpize  : Boolean := False;
      hit_ext     : Boolean := False;
      hit_zend    : Boolean := False;
   begin
      if not specs.uses_base.Contains (HT.SUS (module)) then
         return;
      end if;

      if not no_arguments_present (specs, module) then
         if argument_present (specs, module, "85") then
            flavor := "php85";
         elsif argument_present (specs, module, "83") then
            flavor := "php83";
         elsif argument_present (specs, module, "84") then
            flavor := "php84";
         end if;
         hit_build   := argument_present (specs, module, BUILD);
         hit_phpize  := argument_present (specs, module, "phpize");
         hit_ext     := argument_present (specs, module, "ext");
         hit_zend    := argument_present (specs, module, "zend");
      end if;
      if hit_build or else hit_phpize or else hit_ext or else hit_zend then
         add_buildrun_depends (specs, primary_triplet (flavor));
         add_build_depends (specs, dev_triplet (flavor));
      else
         add_run_depends (specs, primary_triplet (flavor));
      end if;
      if hit_phpize or else hit_ext or else hit_zend then
         add_build_depends (specs, primary_triplet ("autoconf"));
      end if;

   end apply_php_module;


   --------------------------------------------------------------------------------------------
   --  apply_lua_module
   --------------------------------------------------------------------------------------------
   procedure apply_lua_module (specs : in out Portspecs)
   is
      function pick_lua return String;

      module     : constant String := "lua";
      hit_run    : Boolean;
      hit_build  : Boolean;
      hit_both   : Boolean;
      hit_tools  : Boolean;

      function pick_lua return String
      is
         LUA52 : constant String := "lua52";
         LUA53 : constant String := "lua53";
         LUA54 : constant String := "lua54";
         def_setting : constant String := HT.USS (Parameters.configuration.def_lua);
      begin
         if argument_present (specs, module, "5.2") then
            return LUA52;
         elsif argument_present (specs, module, "5.3") then
            return LUA53;
         elsif argument_present (specs, module, "5.4") then
            return LUA54;
         end if;

         --  No valid argument present, use configured setting

         if def_setting = "5.2" then
            return LUA52;
         elsif def_setting = "5.3" then
            return LUA53;
         else
            --  current default: lua54
            return LUA54;
         end if;

      end pick_lua;

      dependency         : constant String := pick_lua;
      dev_subpackage     : constant String := dev_triplet (dependency);
      primary_subpackage : constant String := primary_triplet (dependency);
      tools_subpackage   : constant String := generic_triplet (dependency, "tools");
   begin
      if not specs.uses_base.Contains (HT.SUS (module)) then
         return;
      end if;
      if no_arguments_present (specs, module) then
         hit_build := True;
         hit_both  := False;
         hit_run   := False;
         hit_tools := False;
      else
         hit_build := argument_present (specs, module, BUILD);
         hit_both  := argument_present (specs, module, BUILDRUN);
         hit_run   := argument_present (specs, module, RUN);
         hit_tools := argument_present (specs, module, "tools");

         if not (hit_build or else hit_both or else hit_run) then
            hit_both := True;
         end if;
      end if;

      if hit_both or else (hit_build and hit_run) then
         add_build_depends (specs, dev_subpackage);
         add_buildrun_depends (specs, primary_subpackage);
      elsif hit_build then
         add_build_depends (specs, dev_subpackage);
         add_build_depends (specs, primary_subpackage);
      else
         add_run_depends (specs, primary_subpackage);
      end if;
      if hit_tools then
         add_build_depends (specs, tools_subpackage);
      end if;

      specs.used_lua := HT.SUS (primary_subpackage);
   end apply_lua_module;


   --------------------------------------------------------------------------------------------
   --  apply_tcl_module
   --------------------------------------------------------------------------------------------
   procedure apply_tcl_module (specs : in out Portspecs)
   is
      function  use_the_newest_tcltk return Boolean;
      function  tk_package_name (dev_package : Boolean) return String;
      function  tcl_package_name (dev_package : Boolean) return String;
      procedure link_tk_packages;
      procedure link_tcl_packages;

      module     : constant String := "tcl";
      hit_run    : Boolean;
      hit_build  : Boolean;
      hit_both   : Boolean;
      install_tk : Boolean;
      use_latest : Boolean;

      function use_the_newest_tcltk return Boolean
      is
      begin
         if argument_present (specs, module, "8.6") then
            return False;
         end if;
         if argument_present (specs, module, "9.0") then
            return True;
         end if;
         if HT.USS (Parameters.configuration.def_tcl_tk) = "9.0" then
            return True;
         end if;
         --  If we get here, the port did not specify the version and ravenadm is configured
         --  either to use version 8.6 or it's floating to the default (also 8.6)
         return False;
      end use_the_newest_tcltk;

      function tk_package_name (dev_package : Boolean) return String is
      begin
         if dev_package then
            if use_latest then
               return dev_triplet ("tk90");
            else
               return dev_triplet ("tk86");
            end if;
         else
            if use_latest then
               return generic_triplet ("tk90", "tools");
            else
               return generic_triplet ("tk86", "tools");
            end if;
         end if;
      end tk_package_name;

      function tcl_package_name (dev_package : Boolean) return String is
      begin
         if dev_package then
            if use_latest then
               return dev_triplet ("tcl90");
            else
               return dev_triplet ("tcl86");
            end if;
         else
            if use_latest then
               return TCL90;
            else
               return TCL86;
            end if;
         end if;
      end tcl_package_name;

      procedure link_tk_packages
      is
         dpkg : constant String := tk_package_name (True);
         tpkg : constant String := tk_package_name (False);
      begin
         if hit_build then
            add_build_depends (specs, dpkg);
            add_build_depends (specs, tpkg);
         elsif hit_run then
            add_run_depends (specs, tpkg);
         else
            add_build_depends (specs, dpkg);
            add_buildrun_depends (specs, tpkg);
         end if;
      end link_tk_packages;

      procedure link_tcl_packages
      is
         dpkg : constant String := tcl_package_name (True);
         tpkg : constant String := tcl_package_name (False);
      begin
         if hit_build then
            add_build_depends (specs, dpkg);
            add_build_depends (specs, tpkg);
         elsif hit_run then
            add_run_depends (specs, tpkg);
         else
            add_build_depends (specs, dpkg);
            add_buildrun_depends (specs, tpkg);
         end if;
      end link_tcl_packages;

   begin
      if not specs.uses_base.Contains (HT.SUS (module)) then
         return;
      end if;

      hit_build  := argument_present (specs, module, BUILD);
      hit_run    := not hit_build and then argument_present (specs, module, RUN);
      hit_both   := not hit_build and then not hit_run;
      install_tk := argument_present (specs, module, "tk");
      use_latest := use_the_newest_tcltk;

      if install_tk then
         link_tk_packages;
      else
         link_tcl_packages;
      end if;

   end apply_tcl_module;


   --------------------------------------------------------------------------------------------
   --  apply_ssl_module
   --------------------------------------------------------------------------------------------
   procedure apply_ssl_module (specs : in out Portspecs)
   is
      hit_run    : Boolean;
      hit_build  : Boolean;
      hit_both   : Boolean;
      module     : constant String := "ssl";

   begin
      if not specs.uses_base.Contains (HT.SUS (module)) then
         return;
      end if;
      if no_arguments_present (specs, module) then
         hit_build := False;
         hit_both  := True;
         hit_run   := False;
      else
         hit_build := argument_present (specs, module, BUILD);
         hit_both  := argument_present (specs, module, BUILDRUN);
         hit_run   := argument_present (specs, module, RUN);

         if not (hit_build or else hit_both or else hit_run) then
            hit_both := True;
         end if;
      end if;

      declare
         normvar : constant String := Parameters.ssl_selection (Parameters.configuration);
         myssl   : constant String := specs.get_ssl_variant (normvar);
         primary : constant String := primary_triplet (myssl);
         devpkg  : constant String := dev_triplet (myssl);
      begin
         if hit_both or else (hit_build and hit_run) then
            add_build_depends (specs, devpkg);
            add_buildrun_depends (specs, primary);
         elsif hit_build then
            add_build_depends (specs, devpkg);
         else
            add_run_depends (specs, primary);
         end if;
      end;
   end apply_ssl_module;


   --------------------------------------------------------------------------------------------
   --  apply_bdb_module
   --------------------------------------------------------------------------------------------
   procedure apply_bdb_module (specs : in out Portspecs)
   is
      module : constant String  := "bdb";
   begin
      if argument_present (specs, module, "18") then
         generic_devlib_module (specs, module, "db18");
      else
         generic_devlib_module (specs, module, "db5");
      end if;
   end apply_bdb_module;


   --------------------------------------------------------------------------------------------
   --  apply_perl_module
   --------------------------------------------------------------------------------------------
   procedure apply_perl_module (specs : in out Portspecs)
   is
      function retrieve_dependency return String;

      module        : constant String := "perl";
      pmodbuild     : constant String := "perl-Module-Build:single:";
      pmodbuildtiny : constant String := "perl-Module-Build-Tiny:single:";
      perl_540      : constant String := "540";
      perl_542      : constant String := "542";
      dep_suffix    : String := "   ";
      hit_run       : Boolean;
      hit_build     : Boolean;
      hit_both      : Boolean;
      hit_bmod      : Boolean;
      hit_bmodtiny  : Boolean;
      hit_config    : Boolean;

      function retrieve_dependency return String
      is
         rport_default : String := "perl-" & default_perl;
         def_setting   : String := HT.USS (Parameters.configuration.def_perl);
         override_dep  : String := "perl-" & def_setting;
      begin
         if argument_present (specs, module, perl_540) then
            dep_suffix := perl_540;
            return primary_triplet ("perl-5.40");
         elsif argument_present (specs, module, perl_542) then
            dep_suffix := perl_542;
            return primary_triplet ("perl-5.42");
         else
            if def_setting = ports_default then
               dep_suffix := HT.replace_char (default_perl, LAT.Full_Stop, "");
               return primary_triplet (rport_default);
            else
               dep_suffix := HT.replace_char (def_setting, LAT.Full_Stop, "");
               return primary_triplet (override_dep);
            end if;
         end if;
      end retrieve_dependency;

      dependency : String := retrieve_dependency;
   begin
      if not specs.uses_base.Contains (HT.SUS (module)) then
         return;
      end if;
      if no_arguments_present (specs, module) then
         hit_build    := False;
         hit_both     := True;
         hit_run      := False;
         hit_bmod     := False;
         hit_bmodtiny := False;
         hit_config   := False;
      else
         hit_build    := argument_present (specs, module, BUILD);
         hit_run      := argument_present (specs, module, RUN);
         hit_bmod     := argument_present (specs, module, "buildmod");
         hit_bmodtiny := argument_present (specs, module, "buildmodtiny");
         hit_config   := argument_present (specs, module, "configure");
         hit_both     := hit_config or else
           (hit_run and hit_bmod) or else
           (hit_run and hit_bmodtiny);

         if hit_bmod or else hit_bmodtiny then
            hit_build := True;
         end if;

         if not (hit_build or else hit_both or else hit_run) then
            hit_both := True;
         end if;
      end if;
      if hit_both or else (hit_build and hit_run) then
         add_buildrun_depends (specs, dependency);
      elsif hit_build then
         add_build_depends (specs, dependency);
      elsif hit_run then
         add_run_depends (specs, dependency);
      end if;

      if hit_bmod then
         if specs.get_namebase /= "perl-Module-Build" then
            add_build_depends (specs, pmodbuild & dep_suffix);
         end if;
      elsif hit_bmodtiny then
         if specs.get_namebase /= "perl-Module-Build-Tiny" then
            add_build_depends (specs, pmodbuildtiny & dep_suffix);
         end if;
      end if;

      specs.used_perl := HT.SUS (dep_suffix);

   end apply_perl_module;


   --------------------------------------------------------------------------------------------
   --  apply_xz_module
   --------------------------------------------------------------------------------------------
   procedure apply_xz_module (specs : in out Portspecs)
   is
      xz : constant String := "xz";
   begin
      generic_devlib_module (specs, xz, xz);
   end apply_xz_module;


   --------------------------------------------------------------------------------------------
   --  apply_fontconfig_module
   --------------------------------------------------------------------------------------------
   procedure apply_fontconfig_module (specs : in out Portspecs)
   is
      fontconfig : constant String := "fontconfig";
   begin
      generic_devlib_module (specs, fontconfig, fontconfig);
   end apply_fontconfig_module;


   --------------------------------------------------------------------------------------------
   --  apply_opsys_dependencies
   --------------------------------------------------------------------------------------------
   procedure apply_opsys_dependencies (specs : in out Portspecs)
   is
      procedure scan_dep (position : string_crate.Cursor);
      procedure scan_use (position : string_crate.Cursor);

      type deptype is (os_build, os_run, os_buildrun);

      dt : deptype;
      key_opsys : HT.Text := HT.SUS (UTL.lower_opsys (platform_type));

      procedure scan_dep (position : string_crate.Cursor)
      is
         dependency : String := HT.USS (string_crate.Element (position));
      begin
         case dt is
            when os_build    => add_build_depends (specs, dependency);
            when os_run      => add_run_depends (specs, dependency);
            when os_buildrun => add_buildrun_depends (specs, dependency);
         end case;
      end scan_dep;

      procedure scan_use (position : string_crate.Cursor)
      is
         text_value : HT.Text renames string_crate.Element (position);
         text_stripped : HT.Text := HT.SUS (HT.part_1 (HT.USS (text_value), ":"));
      begin
         specs.uses.Append (text_value);
         if not specs.uses_base.Contains (text_stripped) then
            specs.uses_base.Append (text_stripped);
         end if;
      end scan_use;
   begin
      if specs.skip_opsys_dep then
         return;
      end if;
      if specs.opsys_b_deps.Contains (key_opsys) then
         dt := os_build;
         specs.opsys_b_deps.Element (key_opsys).list.Iterate (scan_dep'Access);
      end if;
      if specs.opsys_r_deps.Contains (key_opsys) then
         dt := os_run;
         specs.opsys_r_deps.Element (key_opsys).list.Iterate (scan_dep'Access);
      end if;
      if specs.opsys_br_deps.Contains (key_opsys) then
         dt := os_buildrun;
         specs.opsys_br_deps.Element (key_opsys).list.Iterate (scan_dep'Access);
      end if;
      if specs.opsys_c_uses.Contains (key_opsys) then
         specs.opsys_c_uses.Element (key_opsys).list.Iterate (scan_use'Access);
      end if;
   end apply_opsys_dependencies;


   --------------------------------------------------------------------------------------------
   --  apply_curly_bracket_conversions
   --------------------------------------------------------------------------------------------
   procedure apply_curly_bracket_conversions (specs : in out Portspecs)
   is
      procedure apply_to_list1 (position : list_crate.Cursor);
      procedure apply_to_list2 (position : list_crate.Cursor);
      procedure apply_to_list3 (position : list_crate.Cursor);
      procedure apply_to_list4 (position : list_crate.Cursor);
      procedure apply_to_list5 (position : list_crate.Cursor);
      procedure alter (Key : HT.Text; Element : in out group_list);

      procedure alter (Key : HT.Text; Element : in out group_list) is
      begin
         apply_cbc_string_crate (Element.list);
      end alter;

      procedure apply_to_list1 (position : list_crate.Cursor) is
      begin
         specs.catch_all.Update_Element (Position => position,
                                         Process  => alter'Access);
      end apply_to_list1;
      procedure apply_to_list2 (position : list_crate.Cursor) is
      begin
         specs.extract_head.Update_Element (Position => position,
                                            Process  => alter'Access);
      end apply_to_list2;
      procedure apply_to_list3 (position : list_crate.Cursor) is
      begin
         specs.extract_tail.Update_Element (Position => position,
                                            Process  => alter'Access);
      end apply_to_list3;
      procedure apply_to_list4 (position : list_crate.Cursor) is
      begin
         specs.var_opsys.Update_Element (Position => position,
                                         Process  => alter'Access);
      end apply_to_list4;
      procedure apply_to_list5 (position : list_crate.Cursor) is
      begin
         specs.var_arch.Update_Element (Position => position,
                                        Process  => alter'Access);
      end apply_to_list5;

   begin
      UTL.apply_cbc_string (specs.install_wrksrc);
      UTL.apply_cbc_string (specs.build_wrksrc);
      UTL.apply_cbc_string (specs.patch_wrksrc);
      UTL.apply_cbc_string (specs.config_wrksrc);
      UTL.apply_cbc_string (specs.config_prefix);
      UTL.apply_cbc_string (specs.config_script);
      UTL.apply_cbc_string (specs.config_target);
      UTL.apply_cbc_string (specs.distname);
      UTL.apply_cbc_string (specs.prefix);

      apply_cbc_string_crate (specs.config_args);
      apply_cbc_string_crate (specs.config_env);
      apply_cbc_string_crate (specs.make_args);
      apply_cbc_string_crate (specs.make_env);
      apply_cbc_string_crate (specs.test_args);
      apply_cbc_string_crate (specs.test_env);
      apply_cbc_string_crate (specs.cflags);
      apply_cbc_string_crate (specs.cxxflags);
      apply_cbc_string_crate (specs.cppflags);
      apply_cbc_string_crate (specs.ldflags);
      apply_cbc_string_crate (specs.cmake_args);
      apply_cbc_string_crate (specs.qmake_args);
      apply_cbc_string_crate (specs.test_args);
      apply_cbc_string_crate (specs.lic_files);
      apply_cbc_string_crate (specs.lic_source);
      apply_cbc_string_crate (specs.lic_terms);
      apply_cbc_string_crate (specs.plist_sub);
      apply_cbc_string_crate (specs.mk_verbatim);
      apply_cbc_string_crate (specs.sub_list);
      apply_cbc_string_crate (specs.users);
      apply_cbc_string_crate (specs.groups);
      apply_cbc_string_crate (specs.mandirs);

      specs.catch_all.Iterate (apply_to_list1'Access);
      specs.extract_head.Iterate (apply_to_list2'Access);
      specs.extract_tail.Iterate (apply_to_list3'Access);
      specs.var_opsys.Iterate (apply_to_list4'Access);
      specs.var_arch.Iterate (apply_to_list5'Access);

   end apply_curly_bracket_conversions;


   --------------------------------------------------------------------------------------------
   --  apply_cbc_string_crate
   --------------------------------------------------------------------------------------------
   procedure apply_cbc_string_crate (crate : in out string_crate.Vector)
   is
      procedure check (position : string_crate.Cursor);
      procedure swap_braces (Element : in out HT.Text);
      procedure swap_braces (Element : in out HT.Text) is
      begin
         UTL.apply_cbc_string (Element);
      end swap_braces;

      procedure check (position : string_crate.Cursor) is
      begin
         crate.Update_Element (position, swap_braces'Access);
      end check;
   begin
      crate.Iterate (check'Access);
   end apply_cbc_string_crate;


   --------------------------------------------------------------------------------------------
   --  argument_present
   --------------------------------------------------------------------------------------------
   function argument_present (specs : Portspecs; module, argument : String) return Boolean
   is
      procedure scan (position : string_crate.Cursor);

      found : Boolean := False;

      procedure scan (position : string_crate.Cursor)
      is
         value_text : HT.Text renames string_crate.Element (position);
         value      : String := HT.USS (value_text);
      begin
         if not found and then HT.count_char (value, LAT.Colon) = 1 then
            declare
               modulestr : String := HT.part_1 (value, ":");
            begin
               if modulestr = module then
                  declare
                     argumentstr : String := HT.part_2 (value, ":");
                     num_commas  : Natural := HT.count_char (argumentstr, LAT.Comma);
                  begin
                     if num_commas = 0 then
                        found := (argument = argumentstr);
                     else
                        for x in 1 .. num_commas + 1 loop
                           if argument = HT.specific_field (argumentstr, x, ",") then
                              found := True;
                              exit;
                           end if;
                        end loop;
                     end if;
                  end;
               end if;
            end;
         end if;
      end scan;
   begin
      specs.uses.Iterate (scan'Access);
      return found;
   end argument_present;


   --------------------------------------------------------------------------------------------
   --  no_arguments_present
   --------------------------------------------------------------------------------------------
   function no_arguments_present (specs : Portspecs; module : String) return Boolean
   is
      procedure scan (position : string_crate.Cursor);

      found : Boolean := False;

      procedure scan (position : string_crate.Cursor)
      is
         value_text : HT.Text renames string_crate.Element (position);
         value      : String := HT.USS (value_text);
      begin
         if not found and then HT.count_char (value, LAT.Colon) = 1 then
            declare
               modulestr : String := HT.part_1 (value, ":");
            begin
               if modulestr = module then
                  found := True;
               end if;
            end;
         end if;
      end scan;
   begin
      specs.uses.Iterate (scan'Access);
      return not found;
   end no_arguments_present;


   --------------------------------------------------------------------------------------------
   --  get_argument
   --------------------------------------------------------------------------------------------
   function get_argument (specs : Portspecs; module : String) return String
   is
      result : HT.Text;
      found : Boolean := False;

      procedure scan (position : string_crate.Cursor)
      is
         value_text : HT.Text renames string_crate.Element (position);
         value      : String := HT.USS (value_text);
      begin
         if not found and then HT.count_char (value, LAT.Colon) = 1 then
            declare
               modulestr : String := HT.part_1 (value, ":");
            begin
               if modulestr = module then
                  found := True;
                  result := HT.SUS (HT.part_2 (value, ":"));
               end if;
            end;
         end if;
      end scan;
   begin
      specs.uses.Iterate (scan'Access);
      return HT.USS (result);
   end get_argument;


   --------------------------------------------------------------------------------------------
   --  shift_extra_patches
   --------------------------------------------------------------------------------------------
   procedure shift_extra_patches (specs : Portspecs; extract_dir   : String)
   is
      num_extra_patch : Natural := specs.get_list_length (sp_extra_patches);
      patchdir : constant String := extract_dir & "/patches";
   begin
      if num_extra_patch > 0 then
         if not DIR.Exists (patchdir) then
            DIR.Create_Directory (patchdir);
         end if;
      end if;
      for item in 1 .. num_extra_patch loop
         declare
            patch  : String := specs.get_list_item (sp_extra_patches, item);
            xp_loc : String := extract_dir & "/files/" & patch;
         begin
            if DIR.Exists (xp_loc) then
               DIR.Rename (Old_Name => xp_loc,
                           New_Name => patchdir & "/patch-zzz-" & patch);
            end if;
         end;
      end loop;
   end shift_extra_patches;

   --------------------------------------------------------------------------------------------
   --  add_build_depends
   --------------------------------------------------------------------------------------------
   procedure add_build_depends (specs : in out Portspecs; dependency : String)
   is
      dependency_text : HT.Text := HT.SUS (dependency);
   begin
      if not specs.build_deps.Contains (dependency_text) then
         specs.build_deps.Append (dependency_text);
      end if;
   end add_build_depends;


   --------------------------------------------------------------------------------------------
   --  add_buildrun_depends
   --------------------------------------------------------------------------------------------
   procedure add_buildrun_depends (specs : in out Portspecs; dependency : String)
   is
      dependency_text : HT.Text := HT.SUS (dependency);
   begin
      if not specs.buildrun_deps.Contains (dependency_text) then
         specs.buildrun_deps.Append (dependency_text);
      end if;
   end add_buildrun_depends;


   --------------------------------------------------------------------------------------------
   --  add_run_depends
   --------------------------------------------------------------------------------------------
   procedure add_run_depends (specs : in out Portspecs; dependency : String)
   is
      dependency_text : HT.Text := HT.SUS (dependency);
   begin
      if not specs.run_deps.Contains (dependency_text) then
         specs.run_deps.Append (dependency_text);
      end if;
   end add_run_depends;


   --------------------------------------------------------------------------------------------
   --  add_exrun_depends
   --------------------------------------------------------------------------------------------
   procedure add_exrun_depends (specs : in out Portspecs; dependency, subpackage : String)
   is
      procedure grow (Key : HT.Text; Element : in out group_list);

      dependency_text : HT.Text := HT.SUS (dependency);
      group           : HT.Text := HT.SUS (subpackage);

      procedure grow (Key : HT.Text; Element : in out group_list) is
      begin
         Element.list.Append (dependency_text);
      end grow;
   begin
      if not specs.extra_rundeps.Contains (group) then
         specs.establish_group (sp_exrun, subpackage);
      end if;
      if not specs.extra_rundeps.Element (group).list.Contains (dependency_text) then
         specs.extra_rundeps.Update_Element (Position => specs.extra_rundeps.Find (group),
                                             Process  => grow'Access);
      end if;
   end add_exrun_depends;


   --------------------------------------------------------------------------------------------
   --  convert_exrun_versions
   --------------------------------------------------------------------------------------------
   procedure convert_exrun_versions (specs : in out Portspecs; variant : String)
   is

      newmap : list_crate.Map;
      xspkg  : HT.Text;
      virgin : Boolean;

      procedure convert_run_depends (position : string_crate.Cursor)
      is
         exrun_value : constant String := HT.USS (string_crate.Element (position));
         subpkg      : constant String := HT.USS (xspkg);
         excomp      : constant Exrun_Components := specs.parse_exrun (subpkg, exrun_value);

         function XY (xdoty : String) return String is
         begin
            if xdoty'Length < 3 then
               return "X";
            end if;
            return xdoty (xdoty'First) & xdoty (xdoty'First + 2);
         end XY;

         procedure grow_triplet (Key : HT.Text; Element : in out group_list) is
         begin
            Element.list.Append (excomp.run_dependency);
         end grow_triplet;

         procedure grow_singleton (Key : HT.Text; Element : in out group_list)
         is
            exrundep : constant String := HT.USS (excomp.run_dependency);
            triplet : HT.Text := HT.SUS ("error:" & exrundep & ":singleton");
         begin
            if specs.subpackage_exists (exrundep) then
               --  ex. EXRUN[tools]= primary
               --  The namebase and the variant must be added dynamically.
               triplet := HT.SUS (specs.get_namebase & ":" & exrundep & ":" & variant);
            elsif exrundep = "ssl" then
               declare
                  setting : String := HT.USS (Parameters.configuration.def_ssl);
               begin
                  if setting = ports_default then
                     triplet := HT.SUS (primary_triplet ("libressl"));
                  else
                     triplet := HT.SUS (primary_triplet (setting));
                  end if;
               end;
            elsif exrundep = "python" then
               if specs.buildrun_deps.Contains (HT.SUS (primary_triplet (PYTHONALT))) then
                  triplet := HT.SUS (primary_triplet (PYTHONALT));
               else
                  triplet := HT.SUS (primary_triplet (PYTHONPRI));
               end if;
            elsif exrundep = "tcl" then
               if specs.buildrun_deps.Contains (HT.SUS (TCL90)) then
                  triplet := HT.SUS (TCL90);
               else
                  triplet := HT.SUS (TCL86);
               end if;
            elsif exrundep = "perl" then
               declare
                  setting : String := HT.USS (Parameters.configuration.def_perl);
               begin
                  if setting = ports_default then
                     triplet := HT.SUS (primary_triplet ("perl-" & default_perl));
                  else
                     triplet := HT.SUS (primary_triplet ("perl-" & setting));
                  end if;
               end;
            elsif exrundep = "ruby" then
               declare
                  setting : String := HT.USS (Parameters.configuration.def_ruby);
               begin
                  if setting = ports_default then
                     triplet := HT.SUS (primary_triplet ("ruby" & XY (default_ruby)));
                  else
                     triplet := HT.SUS (primary_triplet ("ruby" & XY (setting)));
                  end if;
               end;
            elsif exrundep = "mysql" then
               --  only mysql:client is supported by EXRUN
               triplet := HT.SUS (generic_triplet (determine_mysql_namebase, "client"));
            elsif exrundep = "pgsql" then
               --  only pgsql:client is supported by EXRUN
               triplet := HT.SUS (generic_triplet (determine_pgsql_namebase, "client"));
            end if;
            Element.list.Append (triplet);
         end grow_singleton;

      begin
         if excomp.limited_opsys then
            case excomp.run_opsys is
               when platform_type => null;
               when others => return;
            end case;
         end if;
         if excomp.limited_variant then
            if not HT.equivalent (excomp.run_variant, variant) then
               return;
            end if;
         end if;
         if virgin then
            declare
               initial_rec : group_list;
            begin
               initial_rec.group := xspkg;
               newmap.Insert (xspkg, initial_rec);
            end;
            virgin := False;
         end if;
         if excomp.spkg_shorthand then
            newmap.Update_Element (newmap.Find (xspkg), grow_singleton'Access);
         else
            newmap.Update_Element (newmap.Find (xspkg), grow_triplet'Access);
         end if;
      end convert_run_depends;

      procedure scan_subpackage (spkg_position : list_crate.Cursor) is
      begin
         virgin := True;
         xspkg := list_crate.Element (spkg_position).group;
         list_crate.Element (spkg_position).list.Iterate (convert_run_depends'Access);
      end scan_subpackage;
   begin
      specs.extra_rundeps.Iterate (scan_subpackage'Access);
      specs.extra_rundeps.Clear;
      specs.extra_rundeps := newmap;
   end convert_exrun_versions;


   --------------------------------------------------------------------------------------------
   --  apply_extraction_deps
   --------------------------------------------------------------------------------------------
   procedure apply_extraction_deps (specs : in out Portspecs) is
   begin
      --  unzip is already in base
      if not specs.extract_7z.Is_Empty then
         add_build_depends (specs, primary_triplet ("p7zip"));
      end if;
      --  TODO: placeholder for LHA
   end apply_extraction_deps;


   --------------------------------------------------------------------------------------------
   --  transform_defaults
   --------------------------------------------------------------------------------------------
   function transform_defaults (dep, pyx, plx, lux, rbx : String) return String
   is
      function name_subpackage return String;

      trailer : constant String := HT.specific_field (dep, 3, ":");

      function name_subpackage return String is
      begin
         return HT.specific_field (dep, 1, ":") & ":" & HT.specific_field (dep, 2, ":") & ":";
      end name_subpackage;
   begin
      if trailer = "python_default" then
         declare
            setting : String := HT.USS (Parameters.configuration.def_python3);
         begin
            if setting = ports_default or else setting = default_python3 then
               return name_subpackage & PY313;
            else
               return name_subpackage & PY314;
            end if;
         end;
      elsif trailer = "perl_default" then
         declare
            setting : String := HT.USS (Parameters.configuration.def_perl);
         begin
            if setting = ports_default or else setting = default_perl then
               return name_subpackage & "540";
            else
               return name_subpackage & "542";
            end if;
         end;
      elsif trailer = "lua_default" then
         declare
            setting : String := HT.USS (Parameters.configuration.def_lua);
         begin
            if setting = "5.2" then
               return name_subpackage & "lua52";
            elsif setting = "5.3" then
               return name_subpackage & "lua53";
            else
               --  ports_default or default_lua ("5.4")
               return name_subpackage & "lua54";
            end if;
         end;
      elsif trailer = "ruby_default" then
         declare
            setting : String := HT.USS (Parameters.configuration.def_ruby);
         begin
            if setting = "3.4" then
               return name_subpackage & "v34";
            elsif setting = "3.2" then
               return name_subpackage & "v32";
            else
               --  ports_default or default_ruby ("3.3")
               return name_subpackage & "v33";
            end if;
         end;
      elsif trailer = "python_used" then
         return name_subpackage & pyx;
      elsif trailer = "perl_used" then
         return name_subpackage & plx;
      elsif trailer = "lua_used" then
         return name_subpackage & lux;
      elsif trailer = "ruby_used" then
         return name_subpackage & rbx;
      else
         return dep;
      end if;
   end transform_defaults;


   --------------------------------------------------------------------------------------------
   --  apply_default_version_transformations
   --------------------------------------------------------------------------------------------
   procedure apply_default_version_transformations (specs : in out Portspecs)
   is
      procedure check_build    (position : string_crate.Cursor);
      procedure check_buildrun (position : string_crate.Cursor);
      procedure check_run      (position : string_crate.Cursor);
      procedure check_extradep (position : list_crate.Cursor);
      procedure extradep2      (Key : HT.Text; Element : in out group_list);
      procedure alter (Element : in out HT.Text);

      transformed_dep : HT.Text;

      pyx : constant String := HT.USS (specs.used_python);
      plx : constant String := HT.USS (specs.used_perl);
      lux : constant String := HT.USS (specs.used_lua);
      rbx : constant String := HT.USS (specs.used_ruby);

      procedure alter (Element : in out HT.Text) is
      begin
         Element := transformed_dep;
      end alter;

      procedure check_build (position : string_crate.Cursor)
      is
         dep  : String := HT.USS (string_crate.Element (position));
         xdep : String := transform_defaults (dep, pyx, plx, lux, rbx);
      begin
         if xdep /= dep then
            transformed_dep := HT.SUS (xdep);
            specs.build_deps.Update_Element (position, alter'Access);
         end if;
      end check_build;

      procedure check_buildrun (position : string_crate.Cursor)
      is
         dep  : String := HT.USS (string_crate.Element (position));
         xdep : String := transform_defaults (dep, pyx, plx, lux, rbx);
      begin
         if xdep /= dep then
            transformed_dep := HT.SUS (xdep);
            specs.buildrun_deps.Update_Element (position, alter'Access);
         end if;
      end check_buildrun;

      procedure check_run (position : string_crate.Cursor)
      is
         dep  : String := HT.USS (string_crate.Element (position));
         xdep : String := transform_defaults (dep, pyx, plx, lux, rbx);
      begin
         if xdep /= dep then
            transformed_dep := HT.SUS (xdep);
            specs.run_deps.Update_Element (position, alter'Access);
         end if;
      end check_run;

      procedure check_extradep (position : list_crate.Cursor) is
      begin
         specs.extra_rundeps.Update_Element (position, extradep2'Access);
      end check_extradep;

      procedure extradep2 (Key : HT.Text; Element : in out group_list)
      is
         procedure check_payload (position2 : string_crate.Cursor);
            procedure check_payload (position2 : string_crate.Cursor)
            is
               dep : constant String := HT.USS (string_crate.Element (position2));
               xdep : String := transform_defaults (dep, pyx, plx, lux, rbx);
            begin
               if xdep /= dep then
                  transformed_dep := HT.SUS (xdep);
                  Element.list.Update_Element (position2, alter'Access);
               end if;
            end check_payload;
      begin
         Element.list.Iterate (check_payload'Access);
      end extradep2;
   begin
      specs.build_deps.Iterate (check_build'Access);
      specs.buildrun_deps.Iterate (check_buildrun'Access);
      specs.run_deps.Iterate (check_run'Access);
      specs.extra_rundeps.Iterate (check_extradep'Access);
   end apply_default_version_transformations;


   --------------------------------------------------------------------------------------------
   --  apply_gnome_components_dependencies
   --------------------------------------------------------------------------------------------
   procedure apply_gnome_components_dependencies (specs : in out Portspecs)
   is
      --  May require several iterations
      --  First pass: set top-level requirements
      --  repeat subsequent passes on "components" until no changes made
      --  Use "component" array to define gnome component imports

      procedure initial_setup (position : string_crate. Cursor);
      procedure implicate (comp : gnome_type);
      procedure import (comp : gnome_type);
      procedure implies (comp : gnome_type);

      defpy     : constant String := "v" & HT.part_2 (default_python3, ".");
      uses_py   : HT.Text := HT.SUS ("python");
      pybuild   : HT.Text := HT.SUS ("python:build");
      component : array (gnome_type) of Boolean := (others => False);
      new_data  : Boolean;

      procedure implies (comp : gnome_type) is
      begin
         if not component (comp) then
            component (comp) := True;
            new_data := True;
         end if;
      end implies;

      procedure initial_setup (position : string_crate. Cursor)
      is
         component_text : HT.Text renames string_crate.Element (position);
         comp : gnome_type := determine_gnome_component (HT.USS (component_text));
      begin
         component (comp) := True;
      end initial_setup;

      procedure implicate (comp : gnome_type) is
      begin
         case comp is
            when atk =>
               implies (glib);
            when dconf =>
               implies (glib);
            when gdkpixbuf =>
               implies (glib);
            when glibmm =>
               implies (glib);
            when gtk2 | gtk3 | gtk4 =>
               implies (atk);
               implies (glib);
               implies (pango);
            when gtkmm30 | vte =>
               implies (gtk3);
               implies (atk);
               implies (glib);
               implies (pango);
            when gtkmm40 =>
               implies (gtk4);
               implies (atk);
               implies (glib);
               implies (pango);
            when gtksourceview3 =>
               implies (gtk3);
               implies (atk);
               implies (glib);
               implies (pango);
               implies (libxml2);
            when introspection =>
               implies (glib);
            when libglade =>
               implies (libxml2);
               implies (gtk2);
               implies (atk);
               implies (glib);
               implies (pango);
            when libgsf =>
               implies (glib);
               implies (libxml2);
            when libidl =>
               implies (glib);
            when librsvg =>
               implies (libgsf);
               implies (gdkpixbuf);
               implies (pango);
            when libxslt =>
               implies (libxml2);
            when orbit2 =>
               implies (libidl);
            when pygobject =>
               implies (glib);
            when others => null;
         end case;
      end implicate;

      procedure import (comp : gnome_type) is
      begin
         case comp is
            when invalid_component => null;  --  should be impossible
            when atk =>
               add_primdev_submodule (specs, "at-spi2-core");
            when atkmm =>
               add_primdev_submodule (specs, "atkmm");
            when atkmm16 =>
               add_primdev_submodule (specs, "atkmm16");
            when cairo =>
               add_primdev_submodule (specs, "cairo");
            when cairomm =>
               add_primdev_submodule (specs, "cairomm");
            when cairomm10 =>
               add_primdev_submodule (specs, "cairomm10");
            when dconf =>
               add_primdev_submodule (specs, "dconf");
            when gconf =>
               add_primdev_submodule (specs, "gconf");
            when gdkpixbuf =>
               add_primdev_submodule (specs, "gdk-pixbuf");
            when glib =>
               add_build_depends (specs, GLIBDEV);
               add_build_depends (specs, GTDEV);
               add_build_depends (specs, GTBTOOLS);
               add_build_depends (specs, GTSOLINX);
               add_buildrun_depends (specs, GNOMELIB);
               add_buildrun_depends (specs, GTLIB);
               add_buildrun_depends (specs, GTTOOLS);
               if not specs.uses_base.Contains (uses_py) then
                  specs.uses_base.Append (uses_py);
                  if not specs.uses.Contains (pybuild) then
                     specs.uses.Append (pybuild);
                  end if;
               end if;
            when glibmm =>
               add_primdev_submodule (specs, "glibmm");
            when glibmm24 =>
               add_primdev_submodule (specs, "glibmm24");
            when gtk2 =>
               add_primdev_submodule (specs, "gtk2");
            when gtk3 =>
               add_primdev_submodule (specs, "gtk3");
               add_run_depends (specs, single_triplet ("adwaita-icon-theme"));
            when gtk4 =>
               add_primdev_submodule (specs, "gtk4");
            when gtkmm30 =>
               add_primdev_submodule (specs, "gtkmm30");
            when gtkmm40 =>
               add_primdev_submodule (specs, "gtkmm40");
            when gtksourceview3 =>
               add_primdev_submodule (specs, "gtksourceview3");
            when gtksourceview4 =>
               add_primdev_submodule (specs, "gtksourceview4");
            when gtksourceview5 =>
               add_primdev_submodule (specs, "gtksourceview5");
            when intltool =>
               add_build_depends    (specs, primary_triplet ("intltool"));
            when introspection =>
               add_build_depends    (specs, primary_triplet ("gobject-introspection"));
               add_build_depends    (specs, dev_triplet ("gobject-introspection"));
               specs.make_env.Append (HT.SUS ("GI_SCANNER_DISABLE_CACHE=1"));
               specs.make_env.Append (HT.SUS ("XDG_CACHE_HOME=${WRKDIR}"));
            when libcroco =>
               add_primdev_submodule (specs, "libcroco");
            when libglade =>
               add_primdev_submodule (specs, "libglade");
            when libgsf =>
               add_primdev_submodule (specs, "libgsf");
            when libidl =>
               add_primdev_submodule (specs, "libIDL");
            when librsvg =>
               add_primdev_submodule (specs, "librsvg");
            when libsigcxx2 =>
               add_primdev_submodule (specs, "libsigcxx2");
            when libsigcxx3 =>
               add_primdev_submodule (specs, "libsigcxx");
            when libxmlxx5 =>
               add_primdev_submodule (specs, "libxmlxx");
            when libxml2 =>
               add_primdev_submodule (specs, "libxml2");
            when libxslt =>
               add_primdev_submodule (specs, "libxslt");
            when orbit2 =>
               add_primdev_submodule (specs, "ORBit2");
            when pango =>
               add_primdev_submodule (specs, "pango");
            when pangomm =>
               add_primdev_submodule (specs, "pangomm");
            when pangomm14 =>
               add_primdev_submodule (specs, "pangomm14");
            when pygobject =>
               add_build_depends    (specs, "python-PyGObject:dev:" & defpy);
               add_buildrun_depends (specs, "python-PyGObject:primary:" & defpy);
            when vte =>
               add_primdev_submodule (specs, "vte");
         end case;
      end import;
   begin
      specs.gnome_comps.Iterate (initial_setup'Access);
      loop
         new_data := False;
         for x in gnome_type'Range loop
            if component (x) then
               implicate (x);
            end if;
         end loop;
         exit when not new_data;
      end loop;
      for x in gnome_type'Range loop
         if component (x) then
            import (x);
         end if;
      end loop;
   end apply_gnome_components_dependencies;


   --------------------------------------------------------------------------------------------
   --  apply_sdl_components_dependencies
   --------------------------------------------------------------------------------------------
   procedure apply_sdl_components_dependencies  (specs : in out Portspecs)
   is
      procedure import (position : string_crate.Cursor);

      menv1 : HT.Text := HT.SUS ("SDL_CONFIG=""${LOCALBASE}/bin/sdl-config""");
      menv2 : HT.Text := HT.SUS ("SDL_CONFIG=""${LOCALBASE}/bin/sdl2-config""");

      procedure import (position : string_crate.Cursor)
      is
         component_text : HT.Text renames string_crate.Element (position);
         comp : sdl_type := determine_sdl_component (HT.USS (component_text));
      begin
         case comp is
            when invalid_component => null;  --  should be impossible
            when sdl1 => null;
            when sdl2 => null;
            when sound1 => add_primdev_submodule (specs, "sdl1_sound");
            when sound2 => add_primdev_submodule (specs, "sdl2_sound");
            when image1 => add_primdev_submodule (specs, "sdl1_image");
            when image2 => add_primdev_submodule (specs, "sdl2_image");
            when mixer1 => add_primdev_submodule (specs, "sdl1_mixer");
            when mixer2 => add_primdev_submodule (specs, "sdl2_mixer");
            when net1   => add_primdev_submodule (specs, "sdl1_net");
            when net2   => add_primdev_submodule (specs, "sdl2_net");
            when ttf1   => add_primdev_submodule (specs, "sdl1_ttf");
            when ttf2   => add_primdev_submodule (specs, "sdl2_ttf");
         end case;
         case comp is
            when sdl1 | sound1 | image1 | mixer1 | net1 | ttf1 =>
               add_primdev_submodule (specs, "sdl1");
               specs.make_env.Append (menv1);
               specs.config_env.Append (menv1);
            when sdl2 | sound2 | image2 | mixer2 | net2 | ttf2 =>
               add_primdev_submodule (specs, "sdl2");
               specs.make_env.Append (menv2);
               specs.config_env.Append (menv2);
            when invalid_component => null;
         end case;
      end import;
   begin
      specs.sdl_comps.Iterate (import'Access);
   end apply_sdl_components_dependencies;


   --------------------------------------------------------------------------------------------
   --  apply_xorg_components_dependencies
   --------------------------------------------------------------------------------------------
   procedure apply_xorg_components_dependencies (specs : in out Portspecs)
   is
      procedure import (position : string_crate.Cursor);

      --  All xorg components have this format : xorg-{COMPONENT}:primary:std
      --      exceptions: xorg-xbitmaps
      --  All xorg components ending in "proto" are build-only depends (single)
      --      The rest are considered libraries (buildrun type)
      --  All libraries depend on pkgconfig and xorg-macros

      uses_xorg : Boolean := False;

      procedure import (position : string_crate.Cursor)
      is
         component_text : HT.Text renames string_crate.Element (position);
         component  : constant String := HT.USS (component_text);
      begin
         if HT.trails (component, "proto") or else
           component = "xbitmaps"
         then
            add_build_depends (specs, single_triplet ("xorg-" & component));
         else
            add_primdev_submodule (specs, "xorg-" & component);
         end if;
         uses_xorg := True;
      end import;
   begin
      specs.xorg_comps.Iterate (import'Access);
      if uses_xorg and then not HT.trails (specs.get_namebase, "proto") then
         add_build_depends (specs, single_triplet ("xorg-macros"));
         add_build_depends (specs, primary_triplet ("pkgconf"));
      end if;
   end apply_xorg_components_dependencies;


   --------------------------------------------------------------------------------------------
   --  apply_php_extension_dependencies
   --------------------------------------------------------------------------------------------
   procedure apply_php_extension_dependencies (specs : in out Portspecs)
   is
      procedure import (position : string_crate.Cursor);

      php_module  : constant String := "php";
      hit_build   : Boolean := False;
      hit_ext     : Boolean := False;
      --  This defver works until PHP 10 is released
      defver : String (1 .. 2) := default_php (default_php'First) & default_php (default_php'Last);
      flavor : String := "php" & defver;

      procedure import (position : string_crate.Cursor)
      is
         extension_text : HT.Text renames string_crate.Element (position);
         extension      : constant String := HT.USS (extension_text);
         dependency     : constant String := single_triplet (flavor & "-" & extension);
      begin
         if hit_build or else hit_ext then
            add_buildrun_depends (specs, dependency);
         else
            add_run_depends (specs, dependency);
         end if;
      end import;
   begin
      if not no_arguments_present (specs, php_module) then
         if argument_present (specs, php_module, "84") then
            flavor := "php84";
         elsif argument_present (specs, php_module, "83") then
            flavor := "php83";
         elsif argument_present (specs, php_module, "85") then
            flavor := "php85";
         end if;
      end if;
      hit_build := argument_present (specs, php_module, BUILD);
      hit_ext   := argument_present (specs, php_module, "ext");
      specs.php_extensions.Iterate (import'Access);
   end apply_php_extension_dependencies;


   --------------------------------------------------------------------------------------------
   --  determine_mysql_package
   --------------------------------------------------------------------------------------------
   function determine_mysql_namebase return String
   is
      setting : constant String := HT.USS (Parameters.configuration.def_mysql_group);
   begin
      if setting = "innovation" then
         return "mysql";
      elsif setting = "oracle-8.4" then
         return "mysql84";
      elsif setting = "mariadb-10.6" then
         return "mariadb106";
      elsif setting = "mariadb-10.11" then
         return "mariadb1011";
      elsif setting = "mariadb-11.4" then
         return "mariadb114";
      elsif setting = "mariadb-11.8" then
         return "mariadb118";
      elsif setting = "mariadb-latest" then
         return "mariadb";
      else
         --  case: setting = ports_default
         --  case: setting = default_mysql
         --  case: setting = invalid value
         return "mysql80";
      end if;
   end determine_mysql_namebase;


   --------------------------------------------------------------------------------------------
   --  determine_pgsql_namebase
   --------------------------------------------------------------------------------------------
   function determine_pgsql_namebase return String
   is
      setting : constant String := HT.USS (Parameters.configuration.def_postgresql);
   begin
      if setting = "14" then
         return "postgresql14";
      elsif setting = "15" then
         return "postgresql15";
      elsif setting = "16" then
         return "postgresql16";
      elsif setting = "18" then
         return "postgresql18";
      else
         --  case: setting = ports_default
         --  case: setting = default_pgsql
         --  case: setting = invalid value
         return "postgresql17";
      end if;
   end determine_pgsql_namebase;


end Port_Specification.Transform;
