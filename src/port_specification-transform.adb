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
                     when run_depends_on      => specs.run_deps.Append (item);
                     when sub_files_on        => specs.sub_files.Append (item);
                     when sub_list_on         => specs.sub_list.Append (item);
                     when qmake_args_on       => specs.qmake_args.Append (item);
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
                        special := HT.SUS ("-D" & itemstr & ":BOOL-true");
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
                     when build_depends_off    => specs.build_deps.Append (item);
                     when buildrun_depends_off => specs.buildrun_deps.Append (item);
                     when run_depends_off      => specs.run_deps.Append (item);
                     when cmake_args_off       => specs.cmake_args.Append (item);
                     when cflags_off           => specs.cflags.Append (item);
                     when configure_args_off   => specs.config_args.Append (item);
                     when df_index_off         => specs.df_index.Append (item);
                     when gnome_comp_off       => specs.gnome_comps.Append (item);
                     when info_off             => specs.info.Append (item);
                     when qmake_args_off       => specs.qmake_args.Append (item);
                     when makefile_off         => specs.mk_verbatim.Append (item);
                     when make_args_off        => specs.make_args.Append (item);
                     when sub_files_off        => specs.sub_files.Append (item);
                     when sub_list_off         => specs.sub_list.Append (item);
                     when xorg_comp_off        => specs.xorg_comps.Append (item);
                     when cmake_bool_f_both =>
                        special := HT.SUS ("-D" & itemstr & ":BOOL-true");
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
            augment (cflags_off,           rec.CFLAGS_OFF);
            augment (cmake_args_off,       rec.CMAKE_ARGS_OFF);
            augment (configure_args_off,   rec.CONFIGURE_ARGS_OFF);
            augment (df_index_off,         rec.DF_INDEX_OFF);
            augment (gnome_comp_off,       rec.GNOME_COMPONENTS_OFF);
            augment (info_off,             rec.INFO_OFF);
            augment (makefile_off,         rec.MAKEFILE_OFF);
            augment (make_args_off,        rec.MAKE_ARGS_OFF);
            augment (qmake_args_off,       rec.QMAKE_ARGS_OFF);
            augment (run_depends_off,      rec.RUN_DEPENDS_OFF);
            augment (sub_files_off,        rec.SUB_FILES_OFF);
            augment (sub_list_off,         rec.SUB_LIST_OFF);
            augment (uses_off,             rec.USES_OFF);
            augment (xorg_comp_off,        rec.XORG_COMPONENTS_OFF);
         end if;
         augment (cmake_bool_f_both,      rec.CMAKE_BOOL_F_BOTH);
         augment (cmake_bool_t_both,      rec.CMAKE_BOOL_T_BOTH);
         augment (configure_enable_both,  rec.CONFIGURE_ENABLE_BOTH);
         augment (configure_with_both,    rec.CONFIGURE_WITH_BOTH);

         grow_plist_sub (HT.USS (rec.option_name), rec.currently_set_ON);

      end copy_option_over;

      skip_compiler_packages : constant Boolean := Unix.env_variable_defined ("SKIPCCRUN");
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
      apply_info_presence (specs);
      apply_gettext_runtime_module (specs);
      apply_gettext_tools_module (specs);
      apply_gnome_icons_module (specs);
      apply_autoconf_module (specs);
      apply_execinfo_module (specs);
      apply_display_module (specs);
      apply_sqlite_module (specs);
      apply_cmake_module (specs);
      apply_imake_module (specs);
      apply_perl_module (specs);
      apply_bdb_module (specs);
      apply_ssl_module (specs);
      apply_bison_module (specs);
      apply_mysql_module (specs);
      apply_pgsql_module (specs);
      apply_meson_module (specs);
      apply_ninja_module (specs);
      apply_fonts_module (specs);
      apply_python_module (specs);
      apply_ruby_module (specs);
      apply_zlib_module (specs);
      apply_mesa_module (specs);
      apply_jpeg_module (specs);
      apply_lua_module (specs);
      apply_tcl_module (specs);
      apply_php_module (specs);
      apply_png_module (specs);
      apply_gem_module (specs);
      apply_ccache (specs);
      apply_schemas_module (specs);
      apply_firebird_module (specs);
      apply_desktop_utils_module (specs);
      apply_gnome_components_dependencies (specs);
      apply_sdl_components_dependencies (specs);
      apply_xorg_components_dependencies (specs);
      apply_php_extension_dependencies (specs);
      if not skip_compiler_packages then
         apply_gcc_run_module (specs, variant, "ada", "ada_run");
         apply_gcc_run_module (specs, variant, "c++", "cxx_run");
         apply_gcc_run_module (specs, variant, "fortran", "fortran_run");
         apply_gcc_run_module (specs, variant, "cclibs", "libs");
         apply_gcc_run_module (specs, variant, "compiler", "complete");
         if platform_type = sunos then
            --  Solaris 10 doesn't use dl_iterate_phdr, so many packages have executables that
            --  requires libgcc_s.so.  Rather than specify potentially hundreds of C_USES
            --  keywords, just make gcc7:libs:standard a run depends of every package (including
            --  gcc6, gcc7, gcc8 and later).
            add_exrun_cclibs (specs, variant);
         end if;
      end if;
      apply_curly_bracket_conversions (specs);
      apply_default_version_transformations (specs);
      convert_exrun_versions (specs);
   end apply_directives;


   --------------------------------------------------------------------------------------------
   --  set_option_defaults
   --------------------------------------------------------------------------------------------
   procedure set_option_defaults
     (specs         : in out Portspecs;
      variant       : String;
      opsys         : supported_opsys;
      arch_standard : supported_arch;
      osrelease     : String)
   is
      procedure vopt_set   (position : string_crate.Cursor);
      procedure varstd_set (position : string_crate.Cursor);
      procedure opsys_set  (position : string_crate.Cursor);
      procedure set_on     (Key : HT.Text; Element : in out Option_Helper);

      variant_text : HT.Text := HT.SUS (variant);
      all_text     : HT.Text := HT.SUS (options_all);
      arch_text    : HT.Text := HT.SUS (UTL.cpu_arch (arch_standard));
      opsys_text   : HT.Text := HT.SUS (UTL.lower_opsys (opsys));

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
                  opt_name     : HT.Text := HT.SUS (HT.part_1 (option_name, "/"));
                  spec_version : String  := HT.part_2 (option_name, "/");
               begin
                  if GTE (gen_release => osrelease, spec_release => spec_version) then
                     option_text := opt_name;
                  end if;
               end;
            else
               declare
                  opt_name     : HT.Text := HT.SUS (HT.part_1 (option_name, "/"));
                  temp_P2      : String  := HT.part_2 (option_name, "/");
                  spec_version : String  := HT.part_1 (temp_P2);
                  arch_str     : String  := HT.part_2 (temp_P2);
                  meets_ver    : Boolean := spec_version = "" or else
                                            GTE (osrelease, spec_version);
                  meets_arch   : Boolean := HT.contains (arch_str, UTL.cpu_arch (arch_standard));
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
      index     : HT.Text := HT.SUS (broken_all);

      procedure check (position : list_crate.Cursor)
      is
         procedure check_list (position : string_crate.Cursor);

         broken_Key : String  := HT.USS (list_crate.Element (position).group);


         procedure check_list (position : string_crate.Cursor)
         is
            procedure grow (Key : HT.Text; Element : in out group_list);

            reason : String  := HT.USS (string_crate.Element (position));
            used   : Boolean := False;
            split  : Boolean := True;

            procedure grow (Key : HT.Text; Element : in out group_list) is
            begin
               if split then
                  Element.list.Append (HT.SUS (HT.part_2 (reason, ": ")));
               else
                  Element.list.Append (string_crate.Element (position));
               end if;
            end grow;
         begin
            if broken_Key = UTL.cpu_arch (arch_standard) then
               used  := True;
               split := False;
            elsif broken_Key = UTL.lower_opsys (opsys) then
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

            if used then
               if not specs.broken.Contains (index) then
                  specs.establish_group (sp_broken, broken_all);
               end if;
               specs.broken.Update_Element (Position => specs.broken.Find (index),
                                            Process  => grow'Access);
            end if;
         end check_list;

      begin
         if not specs.skip_opsys_dep then
            list_crate.Element (position).list.Iterate (Process => check_list'Access);
         end if;
      end check;

      procedure check_ignore
      is
         procedure grow (Key : HT.Text; Element : in out group_list);
         procedure append_ignore;

         reason : HT.Text;

         procedure grow (Key : HT.Text; Element : in out group_list) is
         begin
               Element.list.Append (reason);
         end grow;

         procedure append_ignore is
         begin
            --  Call after "reason" is set
            if not specs.broken.Contains (index) then
               specs.establish_group (sp_broken, broken_all);
            end if;
            specs.broken.Update_Element (Position => specs.broken.Find (index),
                                         Process  => grow'Access);
         end append_ignore;

         LIST_SSL_FAILURE   : constant String := "Does not build with SSL default '";
         LIST_MYSQL_FAILURE : constant String := "Does not build with MySQL default '";
         LIST_PGSQL_FAILURE : constant String := "Does not build with PGSQL default '";
      begin
         if not specs.skip_opsys_dep then
            if specs.exc_opsys.Contains (HT.SUS (UTL.lower_opsys (opsys))) or else
              (not specs.inc_opsys.Is_Empty and then
                 not specs.inc_opsys.Contains (HT.SUS (UTL.lower_opsys (opsys))))
            then
               reason := HT.SUS ("Specification excludes " & UTL.mixed_opsys (opsys) & " OS");
               append_ignore;
            end if;
            if specs.exc_arch.Contains (HT.SUS (UTL.cpu_arch (arch_standard))) then
               reason := HT.SUS ("Specification excludes " & UTL.cpu_arch (arch_standard) &
                                   " architecture");
               append_ignore;
            end if;
         end if;

         --  Handle BROKEN_SSL directive
         if HT.equivalent (Parameters.configuration.def_ssl, ports_default) then
            if specs.broken_ssl.Contains (HT.SUS (default_ssl)) then
               reason := HT.SUS (LIST_SSL_FAILURE & default_ssl & "'");
               append_ignore;
            end if;
         else
            if specs.broken_ssl.Contains (Parameters.configuration.def_ssl) then
               reason := HT.SUS (LIST_SSL_FAILURE &
                                   HT.USS (Parameters.configuration.def_ssl) & "'");
               append_ignore;
            end if;
         end if;

         --  Handle BROKEN_MYSQL directive
         if HT.equivalent (Parameters.configuration.def_mysql_group, ports_default) then
            if specs.broken_mysql.Contains (HT.SUS (default_mysql)) then
               reason := HT.SUS (LIST_MYSQL_FAILURE & default_mysql & "'");
               append_ignore;
            end if;
         else
            if specs.broken_mysql.Contains (Parameters.configuration.def_mysql_group) then
               reason := HT.SUS (LIST_MYSQL_FAILURE &
                                   HT.USS (Parameters.configuration.def_mysql_group) & "'");
               append_ignore;
            end if;
         end if;

         --  Handle BROKEN_PGSQL directive
         if HT.equivalent (Parameters.configuration.def_postgresql, ports_default) then
            if specs.broken_pgsql.Contains (HT.SUS (default_pgsql)) then
               reason := HT.SUS (LIST_PGSQL_FAILURE & default_pgsql & "'");
               append_ignore;
            end if;
         else
            if specs.broken_pgsql.Contains (Parameters.configuration.def_postgresql) then
               reason := HT.SUS (LIST_PGSQL_FAILURE &
                                   HT.USS (Parameters.configuration.def_postgresql) & "'");
               append_ignore;
            end if;
         end if;

      end check_ignore;

   begin
      specs.broken.Iterate (Process => check'Access);
      check_ignore;
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
   begin
      if not specs.uses.Contains (text_cpe) then
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
   end apply_cpe_module;


   --------------------------------------------------------------------------------------------
   --  apply_scons_module
   --------------------------------------------------------------------------------------------
   procedure apply_scons_module (specs : in out Portspecs)
   is
      module     : String := "scons";
      dependency : String := "scons:single:standard";
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_build_depends (specs, dependency);
      end if;
   end apply_scons_module;


   --------------------------------------------------------------------------------------------
   --  apply_gmake_module
   --------------------------------------------------------------------------------------------
   procedure apply_gmake_module (specs : in out Portspecs)
   is
      module     : String := "gmake";
      dependency : String := "gmake:single:standard";
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_build_depends (specs, dependency);
      end if;
   end apply_gmake_module;


   --------------------------------------------------------------------------------------------
   --  apply_ninja_module
   --------------------------------------------------------------------------------------------
   procedure apply_ninja_module (specs : in out Portspecs)
   is
      module     : String := "ninja";
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_build_depends (specs, NINJA);
      end if;
   end apply_ninja_module;


   --------------------------------------------------------------------------------------------
   --  apply_meson_module
   --------------------------------------------------------------------------------------------
   procedure apply_meson_module (specs : in out Portspecs)
   is
      module     : String := "meson";
      dependency : String := "meson:single:standard";
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_build_depends (specs, dependency);
         add_build_depends (specs, NINJA);
      end if;
   end apply_meson_module;


   --------------------------------------------------------------------------------------------
   --  apply_zlib_module
   --------------------------------------------------------------------------------------------
   procedure apply_zlib_module (specs : in out Portspecs)
   is
      module     : String := "zlib";
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_build_depends (specs, "zlib:static:standard");
         add_buildrun_depends (specs, "zlib:shared:standard");
      end if;
   end apply_zlib_module;


   --------------------------------------------------------------------------------------------
   --  apply_mesa_module
   --------------------------------------------------------------------------------------------
   procedure apply_mesa_module (specs : in out Portspecs)
   is
      module     : String := "mesa";
      dependency : String := "mesa:libs:standard";
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_buildrun_depends (specs, dependency);
      end if;
   end apply_mesa_module;


   --------------------------------------------------------------------------------------------
   --  apply_makeinfo_module
   --------------------------------------------------------------------------------------------
   procedure apply_makeinfo_module  (specs : in out Portspecs)
   is
      module     : String := "makeinfo";
      dependency : String := "texinfo:single:standard";
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_build_depends (specs, dependency);
      end if;
   end apply_makeinfo_module;


   --------------------------------------------------------------------------------------------
   --  apply_readline_module
   --------------------------------------------------------------------------------------------
   procedure apply_readline_module  (specs : in out Portspecs)
   is
      module     : String := "readline";
      dependency : String := "readline:single:standard";
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_buildrun_depends (specs, dependency);
      end if;
   end apply_readline_module;


   --------------------------------------------------------------------------------------------
   --  apply_sqlite_module
   --------------------------------------------------------------------------------------------
   procedure apply_sqlite_module  (specs : in out Portspecs)
   is
      module     : String := "sqlite";
      dependency : String := "sqlite:single:standard";
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_buildrun_depends (specs, dependency);
      end if;
   end apply_sqlite_module;


   --------------------------------------------------------------------------------------------
   --  apply_execinfo_module
   --------------------------------------------------------------------------------------------
   procedure apply_execinfo_module  (specs : in out Portspecs)
   is
      module     : String := "execinfo";
      dependency : String := "libexecinfo:single:standard";
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         if argument_present (specs, module, BUILD) then
            add_build_depends (specs, dependency);
         else
            add_buildrun_depends (specs, dependency);
         end if;
      end if;
   end apply_execinfo_module;


   --------------------------------------------------------------------------------------------
   --  apply_cmake_module
   --------------------------------------------------------------------------------------------
   procedure apply_cmake_module (specs : in out Portspecs)
   is
      module     : String := "cmake";
      dependency : String := "cmake:single:standard";
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
   --  apply_imake_module
   --------------------------------------------------------------------------------------------
   procedure apply_imake_module (specs : in out Portspecs)
   is
      module     : String := "imake";
      dependency : String := "imake:single:standard";
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
           add_build_depends (specs, dependency);
      end if;
   end apply_imake_module;


   --------------------------------------------------------------------------------------------
   --  apply_gcc_run_module
   --------------------------------------------------------------------------------------------
   procedure apply_gcc_run_module (specs : in out Portspecs;
                                   variant : String;
                                   module  : String;
                                   gccsubpackage : String)
   is
      procedure scan (position : string_crate.Cursor);

      dependency : String := default_compiler & ":" & gccsubpackage & ":" & variant_standard;

      procedure scan (position : string_crate.Cursor)
      is
         subpackage : String := HT.USS (string_crate.Element (position));
      begin
         if argument_present (specs, module, subpackage) then
            add_exrun_depends (specs, dependency, subpackage);
         end if;
      end scan;
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         specs.subpackages.Element (HT.SUS (variant)).list.Iterate (scan'Access);
      end if;
   end apply_gcc_run_module;


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
         dependency : String := default_compiler & ":libs:" & variant_standard;
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
   --  apply_firebird_module
   --------------------------------------------------------------------------------------------
   procedure apply_firebird_module (specs : in out Portspecs)
   is
      function determine_dependency return String;

      module : String := "firebird";

      function determine_dependency return String
      is
         suffix  : String := ":client:standard";
         setting : constant String := HT.USS (Parameters.configuration.def_mysql_group);
      begin
         if argument_present (specs, module, "server") then
            suffix := ":server:standard";
         end if;
         if setting = "3.0" then
            return "firebird30" & suffix;
         else
            --  case: setting = ports_default
            --  case: setting = default_firebird
            --  case: setting = invalid value
            return "firebird25" & suffix;
         end if;
      end determine_dependency;

      dependency : String := determine_dependency;
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_buildrun_depends (specs, dependency);
      end if;
   end apply_firebird_module;


   --------------------------------------------------------------------------------------------
   --  apply_mysql_module
   --------------------------------------------------------------------------------------------
   procedure apply_mysql_module (specs : in out Portspecs)
   is
      function determine_dependency return String;

      module : String := "mysql";

      function determine_dependency return String
      is
         suffix  : String := ":client:standard";
         setting : constant String := HT.USS (Parameters.configuration.def_mysql_group);
      begin
         if argument_present (specs, module, "server") then
            suffix := ":server:standard";
         end if;
         if setting = "oracle-5.5" then
            return "mysql55" & suffix;
         elsif setting = "oracle-5.6" then
            return "mysql56" & suffix;
         elsif setting = "mariadb-10.1" then
            return "mariadb101" & suffix;
         elsif setting = "mariadb-10.2" then
            return "mariadb102" & suffix;
         elsif setting = "percona-5.5" then
            return "percona55" & suffix;
         elsif setting = "percona-5.6" then
            return "percona56" & suffix;
         elsif setting = "percona-5.7" then
            return "percona57" & suffix;
         elsif setting = "galera-5.5" then
            return "percona55" & suffix;
         elsif setting = "galera-5.6" then
            return "percona56" & suffix;
         elsif setting = "galera-5.7" then
            return "percona57" & suffix;
         else
            --  case: setting = ports_default
            --  case: setting = default_mysql
            --  case: setting = invalid value
            return "mysql57" & suffix;
         end if;
      end determine_dependency;

      dependency : String := determine_dependency;
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_buildrun_depends (specs, dependency);
      end if;
   end apply_mysql_module;


   --------------------------------------------------------------------------------------------
   --  apply_pgsql_module
   --------------------------------------------------------------------------------------------
   procedure apply_pgsql_module (specs : in out Portspecs)
   is
      function determine_namebase return String;
      procedure set_dependency (subpackage : String);
      procedure set_dependency_on_subpackage (subpackage : String);

      module : String := "pgsql";

      function determine_namebase return String
      is
         setting : constant String := HT.USS (Parameters.configuration.def_mysql_group);
      begin
         if setting = "9.3" then
            return "postgresql93";
         elsif setting = "9.4" then
            return "postgresql94";
         elsif setting = "9.5" then
            return "postgresql95";
         elsif setting = "10" then
            return "postgresql100";
         else
            --  case: setting = ports_default
            --  case: setting = default_pgsql (9.6 right now)
            --  case: setting = invalid value
            return "postgresql96";
         end if;
      end determine_namebase;

      namebase : String := determine_namebase;
      build_only : Boolean := argument_present (specs, module, BUILD);

      procedure set_dependency (subpackage : String)
      is
         dependency : String := namebase & ":" & subpackage & ":standard";
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
      module            : String := "ncurses";
      full_dependency   : String := "ncurses:primary:standard";
      static_dependency : String := "ncurses:primary:static";
   begin
      if not specs.uses_base.Contains (HT.SUS (module)) then
         return;
      end if;
      if no_arguments_present (specs, module) then
         add_buildrun_depends (specs, full_dependency);
      elsif argument_present (specs, module, "static") then
         add_build_depends (specs, static_dependency);
      end if;
   end apply_ncurses_module;


   --------------------------------------------------------------------------------------------
   --  apply_gmake_module
   --------------------------------------------------------------------------------------------
   procedure apply_libtool_module (specs : in out Portspecs)
   is
      module     : String := "libtool";
      dependency : String := "libtool:single:standard";
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
      module            : String := "iconv";
      shared_dependency : String := "libiconv:shared:standard";
      static_dependency : String := "libiconv:static:standard";
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_build_depends (specs, static_dependency);
         if not argument_present (specs, module, BUILD) then
            add_buildrun_depends (specs, shared_dependency);
         end if;
      end if;
   end apply_libiconv_module;


   --------------------------------------------------------------------------------------------
   --  apply_info_presence
   --------------------------------------------------------------------------------------------
   procedure apply_info_presence (specs : in out Portspecs)
   is
      dependency : String := "indexinfo:single:standard";
   begin
      if not specs.info.Is_Empty then
         add_buildrun_depends (specs, dependency);
      end if;
   end apply_info_presence;


   --------------------------------------------------------------------------------------------
   --  apply_ccache
   --------------------------------------------------------------------------------------------
   procedure apply_ccache (specs : in out Portspecs)
   is
      dependency : String := "ccache:primary:standard";
   begin
      if specs.skip_build or else
        specs.skip_ccache or else
        HT.equivalent (Parameters.configuration.dir_ccache, Parameters.no_ccache)
      then
         return;
      end if;
      add_build_depends (specs, dependency);
   end apply_ccache;


   --------------------------------------------------------------------------------------------
   --  apply_pkgconfig_module
   --------------------------------------------------------------------------------------------
   procedure apply_pkgconfig_module (specs : in out Portspecs)
   is
      module     : String := "pkgconfig";
      dependency : String := "pkgconfig:single:standard";
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
      module   : String := "autoreconf";
      AUTOCONF : String := "autoconf:single:standard";
      AUTOMAKE : String := "automake:single:standard";
      LIBTOOL  : String := "libtool:single:standard";
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
      module     : String := "gprbuild";
      dependency : String := "gprbuild:primary:standard";
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
         add_build_depends (specs, "xorg-misc-bitmap-fonts:single:standard");
         add_build_depends (specs, "xorg-font-alias:single:standard");
         add_build_depends (specs, "daemonize:single:standard");
      end if;
   end apply_display_module;


   --------------------------------------------------------------------------------------------
   --  apply_gnome_icons_module
   --------------------------------------------------------------------------------------------
   procedure apply_gnome_icons_module (specs : in out Portspecs)
   is
      module : String := "gnome-icons";
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_run_depends (specs, "gtk-update-icon-cache:single:standard");
      end if;
   end apply_gnome_icons_module;


   --------------------------------------------------------------------------------------------
   --  apply_schemas_module
   --------------------------------------------------------------------------------------------
   procedure apply_schemas_module (specs : in out Portspecs)
   is
      module : String := "schemas";
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_run_depends (specs, GNOMELIB);
      end if;
   end apply_schemas_module;


   --------------------------------------------------------------------------------------------
   --  apply_desktop_utils_module
   --------------------------------------------------------------------------------------------
   procedure apply_desktop_utils_module (specs : in out Portspecs)
   is
      module : String := "desktop-utils";
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         add_buildrun_depends (specs, "desktop-file-utils:single:standard");
      end if;
   end apply_desktop_utils_module;


   --------------------------------------------------------------------------------------------
   --  apply_gettext_tools_module
   --------------------------------------------------------------------------------------------
   procedure apply_gettext_tools_module (specs : in out Portspecs)
   is
      module     : String := "gettext-tools";
      dependency : String := "gettext:tools:standard";
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
   end apply_gettext_tools_module;


   --------------------------------------------------------------------------------------------
   --  apply_gettext_runtime_module
   --------------------------------------------------------------------------------------------
   procedure apply_gettext_runtime_module (specs : in out Portspecs)
   is
      module     : String := "gettext-runtime";
      dependency : String := "gettext:runtime:standard";
      asprintf   : String := "gettext:asprintf:standard";
      hit_run    : Boolean;
      hit_build  : Boolean;
      hit_both   : Boolean;
      hit_aspr   : Boolean;
   begin
      if not specs.uses_base.Contains (HT.SUS (module)) then
         return;
      end if;
      if no_arguments_present (specs, module) then
         hit_build := False;
         hit_both  := True;
         hit_run   := False;
         hit_aspr  := False;
      else
         hit_build := argument_present (specs, module, BUILD);
         hit_both  := argument_present (specs, module, BUILDRUN);
         hit_run   := argument_present (specs, module, RUN);
         hit_aspr  := argument_present (specs, module, "asprintf");

         if not (hit_build or else hit_both or else hit_run) then
            hit_both := True;
         end if;
      end if;

      if hit_both or else (hit_build and hit_run) then
         add_buildrun_depends (specs, dependency);
         if hit_aspr then
            add_buildrun_depends (specs, asprintf);
         end if;
      elsif hit_build then
         add_build_depends (specs, dependency);
         if hit_aspr then
            add_build_depends (specs, asprintf);
         end if;
      else
         add_run_depends (specs, dependency);
         if hit_aspr then
            add_run_depends (specs, asprintf);
         end if;
      end if;
   end apply_gettext_runtime_module;


   --------------------------------------------------------------------------------------------
   --  apply_python_module
   --------------------------------------------------------------------------------------------
   procedure apply_python_module (specs : in out Portspecs)
   is
      module     : constant String := "python";
      SETUPTOOLS : constant String := "python-setuptools:single:";
      PY27       : constant String := "py27";
      PY36       : constant String := "py36";
      PY35       : constant String := "py35";
   begin
      if not specs.uses_base.Contains (HT.SUS (module)) then
         return;
      end if;

      if argument_present (specs, module, "build") then
         if argument_present (specs, module, PY27) then
            add_build_depends (specs, PYTHON27);
            add_build_depends (specs, SETUPTOOLS & PY27);
         elsif argument_present (specs, module, PY35) then
            add_build_depends (specs, PYTHON35);
            add_build_depends (specs, SETUPTOOLS & PY35);
         else -- default to py36
            add_build_depends (specs, PYTHON36);
            add_build_depends (specs, SETUPTOOLS & PY36);
         end if;
      else
         if argument_present (specs, module, PY27) then
            add_buildrun_depends (specs, PYTHON27);
            add_buildrun_depends (specs, SETUPTOOLS & PY27);
         elsif argument_present (specs, module, PY35) then
            add_buildrun_depends (specs, PYTHON35);
            add_buildrun_depends (specs, SETUPTOOLS & PY35);
         else -- default to py36
            add_buildrun_depends (specs, PYTHON36);
            add_buildrun_depends (specs, SETUPTOOLS & PY36);
         end if;
      end if;
   end apply_python_module;


   --------------------------------------------------------------------------------------------
   --  apply_ruby_module
   --------------------------------------------------------------------------------------------
   procedure apply_ruby_module (specs : in out Portspecs)
   is
      module     : constant String := "ruby";
      v23        : constant String := "v23";
      v24        : constant String := "v24";
      v25        : constant String := "v25";
   begin
      if not specs.uses_base.Contains (HT.SUS (module)) or else
        argument_present (specs, module, "interp")
      then
         return;
      end if;

      if argument_present (specs, module, "build") then
         if argument_present (specs, module, v23) then
            add_build_depends (specs, RUBY23);
         elsif argument_present (specs, module, v25) then
            add_build_depends (specs, RUBY25);
         else -- default to ruby24 (current default)
            add_build_depends (specs, RUBY24);
         end if;
      else
         if argument_present (specs, module, v23) then
            add_buildrun_depends (specs, RUBY23);
         elsif argument_present (specs, module, v25) then
            add_buildrun_depends (specs, RUBY25);
         else -- default to ruby24 (current default)
            add_buildrun_depends (specs, RUBY24);
         end if;
      end if;
   end apply_ruby_module;


   --------------------------------------------------------------------------------------------
   --  apply_fonts_module
   --------------------------------------------------------------------------------------------
   procedure apply_fonts_module (specs : in out Portspecs)
   is
      module      : String := "fonts";
      fontconfig  : String := "fontconfig:primary:standard";
      mkfontdir   : String := "xorg-mkfontdir:single:standard";
      mkfontscale : String := "xorg-mkfontscale:single:standard";
   begin
      if not specs.uses_base.Contains (HT.SUS (module)) then
         return;
      end if;
      if no_arguments_present (specs, module) or else
        argument_present (specs, module, "fontsdir")
      then
         add_buildrun_depends (specs, mkfontdir);
         add_buildrun_depends (specs, mkfontscale);
         return;
      end if;
      if argument_present (specs, module, "fc") then
         add_buildrun_depends (specs, fontconfig);
         return;
      end if;
      if argument_present (specs, module, "fcfontsdir") then
         add_buildrun_depends (specs, fontconfig);
         add_buildrun_depends (specs, mkfontdir);
         add_buildrun_depends (specs, mkfontscale);
      end if;
   end apply_fonts_module;


   --------------------------------------------------------------------------------------------
   --  apply_bison_module
   --------------------------------------------------------------------------------------------
   procedure apply_bison_module (specs : in out Portspecs)
   is
      module     : String := "bison";
      dependency : String := "bison:primary:standard";
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
   end apply_bison_module;


   --------------------------------------------------------------------------------------------
   --  apply_jpeg_module
   --------------------------------------------------------------------------------------------
   procedure apply_jpeg_module (specs : in out Portspecs)
   is
      module     : String := "jpeg";
      dependency : String := "jpeg-turbo:primary:standard";
      hit_run    : Boolean;
      hit_build  : Boolean;
      hit_both   : Boolean;
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

      if hit_both or else (hit_build and hit_run) then
         add_buildrun_depends (specs, dependency);
      elsif hit_build then
         add_build_depends (specs, dependency);
      else
         add_run_depends (specs, dependency);
      end if;
   end apply_jpeg_module;


   --------------------------------------------------------------------------------------------
   --  apply_png_module
   --------------------------------------------------------------------------------------------
   procedure apply_png_module (specs : in out Portspecs)
   is
      module     : String := "png";
      dependency : String := "png:single:standard";
      hit_run    : Boolean;
      hit_build  : Boolean;
      hit_both   : Boolean;
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

      if hit_both or else (hit_build and hit_run) then
         add_buildrun_depends (specs, dependency);
      elsif hit_build then
         add_build_depends (specs, dependency);
      else
         add_run_depends (specs, dependency);
      end if;
      hit_build := argument_present (specs, module, BUILD);
   end apply_png_module;


   --------------------------------------------------------------------------------------------
   --  apply_gem_module
   --------------------------------------------------------------------------------------------
   procedure apply_gem_module (specs : in out Portspecs)
   is
      module : String := "gem";
      defver : String (1 .. 2) :=
               default_ruby (default_ruby'First) & default_ruby (default_ruby'Last);
      flavor : String := "v" & defver;
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         if not no_arguments_present (specs, module) then
            if argument_present (specs, module, "v23") then
               flavor := "v23";
            elsif argument_present (specs, module, "v25") then
               flavor := "v25";
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
      std_suffix  : constant String := ":single:standard";
      --  This defver works until PHP 10 is released
      defver : String (1 .. 2) := default_php (default_php'First) & default_php (default_php'Last);
      flavor : String := "php" & defver;
      hit_build   : Boolean := False;
      hit_phpsize : Boolean := False;
      hit_ext     : Boolean := False;
      hit_zend    : Boolean := False;
   begin
      if not specs.uses_base.Contains (HT.SUS (module)) then
         return;
      end if;

      if not no_arguments_present (specs, module) then
         if argument_present (specs, module, "72") then
            flavor := "php72";
         elsif argument_present (specs, module, "71") then
            flavor := "php71";
         elsif argument_present (specs, module, "56") then
            flavor := "php56";
         end if;
         hit_build   := argument_present (specs, module, BUILD);
         hit_phpsize := argument_present (specs, module, "phpsize");
         hit_ext     := argument_present (specs, module, "ext");
         hit_zend    := argument_present (specs, module, "zend");
      end if;
      if hit_build or else hit_phpsize or else hit_ext or else hit_zend then
         add_buildrun_depends (specs, flavor & std_suffix);
      else
         add_run_depends (specs, flavor & std_suffix);
      end if;
      if hit_phpsize or else hit_ext or else hit_zend then
         add_build_depends (specs, "autoconf" & std_suffix);
      end if;

   end apply_php_module;


   --------------------------------------------------------------------------------------------
   --  apply_lua_module
   --------------------------------------------------------------------------------------------
   procedure apply_lua_module (specs : in out Portspecs)
   is
      function pick_lua return String;

      module     : String := "lua";
      hit_run    : Boolean;
      hit_build  : Boolean;
      hit_both   : Boolean;

      function pick_lua return String
      is
         LUA52 : String := "lua52:single:standard";
         LUA53 : String := "lua53:single:standard";
         def_setting : String := HT.USS (Parameters.configuration.def_lua);
      begin
         if argument_present (specs, module, "5.2") then
            return LUA52;
         elsif argument_present (specs, module, "5.3") or else
           def_setting = ports_default or else
           def_setting = "5.3"
         then
            return LUA53;
         else
            return LUA52;
         end if;
      end pick_lua;

      dependency : String := pick_lua;
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

      if hit_both or else (hit_build and hit_run) then
         add_buildrun_depends (specs, dependency);
      elsif hit_build then
         add_build_depends (specs, dependency);
      else
         add_run_depends (specs, dependency);
      end if;
   end apply_lua_module;


   --------------------------------------------------------------------------------------------
   --  apply_tcl_module
   --------------------------------------------------------------------------------------------
   procedure apply_tcl_module (specs : in out Portspecs)
   is
      function pick_tcl return String;

      module     : String := "tcl";
      hit_run    : Boolean;
      hit_build  : Boolean;
      hit_both   : Boolean;

      function pick_tcl return String
      is
         def_setting : String := HT.USS (Parameters.configuration.def_tcl_tk);
      begin
         if argument_present (specs, module, "8.5") then
            return TCL85;
         elsif argument_present (specs, module, "8.6") or else
           def_setting = ports_default or else
           def_setting = "8.6"
         then
            return TCL86;
         else
            return TCL85;
         end if;
      end pick_tcl;

      dependency : String := pick_tcl;
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

      if hit_both or else (hit_build and hit_run) then
         add_buildrun_depends (specs, dependency);
      elsif hit_build then
         add_build_depends (specs, dependency);
      else
         add_run_depends (specs, dependency);
      end if;
   end apply_tcl_module;


   --------------------------------------------------------------------------------------------
   --  apply_ssl_module
   --------------------------------------------------------------------------------------------
   procedure apply_ssl_module (specs : in out Portspecs)
   is
      function ssl_dependency return String;

      hit_run    : Boolean;
      hit_build  : Boolean;
      hit_both   : Boolean;
      module     : String := "ssl";

      function ssl_dependency return String
      is
         nbase   : String := HT.USS (Parameters.configuration.def_ssl);
         suffix  : String := ":single:standard";
      begin
         if nbase = ports_default then
            return default_ssl & suffix;
         else
            return nbase & suffix;
         end if;
      end ssl_dependency;

      dependency : String := ssl_dependency;

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

      if hit_both or else (hit_build and hit_run) then
         add_buildrun_depends (specs, dependency);
      elsif hit_build then
         add_build_depends (specs, dependency);
      else
         add_run_depends (specs, dependency);
      end if;
   end apply_ssl_module;


   --------------------------------------------------------------------------------------------
   --  apply_bdb_module
   --------------------------------------------------------------------------------------------
   procedure apply_bdb_module (specs : in out Portspecs)
   is
      module       : String  := "bdb";
      dep_static_5 : String  := "db5:static:standard";
      dep_static_6 : String  := "db6:static:standard";
      dep_shared_5 : String  := "db5:shared:standard";
      dep_shared_6 : String  := "db6:shared:standard";
      need_static  : Boolean := False;
      need_six     : Boolean := False;
   begin
      if not specs.uses_base.Contains (HT.SUS (module)) then
         return;
      end if;
      if argument_present (specs, module, "static") then
         need_static := True;
      end if;
      if argument_present (specs, module, "6") then
         need_six := True;
      end if;

      if need_six then
         add_build_depends (specs, dep_static_6);
      else
         add_build_depends (specs, dep_static_5);
      end if;
      if not need_static then
         if need_six then
            add_buildrun_depends (specs, dep_shared_6);
         else
            add_buildrun_depends (specs, dep_shared_5);
         end if;
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
      perl_526      : constant String := "526";
      perl_524      : constant String := "524";
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
         suffix        : String := ":primary:standard";
         def_setting   : String := HT.USS (Parameters.configuration.def_perl);
         override_dep  : String := "perl-" & def_setting;
      begin
         if argument_present (specs, module, perl_526) then
            dep_suffix := perl_526;
            return "perl-5.26" & suffix;
         elsif argument_present (specs, module, perl_524) then
            dep_suffix := perl_524;
            return "perl-5.24" & suffix;
         else
            if def_setting = ports_default then
               dep_suffix := HT.replace_char (default_perl, LAT.Full_Stop, "");
               return rport_default & suffix;
            else
               dep_suffix := HT.replace_char (def_setting, LAT.Full_Stop, "");
               return override_dep & suffix;
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

   end apply_perl_module;


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
      apply_cbc_string_crate (specs.plist_sub);
      apply_cbc_string_crate (specs.mk_verbatim);
      apply_cbc_string_crate (specs.sub_list);
      apply_cbc_string_crate (specs.users);
      apply_cbc_string_crate (specs.groups);

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
   --  argument_present
   --------------------------------------------------------------------------------------------
   procedure shift_extra_patches
     (specs         : Portspecs;
      extract_dir   : String)
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
   procedure convert_exrun_versions (specs : in out Portspecs)
   is
      procedure convert1 (position1 : list_crate.Cursor);
      procedure convert2 (Key : HT.Text; Element : in out group_list);
      procedure convert3 (Element : in out HT.Text);

      procedure convert3 (Element : in out HT.Text)
      is
         exrundep : String := HT.USS (Element);
      begin
         if exrundep = "ssl" then
            declare
               setting : String := HT.USS (Parameters.configuration.def_ssl);
            begin
               if setting = ports_default then
                  Element := HT.SUS ("libressl:single:standard");
               else
                  Element := HT.SUS (setting & ":single:standard");
               end if;
            end;
         elsif exrundep = "python" then
            if specs.buildrun_deps.Contains (HT.SUS (PYTHON27)) then
               Element := HT.SUS (PYTHON27);
            elsif specs.buildrun_deps.Contains (HT.SUS (PYTHON35)) then
               Element := HT.SUS (PYTHON35);
            else
              Element := HT.SUS (PYTHON36);
            end if;
         elsif exrundep = "tcl" then
            if specs.buildrun_deps.Contains (HT.SUS (TCL85)) then
               Element := HT.SUS (TCL85);
            else
               Element := HT.SUS (TCL86);
            end if;
         elsif exrundep = "perl" then
            declare
               setting : String := HT.USS (Parameters.configuration.def_perl);
               suffix  : String := ":primary:standard";
            begin
               if setting = ports_default then
                  Element := HT.SUS ("perl-" & default_perl & suffix);
               else
                  Element := HT.SUS ("perl-" & setting & suffix);
               end if;
            end;
         end if;
      end convert3;

      procedure convert2 (Key : HT.Text; Element : in out group_list)
      is
         txt_ssl    : HT.Text := HT.SUS ("ssl");
         txt_python : HT.Text := HT.SUS ("python");
         txt_tcl    : HT.Text := HT.SUS ("tcl");
         txt_perl   : HT.Text := HT.SUS ("perl");
      begin
         if Element.list.Contains (txt_ssl) then
            Element.list.Update_Element (Position => Element.list.Find (txt_ssl),
                                         Process  => convert3'Access);
         end if;
         if Element.list.Contains (txt_python) then
            Element.list.Update_Element (Position => Element.list.Find (txt_python),
                                         Process  => convert3'Access);
         end if;
         if Element.list.Contains (txt_tcl) then
            Element.list.Update_Element (Position => Element.list.Find (txt_tcl),
                                         Process  => convert3'Access);
         end if;
         if Element.list.Contains (txt_perl) then
            Element.list.Update_Element (Position => Element.list.Find (txt_perl),
                                         Process  => convert3'Access);
         end if;
      end convert2;

      procedure convert1 (position1 : list_crate.Cursor) is
      begin
         specs.extra_rundeps.Update_Element (Position => position1,
                                             Process  => convert2'Access);
      end convert1;
   begin
      specs.extra_rundeps.Iterate (convert1'Access);
   end convert_exrun_versions;


   --------------------------------------------------------------------------------------------
   --  apply_extraction_deps
   --------------------------------------------------------------------------------------------
   procedure apply_extraction_deps (specs : in out Portspecs) is
   begin
      --  unzip is already in base
      if not specs.extract_7z.Is_Empty then
         add_build_depends (specs, "p7zip:primary:standard");
      end if;
      --  TODO: placeholder for LHA
   end apply_extraction_deps;


   --------------------------------------------------------------------------------------------
   --  transform_defaults
   --------------------------------------------------------------------------------------------
   function transform_defaults (dep : String) return String
   is
      function name_subpackage return String;
      function name_subpackage return String is
      begin
         return HT.specific_field (dep, 1, ":") & ":" & HT.specific_field (dep, 2, ":") & ":";
      end name_subpackage;
   begin
      if HT.trails (dep, ":python_default") then
         declare
            setting : String := HT.USS (Parameters.configuration.def_python3);
         begin
            if setting = ports_default or else setting = default_python3 then
               return name_subpackage & "py36";
            else
               return name_subpackage & "py35";
            end if;
         end;
      elsif HT.trails (dep, ":perl_default") then
         declare
            setting : String := HT.USS (Parameters.configuration.def_perl);
         begin
            if setting = ports_default or else setting = default_perl then
               return name_subpackage & "526";
            else
               return name_subpackage & "524";
            end if;
         end;
      elsif HT.trails (dep, ":lua_default") then
         declare
            setting : String := HT.USS (Parameters.configuration.def_lua);
         begin
            if setting = ports_default or else setting = default_lua then
               return name_subpackage & "lua53";
            else
               return name_subpackage & "lua52";
            end if;
         end;
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
      procedure alter (Element : in out HT.Text);

      transformed_dep : HT.Text;

      procedure alter (Element : in out HT.Text) is
      begin
         Element := transformed_dep;
      end alter;

      procedure check_build (position : string_crate.Cursor)
      is
         dep  : String := HT.USS (string_crate.Element (position));
         xdep : String := transform_defaults (dep);
      begin
         if xdep /= dep then
            transformed_dep := HT.SUS (xdep);
            specs.build_deps.Update_Element (position, alter'Access);
         end if;
      end check_build;

      procedure check_buildrun (position : string_crate.Cursor)
      is
         dep  : String := HT.USS (string_crate.Element (position));
         xdep : String := transform_defaults (dep);
      begin
         if xdep /= dep then
            transformed_dep := HT.SUS (xdep);
            specs.buildrun_deps.Update_Element (position, alter'Access);
         end if;
      end check_buildrun;

      procedure check_run (position : string_crate.Cursor)
      is
         dep  : String := HT.USS (string_crate.Element (position));
         xdep : String := transform_defaults (dep);
      begin
         if xdep /= dep then
            transformed_dep := HT.SUS (xdep);
            specs.run_deps.Update_Element (position, alter'Access);
         end if;
      end check_run;
   begin
      specs.build_deps.Iterate (check_build'Access);
      specs.buildrun_deps.Iterate (check_buildrun'Access);
      specs.run_deps.Iterate (check_run'Access);
   end apply_default_version_transformations;


   --------------------------------------------------------------------------------------------
   --  apply_gnome_components_dependencies
   --------------------------------------------------------------------------------------------
   procedure apply_gnome_components_dependencies (specs : in out Portspecs)
   is
      procedure import (position : string_crate.Cursor);

      ss            : constant String := ":single:standard";
      ps            : constant String := ":primary:standard";
      port_libxml2  : constant String := "libxml2";
      port_libxslt  : constant String := "libxslt";
      port_gettext  : constant String := "gettext:runtime:standard";
      port_cairo    : constant String := "cairo";
      port_atk      : constant String := "atk";
      port_gtk2     : constant String := "gtk2";
      port_gtk3     : constant String := "gtk3";
      port_pango    : constant String := "pango";
      port_gobspec  : constant String := "gobject-introspection";
      port_intltool : constant String := "intltool";
      port_gsview3  : constant String := "gtksourceview3";
      port_libidl   : constant String := "libIDL";
      port_orbit2   : constant String := "ORBit2";
      port_dconf    : constant String := "dconf";
      port_gconf    : constant String := "gconf";
      port_libglade : constant String := "libglade:single:py27";
      port_pygtk2   : constant String := "python-gtk2:primary:py27";
      port_pygobj2  : constant String := "python-pygobject2:single:py27";
      port_pygobjcm : constant String := "python-pygobject:common:py35";
      port_pygobj   : constant String := "python-pygobject:primary:py" &
        HT.replace_char (default_python3, '.', "");


      procedure import (position : string_crate.Cursor)
      is
         component_text : HT.Text renames string_crate.Element (position);
         comp : gnome_type := determine_gnome_component (HT.USS (component_text));
      begin
         case comp is
            when invalid_component => null;  --  should be impossible
            when glib      => null;
            when libglade  => null;
            when libxml2   => null;
            when gtk2      => null;
            when gtk3      => null;
            when orbit2    => null;
            when dconf     => add_buildrun_depends (specs, port_dconf & ps);
            when libxslt   => add_buildrun_depends (specs, port_libxslt & ss);
            when libidl    => add_buildrun_depends (specs, port_libidl & ss);
            when atk       => add_buildrun_depends (specs, port_atk & ss);
            when cairo     => add_buildrun_depends (specs, port_cairo & ss);
            when pango     => add_buildrun_depends (specs, port_pango & ps);
            when intltool  => add_build_depends    (specs, port_intltool & ss);
            when gdkpixbuf => add_buildrun_depends (specs, "gdk-pixbuf" & ps);
            when libgsf    => add_buildrun_depends (specs, "libgsf" & ps);
            when libcroco  => add_buildrun_depends (specs, "libcroco" & ps);
            when librsvg   => add_buildrun_depends (specs, "librsvg" & ps);
            when vte       => add_buildrun_depends (specs, "vte" & ps);
            when pygobject => add_buildrun_depends (specs, port_pygobj);
                              add_buildrun_depends (specs, port_pygobjcm);
            when pygobj2   => add_buildrun_depends (specs, port_pygobj2);
            when pygtk2    => add_buildrun_depends (specs, port_pygtk2);
                              add_buildrun_depends (specs, port_pygobj2);
            when gtksourceview3 =>
                              add_buildrun_depends (specs, port_gsview3 & ps);
            when introspection =>
                              add_build_depends (specs, port_gobspec & ss);
                              add_build_depends (specs, PYTHON27);
                              specs.make_env.Append (HT.SUS ("GI_SCANNER_DISABLE_CACHE=1"));
                              specs.make_env.Append (HT.SUS ("XDG_CACHE_HOME=${WRKDIR}"));
            when gconf     => add_buildrun_depends (specs, port_gconf & ps);

         end case;
         --  These components imply gtk3
         case comp is
            when gtk3 | gtksourceview3 | vte =>
               add_buildrun_depends (specs, port_gtk3 & ss);
               add_buildrun_depends (specs, port_atk & ss);
               add_buildrun_depends (specs, port_pango & ps);
            when others => null;
         end case;
         --  These components imply glib
         case comp is
            when introspection | glib | atk | pango | gdkpixbuf | pygobject | pygobj2 | pygtk2 |
               libidl | orbit2 | dconf =>
               add_buildrun_depends (specs, GNOMELIB);
               add_buildrun_depends (specs, port_gettext);
            when others => null;
         end case;
         --  These components imply orbit2 (which implies libidl)
         case comp is
            when orbit2 | gconf =>
               add_buildrun_depends (specs, port_orbit2 & ss);
               add_buildrun_depends (specs, port_libidl & ss);
            when others => null;
         end case;
         --  These components imply libglade (which implies libxml2 and gtk2)
         case comp is
            when libglade | pygtk2 =>
               add_buildrun_depends (specs, port_libglade);
            when others => null;
         end case;
         --  These components imply gtk2
         case comp is
            when gtk2 | gconf | libglade | pygtk2 =>
               add_buildrun_depends (specs, port_gtk2 & ss);
               add_buildrun_depends (specs, port_atk & ss);
               add_buildrun_depends (specs, port_pango & ps);
            when others => null;
         end case;
         --  These components imply libxml2
         case comp is
            when libxml2 | libxslt | libglade | pygtk2 | gconf | gtksourceview3 =>
               add_buildrun_depends (specs, port_libxml2 & ss);
            when others => null;
         end case;
      end import;
   begin
      specs.gnome_comps.Iterate (import'Access);
   end apply_gnome_components_dependencies;


   --------------------------------------------------------------------------------------------
   --  apply_sdl_components_dependencies
   --------------------------------------------------------------------------------------------
   procedure apply_sdl_components_dependencies  (specs : in out Portspecs)
   is
      procedure import (position : string_crate.Cursor);

      ss : constant String := ":single:standard";
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
            when gfx1   => add_buildrun_depends (specs, "sdl1_gfx" & ss);
            when gfx2   => add_buildrun_depends (specs, "sdl2_gfx" & ss);
            when image1 => add_buildrun_depends (specs, "sdl1_image" & ss);
            when image2 => add_buildrun_depends (specs, "sdl2_image" & ss);
            when mixer1 => add_buildrun_depends (specs, "sdl1_mixer" & ss);
            when mixer2 => add_buildrun_depends (specs, "sdl2_mixer" & ss);
            when net1   => add_buildrun_depends (specs, "sdl1_net" & ss);
            when net2   => add_buildrun_depends (specs, "sdl2_net" & ss);
            when ttf1   => add_buildrun_depends (specs, "sdl1_ttf" & ss);
            when ttf2   => add_buildrun_depends (specs, "sdl2_ttf2" & ss);
         end case;
         case comp is
            when sdl1 | gfx1 | image1 | mixer1 | net1 | ttf1 =>
               add_buildrun_depends (specs, "sdl1:primary:standard");
               specs.make_env.Append (menv1);
               specs.config_env.Append (menv1);
            when sdl2 | gfx2 | image2 | mixer2 | net2 | ttf2 =>
               add_buildrun_depends (specs, "sdl2:single:standard");
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

      --  All xorg components have this format : xorg-{COMPONENT}:single:standard
      --  All xorg components ending in "proto" are build-only depends
      --      The rest are considered libraries (buildrun type)
      --  All libraries depend on pkgconfig and xorg-macros

      uses_xorg : Boolean := False;
      ss        : constant String := ":single:standard";

      procedure import (position : string_crate.Cursor)
      is
         component_text : HT.Text renames string_crate.Element (position);
         component  : constant String := HT.USS (component_text);
         dependency : constant String := "xorg-" & component & ss;
      begin
         if HT.trails (component, "proto") then
            add_build_depends (specs, dependency);
         else
            add_buildrun_depends (specs, dependency);
         end if;
         uses_xorg := True;
      end import;
   begin
      specs.xorg_comps.Iterate (import'Access);
      if uses_xorg and then not HT.trails (specs.get_namebase, "proto") then
         add_build_depends (specs, "xorg-macros" & ss);
         add_build_depends (specs, "pkgconfig" & ss);
      end if;
   end apply_xorg_components_dependencies;


   --------------------------------------------------------------------------------------------
   --  apply_php_extension_dependencies
   --------------------------------------------------------------------------------------------
   procedure apply_php_extension_dependencies (specs : in out Portspecs)
   is
      procedure import (position : string_crate.Cursor);

      php_module  : constant String := "php";
      std_suffix  : constant String := ":single:standard";
      hit_build   : Boolean := False;
      hit_ext     : Boolean := False;
      --  This defver works until PHP 10 is released
      defver : String (1 .. 2) := default_php (default_php'First) & default_php (default_php'Last);
      flavor : String := "php" & defver;

      procedure import (position : string_crate.Cursor)
      is
         extension_text : HT.Text renames string_crate.Element (position);
         extension      : constant String := HT.USS (extension_text);
         dependency     : constant String := flavor & "-" & extension & std_suffix;
      begin
         if hit_build or else hit_ext then
            add_buildrun_depends (specs, dependency);
         else
            add_run_depends (specs, dependency);
         end if;
      end import;
   begin
      if not no_arguments_present (specs, php_module) then
         if argument_present (specs, php_module, "72") then
            flavor := "php72";
         elsif argument_present (specs, php_module, "71") then
            flavor := "php71";
         elsif argument_present (specs, php_module, "56") then
            flavor := "php56";
         end if;
      end if;
      hit_build := argument_present (specs, php_module, BUILD);
      hit_ext   := argument_present (specs, php_module, "ext");
      specs.php_extensions.Iterate (import'Access);
   end apply_php_extension_dependencies;

end Port_Specification.Transform;
