--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Utilities;
with Ada.Characters.Latin_1;
with Ada.Directories;
with Parameters;

package body Port_Specification.Transform is

   package UTL renames Utilities;
   package LAT renames Ada.Characters.Latin_1;
   package DIR renames Ada.Directories;

   --------------------------------------------------------------------------------------------
   --  apply_directives
   --------------------------------------------------------------------------------------------
   procedure apply_directives
     (specs         : in out Portspecs;
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
                     when install_target_on   => specs.install_tgt.Append (item);
                     when keywords_on         => specs.keywords.Append (item);
                     when ldflags_on          => specs.ldflags.Append (item);
                     when buildrun_depends_on => specs.buildrun_deps.Append (item);
                     when make_args_on        => specs.make_args.Append (item);
                     when make_env_on         => specs.make_env.Append (item);
                     when patchfiles_on       => specs.patchfiles.Append (item);
                     when plist_sub_on        => specs.plist_sub.Append (item);
                     when run_depends_on      => specs.run_deps.Append (item);
                     when sub_files_on        => specs.sub_files.Append (item);
                     when sub_list_on         => specs.sub_list.Append (item);
                     when qmake_on            => specs.qmake_args.Append (item);
                     when test_target_on      => specs.test_tgt.Append (item);
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
                     when cmake_args_off      => specs.cmake_args.Append (item);
                     when configure_args_off  => specs.config_args.Append (item);
                     when qmake_off           => specs.qmake_args.Append (item);
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
            augment (build_depends_on,    rec.BUILD_DEPENDS_ON);
            augment (build_target_on,     rec.BUILD_TARGET_ON);
            augment (cflags_off,          rec.CFLAGS_OFF);
            augment (cflags_on,           rec.CFLAGS_ON);
            augment (cmake_args_on,       rec.CMAKE_ARGS_ON);
            augment (configure_args_on,   rec.CONFIGURE_ARGS_ON);
            augment (configure_args_on,   rec.CONFIGURE_ENV_ON);
            augment (cppflags_on,         rec.CPPFLAGS_ON);
            augment (cxxflags_on,         rec.CXXFLAGS_ON);
            augment (df_index_on,         rec.DF_INDEX_ON);
            augment (extra_patches_on,    rec.EXTRA_PATCHES_ON);
            augment (install_target_on,   rec.INSTALL_TARGET_ON);
            augment (keywords_on,         rec.KEYWORDS_ON);
            augment (ldflags_on,          rec.LDFLAGS_ON);
            augment (buildrun_depends_on, rec.BUILDRUN_DEPENDS_ON);
            augment (make_args_on,        rec.MAKE_ARGS_ON);
            augment (make_env_on,         rec.MAKE_ENV_ON);
            augment (patchfiles_on,       rec.PATCHFILES_ON);
            augment (plist_sub_on,        rec.PLIST_SUB_ON);
            augment (qmake_on,            rec.QMAKE_ON);
            augment (run_depends_on,      rec.RUN_DEPENDS_ON);
            augment (sub_files_on,        rec.SUB_FILES_ON);
            augment (sub_list_on,         rec.SUB_LIST_ON);
            augment (test_target_on,      rec.TEST_TARGET_ON);
            augment (uses_on,             rec.USES_ON);
         else
            augment (cmake_args_off,     rec.CMAKE_ARGS_OFF);
            augment (configure_args_off, rec.CONFIGURE_ARGS_OFF);
            augment (qmake_off,          rec.QMAKE_OFF);
         end if;
         augment (cmake_bool_f_both,     rec.CMAKE_BOOL_F_BOTH);
         augment (cmake_bool_t_both,     rec.CMAKE_BOOL_T_BOTH);
         augment (configure_enable_both, rec.CONFIGURE_ENABLE_BOTH);
         augment (configure_with_both,   rec.CONFIGURE_WITH_BOTH);

         grow_plist_sub (HT.USS (rec.option_name), rec.currently_set_ON);

      end copy_option_over;
   begin
      specs.ops_helpers.Iterate (Process => copy_option_over'Access);
      apply_cpe_module (specs, arch_standard, osmajor);
      apply_gmake_module (specs);
      apply_libiconv_module (specs);
      apply_libtool_module (specs);
      apply_pkgconfig_module (specs);
      apply_ncurses_module (specs);
      apply_info_presence (specs);
      apply_ccache (specs);
      apply_curly_bracket_conversions (specs);
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
         if specs.options_on.Contains (opsys_text) then
            specs.options_on.Element (opsys_text).list.Iterate (Process => opsys_set'Access);
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
   --  GTE
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
               specs.broken.Update_Element (Position => specs.broken.Find (index),
                                            Process  => grow'Access);
            end if;
         end check_list;

      begin
         list_crate.Element (position).list.Iterate (Process => check_list'Access);
      end check;

      procedure check_ignore
      is
         procedure grow (Key : HT.Text; Element : in out group_list);

         reason : HT.Text;

         procedure grow (Key : HT.Text; Element : in out group_list) is
         begin
               Element.list.Append (reason);
         end grow;
      begin
         if specs.exc_opsys.Contains (HT.SUS (UTL.lower_opsys (opsys))) or else
           (not specs.inc_opsys.Is_Empty and then
              not specs.inc_opsys.Contains (HT.SUS (UTL.lower_opsys (opsys))))
         then
            reason := HT.SUS ("Specification excludes " & UTL.mixed_opsys (opsys) & " OS");
         end if;
         if specs.exc_arch.Contains (HT.SUS (UTL.cpu_arch (arch_standard))) then
            reason := HT.SUS ("Specification excludes " & UTL.cpu_arch (arch_standard) &
                                " architecture");
         end if;
         if not HT.IsBlank (reason) then
            specs.broken.Update_Element (Position => specs.broken.Find (index),
                                         Process  => grow'Access);
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
            return HT.USS (specs.catch_all.Element (key_text));
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
   --  apply_gmake_module
   --------------------------------------------------------------------------------------------
   procedure apply_gmake_module (specs : in out Portspecs)
   is
      text_module : HT.Text := HT.SUS ("gmake");
      dependency  : HT.Text := HT.SUS ("gmake:single:lite");
   begin
      if specs.uses_base.Contains (text_module) and then
        not specs.build_deps.Contains (dependency)
      then
         specs.build_deps.Append (dependency);
      end if;
   end apply_gmake_module;


   --------------------------------------------------------------------------------------------
   --  apply_ncurses_module
   --------------------------------------------------------------------------------------------
   procedure apply_ncurses_module (specs : in out Portspecs)
   is
      module            : String  := "ncurses";
      text_module       : HT.Text := HT.SUS (module);
      full_dependency   : HT.Text := HT.SUS ("ncurses:primary:standard");
      static_dependency : HT.Text := HT.SUS ("ncurses:primary:static");
   begin
      if not specs.uses_base.Contains (text_module) then
         return;
      end if;
      if no_arguments_present (specs, module) then
         if not specs.buildrun_deps.Contains (full_dependency) then
            specs.buildrun_deps.Append (full_dependency);
         end if;
      elsif argument_present (specs, module, "static") then
         if not specs.build_deps.Contains (static_dependency) then
            specs.build_deps.Append (static_dependency);
         end if;
      end if;

   end apply_ncurses_module;


   --------------------------------------------------------------------------------------------
   --  apply_gmake_module
   --------------------------------------------------------------------------------------------
   procedure apply_libtool_module (specs : in out Portspecs)
   is
      module     : String  := "libtool";
      dependency : HT.Text := HT.SUS ("libtool:single:standard");
   begin
      if specs.uses_base.Contains (HT.SUS (module)) and then
        argument_present (specs, module, "build") and then
        not specs.build_deps.Contains (dependency)
      then
         specs.build_deps.Append (dependency);
      end if;
   end apply_libtool_module;


   --------------------------------------------------------------------------------------------
   --  apply_libiconv_module
   --------------------------------------------------------------------------------------------
   procedure apply_libiconv_module (specs : in out Portspecs)
   is
      module     : String  := "iconv";
      dependency : HT.Text := HT.SUS ("libiconv:single:standard");
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         if argument_present (specs, module, "build") then
            if not specs.build_deps.Contains (dependency) then
               specs.build_deps.Append (dependency);
            end if;
         else
            if not specs.buildrun_deps.Contains (dependency) then
               specs.buildrun_deps.Append (dependency);
            end if;
         end if;
      end if;
   end apply_libiconv_module;


   --------------------------------------------------------------------------------------------
   --  apply_info_presence
   --------------------------------------------------------------------------------------------
   procedure apply_info_presence (specs : in out Portspecs)
   is
      dependency  : HT.Text := HT.SUS ("indexinfo:single:standard");
   begin
      if not specs.info.Is_Empty and then
        not specs.buildrun_deps.Contains (dependency)
      then
         specs.buildrun_deps.Append (dependency);
      end if;
   end apply_info_presence;


   --------------------------------------------------------------------------------------------
   --  apply_ccache
   --------------------------------------------------------------------------------------------
   procedure apply_ccache (specs : in out Portspecs)
   is
      dependency  : HT.Text := HT.SUS ("ccache:primary:standard");
   begin
      if specs.skip_build or else specs.skip_ccache then
         return;
      end if;
      if HT.equivalent (Parameters.configuration.dir_ccache, Parameters.no_ccache) then
         return;
      end if;
      if not specs.build_deps.Contains (dependency) then
         specs.build_deps.Append (dependency);
      end if;
   end apply_ccache;


   --------------------------------------------------------------------------------------------
   --  apply_pkgconfig_module
   --------------------------------------------------------------------------------------------
   procedure apply_pkgconfig_module (specs : in out Portspecs)
   is
      module     : String  := "pkgconfig";
      dependency : HT.Text := HT.SUS ("pkgconfig:single:standard");
   begin
      if specs.uses_base.Contains (HT.SUS (module)) then
         if no_arguments_present (specs, module) or else
           argument_present (specs, module, "build")
         then
            if not specs.build_deps.Contains (dependency) then
               specs.build_deps.Append (dependency);
            end if;
         else
            if argument_present (specs, module, "buildrun") then
               if not specs.buildrun_deps.Contains (dependency) then
                  specs.buildrun_deps.Append (dependency);
               end if;
            elsif argument_present (specs, module, "run") then
               if not specs.run_deps.Contains (dependency) then
                  specs.run_deps.Append (dependency);
               end if;
            end if;
         end if;
      end if;
   end apply_pkgconfig_module;


   --------------------------------------------------------------------------------------------
   --  apply_curly_bracket_conversions
   --------------------------------------------------------------------------------------------
   procedure apply_curly_bracket_conversions (specs : in out Portspecs) is
   begin
      UTL.apply_cbc_string (specs.install_wrksrc);
      UTL.apply_cbc_string (specs.build_wrksrc);
      UTL.apply_cbc_string (specs.patch_wrksrc);
      UTL.apply_cbc_string (specs.config_wrksrc);
      UTL.apply_cbc_string (specs.config_prefix);
      UTL.apply_cbc_string (specs.config_script);

      apply_cbc_string_crate (specs.config_args);
      apply_cbc_string_crate (specs.config_env);
      apply_cbc_string_crate (specs.make_env);
      apply_cbc_string_crate (specs.make_args);
      apply_cbc_string_crate (specs.cflags);
      apply_cbc_string_crate (specs.cxxflags);
      apply_cbc_string_crate (specs.cppflags);
      apply_cbc_string_crate (specs.ldflags);
      apply_cbc_string_crate (specs.cmake_args);
      apply_cbc_string_crate (specs.qmake_args);
      apply_cbc_string_crate (specs.lic_files);

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
                           if argument = HT.specific_field (argumentstr, x, ":") then
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
   begin
      if num_extra_patch > 0 then
         DIR.Create_Directory (extract_dir & "/patches");
      end if;
      for item in 1 .. num_extra_patch loop
         declare
            patch  : String := specs.get_list_item (sp_extra_patches, item);
            xp_loc : String := extract_dir & "/files/" & patch;
         begin
            if DIR.Exists (xp_loc) then
               DIR.Rename (Old_Name => xp_loc,
                           New_Name => extract_dir & "/patches/patch-zzz-" & patch);
            end if;
         end;
      end loop;
   end shift_extra_patches;

end Port_Specification.Transform;
