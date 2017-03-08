--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Utilities;
with Ada.Characters.Latin_1;

package body Port_Specification.Transform is

   package UTL renames Utilities;
   package LAT renames Ada.Characters.Latin_1;

   --------------------------------------------------------------------------------------------
   --  apply_directives
   --------------------------------------------------------------------------------------------
   procedure apply_directives (specs : in out Portspecs)
   is
      procedure copy_option_over (position : option_crate.Cursor);

      procedure copy_option_over (position : option_crate.Cursor)
      is
         procedure augment (field : spec_option; directive : string_crate.Vector);
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
               case field is
                  when build_depends_on       => specs.build_deps.Append (item);
                  when build_target_on        => specs.build_target.Append (item);
                  when cflags_on              => specs.cflags.Append (item);
                  when cmake_args_on |
                       cmake_args_off         => specs.cmake_args.Append (item);
                  when configure_args_on |
                       configure_args_off     => specs.config_args.Append (item);
                  when configure_env_on       => specs.config_env.Append (item);
                  when cppflags_on            => specs.cppflags.Append (item);
                  when cxxflags_on            => specs.cxxflags.Append (item);
                  when df_index_on            => specs.df_index.Append (item);
                  when extract_only           => specs.extract_only.Append (item);
                  when install_target_on      => specs.install_tgt.Append (item);
                  when keywords_on            => specs.keywords.Append (item);
                  when ldflags_on             => specs.ldflags.Append (item);
                  when lib_depends_on         => specs.lib_deps.Append (item);
                  when make_args_on           => specs.make_args.Append (item);
                  when make_env_on            => specs.make_env.Append (item);
                  when patchfiles_on          => specs.patchfiles.Append (item);
                  when run_depends_on         => specs.run_deps.Append (item);
                  when sub_files_on           => specs.sub_files.Append (item);
                  when sub_list_on            => specs.sub_list.Append (item);
                  when uses_on                => specs.uses.Append (item);
                  when qmake_off |
                       qmake_on               => specs.qmake_args.Append (item);
                  when others => null;
               end case;

               if rec.currently_set_ON then
                  case field is
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
         if rec.currently_set_ON then

            handle_broken;
            augment (build_depends_on,   rec.BUILD_DEPENDS_ON);
            augment (build_target_on,    rec.BUILD_TARGET_ON);
            augment (cflags_on,          rec.CFLAGS_ON);
            augment (cmake_args_on,      rec.CMAKE_ARGS_ON);
            augment (configure_args_on,  rec.CONFIGURE_ARGS_ON);
            augment (configure_args_on,  rec.CONFIGURE_ENV_ON);
            augment (cppflags_on,        rec.CPPFLAGS_ON);
            augment (cxxflags_on,        rec.CXXFLAGS_ON);
            augment (df_index_on,        rec.DF_INDEX_ON);
--              --  EXTRA_PATCHES
--              --  GH stuff (rethink)
            augment (install_target_on,  rec.INSTALL_TARGET_ON);
            augment (keywords_on,        rec.KEYWORDS_ON);
            augment (ldflags_on,         rec.LDFLAGS_ON);
            augment (lib_depends_on,     rec.LIB_DEPENDS_ON);
            augment (make_args_on,       rec.MAKE_ARGS_ON);
            augment (make_env_on,        rec.MAKE_ENV_ON);
            augment (patchfiles_on,      rec.PATCHFILES_ON);
            augment (qmake_on,           rec.QMAKE_ON);
            augment (run_depends_on,     rec.RUN_DEPENDS_ON);
            augment (sub_files_on,       rec.SUB_FILES_ON);
            augment (sub_list_on,        rec.SUB_LIST_ON);
--              --  test-target on
            augment (uses_on,            rec.USES_ON);
         else
            augment (cmake_args_off,     rec.CMAKE_ARGS_OFF);
            augment (configure_args_off, rec.CONFIGURE_ARGS_OFF);
            augment (qmake_off,          rec.QMAKE_OFF);
         end if;
         augment (cmake_bool_f_both,     rec.CMAKE_BOOL_F_BOTH);
         augment (cmake_bool_t_both,     rec.CMAKE_BOOL_T_BOTH);
         augment (configure_enable_both, rec.CONFIGURE_ENABLE_BOTH);
         augment (configure_with_both,   rec.CONFIGURE_WITH_BOTH);

      end copy_option_over;
   begin
      specs.ops_helpers.Iterate (Process => copy_option_over'Access);
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

end Port_Specification.Transform;
