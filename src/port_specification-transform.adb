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
         procedure augment (field : spec_field; directive : string_crate.Vector);

         rec : Option_Helper renames option_crate.Element (position);

         procedure augment (field : spec_field; directive : string_crate.Vector)
         is
            procedure transfer (position : string_crate.Cursor);
            procedure transfer (position : string_crate.Cursor)
            is
               item : HT.Text renames string_crate.Element (position);
            begin
               case field is
                  when sp_build_deps   => specs.build_deps.Append (item);
                  when sp_build_target => specs.build_target.Append (item);
                  when sp_cflags       => specs.cflags.Append (item);
                  when sp_cmake_args   => specs.cmake_args.Append (item);
                  when sp_config_args  => specs.config_args.Append (item);
                  when sp_config_env   => specs.config_env.Append (item);
                  when sp_cppflags     => specs.cppflags.Append (item);
                  when sp_cxxflags     => specs.cxxflags.Append (item);
                  when sp_df_index     => specs.df_index.Append (item);
                  when sp_install_tgt  => specs.install_tgt.Append (item);
                  when sp_keywords     => specs.keywords.Append (item);
                  when sp_ldflags      => specs.ldflags.Append (item);
                  when sp_lib_deps     => specs.lib_deps.Append (item);
                  when sp_make_args    => specs.make_args.Append (item);
                  when sp_make_env     => specs.make_env.Append (item);
                  when sp_patchfiles   => specs.patchfiles.Append (item);
                  when sp_run_deps     => specs.run_deps.Append (item);
                  when sp_sub_files    => specs.sub_files.Append (item);
                  when sp_sub_list     => specs.sub_list.Append (item);
                  when sp_uses         => specs.uses.Append (item);
                  when others => null;
               end case;
            end transfer;
         begin
            directive.Iterate (Process => transfer'Access);
         end augment;
      begin
         if rec.currently_set_ON then
            --  TODO BROKEN_ON
            augment (sp_build_deps,   rec.BUILD_DEPENDS_ON);
            augment (sp_build_target, rec.BUILD_TARGET_ON);
            augment (sp_cflags,       rec.CFLAGS_ON);
            augment (sp_cmake_args,   rec.CMAKE_ARGS_ON);
            augment (sp_config_args,  rec.CONFIGURE_ARGS_ON);
            augment (sp_config_env,   rec.CONFIGURE_ENV_ON);
            augment (sp_cppflags,     rec.CPPFLAGS_ON);
            augment (sp_cxxflags,     rec.CXXFLAGS_ON);
            augment (sp_df_index,     rec.DF_INDEX_ON);
--              --  EXTRA_PATCHES
--              --  GH stuff (rethink)
            augment (sp_install_tgt,  rec.INSTALL_TARGET_ON);
            augment (sp_keywords,     rec.KEYWORDS_ON);
            augment (sp_ldflags,      rec.LDFLAGS_ON);
            augment (sp_lib_deps,     rec.LIB_DEPENDS_ON);
            augment (sp_make_args,    rec.MAKE_ARGS_ON);
            augment (sp_make_env,     rec.MAKE_ENV_ON);
            augment (sp_patchfiles,   rec.PATCHFILES_ON);
--              --  QMAKE_OM
            augment (sp_run_deps,     rec.RUN_DEPENDS_ON);
            augment (sp_sub_files,    rec.SUB_FILES_ON);
            augment (sp_sub_list,     rec.SUB_LIST_ON);
--              --  test-target on
            augment (sp_uses,         rec.USES_ON);
         else
            augment (sp_cmake_args,   rec.CMAKE_ARGS_OFF);
            augment (sp_config_args,  rec.CONFIGURE_ARGS_OFF);
            --  QMAKE_OFF
         end if;
         --  special handing
         --  CMAKE_BOOL_T_BOTH
         --  CMAKE_BOOL_F_BOTH
         --  CONFIGURE_ENABLE_BOTH
         --  CONFIGURE_WITH_BOTH
      end copy_option_over;
   begin
      specs.ops_helpers.Iterate (Process => copy_option_over'Access);
   end apply_directives;


   --------------------------------------------------------------------------------------------
   --  augment
   --------------------------------------------------------------------------------------------
--     procedure augment (specs : in out Portspecs;
--                        main  : spec_field;
--                        option_directive : string_crate.Vector)
--     is
--        procedure copy_over (position : string_crate.Cursor);
--        procedure copy_over (position : string_crate.Cursor)
--        is
--        begin
--           case main is
--              when sp_cflags => specs.cflags.Append (string_crate.Element (position));
--              when others => null;
--           end case;
--        end copy_over;
--     begin
--        option_directive.Iterate (Process => copy_over'Access);
--     end augment;


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
                  if GTE (gen_major  => osrelease, spec_major => spec_version) then
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
   function LTE (gen_major, spec_major : String) return Boolean
   is
      GR : Natural := 999900;
      SR : Natural := 0;
   begin
      if release_format (gen_major) then
         GR := centurian_release (gen_major);
      end if;
      if release_format (spec_major) then
         SR := centurian_release (spec_major);
      end if;
      return (GR <= SR);
   end LTE;


   --------------------------------------------------------------------------------------------
   --  GTE
   --------------------------------------------------------------------------------------------
   function GTE (gen_major, spec_major : String) return Boolean
   is
      GR : Natural := 0;
      SR : Natural := 999900;
   begin
      if release_format (gen_major) then
         GR := centurian_release (gen_major);
      end if;
      if release_format (spec_major) then
         SR := centurian_release (spec_major);
      end if;
      return (GR >= SR);
   end GTE;


end Port_Specification.Transform;
