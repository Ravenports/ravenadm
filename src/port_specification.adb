--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions; use Definitions;
with Utilities;
with Unix;
with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Calendar.Formatting;

package body Port_Specification is

   package UTL renames Utilities;
   package TIO renames Ada.Text_IO;
   package LAT renames Ada.Characters.Latin_1;
   package CAL renames Ada.Calendar;

   --------------------------------------------------------------------------------------------
   --  initialize
   --------------------------------------------------------------------------------------------
   procedure initialize (specs : out Portspecs)
   is
      unset : HT.Text := HT.SUS ("unset");
   begin
      specs.definitions.Clear;

      specs.namebase     := HT.blank;
      specs.version      := HT.blank;
      specs.revision     := 0;
      specs.epoch        := 0;
      specs.job_limit    := 0;
      specs.keywords.Clear;
      specs.variants.Clear;
      specs.taglines.Clear;
      specs.homepage     := HT.blank;
      specs.contacts.Clear;
      specs.dl_sites.Clear;
      specs.distfiles.Clear;
      specs.dist_subdir := HT.blank;
      specs.df_index.Clear;
      specs.patchfiles.Clear;
      specs.subpackages.Clear;
      specs.ops_avail.Clear;
      specs.ops_standard.Clear;
      specs.ops_helpers.Clear;
      specs.optgroups.Clear;
      specs.optgroup_desc.Clear;
      specs.opt_radio.Clear;
      specs.opt_restrict.Clear;
      specs.opt_unlimited.Clear;
      specs.variantopts.Clear;
      specs.options_on.Clear;
      specs.broken.Clear;
      specs.exc_opsys.Clear;
      specs.inc_opsys.Clear;
      specs.exc_arch.Clear;
      specs.uses.Clear;
      specs.uses_base.Clear;
      specs.sub_list.Clear;
      specs.sub_files.Clear;
      specs.extract_only.Clear;
      specs.extract_zip.Clear;
      specs.extract_lha.Clear;
      specs.extract_7z.Clear;
      specs.extract_deb.Clear;
      specs.extract_dirty.Clear;
      specs.extract_head.Clear;
      specs.extract_tail.Clear;
      specs.distname     := HT.blank;

      specs.config_args.Clear;
      specs.config_env.Clear;

      specs.skip_build     := False;
      specs.skip_install   := False;
      specs.skip_ccache    := False;
      specs.destdir_env    := False;
      specs.single_job     := False;
      specs.shift_install  := False;
      specs.debugging_on   := False;
      specs.generated      := False;
      specs.opt_df_index   := False;
      specs.skip_opsys_dep := False;
      specs.infrastructure := False;
      specs.kill_watchdog  := False;
      specs.build_wrksrc   := HT.blank;
      specs.makefile       := HT.blank;
      specs.destdirname    := HT.blank;
      specs.parse_error    := HT.blank;
      specs.make_env.Clear;
      specs.make_args.Clear;
      specs.build_target.Clear;
      specs.build_deps.Clear;
      specs.buildrun_deps.Clear;
      specs.run_deps.Clear;
      specs.opsys_b_deps.Clear;
      specs.opsys_r_deps.Clear;
      specs.opsys_br_deps.Clear;
      specs.opsys_c_uses.Clear;
      specs.cflags.Clear;
      specs.cxxflags.Clear;
      specs.cppflags.Clear;
      specs.ldflags.Clear;
      specs.optimizer_lvl := 2;
      specs.cmake_args.Clear;
      specs.qmake_args.Clear;
      specs.info.Clear;
      specs.install_tgt.Clear;
      specs.plist_sub.Clear;

      specs.make_targets.Clear;
      specs.extra_patches.Clear;
      specs.patch_strip.Clear;
      specs.pfiles_strip.Clear;

      specs.config_outsrc  := False;
      specs.patch_wrksrc   := HT.blank;
      specs.install_wrksrc := HT.blank;
      specs.config_prefix  := HT.blank;
      specs.config_script  := HT.blank;
      specs.config_target  := HT.blank;
      specs.config_wrksrc  := HT.blank;
      specs.config_must    := HT.blank;
      specs.expire_date    := HT.blank;
      specs.deprecated     := HT.blank;
      specs.prefix         := HT.blank;
      specs.last_catchkey  := HT.blank;
      specs.usergroup_pkg  := HT.blank;
      specs.soversion      := HT.blank;
      specs.lic_scheme     := HT.blank;
      specs.used_python    := unset;
      specs.used_perl      := unset;
      specs.used_ruby      := unset;
      specs.used_lua       := unset;

      specs.licenses.Clear;
      specs.lic_names.Clear;
      specs.lic_files.Clear;
      specs.lic_terms.Clear;
      specs.lic_awk.Clear;
      specs.lic_source.Clear;
      specs.users.Clear;
      specs.groups.Clear;
      specs.catch_all.Clear;
      specs.pkg_notes.Clear;
      specs.var_opsys.Clear;
      specs.var_arch.Clear;
      specs.test_tgt.Clear;
      specs.test_args.Clear;
      specs.test_env.Clear;
      specs.extra_rundeps.Clear;
      specs.mandirs.Clear;
      specs.mk_verbatim.Clear;
      specs.broken_ssl.Clear;
      specs.gnome_comps.Clear;
      specs.xorg_comps.Clear;
      specs.sdl_comps.Clear;
      specs.php_extensions.Clear;
      specs.subr_scripts.Clear;
      specs.broken_mysql.Clear;
      specs.broken_pgsql.Clear;

      specs.fatal_rpath := True;

      specs.last_set := so_initialized;

      --  To support cargo module
      specs.cgo_skip_conf  := False;
      specs.cgo_skip_build := False;
      specs.cgo_skip_inst  := False;
      specs.cgo_cargolock  := HT.blank;
      specs.cgo_cargotoml  := HT.blank;
      specs.cgo_cargo_bin  := HT.blank;
      specs.cgo_target_dir := HT.blank;
      specs.cgo_vendor_dir := HT.blank;
      specs.cgo_build_args.Clear;
      specs.cgo_conf_args.Clear;
      specs.cgo_inst_args.Clear;
      specs.cgo_features.Clear;
   end initialize;


   --------------------------------------------------------------------------------------------
   --  do_not_apply_opsys_dependencies
   --------------------------------------------------------------------------------------------
   procedure do_not_apply_opsys_dependencies (specs : in out Portspecs) is
   begin
      specs.skip_opsys_dep := True;
   end do_not_apply_opsys_dependencies;


   --------------------------------------------------------------------------------------------
   --  set_single_string
   --------------------------------------------------------------------------------------------
   procedure set_single_string
     (specs : in out Portspecs;
      field : spec_field;
      value : String)
   is
      procedure verify_entry_is_post_options;

      text_value : HT.Text := HT.SUS (value);

      procedure verify_entry_is_post_options is
      begin
         if spec_order'Pos (specs.last_set) < spec_order'Pos (so_opts_std) then
            raise misordered with field'Img;
         end if;
      end verify_entry_is_post_options;
   begin
      case field is
         when sp_deprecated => null;
         when others =>
            if contains_nonquoted_spaces (value) then
               raise contains_spaces;
            end if;
      end case;
      case field is
         when sp_namebase =>
            if specs.last_set /= so_initialized then
               raise misordered with field'Img;
            end if;
            if invalid_namebase (value, False) then
               raise wrong_value with "illegal characters detected in NAMEBASE";
            end if;
            specs.namebase := text_value;
            specs.last_set := so_namebase;
         when sp_version =>
            if specs.last_set /= so_namebase then
               raise misordered with field'Img;
            end if;
            if invalid_namebase (value, True) then
               raise wrong_value with "illegal characters detected in VERSION";
            end if;
            specs.version := text_value;
            specs.last_set := so_version;
         when sp_homepage =>
            if specs.last_set /= so_taglines then
               raise misordered with field'Img;
            end if;
            if value /= homepage_none and then
              not HT.leads (value, "http://") and then
              not HT.leads (value, "https://")
            then
               raise wrong_value with "Must be '" & homepage_none
                 & "' or valid URL starting with http:// or https://";
            end if;
            specs.homepage := text_value;
            specs.last_set := so_homepage;
         when sp_distsubdir =>
            if specs.last_set /= so_distfiles and then
              specs.last_set /= so_contacts
            then
               raise misordered with field'Img;
            end if;
            specs.dist_subdir := text_value;
            specs.last_set := so_distsubdir;
         when sp_distname =>
            verify_entry_is_post_options;
            specs.distname := text_value;
         when sp_build_wrksrc =>
            verify_entry_is_post_options;
            specs.build_wrksrc := text_value;
         when sp_makefile =>
            verify_entry_is_post_options;
            specs.makefile := text_value;
         when sp_destdirname =>
            verify_entry_is_post_options;
            specs.destdirname := text_value;
         when sp_patch_wrksrc =>
            verify_entry_is_post_options;
            specs.patch_wrksrc := text_value;
         when sp_gnu_cfg_prefix =>
            verify_entry_is_post_options;
            specs.config_prefix := text_value;
         when sp_config_script =>
            verify_entry_is_post_options;
            specs.config_script := text_value;
         when sp_config_target =>
            verify_entry_is_post_options;
            specs.config_target := text_value;
         when sp_config_wrksrc =>
            verify_entry_is_post_options;
            specs.config_wrksrc := text_value;
         when sp_soversion =>
            verify_entry_is_post_options;
            if HT.count_char (value, '.') > 2 then
               raise wrong_value with "SOVERSION has a two period character limit";
            end if;
            specs.soversion := text_value;
         when sp_must_config =>
            verify_entry_is_post_options;
            if value /= boolean_yes and then
              value /= "gnu"
            then
               raise wrong_value with "MUST_CONFIGURE may only be 'yes' or 'gnu'";
            end if;
            specs.config_must := text_value;
         when sp_deprecated =>
            verify_entry_is_post_options;
            specs.deprecated := text_value;
         when sp_expiration =>
            verify_entry_is_post_options;
            if not ISO8601_format (value) then
               raise wrong_value with "Not valid ISO 8601 date for EXPIRATION_DATE";
            end if;
            specs.expire_date := text_value;
         when sp_install_wrksrc =>
            verify_entry_is_post_options;
            specs.install_wrksrc := text_value;
         when sp_prefix =>
            verify_entry_is_post_options;
            specs.prefix := text_value;
         when sp_lic_scheme =>
            verify_entry_is_post_options;
            if value = "solo" or else
              value = "dual" or else
              value = "multi"
            then
               specs.lic_scheme := text_value;
            else
               raise wrong_value with "Not valid license scheme: " & value;
            end if;
         when sp_ug_pkg =>
            verify_entry_is_post_options;
            if not specs.subpackage_exists (value) then
               raise wrong_value with "USERGROUP_SPKG must match a valid subpackage";
            end if;
            if value = spkg_complete then
               raise wrong_value with "USERGROUP_SPKG must not be set to meta-package";
            end if;
            specs.usergroup_pkg := text_value;
         when others =>
            raise wrong_type with field'Img;
      end case;

   end set_single_string;


   --------------------------------------------------------------------------------------------
   --  append_list
   --------------------------------------------------------------------------------------------
   procedure append_list
     (specs : in out Portspecs;
      field : spec_field;
      value : String)
   is
      procedure verify_entry_is_post_options;
      procedure verify_df_index;
      procedure verify_special_exraction;
      procedure set_as_standard_option (Key : HT.Text; Element : in out Option_Helper);

      text_value : HT.Text := HT.SUS (value);

      procedure verify_entry_is_post_options is
      begin
         if spec_order'Pos (specs.last_set) < spec_order'Pos (so_opts_std) then
            raise misordered with field'Img;
         end if;
      end verify_entry_is_post_options;

      procedure verify_df_index is
      begin
         if not specs.dist_index_is_valid (value) then
            raise wrong_value with "distfile index '" & value & "' is not valid";
         end if;
      end verify_df_index;

      procedure verify_special_exraction is
      begin
         if specs.extract_zip.Contains (text_value) or else
           specs.extract_7z.Contains (text_value)  or else
           specs.extract_lha.Contains (text_value)
         then
            raise dupe_list_value with value;
         end if;
      end verify_special_exraction;

      procedure set_as_standard_option (Key : HT.Text; Element : in out Option_Helper) is
      begin
         Element.standard_option := True;
      end set_as_standard_option;
   begin
      if contains_nonquoted_spaces (value) then
         raise contains_spaces;
      end if;
      case field is
         when sp_keywords =>
            if specs.last_set /= so_keywords and then
              specs.last_set /= so_epoch and then
              specs.last_set /= so_revision and then
              specs.last_set /= so_version
            then
               raise misordered with field'Img;
            end if;
            if not keyword_is_valid (value) then
               raise wrong_value with "Keyword '" & value & "' is not recognized";
            end if;
            if specs.keywords.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            specs.keywords.Append (text_value);
            specs.last_set := so_keywords;
         when sp_variants =>
            if specs.last_set /= so_variants and then
              specs.last_set /= so_keywords
            then
               raise misordered with field'Img;
            end if;
            if value'Length > 15 then
               raise wrong_value with "'" & value & "' value is too long (15-char limit)";
            end if;
            if specs.variants.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            if HT.contains (value, "-") then
               raise wrong_value with "hyphens are not allowed in variant names (" & value & ")";
            end if;
            specs.variants.Append (text_value);
            specs.last_set := so_variants;
         when sp_contacts =>
            if specs.last_set /= so_contacts and then
              specs.last_set /= so_homepage
            then
               raise misordered with field'Img;
            end if;
            if not specs.all_taglines_defined then
               raise wrong_value with "Every variant must have SDESC definition.";
            end if;
            if not specs.contacts.Is_Empty then
               if specs.contacts.Contains (HT.SUS (contact_nobody)) then
                  raise wrong_value with "contact '" & contact_nobody & "' must be solitary";
               end if;
               if specs.contacts.Contains (HT.SUS (contact_automaton)) then
                  raise wrong_value with "contact '" & contact_automaton & "' must be solitary";
               end if;
               if value = contact_nobody or else value = contact_automaton then
                  raise wrong_value with "contact '" & value & "' must be solitary";
               end if;
            end if;
            if value /= contact_nobody and then
              value /= contact_automaton and then
              not (HT.contains (value, "_") and then
                   HT.contains (value, "[") and then
                   HT.contains (value, "@") and then
                   HT.contains (value, "]") and then
                  value (value'Last) = LAT.Right_Square_Bracket)
            then
               raise wrong_value with "incorrect contact format of '" & value & "'";
            end if;
            if specs.contacts.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            specs.contacts.Append (text_value);
            specs.last_set := so_contacts;
         when sp_distfiles =>
            if specs.last_set /= so_distfiles and then
              specs.last_set /= so_dl_sites
            then
               raise misordered with field'Img;
            end if;
            if specs.distfiles.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            if not HT.contains (value, ":") then
               raise missing_require with "No download group prefix present in distfile";
            end if;
            declare
               group : String  := HT.part_2 (value, ":");
               key   : HT.Text := HT.SUS (group);
            begin
               if not specs.dl_sites.Contains (key) then
                  raise missing_require with
                    "Download group " & HT.SQ (group) & " hasn't been defined yet.";
               end if;
               if specs.dl_sites.Element (key).list.Is_Empty then
                  raise missing_require with
                    "download group " & HT.SQ (group) & " referenced, but SITES[" & group
                    & "] has not been defined.";
               end if;
            end;
            specs.distfiles.Append (text_value);
            specs.last_set := so_distfiles;
            declare
               group_index : String := HT.int2str (Integer (specs.distfiles.Length));
            begin
               specs.establish_group (sp_ext_head, group_index);
               specs.establish_group (sp_ext_tail, group_index);
            end;
         when sp_df_index =>
            if specs.last_set /= so_df_index and then
              specs.last_set /= so_distsubdir and then
              specs.last_set /= so_distfiles
            then
               raise misordered with field'Img;
            end if;
            if specs.df_index.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            verify_df_index;
            specs.df_index.Append (text_value);
            specs.last_set := so_df_index;
         when sp_opts_avail =>
            if specs.last_set /= so_opts_avail and then
              specs.last_set /= so_subpackages
            then
               raise misordered with field'Img;
            end if;
            if specs.ops_avail.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            if value = options_none then
               if not specs.ops_avail.Is_Empty then
                  raise wrong_value with "'" & options_none & "' must be set first and alone";
               end if;
            else
               if HT.uppercase (value) /= value then
                  raise wrong_value with "option value '" & value & "' is not capitalized";
               end if;
            end if;
            if value'Length > 14 then
               raise wrong_value with "'" & value & "' name is too long (14-char limit)";
            end if;
            if HT.contains (value, ":") then
               raise wrong_value with "'" & value & "' cannot contain colons";
            end if;
            if HT.contains (value, ",") then
               raise wrong_value with "'" & value & "' cannot contain commas";
            end if;
            specs.ops_avail.Append (text_value);
            specs.last_set := so_opts_avail;
            declare
               initial_rec : Option_Helper;
            begin
               initial_rec.option_name := text_value;
               specs.ops_helpers.Insert (text_value, initial_rec);
            end;
         when sp_opts_standard =>
            if specs.last_set /= so_opts_std and then
              specs.last_set /= so_opts_avail
            then
               raise misordered with field'Img;
            end if;
            if specs.ops_standard.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            if value = options_none then
               if not specs.ops_standard.Is_Empty then
                  raise wrong_value with "'" & options_none & "' must be set first and alone";
               end if;
            else
               if not specs.ops_avail.Contains (text_value) then
                  raise wrong_value with "'" & value & "' must be present in OPTIONS_AVAILABLE";
               end if;
               specs.ops_helpers.Update_Element
                 (Position => specs.ops_helpers.Find (text_value),
                  Process  => set_as_standard_option'Access);
            end if;
            specs.ops_standard.Append (text_value);
            specs.last_set := so_opts_std;
         when sp_og_radio | sp_og_restrict | sp_og_unlimited =>
            verify_entry_is_post_options;
            if value'Length > 12 then
               raise wrong_value with "'" & value & "' name is too long (12-char limit)";
            end if;
            if HT.uppercase (value) /= value then
               raise wrong_value with "group name '" & value & "' is not capitalized";
            end if;
            if specs.opt_radio.Contains (text_value) or else
              specs.opt_restrict.Contains (text_value) or else
              specs.opt_unlimited.Contains (text_value)
            then
               raise dupe_list_value with "'" & value & "' has already been used in a group name";
            end if;
            case field is
               when sp_og_radio     => specs.opt_radio.Append (text_value);
               when sp_og_restrict  => specs.opt_restrict.Append (text_value);
               when sp_og_unlimited => specs.opt_unlimited.Append (text_value);
               when others => null;
            end case;
         when sp_exc_opsys =>
            verify_entry_is_post_options;
            if not specs.inc_opsys.Is_Empty then
               raise wrong_value with "NOT_FOR_OPSYS can't be used after ONLY_FOR_OPSYS";
            end if;
            if specs.exc_opsys.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            if not UTL.valid_lower_opsys (value) then
               raise wrong_value with "opsys '" & value & "' is not valid.";
            end if;
            specs.exc_opsys.Append (text_value);
         when sp_inc_opsys =>
            verify_entry_is_post_options;
            if not specs.exc_opsys.Is_Empty then
               raise wrong_value with "ONLY_FOR_OPSYS can't be used after NOT_FOR_OPSYS";
            end if;
            if specs.inc_opsys.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            if not UTL.valid_lower_opsys (value) then
               raise wrong_value with "opsys '" & value & "' is not valid.";
            end if;
            specs.inc_opsys.Append (text_value);
         when sp_exc_arch =>
            verify_entry_is_post_options;
            if specs.exc_arch.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            if not UTL.valid_cpu_arch (value) then
               raise wrong_value with "'" & value & "' is not a valid architecture.";
            end if;
            specs.exc_arch.Append (text_value);
         when sp_ext_only =>
            verify_entry_is_post_options;
            verify_df_index;
            if specs.extract_only.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            specs.extract_only.Append (text_value);
         when sp_ext_7z =>
            verify_entry_is_post_options;
            verify_df_index;
            verify_special_exraction;
            specs.extract_7z.Append (text_value);
         when sp_ext_lha =>
            verify_entry_is_post_options;
            verify_df_index;
            verify_special_exraction;
            specs.extract_lha.Append (text_value);
         when sp_ext_zip =>
            verify_entry_is_post_options;
            verify_df_index;
            verify_special_exraction;
            specs.extract_zip.Append (text_value);
         when sp_ext_deb =>
            verify_entry_is_post_options;
            verify_df_index;
            verify_special_exraction;
            specs.extract_deb.Append (text_value);
         when sp_ext_dirty =>
            verify_entry_is_post_options;
            verify_df_index;
            if specs.extract_dirty.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            specs.extract_dirty.Append (text_value);
         when sp_make_args =>
            verify_entry_is_post_options;
            specs.make_args.Append (text_value);
         when sp_make_env =>
            verify_entry_is_post_options;
            specs.make_env.Append (text_value);
         when sp_build_target =>
            verify_entry_is_post_options;
            specs.build_target.Append (text_value);
         when sp_install_tgt =>
            verify_entry_is_post_options;
            specs.install_tgt.Append (text_value);
         when sp_test_tgt =>
            verify_entry_is_post_options;
            specs.test_tgt.Append (text_value);
         when sp_test_env =>
            verify_entry_is_post_options;
            specs.test_env.Append (text_value);
         when sp_cflags =>
            verify_entry_is_post_options;
            specs.cflags.Append (text_value);
         when sp_cxxflags =>
            verify_entry_is_post_options;
            specs.cxxflags.Append (text_value);
         when sp_cppflags =>
            verify_entry_is_post_options;
            specs.cppflags.Append (text_value);
         when sp_ldflags =>
            verify_entry_is_post_options;
            specs.ldflags.Append (text_value);
         when sp_config_args =>
            verify_entry_is_post_options;
            specs.config_args.Append (text_value);
         when sp_config_env =>
            verify_entry_is_post_options;
            specs.config_env.Append (text_value);
         when sp_patchfiles =>
            verify_entry_is_post_options;
            specs.patchfiles.Append (text_value);
         when sp_pfiles_strip =>
            verify_entry_is_post_options;
            specs.pfiles_strip.Append (text_value);
         when sp_patch_strip =>
            verify_entry_is_post_options;
            specs.patch_strip.Append (text_value);
         when sp_extra_patches =>
            verify_entry_is_post_options;
            specs.extra_patches.Append (text_value);
         when sp_plist_sub =>
            verify_entry_is_post_options;
            specs.plist_sub.Append (text_value);
         when sp_users =>
            verify_entry_is_post_options;
            specs.users.Append (text_value);
         when sp_groups =>
            verify_entry_is_post_options;
            specs.groups.Append (text_value);
         when sp_sub_list =>
            verify_entry_is_post_options;
            specs.sub_list.Append (text_value);
         when sp_sub_files =>
            verify_entry_is_post_options;
            specs.sub_files.Append (text_value);
         when sp_mandirs =>
            verify_entry_is_post_options;
            specs.mandirs.Append (text_value);
         when sp_test_args =>
            verify_entry_is_post_options;
            specs.test_args.Append (text_value);
         when sp_cmake_args =>
            verify_entry_is_post_options;
            specs.cmake_args.Append (text_value);
         when sp_qmake_args =>
            verify_entry_is_post_options;
            specs.qmake_args.Append (text_value);
         when sp_verbatim =>
            verify_entry_is_post_options;
            specs.mk_verbatim.Append (text_value);
         when sp_build_deps | sp_buildrun_deps | sp_run_deps =>
            verify_entry_is_post_options;
            if not valid_dependency_format (value) then
               raise wrong_value with "invalid dependency format '" & value & "'";
            end if;
            if specs.build_deps.Contains (text_value) or else
              specs.buildrun_deps.Contains (text_value) or else
              specs.run_deps.Contains (text_value)
            then
               raise dupe_list_value with "Duplicate dependency '" & value & "'";
            end if;
            case field is
               when sp_build_deps    => specs.build_deps.Append (text_value);
               when sp_buildrun_deps => specs.buildrun_deps.Append (text_value);
               when sp_run_deps      => specs.run_deps.Append (text_value);
               when others => null;
            end case;
         when sp_uses =>
            verify_entry_is_post_options;
            if not valid_uses_module (value) then
               raise wrong_value with "invalid USES module '" & value & "'";
            end if;
            for x in smodules'Range loop
               if HT.leads (value, base_module (x)) and then
                 specs.module_subpackage_failed (base_module (x), value)
               then
                  raise wrong_value
                    with base_module (x) & " USES module doesn't have a subpackage argument";
               end if;
            end loop;
            declare
               errmsg        : HT.Text;
               text_stripped : HT.Text := HT.SUS (HT.part_1 (value, ":"));
               macfix        : HT.Text := HT.SUS ("macfix");
            begin
               if specs.extra_uses_modules_sanity_check_passes (value, errmsg) then
                  if specs.uses_base.Contains (text_stripped) then
                     raise dupe_list_value with "Duplicate USES base module '" & value & "'";
                  else
                     specs.uses_base.Append (text_stripped);
                  end if;
                  specs.uses.Append (text_value);
               else
                  raise wrong_value with HT.USS (errmsg);
               end if;
            end;
         when sp_info =>
            verify_entry_is_post_options;
            if specs.info.Contains (text_value) then
               raise dupe_list_value with "Duplicate INFO entry '" & value & "'";
            end if;
            declare
               msg : String := specs.info_page_check_message (value);
            begin
               if not HT.IsBlank (msg) then
                  raise wrong_type with msg;
               end if;
            end;
            if not specs.valid_info_page (value) then
               raise wrong_value with "INFO subdirectories must match on every entry";
            end if;
            specs.info.Append (text_value);
         when sp_gnome =>
            verify_entry_is_post_options;
            declare
               comp : gnome_type := determine_gnome_component (value);
            begin
               if comp = invalid_component then
                  raise wrong_value with "GNOME_COMPONENTS component not recognized: " & value;
               end if;
            end;
            specs.gnome_comps.Append (text_value);
         when sp_xorg =>
            verify_entry_is_post_options;
            declare
               comp : xorg_type := determine_xorg_component (value);
            begin
               if comp = invalid_component then
                  raise wrong_value with "XORG_COMPONENTS component not recognized: " & value;
               end if;
            end;
            specs.xorg_comps.Append (text_value);
         when sp_sdl =>
            verify_entry_is_post_options;
            declare
               comp : sdl_type := determine_sdl_component (value);
            begin
               if comp = invalid_component then
                  raise wrong_value with "SDL_COMPONENTS component not recognized: " & value;
               end if;
            end;
            specs.sdl_comps.Append (text_value);
         when sp_phpext =>
            verify_entry_is_post_options;
            declare
               phpext : phpext_type := determine_php_extension (value);
            begin
               if phpext = invalid_extension then
                  raise wrong_value with "PHP_EXTENSIONS extension not recognized: " & value;
               end if;
            end;
            specs.php_extensions.Append (text_value);
         when sp_rcscript =>
            verify_entry_is_post_options;
            if not HT.contains (value, ":") then
               raise wrong_value with "Invalid RC_SUBR format, must be filename:subpackage";
            end if;
            declare
               filename : String := HT.part_1 (value, ":");
               testpkg : String := HT.part_2 (value, ":");
            begin
               if not specs.subpackage_exists (testpkg) then
                  raise wrong_type with "RC_SUBR subpackage unrecognized: " & testpkg;
               end if;
            end;
            specs.subr_scripts.Append (text_value);
         when sp_licenses =>
            verify_entry_is_post_options;
            if not HT.contains (value, ":") then
               raise wrong_value with "Invalid LICENSE format, must be name:subpackage";
            end if;
            declare
               testlic : String := HT.part_1 (value, ":");
               testpkg : String := HT.part_2 (value, ":");
               lic    : license_type := determine_license (testlic);
            begin
               if lic = INVALID then
                  raise wrong_value with "LICENSE not recognized: " & testlic;
               end if;
               if not specs.subpackage_exists (testpkg) then
                  raise wrong_value with "LICENSE subpackage unrecognized: " & testpkg;
               end if;
            end;
            specs.licenses.Append (text_value);
         when sp_lic_name =>
            verify_entry_is_post_options;
            if not HT.contains (value, ":") then
               raise wrong_value with "Invalid LICENSE_NAME format, must be CUSTOMx:" &
                 LAT.Quotation & "name" & LAT.Quotation;
            end if;
            declare
               procedure scan (position : string_crate.Cursor);

               testlic : String := HT.part_1 (value, ":");
               name    : String := HT.part_2 (value, ":");
               lic     : license_type := determine_license (testlic);
               found   : Boolean := False;

               procedure scan (position : string_crate.Cursor)
               is
                  license_val : String := HT.USS (string_crate.Element (position));
                  name_part   : String := HT.part_1 (license_val, ":");
               begin
                  if not found then
                     if testlic = name_part then
                        found := True;
                     end if;
                  end if;
               end scan;
            begin
               case lic is
                  when CUSTOM1 | CUSTOM2 | CUSTOM3 | CUSTOM4 => null;
                  when others =>
                     raise wrong_value with "LICENSE_NAME only definable with CUSTOMx license";
               end case;
               if name (name'First) /= LAT.Quotation or else
                 name (name'Last) /= LAT.Quotation
               then
                  raise wrong_value with "LICENSE_NAME value must be quoted: " & name;
               end if;
               specs.licenses.Iterate (scan'Access);
               if not found then
                  raise wrong_value with "LICENSE " & testlic & " not previously defined " &
                    "for " & value;
               end if;
            end;
            specs.lic_names.Append (text_value);
         when sp_lic_file =>
            verify_entry_is_post_options;
            if not HT.contains (value, ":") then
               raise wrong_value with "Invalid LICENSE_FILE format, must be LICENSE:path";
            end if;
            declare
               procedure scan (position : string_crate.Cursor);

               testlic : String := HT.part_1 (value, ":");
               path    : String := HT.part_2 (value, ":");
               lic     : license_type := determine_license (testlic);
               found   : Boolean := False;

               procedure scan (position : string_crate.Cursor)
               is
                  license_val : String := HT.USS (string_crate.Element (position));
                  name_part   : String := HT.part_1 (license_val, ":");
               begin
                  if not found then
                     if testlic = name_part then
                        found := True;
                     end if;
                  end if;
               end scan;
            begin
               if lic = INVALID then
                  raise wrong_value with "License not recognized: " & testlic;
               end if;
               if path /= "stock" and then
                 not HT.contains (path, "/")
               then
                  raise wrong_value with "LICENSE_FILE path component missing directory " &
                    "separator: " & path;
               end if;
               specs.licenses.Iterate (scan'Access);
               if not found then
                  raise wrong_value with "License " & testlic & " not previously defined " &
                    " for " & value;
               end if;
            end;
            specs.lic_files.Append (text_value);
         when sp_lic_terms =>
            verify_entry_is_post_options;
            if not HT.contains (value, ":") then
               raise wrong_value with "Invalid LICENSE_TERMS format, must be subpackage:path";
            end if;
            declare
               testpkg  : String := HT.part_1 (value, ":");
               testpath : String := HT.part_2 (value, ":");
            begin
               if not specs.subpackage_exists (testpkg) then
                  raise wrong_value with "LICENSE_TERMS subpackage unrecognized: " & testpkg;
               end if;
               if not HT.contains (testpath, "/") then
                  raise wrong_value with "LICENSE_TERMS path component missing directory " &
                    "separator: " & testpath;
               end if;
            end;
            specs.lic_terms.Append (text_value);
         when sp_lic_src =>
            verify_entry_is_post_options;
            if not HT.contains (value, ":") then
               raise wrong_value with "Invalid LICENSE_SOURCE format, must be LICENSE:path";
            end if;
            declare
               procedure scan (position : string_crate.Cursor);

               testlic : String := HT.part_1 (value, ":");
               path    : String := HT.part_2 (value, ":");
               lic     : license_type := determine_license (testlic);
               found   : Boolean := False;

               procedure scan (position : string_crate.Cursor)
               is
                  license_val : String := HT.USS (string_crate.Element (position));
                  name_part   : String := HT.part_1 (license_val, ":");
               begin
                  if not found then
                     if testlic = name_part then
                        found := True;
                     end if;
                  end if;
               end scan;
            begin
               if not HT.contains (path, "/") then
                  raise wrong_value with "LICENSE_SOURCE path component missing directory " &
                    "separator: " & path;
               end if;
               if testlic /= "TERMS" then
                  if lic = INVALID then
                     raise wrong_value with "License not recognized: " & testlic;
                  end if;
                  specs.licenses.Iterate (scan'Access);
                  if not found then
                     raise wrong_value with
                       "License " & testlic & " not previously defined for " & value;
                  end if;
               end if;
            end;
            specs.lic_source.Append (text_value);
         when sp_lic_awk =>
            verify_entry_is_post_options;
            if not HT.contains (value, ":") then
               raise wrong_value with "Invalid LICENSE_AWK format, must be LICENSE:" &
                 LAT.Quotation & "awk code" & LAT.Quotation;
            end if;
            declare
               procedure scan (position : string_crate.Cursor);

               testlic : String := HT.part_1 (value, ":");
               name    : String := HT.part_2 (value, ":");
               lic     : license_type := determine_license (testlic);
               found   : Boolean := False;

               procedure scan (position : string_crate.Cursor)
               is
                  license_val : String := HT.USS (string_crate.Element (position));
                  name_part   : String := HT.part_1 (license_val, ":");
               begin
                  if not found then
                     if testlic = name_part then
                        found := True;
                     end if;
                  end if;
               end scan;
            begin
               if name (name'First) /= LAT.Quotation or else
                 name (name'Last) /= LAT.Quotation
               then
                  raise wrong_value with "LICENSE_AWK value must be quoted: " & name;
               end if;
               if testlic /= "TERMS" then
                  if lic = INVALID then
                     raise wrong_value with "License not recognized: " & testlic;
                  end if;
                  specs.licenses.Iterate (scan'Access);
                  if not found then
                     raise wrong_value with
                       "LICENSE " & testlic & " not previously defined for " & value;
                  end if;
               end if;
            end;
            specs.lic_awk.Append (text_value);
         when sp_broken_ssl =>
            verify_entry_is_post_options;
            if specs.broken_ssl.Contains (text_value) then
               raise dupe_list_value with "Duplicate item '" & value & "'";
            end if;
            if value = "openssl10" or else
              value = "openssl11" or else
              value = "openssl30" or else
              value = "libressl" or else
              value = "libressl-devel"
            then
               specs.broken_ssl.Append (text_value);
            else
               raise wrong_value with "invalid BROKEN_SSL setting '" & value & "'";
            end if;
         when sp_broken_mysql =>
            verify_entry_is_post_options;
            if specs.broken_mysql.Contains (text_value) then
               raise dupe_list_value with "Duplicate item '" & value & "'";
            end if;
            if valid_broken_mysql_value (value) then
               specs.broken_mysql.Append (text_value);
            else
               raise wrong_value with "invalid BROKEN_MYSQL setting '" & value & "'";
            end if;
         when sp_broken_pgsql =>
            verify_entry_is_post_options;
            if specs.broken_pgsql.Contains (text_value) then
               raise dupe_list_value with "Duplicate item '" & value & "'";
            end if;
            if valid_broken_pgsql_value (value) then
               specs.broken_pgsql.Append (text_value);
            else
               raise wrong_value with "invalid BROKEN_PGSQL setting '" & value & "'";
            end if;
         when sp_cgo_cargs =>
            verify_entry_is_post_options;
            specs.cgo_conf_args.Append (text_value);
         when sp_cgo_bargs =>
            verify_entry_is_post_options;
            specs.cgo_build_args.Append (text_value);
         when sp_cgo_iargs =>
            verify_entry_is_post_options;
            specs.cgo_inst_args.Append (text_value);
         when sp_cgo_feat =>
            verify_entry_is_post_options;
            specs.cgo_features.Append (text_value);
         when others =>
            raise wrong_type with field'Img;
      end case;

   end append_list;


   --------------------------------------------------------------------------------------------
   --  append_array
   --------------------------------------------------------------------------------------------
   procedure establish_group
     (specs : in out Portspecs;
      field : spec_field;
      group : String)
   is
      text_group : HT.Text := HT.SUS (group);
      initial_rec : group_list;
   begin
      if HT.contains (S => group, fragment => " ") then
         raise contains_spaces;
      end if;
      initial_rec.group := text_group;
      case field is
         when sp_dl_groups =>
            if specs.last_set /= so_dl_groups and then
              specs.last_set /= so_contacts
            then
               raise misordered with field'Img;
            end if;
            if specs.dl_sites.Is_Empty then
               if group /= dlgroup_main and then
                 group /= dlgroup_none
               then
                  raise wrong_value with "First download group must be '" & dlgroup_main &
                    "' or '" & dlgroup_none & "'";
               end if;
            else
               if group = dlgroup_none then
                  raise wrong_value with "download group '" & group &
                    "' follows group definition";
               end if;
               if group = dlgroup_main then
                  raise wrong_value with "'" & group & "' download group must be " &
                    "defined earlier";
               end if;
            end if;
            if group'Length > 15 then
               raise wrong_value with "'" & group & "' value is too long (15-char limit)";
            end if;
            if HT.contains (group, "+") then
               raise wrong_type with "'" & group & "' contains illegal character '+'";
            end if;
            if specs.dl_sites.Contains (text_group) then
               raise dupe_list_value with group;
            end if;
            specs.dl_sites.Insert (text_group, initial_rec);
            specs.last_set := so_dl_groups;
         when sp_subpackages =>
            --  variant, order, length and uniqueness already checked
            --  don't update last_set either
            specs.subpackages.Insert (text_group, initial_rec);
         when sp_vopts =>
            --  variant, order, length and uniqueness already checked
            --  don't update last_set either
            specs.variantopts.Insert (text_group, initial_rec);
         when sp_ext_head =>
            specs.extract_head.Insert (text_group, initial_rec);
         when sp_ext_tail =>
            specs.extract_tail.Insert (text_group, initial_rec);
         when sp_makefile_targets =>
            specs.make_targets.Insert (text_group, initial_rec);
         when sp_options_on =>
            specs.options_on.Insert (text_group, initial_rec);
         when sp_broken =>
            specs.broken.Insert (text_group, initial_rec);
         when sp_var_opsys =>
            specs.var_opsys.Insert (text_group, initial_rec);
         when sp_var_arch =>
            specs.var_arch.Insert (text_group, initial_rec);
         when sp_exrun =>
            specs.extra_rundeps.Insert (text_group, initial_rec);
         when sp_catchall =>
            specs.catch_all.Insert (text_group, initial_rec);
         when sp_opt_descr =>
            specs.optgroup_desc.Insert (text_group, initial_rec);
         when sp_opt_group =>
            specs.optgroups.Insert (text_group, initial_rec);
         when sp_os_bdep =>
            specs.opsys_b_deps.Insert (text_group, initial_rec);
         when sp_os_rdep =>
            specs.opsys_r_deps.Insert (text_group, initial_rec);
         when sp_os_brdep =>
            specs.opsys_br_deps.Insert (text_group, initial_rec);
         when sp_os_uses =>
            specs.opsys_c_uses.Insert (text_group, initial_rec);
         when others =>
            raise wrong_type with field'Img;
      end case;
   end establish_group;


   --------------------------------------------------------------------------------------------
   --  append_array
   --------------------------------------------------------------------------------------------
   procedure append_array
     (specs : in out Portspecs;
      field : spec_field;
      key   : String;
      value : String;
      allow_spaces : Boolean)
   is
      procedure grow (Key : HT.Text; Element : in out group_list);
      procedure verify_entry_is_post_options;

      text_key   : HT.Text := HT.SUS (key);
      text_value : HT.Text := HT.SUS (value);

      procedure grow (Key : HT.Text; Element : in out group_list) is
      begin
         Element.list.Append (text_value);
      end grow;

      procedure verify_entry_is_post_options is
      begin
         if spec_order'Pos (specs.last_set) < spec_order'Pos (so_opts_std) then
            raise misordered with field'Img;
         end if;
      end verify_entry_is_post_options;
   begin
      if not allow_spaces and then
        HT.contains (S => value, fragment => " ")
      then
         raise contains_spaces;
      end if;
      case field is
         when sp_taglines =>
            if specs.last_set /= so_taglines and then
              specs.last_set /= so_variants
            then
               raise misordered with field'Img;
            end if;
            if specs.taglines.Contains (text_key) then
               raise dupe_spec_key with key & " (SDESC)";
            end if;
            --  SDESC requirements checked by caller.  Assume string is legal.
            specs.taglines.Insert (Key      => text_key,
                                   New_Item => text_value);
            specs.last_set := so_taglines;
         when sp_dl_sites =>
            if specs.last_set /= so_dl_sites and then
              specs.last_set /= so_dl_groups
            then
               raise misordered with field'Img;
            end if;
            if not specs.dl_sites.Contains (text_key) then
               raise missing_group with key;
            end if;
            if specs.dl_sites.Element (text_key).list.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            specs.dl_sites.Update_Element (Position => specs.dl_sites.Find (text_key),
                                           Process  => grow'Access);
            specs.last_set := so_dl_sites;
         when sp_subpackages =>
            if spec_order'Pos (specs.last_set) > spec_order'Pos (so_subpackages) or else
              spec_order'Pos (specs.last_set) < spec_order'Pos (so_dl_groups)
            then
               raise misordered with field'Img;
            end if;
            if not specs.subpackages.Contains (text_key) then
               raise missing_group with key;
            end if;
            if specs.subpackages.Element (text_key).list.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            if value = spkg_complete and then
              not specs.subpackages.Element (text_key).list.Is_Empty
            then
               raise wrong_value with "The '" & value & "' subpackage must be set first";
            end if;
            if key'Length > 15 then
               raise wrong_value with "'" & value & "' value is too long (15-char limit)";
            end if;
            if HT.contains (value, "-") then
               raise wrong_value with "subpackage names cannot contain hyphens (" & value & ")";
            end if;
            specs.subpackages.Update_Element (Position => specs.subpackages.Find (text_key),
                                              Process  => grow'Access);
            specs.last_set := so_subpackages;
         when sp_vopts =>
            if specs.last_set /= so_vopts and then
              specs.last_set /= so_opts_std
            then
               raise misordered with field'Img;
            end if;
            if not specs.variantopts.Contains (text_key) then
               raise missing_group with key;
            end if;
            declare
               strlast : Natural;
               WON     : HT.Text;
               WOFF    : HT.Text;
            begin
               if HT.trails (value, "=ON") then
                  strlast := value'Last - 3;
                  WON     := text_value;
                  WOFF    := HT.SUS (value (value'First .. strlast) & "=OFF");
               elsif HT.trails (value, "=OFF") then
                  strlast := value'Last - 4;
                  WOFF    := text_value;
                  WON     := HT.SUS (value (value'First .. strlast) & "=ON");
               else
                  raise wrong_value with "'" & value & "' doesn't end in '=ON' or '=OFF'";
               end if;
               if specs.variantopts.Element (text_key).list.Contains (WON) or else
                 specs.variantopts.Element (text_key).list.Contains (WOFF)
               then
                  raise dupe_list_value with value;
               end if;
               if not specs.ops_avail.Contains (HT.SUS (value (value'First .. strlast))) then
                  raise wrong_value with "'" & value (value'First .. strlast)
                    & "' was not present in OPTIONS_AVAILABLE";
               end if;
            end;
            specs.variantopts.Update_Element (Position => specs.variantopts.Find (text_key),
                                              Process  => grow'Access);
            specs.last_set := so_vopts;
         when sp_ext_head =>
            verify_entry_is_post_options;
            if not specs.extract_head.Contains (text_key) then
               raise missing_group with key;
            end if;
            if not specs.extract_head.Element (text_key).list.Is_Empty then
               raise wrong_value with "Only 1 entry is allowed";
            end if;
            specs.extract_head.Update_Element (Position => specs.extract_head.Find (text_key),
                                               Process  => grow'Access);
         when sp_ext_tail =>
            verify_entry_is_post_options;
            if not specs.extract_tail.Contains (text_key) then
               raise missing_group with key;
            end if;
            if not specs.extract_tail.Element (text_key).list.Is_Empty then
               raise wrong_value with "Only 1 entry is allowed";
            end if;
            specs.extract_tail.Update_Element (Position => specs.extract_tail.Find (text_key),
                                               Process  => grow'Access);
         when sp_makefile_targets =>
            verify_entry_is_post_options;
            if not specs.make_targets.Contains (text_key) then
               raise missing_group with key;
            end if;
            specs.make_targets.Update_Element (Position => specs.make_targets.Find (text_key),
                                               Process  => grow'Access);
         when sp_options_on =>
            verify_entry_is_post_options;
            if not specs.options_on.Contains (text_key) then
               --  Group already validated, so create if it dosn't exist
               specs.establish_group (sp_options_on, key);
               --  "all" must exist if non-"all" keys are used though
               if not specs.options_on.Contains (HT.SUS (options_all)) then
                  specs.establish_group (sp_options_on, options_all);
               end if;
            end if;
            if not specs.valid_OPT_ON_value (key, value) then
               raise wrong_value with "OPT_ON value '" & value &
                 "' either doesn't match option list, is present in 'all' section, " &
                 "or it is misformatted";
            end if;
            specs.options_on.Update_Element (Position => specs.options_on.Find (text_key),
                                             Process  => grow'Access);
         when sp_broken =>
            verify_entry_is_post_options;
            if not specs.broken.Contains (text_key) then
               --  Group already validated, so create if it dosn't exist
               specs.establish_group (sp_broken, key);
            end if;
            specs.broken.Update_Element (Position => specs.broken.Find (text_key),
                                         Process  => grow'Access);
         when sp_catchall =>
            verify_entry_is_post_options;
            if not specs.catch_all.Contains (text_key) then
               specs.establish_group (sp_catchall, key);
            end if;
            specs.catch_all.Update_Element (Position => specs.catch_all.Find (text_key),
                                            Process  => grow'Access);
            specs.last_catchkey := text_key;
         when sp_var_opsys =>
            verify_entry_is_post_options;
            if specs.var_opsys.Contains (text_key) and then
              specs.var_opsys.Element (text_key).list.Contains (text_value)
            then
               raise wrong_value with "duplicate definition: " & key & "=" & value;
            end if;
            if not specs.var_opsys.Contains (text_key) then
               specs.establish_group (sp_var_opsys, key);
            end if;
            specs.var_opsys.Update_Element (Position => specs.var_opsys.Find (text_key),
                                            Process  => grow'Access);
         when sp_var_arch =>
            verify_entry_is_post_options;
            if specs.var_arch.Contains (text_key) and then
              specs.var_arch.Element (text_key).list.Contains (text_value)
            then
               raise wrong_value with "duplicate definition: " & key & "=" & value;
            end if;
            if not specs.var_arch.Contains (text_key) then
               specs.establish_group (sp_var_arch, key);
            end if;
            specs.var_arch.Update_Element (Position => specs.var_arch.Find (text_key),
                                           Process  => grow'Access);
         when sp_exrun =>
            verify_entry_is_post_options;
            if not specs.subpackage_exists (key) then
               raise wrong_type with "subpackage key '" & key & "' has not been defined.";
            end if;
            if not valid_dependency_format (value) then
               if value /= "ssl" and then
                 value /= "python" and then
                 value /= "tcl" and then
                 value /= "perl" and then
                 value /= "mysql" and then
                 value /= "pgsql"
               then
                  raise wrong_value with "invalid dependency format '" & value & "'";
               end if;
            end if;
            if specs.extra_rundeps.Contains (text_key) and then
              specs.extra_rundeps.Element (text_key).list.Contains (text_value)
            then
               raise wrong_value with "duplicate definition: " & key & "=" & value;
            end if;
            if not specs.extra_rundeps.Contains (text_key) then
               specs.establish_group (sp_exrun, key);
            end if;
            specs.extra_rundeps.Update_Element (Position => specs.extra_rundeps.Find (text_key),
                                                Process  => grow'Access);
         when sp_opt_descr =>
            verify_entry_is_post_options;
            if not specs.opt_radio.Contains (text_key) and then
              not specs.opt_restrict.Contains (text_key) and then
              not specs.opt_unlimited.Contains (text_key)
            then
               raise missing_group with key;
            end if;
            if not specs.optgroup_desc.Contains (text_key) then
               specs.establish_group (sp_opt_descr, key);
            end if;
            if not specs.optgroup_desc.Element (text_key).list.Is_Empty then
               raise wrong_value with "Group " & key & " description already defined";
            end if;
            if HT.contains (value, ":") then
               raise wrong_value with "Group description cannot contain colon characters";
            end if;
            specs.optgroup_desc.Update_Element (Position => specs.optgroup_desc.Find (text_key),
                                                Process  => grow'Access);
         when sp_opt_group =>
            verify_entry_is_post_options;
            if not specs.opt_radio.Contains (text_key) and then
              not specs.opt_restrict.Contains (text_key) and then
              not specs.opt_unlimited.Contains (text_key)
            then
               raise missing_group with key;
            end if;
            if not specs.ops_standard.Contains (text_value) then
               raise wrong_value with "Option '" & value & "' is not a member of OPTIONS_STANDARD";
            end if;
            if not specs.optgroups.Contains (text_key) then
               specs.establish_group (sp_opt_group, key);
            end if;
            if specs.option_already_in_group (value) then
               raise wrong_value with "Option '" & value & "' already belongs to an option group";
            end if;
            specs.optgroups.Update_Element (Position => specs.optgroups.Find (text_key),
                                            Process  => grow'Access);
         when sp_os_bdep | sp_os_rdep | sp_os_brdep =>
            verify_entry_is_post_options;
            if not valid_dependency_format (value) then
               raise wrong_value with "invalid dependency format '" & value & "'";
            end if;

            declare
               good : Boolean := True;
            begin
               case field is
                  when sp_os_bdep =>
                     good := not specs.opsys_b_deps.Contains (text_key) or else
                       not specs.opsys_b_deps.Element (text_key).list.Contains (text_value);
                  when sp_os_brdep =>
                     good := not specs.opsys_br_deps.Contains (text_key) or else
                       not specs.opsys_br_deps.Element (text_key).list.Contains (text_value);
                  when sp_os_rdep =>
                     good := not specs.opsys_r_deps.Contains (text_key) or else
                       not specs.opsys_r_deps.Element (text_key).list.Contains (text_value);
                  when others => null;
               end case;
               if not good then
                  raise wrong_value with "duplicate definition: " & key & "=" & value;
               end if;
            end;

            case field is
               when sp_os_bdep =>
                  if not specs.opsys_b_deps.Contains (text_key) then
                     specs.establish_group (sp_os_bdep, key);
                  end if;
                  specs.opsys_b_deps.Update_Element
                    (Position => specs.opsys_b_deps.Find (text_key),
                     Process  => grow'Access);
               when sp_os_brdep =>
                  if not specs.opsys_br_deps.Contains (text_key) then
                     specs.establish_group (sp_os_brdep, key);
                  end if;
                  specs.opsys_br_deps.Update_Element
                    (Position => specs.opsys_br_deps.Find (text_key),
                     Process  => grow'Access);
               when sp_os_rdep =>
                  if not specs.opsys_r_deps.Contains (text_key) then
                     specs.establish_group (sp_os_rdep, key);
                  end if;
                  specs.opsys_r_deps.Update_Element
                    (Position => specs.opsys_r_deps.Find (text_key),
                     Process  => grow'Access);
               when others => null;
            end case;
         when sp_os_uses =>
            verify_entry_is_post_options;
            if not valid_uses_module (value) then
               raise wrong_value with "invalid USES module '" & value & "'";
            end if;
            if specs.uses.Contains (text_value) or else
              (specs.opsys_c_uses.Contains (text_key) and then
               specs.opsys_c_uses.Element (text_key).list.Contains (text_value))
            then
               raise dupe_list_value with "Duplicate USES module '" & value & "'";
            end if;
            for x in smodules'Range loop
               if HT.leads (value, base_module (x)) and then
                 specs.module_subpackage_failed (base_module (x), value)
               then
                  raise wrong_value
                    with base_module (x) & " USES module doesn't have a subpackage argument";
               end if;
            end loop;
            declare
               errmsg : HT.Text;
            begin
               if not specs.extra_uses_modules_sanity_check_passes (value, errmsg) then
                  raise wrong_value with HT.USS (errmsg);
               end if;
            end;
            if not specs.opsys_c_uses.Contains (text_key) then
               specs.establish_group (sp_os_uses, key);
            end if;
            specs.opsys_c_uses.Update_Element
              (Position => specs.opsys_c_uses.Find (text_key),
               Process  => grow'Access);
         when others =>
            raise wrong_type with field'Img;
      end case;
   end append_array;


   --------------------------------------------------------------------------------------------
   --  set_natural_integer
   --------------------------------------------------------------------------------------------
   procedure set_natural_integer
     (specs : in out Portspecs;
      field : spec_field;
      value : Natural) is
      procedure verify_entry_is_post_options;
      procedure verify_entry_is_post_options is
      begin
         if spec_order'Pos (specs.last_set) < spec_order'Pos (so_opts_std) then
            raise misordered with field'Img;
         end if;
      end verify_entry_is_post_options;
   begin
      case field is
         when sp_revision =>
            if specs.last_set /= so_version then
               raise misordered with field'Img;
            end if;
            specs.revision := value;
            specs.last_set := so_revision;
         when sp_epoch =>
            if specs.last_set /= so_revision and then
              specs.last_set /= so_version
            then
               raise misordered with field'Img;
            end if;
            specs.epoch := value;
            specs.last_set := so_epoch;
         when sp_opt_level =>
            verify_entry_is_post_options;
            if value > 3 then
               raise wrong_value with "OPTIMIZER_LEVEL is limited to 3";
            end if;
            specs.optimizer_lvl := value;
         when sp_job_limit =>
            verify_entry_is_post_options;
            if value < 1 then
               raise wrong_value with "MAKE_JOBS_NUMBER_LIMIT must be at least 1";
            end if;
            if value > Natural (cpu_range'Last) then
               raise wrong_value with "MAKE_JOBS_NUMBER_LIMIT is limited to " & cpu_range'Last'Img;
            end if;
            specs.job_limit := value;
         when others =>
            raise wrong_type with field'Img;
      end case;
   end set_natural_integer;


   --------------------------------------------------------------------------------------------
   --  set_boolean
   --------------------------------------------------------------------------------------------
   procedure set_boolean
     (specs : in out Portspecs;
      field : spec_field;
      value : Boolean) is
   begin
      case field is
         when sp_skip_build =>
            specs.skip_build := value;
         when sp_destdir_env =>
            specs.destdir_env := value;
         when sp_single_job =>
            specs.single_job := value;
         when sp_skip_install =>
            specs.skip_install := value;
         when sp_cfg_outsrc =>
            specs.config_outsrc := value;
         when sp_inst_tchain =>
            specs.shift_install := value;
         when sp_skip_ccache =>
            specs.skip_ccache := value;
         when sp_rpath_warning =>
            specs.fatal_rpath := False;
         when sp_debugging =>
            specs.debugging_on := value;
         when sp_generated =>
            specs.generated := value;
         when sp_infra =>
            specs.infrastructure := value;
         when sp_killdog =>
            specs.kill_watchdog := value;
         when sp_cgo_conf =>
            specs.cgo_skip_conf := value;
         when sp_cgo_build =>
            specs.cgo_skip_build := value;
         when sp_cgo_inst =>
            specs.cgo_skip_inst := value;
         when others =>
            raise wrong_type with field'Img;
      end case;
   end set_boolean;


   --------------------------------------------------------------------------------------------
   --  build_option_helper
   --------------------------------------------------------------------------------------------
   procedure build_option_helper
     (specs  : in out Portspecs;
      field  : spec_option;
      option : String;
      value  : String)
   is
      procedure Change (Key : HT.Text; Element : in out Option_Helper);

      option_text  : HT.Text := HT.SUS (option);
      value_text   : HT.Text := HT.SUS (value);
      mycursor     : option_crate.Cursor;
      allow_spaces : Boolean;

      procedure Change (Key : HT.Text; Element : in out Option_Helper) is
      begin
         case field is
            when broken_on =>
               Element.BROKEN_ON := value_text;
            when buildrun_depends_off =>
               Element.BUILDRUN_DEPENDS_OFF.Append (value_text);
            when buildrun_depends_on =>
               Element.BUILDRUN_DEPENDS_ON.Append (value_text);
            when build_depends_off =>
               Element.BUILD_DEPENDS_OFF.Append (value_text);
            when build_depends_on =>
               Element.BUILD_DEPENDS_ON.Append (value_text);
            when build_target_off =>
               Element.BUILD_TARGET_OFF.Append (value_text);
            when build_target_on =>
               Element.BUILD_TARGET_ON.Append (value_text);
            when cflags_off =>
               Element.CFLAGS_OFF.Append (value_text);
            when cflags_on =>
               Element.CFLAGS_ON.Append (value_text);
            when cmake_args_off =>
               Element.CMAKE_ARGS_OFF.Append (value_text);
            when cmake_args_on =>
               Element.CMAKE_ARGS_ON.Append (value_text);
            when cmake_bool_f_both =>
               Element.CMAKE_BOOL_F_BOTH.Append (value_text);
            when cmake_bool_t_both =>
               Element.CMAKE_BOOL_T_BOTH.Append (value_text);
            when configure_args_off =>
               Element.CONFIGURE_ARGS_OFF.Append (value_text);
            when configure_args_on =>
               Element.CONFIGURE_ARGS_ON.Append (value_text);
            when configure_enable_both =>
               Element.CONFIGURE_ENABLE_BOTH.Append (value_text);
            when configure_env_off =>
               Element.CONFIGURE_ENV_OFF.Append (value_text);
            when configure_env_on =>
               Element.CONFIGURE_ENV_ON.Append (value_text);
            when configure_with_both =>
               Element.CONFIGURE_WITH_BOTH.Append (value_text);
            when cppflags_off =>
               Element.CPPFLAGS_OFF.Append (value_text);
            when cppflags_on =>
               Element.CPPFLAGS_ON.Append (value_text);
            when cxxflags_off =>
               Element.CXXFLAGS_OFF.Append (value_text);
            when cxxflags_on =>
               Element.CXXFLAGS_ON.Append (value_text);
            when description =>
               Element.option_description := value_text;
            when df_index_off =>
               Element.DF_INDEX_OFF.Append (value_text);
            when df_index_on =>
               Element.DF_INDEX_ON.Append (value_text);
            when extract_only_off =>
               Element.EXTRACT_ONLY_OFF.Append (value_text);
            when extract_only_on  =>
               Element.EXTRACT_ONLY_ON.Append (value_text);
            when extra_patches_off =>
               Element.EXTRA_PATCHES_OFF.Append (value_text);
            when extra_patches_on =>
               Element.EXTRA_PATCHES_ON.Append (value_text);
            when gnome_comp_off =>
               Element.GNOME_COMPONENTS_OFF.Append (value_text);
            when gnome_comp_on =>
               Element.GNOME_COMPONENTS_ON.Append (value_text);
            when implies_on =>
               Element.IMPLIES_ON.Append (value_text);
            when info_off =>
               Element.INFO_OFF.Append (value_text);
            when info_on =>
               Element.INFO_ON.Append (value_text);
            when install_target_off =>
               Element.INSTALL_TARGET_OFF.Append (value_text);
            when install_target_on =>
               Element.INSTALL_TARGET_ON.Append (value_text);
            when keywords_off =>
               Element.KEYWORDS_OFF.Append (value_text);
            when keywords_on =>
               Element.KEYWORDS_ON.Append (value_text);
            when ldflags_off =>
               Element.LDFLAGS_OFF.Append (value_text);
            when ldflags_on =>
               Element.LDFLAGS_ON.Append (value_text);
            when makefile_off =>
               Element.MAKEFILE_OFF.Append (value_text);
            when makefile_on =>
               Element.MAKEFILE_ON.Append (value_text);
            when make_args_off =>
               Element.MAKE_ARGS_OFF.Append (value_text);
            when make_args_on =>
               Element.MAKE_ARGS_ON.Append (value_text);
            when make_env_off =>
               Element.MAKE_ENV_OFF.Append (value_text);
            when make_env_on =>
               Element.MAKE_ENV_ON.Append (value_text);
            when only_for_opsys_on =>
               Element.ONLY_FOR_OPSYS_ON.Append (value_text);
            when patchfiles_off =>
               Element.PATCHFILES_OFF.Append (value_text);
            when patchfiles_on =>
               Element.PATCHFILES_ON.Append (value_text);
            when plist_sub_off =>
               Element.PLIST_SUB_OFF.Append (value_text);
            when plist_sub_on =>
               Element.PLIST_SUB_ON.Append (value_text);
            when prevents_on =>
               Element.PREVENTS_ON.Append (value_text);
            when qmake_args_off =>
               Element.QMAKE_ARGS_OFF.Append (value_text);
            when qmake_args_on =>
               Element.QMAKE_ARGS_ON.Append (value_text);
            when run_depends_off =>
               Element.RUN_DEPENDS_OFF.Append (value_text);
            when run_depends_on =>
               Element.RUN_DEPENDS_ON.Append (value_text);
            when sub_list_off =>
               Element.SUB_LIST_OFF.Append (value_text);
            when sub_files_off =>
               Element.SUB_FILES_OFF.Append (value_text);
            when sub_files_on =>
               Element.SUB_FILES_ON.Append (value_text);
            when sub_list_on =>
               Element.SUB_LIST_ON.Append (value_text);
            when test_target_off =>
               Element.TEST_TARGET_OFF.Append (value_text);
            when test_target_on =>
               Element.TEST_TARGET_ON.Append (value_text);
            when uses_off =>
               Element.USES_OFF.Append (value_text);
            when uses_on =>
               Element.USES_ON.Append (value_text);
            when xorg_comp_off =>
               Element.XORG_COMPONENTS_OFF.Append (value_text);
            when xorg_comp_on =>
               Element.XORG_COMPONENTS_ON.Append (value_text);
            when php_ext_off =>
               Element.PHP_EXTENSIONS_OFF.Append (value_text);
            when php_ext_on =>
               Element.PHP_EXTENSIONS_ON.Append (value_text);
            when not_helper_format | not_supported_helper =>
               null;
         end case;
      end Change;
   begin
      if spec_order'Pos (specs.last_set) < spec_order'Pos (so_opts_std) then
         raise misordered with field'Img;
      end if;
      case field is
         when build_target_on | cmake_bool_f_both | cmake_bool_t_both | configure_enable_both |
              configure_with_both | df_index_on | extra_patches_on | extract_only_on |
              implies_on | info_on | install_target_on | keywords_on  | buildrun_depends_on |
              patchfiles_on | prevents_on | run_depends_on | sub_files_on | test_target_on |
              uses_on | uses_off | buildrun_depends_off | run_depends_off | build_depends_on |
              gnome_comp_on | xorg_comp_on | info_off | df_index_off | sub_files_off =>
            allow_spaces := False;
         when others =>
            allow_spaces := True;
      end case;
      if not allow_spaces and then
        HT.contains (S => value, fragment => " ")
      then
         raise contains_spaces;
      end if;
      if not specs.ops_helpers.Contains (option_text) then
         raise wrong_value with "option '" & option & "' is not defined.";
      end if;
      mycursor := specs.ops_helpers.Find (Key => option_text);

      --  validate first
      case field is
         when broken_on | build_target_off | build_target_on | cmake_args_off | cmake_args_on |
              cmake_bool_f_both | cmake_bool_t_both | configure_args_off | configure_args_on |
              configure_enable_both | configure_env_off | configure_env_on |
              configure_with_both | cflags_off | cflags_on | cppflags_off | cppflags_on |
              cxxflags_off | cxxflags_on | extra_patches_off | extra_patches_on |
              install_target_off | install_target_on | ldflags_off | ldflags_on |
              make_args_off | make_args_on | make_env_off | make_env_on |
              patchfiles_off | patchfiles_on | plist_sub_off | plist_sub_on |
              qmake_args_on | qmake_args_off | sub_files_off | sub_files_on |
              sub_list_off | sub_list_on | test_target_off | test_target_on |
              makefile_on | makefile_off =>
            --  No validation required
            null;
         when build_depends_on | buildrun_depends_on | run_depends_on |
              build_depends_off | buildrun_depends_off | run_depends_off =>
            if not valid_dependency_format (value) then
               raise wrong_value with "invalid dependency format '" & value & "'";
            end if;
         when xorg_comp_on | xorg_comp_off =>
            if determine_xorg_component (value) = invalid_component then
               raise wrong_value with "xorg component '" & value & "' is not valid";
            end if;
         when gnome_comp_on | gnome_comp_off =>
            if determine_gnome_component (value) = invalid_component then
               raise wrong_value with "gnome component '" & value & "' is not valid";
            end if;
         when php_ext_on | php_ext_off =>
            if determine_php_extension (value) = invalid_extension then
               raise wrong_value with "php extension '" & value & "' is not valid";
            end if;
         when df_index_off =>
            if not specs.dist_index_is_valid (value) then
               raise wrong_value with "distfile index '" & value & "' is not valid";
            end if;
            if specs.ops_helpers.Element (option_text).DF_INDEX_OFF.Contains (value_text) or else
              specs.df_index.Contains (value_text)
            then
               raise dupe_list_value with value;
            end if;
            specs.opt_df_index := True;
         when df_index_on =>
            if not specs.dist_index_is_valid (value) then
               raise wrong_value with "distfile index '" & value & "' is not valid";
            end if;
            if specs.ops_helpers.Element (option_text).DF_INDEX_ON.Contains (value_text) or else
              specs.df_index.Contains (value_text)
            then
               raise dupe_list_value with value;
            end if;
            specs.opt_df_index := True;
         when extract_only_on =>
            if not specs.dist_index_is_valid (value) then
               raise wrong_value with "distfile index '" & value & "' is not valid";
            end if;
            if specs.ops_helpers.Element (option_text).EXTRACT_ONLY_ON.Contains (value_text) then
               raise dupe_list_value with value;
            end if;
         when extract_only_off =>
            if not specs.dist_index_is_valid (value) then
               raise wrong_value with "distfile index '" & value & "' is not valid";
            end if;
            if specs.ops_helpers.Element (option_text).EXTRACT_ONLY_OFF.Contains (value_text) then
               raise dupe_list_value with value;
            end if;
         when implies_on =>
            if not specs.ops_avail.Contains (value_text) then
               raise wrong_value with "Not a defined option: '" & value & "'";
            end if;
            if HT.equivalent (option_text, value_text) then
               raise wrong_value with "Option refers to itself: '" & value & "'";
            end if;
            if specs.ops_helpers.Element (option_text).IMPLIES_ON.Contains (value_text) then
               raise dupe_list_value with value;
            end if;
         when prevents_on =>
            if not specs.ops_avail.Contains (value_text) then
               raise wrong_value with "Not a defined option: '" & value & "'";
            end if;
            if HT.equivalent (option_text, value_text) then
               raise wrong_value with "Option refers to itself: '" & value & "'";
            end if;
            if specs.ops_helpers.Element (option_text).PREVENTS_ON.Contains (value_text) then
               raise dupe_list_value with value;
            end if;
         when keywords_on =>
            if not keyword_is_valid (value) then
               raise wrong_value with "Keyword '" & value & "' is not recognized";
            end if;
            if option_crate.Element (mycursor).KEYWORDS_ON.Contains (value_text) then
               raise dupe_list_value with value;
            end if;
         when keywords_off =>
            if not keyword_is_valid (value) then
               raise wrong_value with "Keyword '" & value & "' is not recognized";
            end if;
            if option_crate.Element (mycursor).KEYWORDS_OFF.Contains (value_text) then
               raise dupe_list_value with value;
            end if;
         when uses_on | uses_off =>
            if not valid_uses_module (value) then
               raise wrong_value with "USES '" & value & "' is not recognized";
            end if;
            declare
               errmsg : HT.Text;
            begin
               if not specs.extra_uses_modules_sanity_check_passes (value, errmsg) then
                  raise wrong_value with HT.USS (errmsg);
               end if;
            end;
         when info_on | info_off =>
            declare
               msg : String := specs.info_page_check_message (value);
            begin
               if not HT.IsBlank (msg) then
                  raise wrong_type with msg;
               end if;
            end;
            if not specs.valid_info_page (value) then
               raise wrong_value with "INFO subdirectories must match on every entry";
            end if;
         when description =>
            if value'Length > 50 then
               raise wrong_value with "option descriptions are limited to 50 characters";
            end if;
            if HT.contains (value, ":") then
               raise wrong_value with "option descriptions cannot contain colon characters";
            end if;
         when only_for_opsys_on =>
            if not UTL.valid_lower_opsys (value) then
               raise wrong_value with "opsys '" & value & "' is not valid.";
            end if;
         when not_supported_helper | not_helper_format =>
            null;
      end case;

      specs.ops_helpers.Update_Element (Position => mycursor, Process => Change'Access);
   end build_option_helper;


   --------------------------------------------------------------------------------------------
   --  option_helper_unset
   --------------------------------------------------------------------------------------------
   function option_helper_unset
     (specs  : Portspecs;
      field  : spec_option;
      option : String) return Boolean
   is
      option_text : HT.Text := HT.SUS (option);
   begin
      if not specs.ops_helpers.Contains (option_text) then
         return False;
      end if;
      declare
         rec : Option_Helper renames specs.ops_helpers.Element (option_text);
      begin
         case field is
            when broken_on             => return HT.IsBlank (rec.BROKEN_ON);
            when buildrun_depends_off  => return rec.BUILDRUN_DEPENDS_OFF.Is_Empty;
            when buildrun_depends_on   => return rec.BUILDRUN_DEPENDS_ON.Is_Empty;
            when build_depends_off     => return rec.BUILD_DEPENDS_OFF.Is_Empty;
            when build_depends_on      => return rec.BUILD_DEPENDS_ON.Is_Empty;
            when build_target_off      => return rec.BUILD_TARGET_OFF.Is_Empty;
            when build_target_on       => return rec.BUILD_TARGET_ON.Is_Empty;
            when cflags_off            => return rec.CFLAGS_OFF.Is_Empty;
            when cflags_on             => return rec.CFLAGS_ON.Is_Empty;
            when cmake_args_off        => return rec.CMAKE_ARGS_OFF.Is_Empty;
            when cmake_args_on         => return rec.CMAKE_ARGS_ON.Is_Empty;
            when cmake_bool_f_both     => return rec.CMAKE_BOOL_F_BOTH.Is_Empty;
            when cmake_bool_t_both     => return rec.CMAKE_BOOL_T_BOTH.Is_Empty;
            when configure_args_off    => return rec.CONFIGURE_ARGS_OFF.Is_Empty;
            when configure_args_on     => return rec.CONFIGURE_ARGS_ON.Is_Empty;
            when configure_enable_both => return rec.CONFIGURE_ENABLE_BOTH.Is_Empty;
            when configure_env_off     => return rec.CONFIGURE_ENV_OFF.Is_Empty;
            when configure_env_on      => return rec.CONFIGURE_ENV_ON.Is_Empty;
            when configure_with_both   => return rec.CONFIGURE_WITH_BOTH.Is_Empty;
            when cppflags_off          => return rec.CPPFLAGS_OFF.Is_Empty;
            when cppflags_on           => return rec.CPPFLAGS_ON.Is_Empty;
            when cxxflags_off          => return rec.CXXFLAGS_OFF.Is_Empty;
            when cxxflags_on           => return rec.CXXFLAGS_ON.Is_Empty;
            when df_index_off          => return rec.DF_INDEX_OFF.Is_Empty;
            when df_index_on           => return rec.DF_INDEX_ON.Is_Empty;
            when description           => return HT.IsBlank (rec.option_description);
            when extract_only_off      => return rec.EXTRACT_ONLY_OFF.Is_Empty;
            when extract_only_on       => return rec.EXTRACT_ONLY_ON.Is_Empty;
            when extra_patches_off     => return rec.EXTRA_PATCHES_OFF.Is_Empty;
            when extra_patches_on      => return rec.EXTRA_PATCHES_ON.Is_Empty;
            when gnome_comp_off        => return rec.GNOME_COMPONENTS_OFF.Is_Empty;
            when gnome_comp_on         => return rec.GNOME_COMPONENTS_ON.Is_Empty;
            when implies_on            => return rec.IMPLIES_ON.Is_Empty;
            when info_off              => return rec.INFO_OFF.Is_Empty;
            when info_on               => return rec.INFO_ON.Is_Empty;
            when install_target_off    => return rec.INSTALL_TARGET_OFF.Is_Empty;
            when install_target_on     => return rec.INSTALL_TARGET_ON.Is_Empty;
            when keywords_off          => return rec.KEYWORDS_OFF.Is_Empty;
            when keywords_on           => return rec.KEYWORDS_ON.Is_Empty;
            when ldflags_off           => return rec.LDFLAGS_OFF.Is_Empty;
            when ldflags_on            => return rec.LDFLAGS_ON.Is_Empty;
            when makefile_off          => return rec.MAKEFILE_OFF.Is_Empty;
            when makefile_on           => return rec.MAKEFILE_ON.Is_Empty;
            when make_args_off         => return rec.MAKE_ARGS_OFF.Is_Empty;
            when make_args_on          => return rec.MAKE_ARGS_ON.Is_Empty;
            when make_env_off          => return rec.MAKE_ENV_OFF.Is_Empty;
            when make_env_on           => return rec.MAKE_ENV_ON.Is_Empty;
            when only_for_opsys_on     => return rec.ONLY_FOR_OPSYS_ON.Is_Empty;
            when patchfiles_off        => return rec.PATCHFILES_OFF.Is_Empty;
            when patchfiles_on         => return rec.PATCHFILES_ON.Is_Empty;
            when plist_sub_off         => return rec.PLIST_SUB_OFF.Is_Empty;
            when plist_sub_on          => return rec.PLIST_SUB_ON.Is_Empty;
            when php_ext_off           => return rec.PHP_EXTENSIONS_OFF.Is_Empty;
            when php_ext_on            => return rec.PHP_EXTENSIONS_ON.Is_Empty;
            when prevents_on           => return rec.PREVENTS_ON.Is_Empty;
            when qmake_args_off        => return rec.QMAKE_ARGS_OFF.Is_Empty;
            when qmake_args_on         => return rec.QMAKE_ARGS_ON.Is_Empty;
            when run_depends_off       => return rec.RUN_DEPENDS_OFF.Is_Empty;
            when run_depends_on        => return rec.RUN_DEPENDS_ON.Is_Empty;
            when sub_files_off         => return rec.SUB_FILES_OFF.Is_Empty;
            when sub_files_on          => return rec.SUB_FILES_ON.Is_Empty;
            when sub_list_off          => return rec.SUB_LIST_OFF.Is_Empty;
            when sub_list_on           => return rec.SUB_LIST_ON.Is_Empty;
            when test_target_off       => return rec.TEST_TARGET_OFF.Is_Empty;
            when test_target_on        => return rec.TEST_TARGET_ON.Is_Empty;
            when uses_off              => return rec.USES_OFF.Is_Empty;
            when uses_on               => return rec.USES_ON.Is_Empty;
            when xorg_comp_off         => return rec.XORG_COMPONENTS_OFF.Is_Empty;
            when xorg_comp_on          => return rec.XORG_COMPONENTS_ON.Is_Empty;
            when not_helper_format     => return False;
            when not_supported_helper  => return False;
         end case;
      end;
   end option_helper_unset;


   --------------------------------------------------------------------------------------------
   --  variant_exists
   --------------------------------------------------------------------------------------------
   function variant_exists (specs : Portspecs; variant : String) return Boolean is
   begin
      return specs.variants.Contains (Item => HT.SUS (variant));
   end variant_exists;


   --------------------------------------------------------------------------------------------
   --  option_exists
   --------------------------------------------------------------------------------------------
   function option_exists (specs : Portspecs; option : String) return Boolean
   is
      option_text : HT.Text := HT.SUS (option);
   begin
      if option = "" then
         return False;
      end if;
      return specs.ops_avail.Contains (option_text);
   end option_exists;


   --------------------------------------------------------------------------------------------
   --  group_exists
   --------------------------------------------------------------------------------------------
   function group_exists
     (specs : Portspecs;
      field : spec_field;
      group : String) return Boolean
   is
      text_group : HT.Text := HT.SUS (group);
   begin
      case field is
         when sp_dl_sites =>
            return specs.dl_sites.Contains (text_group);
         when sp_subpackages =>
            return specs.subpackages.Contains (text_group);
         when sp_vopts =>
            return specs.variantopts.Contains (text_group);
         when sp_ext_head =>
            return specs.extract_head.Contains (text_group);
         when sp_ext_tail =>
            return specs.extract_tail.Contains (text_group);
         when sp_makefile_targets =>
            return specs.make_targets.Contains (text_group);
         when sp_options_on =>
            return specs.options_on.Contains (text_group);
         when sp_broken =>
            return specs.broken.Contains (text_group);
         when sp_var_opsys =>
            return specs.var_opsys.Contains (text_group);
         when sp_var_arch =>
            return specs.var_arch.Contains (text_group);
         when sp_exrun =>
            return specs.extra_rundeps.Contains (text_group);
         when sp_catchall =>
            return specs.catch_all.Contains (text_group);
         when sp_opt_descr =>
            return specs.optgroup_desc.Contains (text_group);
         when sp_opt_group =>
            return specs.optgroups.Contains (text_group);
         when sp_os_bdep =>
            return specs.opsys_b_deps.Contains (text_group);
         when sp_os_rdep =>
            return specs.opsys_r_deps.Contains (text_group);
         when sp_os_brdep =>
            return specs.opsys_br_deps.Contains (text_group);
         when others =>
            return False;
      end case;
   end group_exists;


   --------------------------------------------------------------------------------------------
   --  option_current_setting
   --------------------------------------------------------------------------------------------
   function option_current_setting (specs : Portspecs; option : String) return Boolean
   is
      option_text : HT.Text := HT.SUS (option);
   begin
      if not specs.option_exists (option) then
         raise invalid_option with option;
      end if;
      if specs.ops_helpers.Contains (option_text) then
         return specs.ops_helpers.Element (option_text).currently_set_ON;
      end if;
      return False;
   end option_current_setting;


   --------------------------------------------------------------------------------------------
   --  all_taglines_defined
   --------------------------------------------------------------------------------------------
   function all_taglines_defined (specs : Portspecs) return Boolean
   is
      procedure check (position : string_crate.Cursor);

      all_present : Boolean := True;

      procedure check (position : string_crate.Cursor)
      is
         variant : HT.Text := string_crate.Element (position);
      begin
         if not specs.taglines.Contains (variant) then
            all_present := False;
         end if;
      end check;
   begin
      specs.variants.Iterate (Process => check'Access);
      return all_present;
   end all_taglines_defined;


   --------------------------------------------------------------------------------------------
   --  check_variants
   --------------------------------------------------------------------------------------------
   function check_variants (specs : Portspecs) return String
   is
      procedure check (position : string_crate.Cursor);
      procedure check_option (position : string_crate.Cursor);

      result  : HT.Text := HT.blank;
      variant : HT.Text;

      --  OPTIONS_AVAILABLE process
      procedure check_option (position : string_crate.Cursor)
      is
         option : HT.Text := string_crate.Element (position);
      begin
         if HT.IsBlank (result) then
            declare
               step  : String  := HT.USS (option);
               WON   : HT.Text := HT.SUS (step & "=ON");
               WOFF  : HT.Text := HT.SUS (step & "=OFF");
            begin
               if not specs.variantopts.Element (variant).list.Contains (WON) and then
                 not specs.variantopts.Element (variant).list.Contains (WOFF)
               then
                  result := HT.SUS (HT.USS (variant) & ":" & step);
               end if;
            end;
         end if;
      end check_option;

      --  variant process
      procedure check (position : string_crate.Cursor) is
      begin
         variant := string_crate.Element (position);
         if HT.IsBlank (result) then
            if HT.USS (variant) /= variant_standard then

               --  It's impossible that variantopts doesn't have variant, so don't test
               specs.ops_avail.Iterate (Process => check_option'Access);
            end if;
         end if;
      end check;
   begin
      specs.variants.Iterate (Process => check'Access);
      return HT.USS (result);
   end check_variants;


   --------------------------------------------------------------------------------------------
   --  contains_nonquoted_spaces
   --------------------------------------------------------------------------------------------
   function contains_nonquoted_spaces (word : String) return Boolean
   is
      mask    : String  := UTL.mask_quoted_string (word);
   begin
      return HT.contains (S => mask, fragment => " ");
   end contains_nonquoted_spaces;


   --------------------------------------------------------------------------------------------
   --  adjust_defaults_port_parse
   --------------------------------------------------------------------------------------------
   procedure adjust_defaults_port_parse (specs : in out Portspecs)
   is
      procedure grow (Key : HT.Text; Element : in out group_list);

      empty_comment : HT.Text := HT.SUS ("# empty");
      rust_crates   : HT.Text := HT.SUS ("rust/crates");

      procedure grow (Key : HT.Text; Element : in out group_list) is
      begin
         Element.list.Append (empty_comment);
      end grow;
   begin
      for X in Integer range 1 .. Integer (specs.extract_head.Length) loop
         declare
            N : HT.Text := HT.SUS (HT.int2str (X));
         begin
            if not specs.extract_head.Element (N).list.Is_Empty and then
              specs.extract_tail.Element (N).list.Is_Empty
            then
               specs.extract_tail.Update_Element (Position => specs.extract_tail.Find (N),
                                                  Process  => grow'Access);
            else
               if not specs.extract_tail.Element (N).list.Is_Empty and then
                 specs.extract_head.Element (N).list.Is_Empty
               then
                  specs.extract_head.Update_Element (Position => specs.extract_head.Find (N),
                                                     Process  => grow'Access);
               end if;
            end if;
         end;
      end loop;
      if specs.df_index.Is_Empty and then
        not specs.opt_df_index and then
        not specs.distfiles.Is_Empty
      then
         specs.df_index.Append (HT.SUS ("1"));
      end if;
      if Unix.env_variable_defined ("SKIPCCRUN") then
         specs.fatal_rpath := False;
      end if;
      if specs.debugging_on then
         specs.optimizer_lvl := 0;
      end if;
      if specs.uses_base.Contains (HT.SUS ("cargo")) then
         if HT.IsBlank (specs.dist_subdir) then
            specs.dist_subdir := rust_crates;
         elsif not HT.equivalent (specs.dist_subdir, rust_crates) then
            raise dupe_spec_key with "DIST_SUBDIR set with USE=cargo";
         end if;
      end if;
   end adjust_defaults_port_parse;


   --------------------------------------------------------------------------------------------
   --  dist_index_is_valid
   --------------------------------------------------------------------------------------------
   function dist_index_is_valid (specs : Portspecs; test_index : String) return Boolean
   is
      mynum : Integer := Integer'Value (test_index);
   begin
      return (mynum >= 1 and then mynum <= Integer (specs.distfiles.Length));
   exception
      when Constraint_Error =>
         return False;
   end dist_index_is_valid;


   --------------------------------------------------------------------------------------------
   --  valid_OPT_ON_value
   --------------------------------------------------------------------------------------------
   function valid_OPT_ON_value (specs : Portspecs;
                                key   : String;
                                word  : String) return Boolean
   is
      function looks_like_release (wrkstr : String) return Boolean;
      function looks_like_release (wrkstr : String) return Boolean is
      begin
         for X in wrkstr'Range loop
            case wrkstr (X) is
               when '0' .. '9' | '.' => null;
               when others => return False;
            end case;
         end loop;
         return True;
      end looks_like_release;
   begin
      if key = options_all or else
        UTL.valid_cpu_arch (key)
      then
         --  "all" and arch types Limited to existing options
         return specs.option_exists (word);
      end if;

      --  Note: "all" must come first and nothing following can define options that are
      --  already defined in "all".
      if specs.option_exists (word) then
         return not specs.option_present_in_OPT_ON_all (word);
      end if;

      declare
         P2_1 : String := HT.part_1 (word, "/");
         P2_2 : String := HT.part_2 (word, "/");
      begin
         if P2_2'Length = 0 then
            return False;
         end if;
         if specs.option_exists (P2_1) then
            if specs.option_present_in_OPT_ON_all (P2_1) then
               return False;
            end if;
         else
            return False;
         end if;
         if HT.contains (P2_2, "/") then
            --  This is a triplet
            declare
               P3_1 : String := HT.part_1 (P2_2, "/");
               P3_2 : String := HT.part_2 (P2_2, "/");
            begin
               if P3_1 /= "" and then
                 not looks_like_release (P3_1)
               then
                  return False;
               end if;
               --  Here: P2_2 matches an option
               --        P3_1 is empty string or a release
               declare
                  num_bars   : Natural := HT.count_char (P3_2, LAT.Vertical_Line);
                  bck_marker : Natural := P3_2'First;
                  vrt_marker : Natural := bck_marker;
               begin
                  if num_bars = 0 then
                     return UTL.valid_cpu_arch (P3_2);
                  end if;
                  if num_bars = 1 then
                     return UTL.valid_cpu_arch (HT.part_1 (P3_2, "|")) and then
                       UTL.valid_cpu_arch (HT.part_2 (P3_2, "|"));
                  end if;
                  for V in Positive range 1 .. num_bars loop
                     loop
                        exit when P3_2 (vrt_marker) = LAT.Vertical_Line;
                        vrt_marker := vrt_marker + 1;
                     end loop;
                     if not UTL.valid_cpu_arch (P3_2 (bck_marker .. vrt_marker - 1)) then
                        return False;
                     end if;
                     bck_marker := vrt_marker + 1;
                     vrt_marker := bck_marker;
                  end loop;
                  return UTL.valid_cpu_arch (P3_2 (bck_marker .. P3_2'Last));
               end;
            end;
         else
            --  Only [0-9.] allowed
            return looks_like_release (P2_2);
         end if;
      end;
   end valid_OPT_ON_value;


   --------------------------------------------------------------------------------------------
   --  option_present_in_OPT_ON_all
   --------------------------------------------------------------------------------------------
   function option_present_in_OPT_ON_all (specs : Portspecs;
                                          option_name : String) return Boolean is
   begin
      return specs.options_on.Element (HT.SUS (options_all)).list.Contains (HT.SUS (option_name));
   end option_present_in_OPT_ON_all;


   --------------------------------------------------------------------------------------------
   --  ISO8601_format
   --------------------------------------------------------------------------------------------
   function ISO8601_format (value : String) return Boolean
   is
      --  Requires YYYY-MM-DD, YYYY >= 2017, MM 01..12, DD 01..31, converts
   begin
      if value'Length /= 10 then
         return False;
      end if;
      declare
         year : Integer;
         month : Integer;
         day   : Integer;
         wrkstr : String (1 .. 10) := value;
         dummy : CAL.Time;
      begin
         if not (wrkstr (5) = LAT.Hyphen) or else
           not (wrkstr (8) = LAT.Hyphen)
         then
            return False;
         end if;
         year  := Integer'Value (wrkstr (1 .. 4));
         month := Integer'Value (wrkstr (6 .. 7));
         day   := Integer'Value (wrkstr (9 .. 10));
         if year < 2017 or else
           month < 1 or else
           month > 12 or else
           day < 1 or else
           day > 31
         then
            return False;
         end if;
         dummy := CAL.Formatting.Value (wrkstr & " 00:00:00");
         return True;
      end;
   exception
      when others =>
         return False;
   end ISO8601_format;


   --------------------------------------------------------------------------------------------
   --  check_deprecation
   --------------------------------------------------------------------------------------------
   function deprecation_valid (specs : Portspecs) return Boolean
   is
      DEP : Boolean := not HT.IsBlank (specs.deprecated);
      EXP : Boolean := not HT.IsBlank (specs.expire_date);
   begin
      return (DEP and EXP) or else (not DEP and not EXP);
   end deprecation_valid;


   --------------------------------------------------------------------------------------------
   --  get_namebase
   --------------------------------------------------------------------------------------------
   function get_namebase (specs  : Portspecs) return String is
   begin
      return HT.USS (specs.namebase);
   end get_namebase;


   --------------------------------------------------------------------------------------------
   --  get_options_list
   --------------------------------------------------------------------------------------------
   function get_options_list (specs : Portspecs; variant : String) return String
   is
      procedure dump_option  (position : option_crate.Cursor);

      joined : HT.Text;
      is_standard : constant Boolean := (variant = variant_standard);

      procedure dump_option (position : option_crate.Cursor)
      is
         rec : Option_Helper renames option_crate.Element (position);
         optname : String := HT.USS (rec.option_name);
      begin
         if optname = "none" then
            return;
         end if;
         if is_standard and then not rec.standard_option then
            return;
         end if;
         if rec.currently_set_ON then
            HT.SU.Append (joined, " " & optname & ": on,");
         else
            HT.SU.Append (joined, " " & optname & ": off,");
         end if;
      end dump_option;
   begin
      specs.ops_helpers.Iterate (dump_option'Access);
      return HT.USS (joined);
   end get_options_list;


   --------------------------------------------------------------------------------------------
   --  option_block_for_dialog
   --------------------------------------------------------------------------------------------
   function option_block_for_dialog (specs : Portspecs) return String
   is
      --  block format is
      --  GROUP:GROUP-TYPE:GROUP-DESC:OPT-NAME:DEFAULT-VAL:CURRENT-VAL:OPT-DESC:I:P
      --  "I" = IMPLIES, "P" = PREVENTS  (binary string, e.g. "0010000001"

      type group_type is (radio, restrict, sinlimit);
      procedure scan1 (position : string_crate.Cursor);
      procedure group_scan (position : string_crate.Cursor);
      procedure group_scn2 (position : string_crate.Cursor);
      procedure group_option_scan (position : string_crate.Cursor);
      procedure group_option_scn2 (position : string_crate.Cursor);
      procedure nogroup (position : string_crate.Cursor);
      procedure nogrup2 (position : string_crate.Cursor);
      function bool2str (b : Boolean) return String;
      function description (option_name, opt_description : HT.Text) return String;
      function affection (option_name : HT.Text; implies : Boolean) return String;
      function option_index (option_name : HT.Text) return Natural;

      tmpstore    : string_crate.Vector;
      tmpstor2    : string_crate.Vector;
      disp_order  : string_crate.Vector;
      group       : group_type;
      answer      : HT.Text;
      singles     : HT.Text;
      group_index : HT.Text;

      function bool2str (b : Boolean) return String is
      begin
         if b then
            return "1";
         else
            return "0";
         end if;
      end bool2str;

      procedure scan1 (position : string_crate.Cursor) is
      begin
         tmpstore.Append (string_crate.Element (position));
         tmpstor2.Append (string_crate.Element (position));
      end scan1;

      procedure group_scan (position : string_crate.Cursor) is
      begin
         group_index := string_crate.Element (position);
         specs.optgroups.Element (group_index).list.Iterate (group_option_scan'Access);
      end group_scan;

      procedure group_scn2 (position : string_crate.Cursor) is
      begin
         group_index := string_crate.Element (position);
         specs.optgroups.Element (group_index).list.Iterate (group_option_scn2'Access);
      end group_scn2;

      procedure group_option_scan (position : string_crate.Cursor)
      is
         function group_type return String;

         option_name : HT.Text renames string_crate.Element (position);
         helper : Option_Helper renames specs.ops_helpers.Element (option_name);
         gdesc : HT.Text renames specs.optgroup_desc.Element (group_index).list.First_Element;

         function group_type return String is
         begin
            case group is
               when radio    => return "RADIO";
               when restrict => return "RESTR";
               when sinlimit => return "UNLIM";
            end case;
         end group_type;

      begin
         if specs.ops_helpers.Contains (option_name) then
            if tmpstore.Contains (option_name) then
               tmpstore.Delete (tmpstore.Find_Index (option_name));
            end if;
            HT.SU.Append (answer,
                          HT.USS (group_index) &
                            LAT.Colon & group_type &
                            LAT.Colon & HT.USS (gdesc) &
                            LAT.Colon & HT.USS (option_name) &
                            LAT.Colon & bool2str (helper.set_ON_by_default) &
                            LAT.Colon & bool2str (helper.currently_set_ON) &
                            LAT.Colon & description (option_name, helper.option_description) &
                            LAT.Colon & affection (option_name, True) &
                            LAT.Colon & affection (option_name, False) &
                            LAT.LF);
         end if;
      end group_option_scan;

      procedure group_option_scn2 (position : string_crate.Cursor)
      is
         option_name : HT.Text renames string_crate.Element (position);
      begin
         if specs.ops_helpers.Contains (option_name) then
            if tmpstor2.Contains (option_name) then
               tmpstor2.Delete (tmpstor2.Find_Index (option_name));
            end if;
            disp_order.Append (option_name);
         end if;
      end group_option_scn2;

      procedure nogroup (position : string_crate.Cursor)
      is
         option_name : HT.Text renames string_crate.Element (position);
         helper : Option_Helper renames specs.ops_helpers.Element (option_name);
      begin
         if specs.ops_helpers.Contains (option_name) then
            HT.SU.Append (singles,
                          LAT.Colon &
                            LAT.Colon &
                            LAT.Colon & HT.USS (option_name) &
                            LAT.Colon & bool2str (helper.set_ON_by_default) &
                            LAT.Colon & bool2str (helper.currently_set_ON) &
                            LAT.Colon & description (option_name, helper.option_description) &
                            LAT.Colon & affection (option_name, True) &
                            LAT.Colon & affection (option_name, False) &
                            LAT.LF);
         end if;
      end nogroup;

      procedure nogrup2 (position : string_crate.Cursor)
      is
         option_name : HT.Text renames string_crate.Element (position);
      begin
         if specs.ops_helpers.Contains (option_name) then
            disp_order.Prepend (option_name);
         end if;
      end nogrup2;

      function description (option_name, opt_description : HT.Text) return String is
      begin
         if HT.IsBlank (opt_description) then
            declare
               dos : described_option_set := described_option (HT.USS (option_name));
            begin
               if dos = OPT_NOT_DEFINED then
                  return "error-missing option description";
               else
                  return default_description (dos);
               end if;
            end;
         else
            return HT.USS (opt_description);
         end if;
      end description;

      function option_index (option_name : HT.Text) return Natural
      is
         procedure scan (position : string_crate.Cursor);

         result  : Natural := 0;
         counter : Natural := 0;

         procedure scan (position : string_crate.Cursor)
         is
            name : HT.Text renames string_crate.Element (position);
         begin
            if result = 0 then
               counter := counter + 1;
               if HT.equivalent (name, option_name) then
                  result := counter;
               end if;
            end if;
         end scan;
      begin
         disp_order.Iterate (scan'Access);
         return result;
      end option_index;

      function affection (option_name : HT.Text; implies : Boolean) return String
      is
         strlen : Natural := Natural (disp_order.Length);
      begin
         declare
            procedure setone (position : string_crate.Cursor);

            result : String (1 .. strlen) := (others => '0');

            procedure setone (position : string_crate.Cursor)
            is
               optname2 : HT.Text renames string_crate.Element (position);
               ndx : Natural := option_index (optname2);
            begin
               if ndx > 0 then
                  result (ndx) := '1';
               end if;
            end setone;
         begin
            if implies then
               specs.ops_helpers.Element (option_name).IMPLIES_ON.Iterate (setone'Access);
            else
               specs.ops_helpers.Element (option_name).PREVENTS_ON.Iterate (setone'Access);
            end if;
            return result;
         end;
      end affection;

   begin
      specs.ops_standard.Iterate (scan1'Access);

      group := radio;    specs.opt_radio.Iterate (group_scn2'Access);
      group := restrict; specs.opt_restrict.Iterate (group_scn2'Access);
      group := sinlimit; specs.opt_unlimited.Iterate (group_scn2'Access);
      tmpstor2.Reverse_Iterate (nogrup2'Access);

      group := radio;    specs.opt_radio.Iterate (group_scan'Access);
      group := restrict; specs.opt_restrict.Iterate (group_scan'Access);
      group := sinlimit; specs.opt_unlimited.Iterate (group_scan'Access);
      tmpstore.Iterate (nogroup'Access);

      return HT.USS (singles) & HT.USS (answer);
   end option_block_for_dialog;


   --------------------------------------------------------------------------------------------
   --  get_field_value
   --------------------------------------------------------------------------------------------
   function get_field_value (specs : Portspecs; field : spec_field) return String
   is
      procedure concat       (position : string_crate.Cursor);
      procedure scan_contact (position : string_crate.Cursor);
      procedure dump_license (position : string_crate.Cursor);

      joined : HT.Text;

      procedure concat (position : string_crate.Cursor) is
      begin
         if not HT.IsBlank (joined) then
            HT.SU.Append (joined, ", ");
         end if;
         HT.SU.Append (joined, string_crate.Element (position));
      end concat;

      procedure dump_license (position : string_crate.Cursor)
      is
         lic_desc : HT.Text;
      begin
         if not HT.IsBlank (joined) then
            HT.SU.Append (joined, ", ");
         end if;
         declare
            procedure scan_lic_names (position : string_crate.Cursor);

            lic     : String := HT.part_1 (HT.USS (string_crate.Element (position)), ":");
            lictype : license_type := determine_license (lic);

            procedure scan_lic_names (position : string_crate.Cursor)
            is
               raw : String := HT.USS (string_crate.Element (position));
            begin
               if HT.leads (raw, lic) then
                  lic_desc := HT.SUS (HT.part_2 (raw, ":"));
               end if;
            end scan_lic_names;
         begin
            case lictype is
               when CUSTOM1 | CUSTOM2 | CUSTOM3 | CUSTOM4 =>
                  specs.lic_names.Iterate (scan_lic_names'Access);
                  HT.SU.Append (joined, lic_desc);
               when others =>
                  HT.SU.Append (joined, LAT.Quotation & lic & LAT.Quotation);
            end case;
         end;
      end dump_license;

      procedure scan_contact (position : string_crate.Cursor)
      is
         contact : String := HT.USS (string_crate.Element (position));
         email   : String := HT.part_2 (contact, "[");
         guy     : String := HT.replace_all (S      => HT.part_1 (contact, "["),
                                             reject => LAT.Low_Line,
                                             shiny  => LAT.Space);
      begin
         if not HT.IsBlank (joined) then
            HT.SU.Append (joined, ", ");
         end if;
         if contact = contact_nobody then
            HT.SU.Append (joined, contact);
         else
            HT.SU.Append (joined, guy & " [" & email);
         end if;
      end scan_contact;
   begin
      case field is
         when sp_namebase   => return HT.USS (specs.namebase);
         when sp_version    => return HT.USS (specs.version);
         when sp_revision   => return HT.int2str (specs.revision);
         when sp_epoch      => return HT.int2str (specs.epoch);
         when sp_homepage   => return HT.USS (specs.homepage);
         when sp_distsubdir => return HT.USS (specs.dist_subdir);
         when sp_prefix     => return HT.USS (specs.prefix);
         when sp_deprecated => return HT.USS (specs.deprecated);
         when sp_expiration => return HT.USS (specs.expire_date);
         when sp_ug_pkg     => return HT.USS (specs.usergroup_pkg);
         when sp_contacts   =>
            specs.contacts.Iterate (scan_contact'Access);
            return HT.USS (joined);
         when sp_keywords =>
            specs.keywords.Iterate (concat'Access);
            return HT.USS (joined);
         when sp_licenses =>
            specs.licenses.Iterate (dump_license'Access);
            return HT.USS (joined);
         when sp_users =>
            specs.users.Iterate (concat'Access);
            return HT.USS (joined);
         when sp_groups =>
            specs.groups.Iterate (concat'Access);
            return HT.USS (joined);
         when sp_variants =>
            specs.variants.Iterate (concat'Access);
            return HT.USS (joined);
         when others =>
            raise wrong_type with field'Img;
      end case;
   end get_field_value;


   --------------------------------------------------------------------------------------------
   --  get_web_contacts
   --------------------------------------------------------------------------------------------
   function get_web_contacts (specs : Portspecs; subject : String) return String
   is
      procedure scan_contact (position : string_crate.Cursor);

      joined : HT.Text;

      procedure scan_contact (position : string_crate.Cursor)
      is
         contact : String := HT.USS (string_crate.Element (position));
         email   : String := HT.part_1 (HT.part_2 (contact, "["), "]");
         guy     : String := HT.replace_all (S      => HT.part_1 (contact, "["),
                                                        reject => LAT.Low_Line,
                                                        shiny  => LAT.Space);
      begin
         if not HT.IsBlank (joined) then
            HT.SU.Append (joined, ", ");
         end if;
         if contact = contact_nobody then
            HT.SU.Append (joined, contact);
         else
            HT.SU.Append (joined, "<a class=" & LAT.Quotation & "contact" & LAT.Quotation &
                            " href=" & LAT.Quotation & "mailto:" & email & "?subject=" &
                            subject & LAT.Quotation & ">" & guy & "</a>");
         end if;
      end scan_contact;
   begin
      specs.contacts.Iterate (scan_contact'Access);
      return HT.USS (joined);
   end get_web_contacts;


   --------------------------------------------------------------------------------------------
   --  get_json_contacts
   --------------------------------------------------------------------------------------------
   function get_json_contacts (specs : Portspecs) return String
   is
      procedure scan_contact (position : string_crate.Cursor);
      function quote (value : String) return String;

      joined : HT.Text;

      function quote (value : String) return String is
      begin
         return LAT.Quotation & value & LAT.Quotation;
      end quote;

      procedure scan_contact (position : string_crate.Cursor)
      is
         contact : String := HT.USS (string_crate.Element (position));
         email   : String := HT.part_1 (HT.part_2 (contact, "["), "]");
         guy     : String := HT.replace_all (S      => HT.part_1 (contact, "["),
                                             reject => LAT.Low_Line,
                                             shiny  => LAT.Space);
      begin
         if contact /= contact_nobody then
            if not HT.IsBlank (joined) then
               HT.SU.Append (joined, ", ");
            end if;
            HT.SU.Append (joined, "{ " & quote ("name") & ": " & quote (guy) &
                            ", " & quote ("email") & ": " & quote (email) & " }");

         end if;
      end scan_contact;
   begin
      specs.contacts.Iterate (scan_contact'Access);
      if HT.IsBlank (joined) then
         return "";
      else
         return "    ," & quote ("contacts") & ": [ " & HT.USS (joined) & " ]" & LAT.LF;
      end if;
   end get_json_contacts;


   --------------------------------------------------------------------------------------------
   --  get_tagline
   --------------------------------------------------------------------------------------------
   function get_tagline (specs : Portspecs; variant : String) return String
   is
      key : HT.Text := HT.SUS (variant);
   begin
      if specs.taglines.Contains (key) then
         return HT.USS (specs.taglines.Element (key));
      else
         return "";
      end if;
   end get_tagline;


   --------------------------------------------------------------------------------------------
   --  valid_dependency_format
   --------------------------------------------------------------------------------------------
   function valid_dependency_format (value : String) return Boolean is
   begin
      if HT.count_char (value, LAT.Colon) /= 2 then
         return False;
      end if;
      declare
         P1 : String := HT.part_1 (value, ":");
         P2 : String := HT.part_2 (value, ":");
      begin
         if P1'Length = 0 then
            return False;
         end if;
         declare
            dos  : String := HT.part_1 (P2, ":");
            tres : String := HT.part_2 (P2, ":");
         begin
            if dos'Length = 0 or else tres'Length = 0 then
               return False;
            end if;
         end;
      end;
      return True;
   end valid_dependency_format;



   --------------------------------------------------------------------------------------------
   --  calculate_pkgversion
   --------------------------------------------------------------------------------------------
   function calculate_pkgversion (specs : Portspecs) return String
   is
      function suf1 return String;
      function suf2 return String;

      VERA : String := HT.USS (specs.version);
      VERB : String := HT.replace_all (VERA, LAT.Hyphen, LAT.Full_Stop);
      VERC : String := HT.replace_all (VERB, LAT.Comma, LAT.Full_Stop);
      VERD : String := HT.replace_all (VERC, LAT.Low_Line, LAT.Full_Stop);

      function suf1 return String is
      begin
         if specs.revision = 0 then
            return "";
         else
            return "_" & HT.int2str (specs.revision);
         end if;
      end suf1;

      function suf2 return String is
      begin
         if specs.epoch = 0 then
            return "";
         else
            return "," & HT.int2str (specs.epoch);
         end if;
      end suf2;
   begin
      return VERD & suf1 & suf2;
   end calculate_pkgversion;


   --------------------------------------------------------------------------------------------
   --  get_number_of_variants
   --------------------------------------------------------------------------------------------
   function get_number_of_variants (specs : Portspecs) return Natural is
   begin
      return Natural (specs.variants.Length);
   end get_number_of_variants;


   --------------------------------------------------------------------------------------------
   --  get_list_length
   --------------------------------------------------------------------------------------------
   function get_list_length (specs : Portspecs; field : spec_field) return Natural is
   begin
      case field is
         when sp_build_deps    => return Natural (specs.build_deps.Length);
         when sp_buildrun_deps => return Natural (specs.buildrun_deps.Length);
         when sp_run_deps      => return Natural (specs.run_deps.Length);
         when sp_opts_standard => return Natural (specs.ops_standard.Length);
         when sp_opts_avail    => return Natural (specs.ops_avail.Length);
         when sp_notes         => return Natural (specs.pkg_notes.Length);
         when sp_extra_patches => return Natural (specs.extra_patches.Length);
         when sp_distfiles     => return Natural (specs.distfiles.Length);
         when others =>
            raise wrong_type with field'Img;
      end case;
   end get_list_length;


   --------------------------------------------------------------------------------------------
   --  get_list_item
   --------------------------------------------------------------------------------------------
   function get_list_item (specs : Portspecs; field : spec_field; item : Natural) return String
   is
      procedure scan (position : string_crate.Cursor);
      procedure scan_note (position : def_crate.Cursor);
      procedure scan_distfile (position : string_crate.Cursor);

      counter : Natural := 0;
      result  : HT.Text;

      procedure scan (position : string_crate.Cursor) is
      begin
         if HT.IsBlank (result) then
            counter := counter + 1;
            if counter = item then
               result := string_crate.Element (position);
            end if;
         end if;
      end scan;

      procedure scan_note (position : def_crate.Cursor) is
      begin
         if HT.IsBlank (result) then
            counter := counter + 1;
            if counter = item then
               result := HT.SUS (HT.USS (def_crate.Key (position)) & LAT.Equals_Sign &
                                   HT.USS (def_crate.Element (position)));
            end if;
         end if;
      end scan_note;

      procedure scan_distfile (position : string_crate.Cursor) is
      begin
         if HT.IsBlank (result) then
            counter := counter + 1;
            if counter = item then
               result := HT.SUS (translate_distfile
                                 (specs, HT.USS (string_crate.Element (position))));
            end if;
         end if;
      end scan_distfile;
   begin
      case field is
         when sp_build_deps    => specs.build_deps.Iterate (scan'Access);
         when sp_buildrun_deps => specs.buildrun_deps.Iterate (scan'Access);
         when sp_run_deps      => specs.run_deps.Iterate (scan'Access);
         when sp_opts_standard => specs.ops_standard.Iterate (scan'Access);
         when sp_opts_avail    => specs.ops_avail.Iterate (scan'Access);
         when sp_variants      => specs.variants.Iterate (scan'Access);
         when sp_notes         => specs.pkg_notes.Iterate (scan_note'Access);
         when sp_extra_patches => specs.extra_patches.Iterate (scan'Access);
         when sp_distfiles     => specs.distfiles.Iterate (scan_distfile'Access);
         when others =>
            raise wrong_type with field'Img;
      end case;
      return HT.USS (result);
   end get_list_item;


   --------------------------------------------------------------------------------------------
   --  translate_distfile
   --------------------------------------------------------------------------------------------
   function translate_distfile (specs : Portspecs; distfile : String) return String
   is
      tarball : String := HT.part_1 (distfile, ":");
   begin
      if tarball = "generated" then
         declare
            group  : String := HT.part_2 (distfile, ":");
            dlsite : String :=
              HT.USS (specs.dl_sites.Element (HT.SUS (group)).list.First_Element);
         begin
            if HT.leads (dlsite, "GITHUB/") then
               return generate_github_distfile (dlsite);
            elsif HT.leads (dlsite, "GITLAB/") then
               return generate_gitlab_distfile (dlsite);
            elsif HT.leads (dlsite, "CRATES/") then
               return generate_crates_distfile (dlsite);
            else
               --  future generations
              return "implement me: " & distfile;
            end if;
         end;
      else
         return tarball;
      end if;
   end translate_distfile;


   --------------------------------------------------------------------------------------------
   --  repology_distfile
   --------------------------------------------------------------------------------------------
   function repology_distfile (specs : Portspecs; distfile : String) return String
   is
      tarball : constant String := translate_distfile (specs, distfile);
      group   : constant String := HT.part_2 (distfile, ":");
   begin
      if specs.dl_sites.Element (HT.SUS (group)).list.Is_Empty then
         return "missing_site_definition://" & tarball;
      end if;
      declare
         dlsite  : String := HT.USS (specs.dl_sites.Element (HT.SUS (group)).list.First_Element);
      begin
         if HT.contains (dlsite, "://") then
            return dlsite & tarball;
         else
            return "mirror://" & dlsite & "/" & tarball;
         end if;
      end;
   exception
      when others =>
         TIO.Put_Line ("failed repology_distfile for " & HT.DQ (specs.get_namebase)
                       & ", group=" & group);
         return "parse error: " & tarball;
   end repology_distfile;


   --------------------------------------------------------------------------------------------
   --  get_repology_distfile
   --------------------------------------------------------------------------------------------
   function get_repology_distfile (specs : Portspecs; item : Natural) return String
   is
      procedure scan_distfile (position : string_crate.Cursor);

      counter : Natural := 0;
      result  : HT.Text;

      procedure scan_distfile (position : string_crate.Cursor) is
      begin
         if HT.IsBlank (result) then
            counter := counter + 1;
            if counter = item then
               result := HT.SUS (repology_distfile
                                 (specs, HT.USS (string_crate.Element (position))));
            end if;
         end if;
      end scan_distfile;
   begin
      specs.distfiles.Iterate (scan_distfile'Access);
      return HT.USS (result);
   end get_repology_distfile;


   --------------------------------------------------------------------------------------------
   --  get_subpackage_length
   --------------------------------------------------------------------------------------------
   function get_subpackage_length (specs : Portspecs; variant : String) return Natural
   is
      variant_text : HT.Text := HT.SUS (variant);
   begin
      if specs.subpackages.Contains (variant_text) then
         return Natural (specs.subpackages.Element (variant_text).list.Length);
      else
         return 0;
      end if;
   end get_subpackage_length;


   --------------------------------------------------------------------------------------------
   --  get_subpackage_item
   --------------------------------------------------------------------------------------------
   function get_subpackage_item
     (specs   : Portspecs;
      variant : String;
      item    : Natural) return String
   is
      procedure scan (position : string_crate.Cursor);

      variant_text : HT.Text := HT.SUS (variant);
      counter : Natural := 0;
      result  : HT.Text;

      procedure scan (position : string_crate.Cursor) is
      begin
         if HT.IsBlank (result) then
            counter := counter + 1;
            if counter = item then
               result := string_crate.Element (position);
            end if;
         end if;
      end scan;
   begin
      if specs.subpackages.Contains (variant_text) then
         specs.subpackages.Element (variant_text).list.Iterate (scan'Access);
      end if;
      return HT.USS (result);
   end get_subpackage_item;


   --------------------------------------------------------------------------------------------
   --  get_number_extra_run
   --------------------------------------------------------------------------------------------
   function get_number_extra_run (specs : Portspecs; subpackage : String) return Natural
   is
      spkg_text : HT.Text := HT.SUS (subpackage);
   begin
      if specs.extra_rundeps.Contains (spkg_text) then
         return Natural (specs.extra_rundeps.Element (spkg_text).list.Length);
      else
         return 0;
      end if;
   end get_number_extra_run;


   --------------------------------------------------------------------------------------------
   --  get_extra_runtime
   --------------------------------------------------------------------------------------------
   function get_extra_runtime
     (specs      : Portspecs;
      subpackage : String;
      item       : Natural) return String
   is
      procedure scan (position : string_crate.Cursor);

      spkg_text : HT.Text := HT.SUS (subpackage);
      counter   : Natural := 0;
      result    : HT.Text;

      procedure scan (position : string_crate.Cursor) is
      begin
         if HT.IsBlank (result) then
            counter := counter + 1;
            if counter = item then
               result := string_crate.Element (position);
            end if;
         end if;
      end scan;
   begin
      if specs.extra_rundeps.Contains (spkg_text) then
         specs.extra_rundeps.Element (spkg_text).list.Iterate (scan'Access);
      end if;
      return HT.USS (result);
   end get_extra_runtime;


   --------------------------------------------------------------------------------------------
   --  aggregated_ignore_reason
   --------------------------------------------------------------------------------------------
   function aggregated_ignore_reason (specs : Portspecs) return String
   is
      procedure precheck (position : list_crate.Cursor);
      procedure scribe   (position : list_crate.Cursor);

      result      : HT.Text;
      num_reasons : Natural := 0;
      curnum      : Natural := 1;

      procedure precheck (position : list_crate.Cursor)
      is
         procedure precheck_list (position : string_crate.Cursor);

         broken_Key : String := HT.USS (list_crate.Element (position).group);

         procedure precheck_list (position : string_crate.Cursor) is
         begin
            if broken_Key = broken_all then
               num_reasons := num_reasons + 1;
            end if;
         end precheck_list;
      begin
         list_crate.Element (position).list.Iterate (Process => precheck_list'Access);
      end precheck;

      procedure scribe (position : list_crate.Cursor)
      is
         procedure check_list (position : string_crate.Cursor);

         broken_Key : String := HT.USS (list_crate.Element (position).group);

         procedure check_list (position : string_crate.Cursor)
         is
            reason : String  := HT.USS (string_crate.Element (position));
            used   : Boolean := False;
         begin
            if broken_Key = broken_all then
               if num_reasons > 1 then
                  HT.SU.Append (result, "[Reason " & HT.int2str (curnum) & "] ");
               end if;
               HT.SU.Append (result, reason);
               curnum := curnum + 1;
            end if;
         end check_list;
      begin
         list_crate.Element (position).list.Iterate (Process => check_list'Access);
      end scribe;
   begin
      specs.broken.Iterate (Process => precheck'Access);
      if num_reasons > 0 then
         specs.broken.Iterate (Process => scribe'Access);
      end if;
      return HT.USS (result);
   end aggregated_ignore_reason;


   --------------------------------------------------------------------------------------------
   --  broken_all_set
   --------------------------------------------------------------------------------------------
   function broken_all_set (specs : Portspecs) return Boolean
   is
      procedure precheck (position : list_crate.Cursor);

      result : Boolean := False;

      procedure precheck (position : list_crate.Cursor)
      is
         procedure precheck_list (position : string_crate.Cursor);

         broken_Key : String := HT.USS (list_crate.Element (position).group);

         procedure precheck_list (position : string_crate.Cursor) is
         begin
            if broken_Key = broken_all then
               result := True;
            end if;
         end precheck_list;
      begin
         list_crate.Element (position).list.Iterate (Process => precheck_list'Access);
      end precheck;
   begin
      specs.broken.Iterate (Process => precheck'Access);
      return result;
   end broken_all_set;


   --------------------------------------------------------------------------------------------
   --  invalid_namebase
   --------------------------------------------------------------------------------------------
   function invalid_namebase (value : String; allow_comma : Boolean) return Boolean is
   begin
      for x in value'Range loop
         case value (x) is
            when '0' .. '9' => null;
            when 'A' .. 'Z' => null;
            when 'a' .. 'z' => null;
            when '_' | '-' | '.' => null;
            when ',' =>
               if not allow_comma then
                  return True;
               end if;
            when others => return True;
         end case;
      end loop;
      --  namebases can contain dots, but not as first character
      if value'Length > 0 and then value (value'First) = '.' then
         return True;
      end if;
      return False;
   end invalid_namebase;


   --------------------------------------------------------------------------------------------
   --  valid_uses_module
   --------------------------------------------------------------------------------------------
   function options_summary (specs : Portspecs; variant : String) return String
   is
      procedure scan (position : option_crate.Cursor);
      procedure format (position : string_crate.Cursor);
      function obtain_description (optname : String; optname_text : HT.Text) return String;

      tempstore : string_crate.Vector;
      block : HT.Text;

      procedure scan (position : option_crate.Cursor)
      is
         rec : Option_Helper renames option_crate.Element (position);
      begin
         tempstore.Append (rec.option_name);
      end scan;

      function obtain_description (optname : String; optname_text : HT.Text) return String
      is
         given_desc : constant String :=
           HT.USS (specs.ops_helpers.Element (optname_text).option_description);
         desc_opt   : described_option_set;
      begin
         if HT.IsBlank (given_desc) then
            --  It should never happen that desc_opt = OPT_NOT_DEFINED
            desc_opt := described_option (optname);
            if desc_opt /= OPT_NOT_DEFINED then
               return default_description (desc_opt);
            end if;
         end if;
         return given_desc;
      end obtain_description;

      procedure format (position : string_crate.Cursor)
      is
         optname_text : HT.Text renames string_crate.Element (position);
         optname : String := HT.USS (optname_text);
         curval  : Boolean := specs.ops_helpers.Element (optname_text).currently_set_ON;
         desc    : String  := obtain_description (optname, optname_text);
         --  The option name is limited to 14 characters.  Format:
         --  123456789-12345678-1234
         --  OPTION_NAMEXXX  OFF  Description ...
         --  OPTION_2        ON   Description ...
         part1   : String (1 .. 16) := (others => ' ');
         part2   : String (1 .. 5);
      begin
         if variant = variant_standard then
            if not specs.ops_standard.Contains (optname_text) then
               --  Don't display non-standard options for standard variant
               return;
            end if;
         end if;
         part1 (1 .. optname'Length) := optname;
         if curval then
            part2 := "ON   ";
         else
            part2 := "OFF  ";
         end if;
         HT.SU.Append (block, part1 & part2 & desc & LAT.LF);
      end format;
   begin
      if specs.ops_avail.Contains (HT.SUS (options_none)) then
         return "This port has no build options.";
      end if;
      specs.ops_helpers.Iterate (scan'Access);
      sorter.Sort (Container => tempstore);
      tempstore.Iterate (format'Access);
      return HT.USS (block);
   end options_summary;


   --------------------------------------------------------------------------------------------
   --  missing_subpackage_definition
   --------------------------------------------------------------------------------------------
   function missing_subpackage_definition (specs : Portspecs) return Boolean
   is
      procedure check (position : list_crate.Cursor);

      triggered : Boolean := False;

      procedure check (position : list_crate.Cursor)
      is
         rec : group_list renames list_crate.Element (position);
      begin
         if rec.list.Is_Empty then
            triggered := True;
         end if;
      end check;
   begin
      specs.subpackages.Iterate (check'Access);
      return triggered;
   end missing_subpackage_definition;


   --------------------------------------------------------------------------------------------
   --  combined_dependency_origins
   --------------------------------------------------------------------------------------------
   function combined_dependency_origins
     (specs        : Portspecs;
      include_run  : Boolean;
      limit_to_run : Boolean) return String
   is
      procedure scan  (position : string_crate.Cursor);
      procedure print (position : string_crate.Cursor);
      procedure scan_package (position : list_crate.Cursor);

      combined : string_crate.Vector;
      result   : HT.Text;

      procedure scan (position : string_crate.Cursor)
      is
         --  One exception:
         --  When a port has a dependency on itself (possible with EXRUN[]), skip

         text_value  : HT.Text renames string_crate.Element (position);
         dep_namebase : constant String :=  HT.specific_field (HT.USS (text_value), 1, ":");
      begin
         if not HT.equivalent (specs.namebase, dep_namebase) then
            if not combined.Contains (text_value) then
               combined.Append (text_value);
            end if;
         end if;
      end scan;

      procedure print (position : string_crate.Cursor)
      is
         text_value : HT.Text renames string_crate.Element (position);
      begin
         HT.SU.Append (result, HT.USS (text_value) & LAT.LF);
      end print;

      procedure scan_package (position : list_crate.Cursor)
      is
         rec : group_list renames list_crate.Element (position);
      begin
         rec.list.Iterate (scan'Access);
      end scan_package;

   begin
      if not limit_to_run then
         specs.build_deps.Iterate (scan'Access);
      end if;

      specs.buildrun_deps.Iterate (scan'Access);

      if limit_to_run or else include_run then
         specs.run_deps.Iterate (scan'Access);
         specs.extra_rundeps.Iterate (scan_package'Access);
      end if;

      combined.Iterate (print'Access);
      return HT.USS (result);
   end combined_dependency_origins;


   --------------------------------------------------------------------------------------------
   --  valid_uses_module
   --------------------------------------------------------------------------------------------
   function valid_uses_module (value : String) return Boolean
   is
      total_modules : constant Positive := 72;

      subtype uses_string is String (1 .. 15);

      --  Keep in alphabetical order for future conversion to binary search
      all_keywords : constant array (1 .. total_modules) of uses_string :=
        (
         "ada            ",
         "autoreconf     ",
         "bdb            ",
         "bison          ",
         "bz2            ",
         "c++            ",
         "cargo          ",
         "cclibs         ",
         "charsetfix     ",
         "clang          ",
         "cmake          ",
         "compiler       ",
         "cpe            ",
         "cran           ",
         "desktop-utils  ",
         "destdirfix     ",
         "display        ",
         "dos2unix       ",
         "execinfo       ",
         "expat          ",
         "fbsd10fix      ",
         "firebird       ",
         "fonts          ",
         "fortran        ",
         "gem            ",
         "gettext-runtime",
         "gettext-tools  ",
         "gif            ",
         "gmake          ",
         "gnome-icons    ",
         "gprbuild       ",
         "gtk-doc        ",
         "iconv          ",
         "imake          ",
         "intltoolfix    ",
         "jpeg           ",
         "libtool        ",
         "lua            ",
         "lz4            ",
         "lzo            ",
         "macfix         ",
         "makeinfo       ",
         "mesa           ",
         "meson          ",
         "mime-info      ",
         "mysql          ",
         "ncurses        ",
         "ninja          ",
         "pcre           ",
         "perl           ",
         "perl-interp    ",
         "pgsql          ",
         "php            ",
         "pkgconfig      ",
         "png            ",
         "python         ",
         "qt5            ",
         "qt6            ",
         "readline       ",
         "ruby           ",
         "schemas        ",
         "scons          ",
         "shebangfix     ",
         "solaris-funcs  ",
         "solfix         ",
         "sqlite         ",
         "ssl            ",
         "tcl            ",
         "tiff           ",
         "xz             ",
         "zlib           ",
         "zstd           "
        );
      bandolier : uses_string := (others => ' ');

   begin
      declare
         module : String := HT.part_1 (value, ":");
      begin
         if module'Length > uses_string'Length then
            return False;
         end if;

         bandolier (1 .. module'Length) := module;
      end;

      for index in all_keywords'Range loop
         if all_keywords (index) = bandolier then
            return True;
         end if;
      end loop;

      return False;
   end valid_uses_module;


   --------------------------------------------------------------------------------------------
   --  valid_info_page
   --------------------------------------------------------------------------------------------
   function valid_info_page (specs : in out Portspecs; value : String) return Boolean
   is
      procedure grow (Key : HT.Text; Element : in out group_list);
      --  duplicity has already been checked when we get to this routine
      --  INFO_SUBDIR is defined in catchall as a result.

      num_sep     : Natural := HT.count_char (value, LAT.Solidus);
      INFO_SUBDIR : constant String := "INFO_SUBDIR";
      first_one   : Boolean := not specs.catch_all.Contains (HT.SUS (INFO_SUBDIR));
      NO_SUBDIR   : constant String := ".";
      saved_value : HT.Text;

      procedure grow (Key : HT.Text; Element : in out group_list) is
      begin
         Element.list.Append (saved_value);
      end grow;
   begin
      if first_one then
         specs.establish_group (sp_catchall, INFO_SUBDIR);
         if num_sep = 0 then
            saved_value := HT.SUS (NO_SUBDIR);
         else
            saved_value := HT.SUS (HT.head (value, "/"));
         end if;
         specs.catch_all.Update_Element (Position => specs.catch_all.Find (HT.SUS (INFO_SUBDIR)),
                                         Process  => grow'Access);
         return True;
      else
         saved_value := specs.catch_all.Element (HT.SUS (INFO_SUBDIR)).list.First_Element;
         if num_sep = 0 then
            return HT.equivalent (saved_value, NO_SUBDIR);
         else
            return HT.equivalent (saved_value, HT.head (value, "/"));
         end if;
      end if;
   end valid_info_page;


   --------------------------------------------------------------------------------------------
   --  valid_broken_mysql_value
   --------------------------------------------------------------------------------------------
   function valid_broken_mysql_value (value : String) return Boolean is
   begin
      return
        (
         HT.leads (value, "oracle-") and then
             (value = "oracle-5.7" or else
              value = "oracle-8.0")
        )
        or else
          (
           HT.leads (value, "percona-") and then
             (value = "percona-5.6" or else
              value = "percona-5.7" or else
              value = "percona-8.0")
          )
          or else
            (HT.leads (value, "mariadb-") and then
               (value = "mariadb-10.2" or else
                value = "mariadb-10.3" or else
                value = "mariadb-10.4" or else
                value = "mariadb-10.5")
            );
   end valid_broken_mysql_value;


   --------------------------------------------------------------------------------------------
   --  valid_broken_pgsql_value
   --------------------------------------------------------------------------------------------
   function valid_broken_pgsql_value (value : String) return Boolean is
   begin
      return
        value = "14" or else
        value = "13" or else
        value = "12" or else
        value = "11" or else
        value = "10";
   end valid_broken_pgsql_value;


   --------------------------------------------------------------------------------------------
   --  rpath_check_errors_are_fatal
   --------------------------------------------------------------------------------------------
   function rpath_check_errors_are_fatal (specs : Portspecs) return Boolean is
   begin
      return specs.fatal_rpath;
   end rpath_check_errors_are_fatal;


   --------------------------------------------------------------------------------------------
   --  debugging_is_on
   --------------------------------------------------------------------------------------------
   function debugging_is_on (specs : Portspecs) return Boolean is
   begin
      return specs.debugging_on;
   end debugging_is_on;


   --------------------------------------------------------------------------------------------
   --  determine_gnome_component
   --------------------------------------------------------------------------------------------
   function determine_gnome_component (component : String) return gnome_type
   is
      total_keywords : constant Positive := gnome_type'Pos (gnome_type'Last) + 1;

      subtype keyword_string is String (1 .. 18);

      type keyword_pair is
         record
            keyword : keyword_string;
            keytype : gnome_type;
         end record;

      --  It is critical that this list be alphabetized correctly.

      all_keywords : constant array (1 .. total_keywords) of keyword_pair :=
        (
         ("INVALID           ", invalid_component),
         ("atk               ", atk),
         ("atkmm             ", atkmm),
         ("atkmm16           ", atkmm16),
         ("cairo             ", cairo),
         ("cairomm           ", cairomm),
         ("cairomm10         ", cairomm10),
         ("dconf             ", dconf),
         ("gconf             ", gconf),
         ("gdkpixbuf         ", gdkpixbuf),
         ("glib              ", glib),
         ("glibmm            ", glibmm),
         ("glibmm24          ", glibmm24),
         ("gtk2              ", gtk2),
         ("gtk3              ", gtk3),
         ("gtk4              ", gtk4),
         ("gtkmm30           ", gtkmm30),
         ("gtkmm40           ", gtkmm40),
         ("gtksourceview3    ", gtksourceview3),
         ("intltool          ", intltool),
         ("introspection     ", introspection),
         ("libcroco          ", libcroco),
         ("libglade          ", libglade),
         ("libgsf            ", libgsf),
         ("libidl            ", libidl),
         ("librsvg           ", librsvg),
         ("libsigc++20       ", libsigcxx2),
         ("libxml++26        ", libxmlxx2),
         ("libxml2           ", libxml2),
         ("libxslt           ", libxslt),
         ("orbit2            ", orbit2),
         ("pango             ", pango),
         ("pangomm           ", pangomm),
         ("pangomm14         ", pangomm14),
         ("pygobject         ", pygobject),
         ("vte               ", vte)
        );

--  atspi
--  esound
--  evolutionserver
--  gconfmm26
--  gnomecontrolcenter
--  gnomedesktop
--  gnomedocutils
--  gnomemenus
--  gnomemimedata
--  gnomeprefix
--  gnomesharp20
--  gnomespeech
--  gnomevfs2
--  gsound
--  gtkiconcache
--  gtkhtml3
--  gtkhtml4
--  gtkmm20
--  gtksharp20
--  gtksourceview
--  gtksourceviewmm
--  gvfs
--  libartlgpl2
--  libbonobo
--  libbonoboui
--  libgda-ui
--  libgdamm
--  libgnome
--  libgnomecanvas
--  libgnomekbd
--  libgnomeprint
--  libgnomeprintui
--  libgnomeui
--  libgtkhtml
--  libgtksourceviewmm
--  libsigc++12
--  libwnck
--  metacity
--  nautilus3
--  pangox-compat
--  pygnome2
--  pygtksourceview
--  referencehack

      bandolier    : keyword_string := (others => LAT.Space);
      Low          : Natural := all_keywords'First;
      High         : Natural := all_keywords'Last;
      Mid          : Natural;
   begin
      if component'Length > keyword_string'Length or else
        component'Length < 3
      then
         return invalid_component;
      end if;

      bandolier (1 .. component'Length) := component;

      loop
         Mid := (Low + High) / 2;
         if bandolier = all_keywords (Mid).keyword  then
            return all_keywords (Mid).keytype;
         elsif bandolier < all_keywords (Mid).keyword then
            exit when Low = Mid;
            High := Mid - 1;
         else
            exit when High = Mid;
            Low := Mid + 1;
         end if;
      end loop;
      return invalid_component;
   end determine_gnome_component;


   --------------------------------------------------------------------------------------------
   --  determine_xorg_component
   --------------------------------------------------------------------------------------------
   function determine_xorg_component (component : String) return xorg_type
   is
      total_keywords : constant Positive := xorg_type'Pos (xorg_type'Last) + 1;

      subtype keyword_string is String (1 .. 18);

      type keyword_pair is
         record
            keyword : keyword_string;
            keytype : xorg_type;
         end record;

      --  It is critical that this list be alphabetized correctly.

      all_keywords : constant array (1 .. total_keywords) of keyword_pair :=
        (
         ("INVALID           ", invalid_component),
         ("dmx               ", dmx),
         ("fontcacheproto    ", fontcacheproto),
         ("fontenc           ", fontenc),
         ("fontutil          ", fontutil),
         ("ice               ", ice),
         ("pciaccess         ", pciaccess),
         ("pixman            ", pixman),
         ("printproto        ", printproto),
         ("sm                ", sm),
         ("x11               ", x11),
         ("xau               ", xau),
         ("xaw               ", xaw),
         ("xbitmaps          ", xbitmaps),
         ("xcb               ", xcb),
         ("xcb-render-util   ", xcb_render_util),
         ("xcb-util          ", xcb_util),
         ("xcb-util-cursor   ", xcb_util_cursor),
         ("xcb-util-image    ", xcb_util_image),
         ("xcb-util-keysyms  ", xcb_util_keysyms),
         ("xcb-util-wm       ", xcb_util_wm),
         ("xcb-util-xrm      ", xcb_util_xrm),
         ("xcomposite        ", xcomposite),
         ("xcursor           ", xcursor),
         ("xdamage           ", xdamage),
         ("xdmcp             ", xdmcp),
         ("xext              ", xext),
         ("xfixes            ", xfixes),
         ("xfont             ", xfont),
         ("xfont2            ", xfont2),
         ("xfontcache        ", xfontcache),
         ("xft               ", xft),
         ("xi                ", xi),
         ("xinerama          ", xinerama),
         ("xkbfile           ", xkbfile),
         ("xmu               ", xmu),
         ("xorgproto         ", xorgproto),
         ("xpm               ", xpm),
         ("xprop             ", xprop),
         ("xrandr            ", xrandr),
         ("xrender           ", xrender),
         ("xres              ", xres),
         ("xscrnsaver        ", xscrnsaver),
         ("xset              ", xset),
         ("xshmfence         ", xshmfence),
         ("xt                ", xt),
         ("xtransproto       ", xtransproto),
         ("xtst              ", xtst),
         ("xv                ", xv),
         ("xvmc              ", xvmc),
         ("xxf86dga          ", xxf86dga),
         ("xxf86vm           ", xxf86vm)
        );

      bandolier    : keyword_string := (others => LAT.Space);
      Low          : Natural := all_keywords'First;
      High         : Natural := all_keywords'Last;
      Mid          : Natural;
   begin
      if component'Length > keyword_string'Length or else
        component'Length < 2
      then
         return invalid_component;
      end if;

      bandolier (1 .. component'Length) := component;

      loop
         Mid := (Low + High) / 2;
         if bandolier = all_keywords (Mid).keyword  then
            return all_keywords (Mid).keytype;
         elsif bandolier < all_keywords (Mid).keyword then
            exit when Low = Mid;
            High := Mid - 1;
         else
            exit when High = Mid;
            Low := Mid + 1;
         end if;
      end loop;
      return invalid_component;
   end determine_xorg_component;


   --------------------------------------------------------------------------------------------
   --  determine_sdl_component
   --------------------------------------------------------------------------------------------
   function determine_sdl_component (component : String) return sdl_type
   is

      total_keywords : constant Positive := sdl_type'Pos (sdl_type'Last) + 1;

      subtype keyword_string is String (1 .. 8);

      type keyword_pair is
         record
            keyword : keyword_string;
            keytype : sdl_type;
         end record;

      --  It is critical that this list be alphabetized correctly.

      all_keywords : constant array (1 .. total_keywords) of keyword_pair :=
        (
         ("INVALID ", invalid_component),
         ("gfx1    ", gfx1),
         ("gfx2    ", gfx2),
         ("image1  ", image1),
         ("image2  ", image2),
         ("mixer1  ", mixer1),
         ("mixer2  ", mixer2),
         ("net1    ", net1),
         ("net2    ", net2),
         ("sdl1    ", sdl1),
         ("sdl2    ", sdl2),
         ("ttf1    ", ttf1),
         ("ttf2    ", ttf2)
        );

      bandolier    : keyword_string := (others => LAT.Space);
      Low          : Natural := all_keywords'First;
      High         : Natural := all_keywords'Last;
      Mid          : Natural;
   begin
      if component'Length > keyword_string'Length or else
        component'Length < 2
      then
         return invalid_component;
      end if;

      bandolier (1 .. component'Length) := component;

      loop
         Mid := (Low + High) / 2;
         if bandolier = all_keywords (Mid).keyword  then
            return all_keywords (Mid).keytype;
         elsif bandolier < all_keywords (Mid).keyword then
            exit when Low = Mid;
            High := Mid - 1;
         else
            exit when High = Mid;
            Low := Mid + 1;
         end if;
      end loop;
      return invalid_component;
   end determine_sdl_component;


   --------------------------------------------------------------------------------------------
   --  determine_php_extension
   --------------------------------------------------------------------------------------------
   function determine_php_extension (component : String) return phpext_type
   is

      total_keywords : constant Positive := phpext_type'Pos (phpext_type'Last) + 1;

      subtype keyword_string is String (1 .. 12);

      type keyword_pair is
         record
            keyword : keyword_string;
            keytype : phpext_type;
         end record;

      --  It is critical that this list be alphabetized correctly.

      all_keywords : constant array (1 .. total_keywords) of keyword_pair :=
        (
         ("INVALID     ", invalid_extension),
         ("bcmath      ", bcmath),
         ("bitset      ", bitset),
         ("bz2         ", bz2),
         ("calendar    ", calendar),
         ("ctype       ", ctype),
         ("curl        ", curl),
         ("dba         ", dba),
         ("dom         ", dom),
         ("enchant     ", enchant),
         ("exif        ", exif),
         ("ffi         ", ffi),
         ("fileinfo    ", fileinfo),
         ("filter      ", filter),
         ("ftp         ", ftp),
         ("gd          ", gd),
         ("gettext     ", gettext),
         ("gmp         ", gmp),
         ("hash        ", hash),
         ("iconv       ", iconv),
         ("igbinary    ", igbinary),
         ("imap        ", imap),
         ("interbase   ", interbase),
         ("intl        ", intl),
         ("json        ", jsonext),
         ("ldap        ", ldap),
         ("mbstring    ", mbstring),
         ("mcrypt      ", mcrypt),
         ("memcache    ", memcache),
         ("memcached   ", memcached),
         ("mysqli      ", mysqli),
         ("odbc        ", odbc),
         ("opcache     ", opcache),
         ("openssl     ", openssl),
         ("pcntl       ", pcntl),
         ("pdf         ", pdf),
         ("pdo         ", pdo),
         ("pdo_dblib   ", pdo_dblib),
         ("pdo_firebird", pdo_firebird),
         ("pdo_mysql   ", pdo_mysql),
         ("pdo_odbc    ", pdo_odbc),
         ("pdo_pgsql   ", pdo_pgsql),
         ("pdo_sqlite  ", pdo_sqlite),
         ("pgsql       ", pgsql),
         ("phar        ", phar),
         ("posix       ", posix),
         ("pspell      ", pspell),
         ("radius      ", radius),
         ("readline    ", readline),
         ("recode      ", recode),
         ("redis       ", redis),
         ("session     ", session),
         ("shmop       ", shmop),
         ("simplexml   ", simplexml),
         ("snmp        ", snmp),
         ("soap        ", soap),
         ("sockets     ", sockets),
         ("sodium      ", sodium),
         ("sqlite3     ", sqlite3),
         ("sysvmsg     ", sysvmsg),
         ("sysvsem     ", sysvsem),
         ("sysvshm     ", sysvshm),
         ("tidy        ", tidy),
         ("tokenizer   ", tokenizer),
         ("wddx        ", wddx),
         ("xml         ", xml),
         ("xmlreader   ", xmlreader),
         ("xmlrpc      ", xmlrpc),
         ("xmlwriter   ", xmlwriter),
         ("xsl         ", xsl),
         ("zip         ", zip),
         ("zlib        ", zlib)
        );

      bandolier    : keyword_string := (others => LAT.Space);
      Low          : Natural := all_keywords'First;
      High         : Natural := all_keywords'Last;
      Mid          : Natural;
   begin
      if component'Length > keyword_string'Length or else
        component'Length < 2
      then
         return invalid_extension;
      end if;

      bandolier (1 .. component'Length) := component;

      loop
         Mid := (Low + High) / 2;
         if bandolier = all_keywords (Mid).keyword  then
            return all_keywords (Mid).keytype;
         elsif bandolier < all_keywords (Mid).keyword then
            exit when Low = Mid;
            High := Mid - 1;
         else
            exit when High = Mid;
            Low := Mid + 1;
         end if;
      end loop;
      return invalid_extension;
   end determine_php_extension;


   --------------------------------------------------------------------------------------------
   --  keyword_is_valid
   --------------------------------------------------------------------------------------------
   function keyword_is_valid (keyword : String) return Boolean
   is
      total_keywords : constant Positive := 72;

      subtype keyword_string is String (1 .. 13);

      --  It is critical that this list be alphabetized correctly.
      all_keywords : constant array (1 .. total_keywords) of keyword_string :=
        (
         "accessibility",
         "ada          ",
         "arabic       ",
         "archivers    ",
         "astro        ",
         "audio        ",
         "benchmarks   ",
         "biology      ",
         "c++          ",
         "cad          ",
         "chinese      ",
         "comms        ",
         "converters   ",
         "cran         ",
         "csharp       ",
         "databases    ",
         "deskutils    ",
         "devel        ",
         "dns          ",
         "editors      ",
         "emulators    ",
         "finance      ",
         "french       ",
         "ftp          ",
         "games        ",
         "geography    ",
         "german       ",
         "graphics     ",
         "irc          ",
         "italian      ",
         "japanese     ",
         "java         ",
         "javascript   ",
         "korean       ",
         "lang         ",
         "lisp         ",
         "mail         ",
         "math         ",
         "misc         ",
         "multimedia   ",
         "net          ",
         "net_im       ",
         "net_mgmt     ",
         "net_p2p      ",
         "news         ",
         "perl         ",
         "php          ",
         "print        ",
         "python       ",
         "raven        ",
         "ruby         ",
         "russian      ",
         "rust         ",
         "scheme       ",
         "science      ",
         "security     ",
         "shells       ",
         "spanish      ",
         "sysutils     ",
         "textproc     ",
         "vietnamese   ",
         "wayland      ",
         "www          ",
         "x11          ",
         "x11_clocks   ",
         "x11_drivers  ",
         "x11_fm       ",
         "x11_fonts    ",
         "x11_servers  ",
         "x11_themes   ",
         "x11_toolkits ",
         "x11_wm       "
        );

      testword_len : constant Natural := keyword'Length;
      bandolier    : keyword_string := (others => LAT.Space);
      Low          : Natural := all_keywords'First;
      High         : Natural := all_keywords'Last;
      Mid          : Natural;

   begin
      if testword_len < 3 or else testword_len > keyword_string'Length then
         return False;
      end if;
      bandolier (1 .. testword_len) := keyword;

      loop
         Mid := (Low + High) / 2;
         if bandolier = all_keywords (Mid) then
            return True;
         elsif bandolier < all_keywords (Mid) then
            exit when Low = Mid;
            High := Mid - 1;
         else
            exit when High = Mid;
            Low := Mid + 1;
         end if;
      end loop;
      return False;

   end keyword_is_valid;


   --------------------------------------------------------------------------------------------
   --  determine_license
   --------------------------------------------------------------------------------------------
   function determine_license (value : String) return license_type
   is
      total_keywords : constant Positive := license_type'Pos (license_type'Last) + 1;

      subtype keyword_string is String (1 .. 10);

      type keyword_pair is
         record
            keyword : keyword_string;
            keytype : license_type;
         end record;

      --  It is critical that this list be alphabetized correctly.
      all_keywords : constant array (1 .. total_keywords) of keyword_pair :=
        (
         ("AFL       ", AFL),
         ("AGPLv3    ", AGPLv3),
         ("AGPLv3+   ", AGPLv3x),
         ("APACHE10  ", APACHE10),
         ("APACHE11  ", APACHE11),
         ("APACHE20  ", APACHE20),
         ("ART10     ", ART10),
         ("ART20     ", ART20),
         ("ARTPERL10 ", ARTPERL10),
         ("BSD2CLAUSE", BSD2CLAUSE),
         ("BSD3CLAUSE", BSD3CLAUSE),
         ("BSD4CLAUSE", BSD4CLAUSE),
         ("BSDGROUP  ", BSDGROUP),
         ("CC0_10    ", CC0_10),
         ("CC_30     ", CC_30),
         ("CC_40     ", CC_40),
         ("CC_NCND_30", CC_NCND_30),
         ("CC_NCND_40", CC_NCND_40),
         ("CC_NCSA_30", CC_NCSA_30),
         ("CC_NCSA_40", CC_NCSA_40),
         ("CC_NC_30  ", CC_NC_30),
         ("CC_NC_40  ", CC_NC_40),
         ("CC_ND_30  ", CC_ND_30),
         ("CC_ND_40  ", CC_ND_40),
         ("CC_SA_30  ", CC_SA_30),
         ("CC_SA_40  ", CC_SA_40),
         ("CDDL      ", CDDL),
         ("CUSTOM1   ", CUSTOM1),
         ("CUSTOM2   ", CUSTOM2),
         ("CUSTOM3   ", CUSTOM3),
         ("CUSTOM4   ", CUSTOM4),
         ("GFDL      ", GFDL),
         ("GMGPL     ", GMGPL),
         ("GMGPL3    ", GMGPL3),
         ("GPLv1     ", GPLv1),
         ("GPLv1+    ", GPLv1x),
         ("GPLv2     ", GPLv2),
         ("GPLv2+    ", GPLv2x),
         ("GPLv3     ", GPLv3),
         ("GPLv3+    ", GPLv3x),
         ("GPLv3RLE  ", GPLv3RLE),
         ("GPLv3RLE+ ", GPLv3RLEx),
         ("HPND      ", HPND),
         ("INVALID   ", INVALID),
         ("ISCL      ", ISCL),
         ("LGPL20    ", LGPL20),
         ("LGPL20+   ", LGPL20x),
         ("LGPL21    ", LGPL21),
         ("LGPL21+   ", LGPL21x),
         ("LGPL3     ", LGPL3),
         ("LGPL3+    ", LGPL3x),
         ("MIT       ", MIT),
         ("MPL       ", MPL),
         ("OpenSSL   ", OPENSSL),
         ("PSFL      ", PSFL),
         ("PUBDOM    ", PUBDOM),
         ("PostgreSQL", POSTGRESQL),  --  lowercase "o" after all capitals
         ("RUBY      ", RUBY),
         ("ZLIB      ", ZLIB)
        );

      bandolier    : keyword_string := (others => LAT.Space);
      Low          : Natural := all_keywords'First;
      High         : Natural := all_keywords'Last;
      Mid          : Natural;
   begin
      if value'Length > keyword_string'Length or else
        value'Length < 3
      then
         return INVALID;
      end if;

      bandolier (1 .. value'Length) := value;

      loop
         Mid := (Low + High) / 2;
         if bandolier = all_keywords (Mid).keyword  then
            return all_keywords (Mid).keytype;
         elsif bandolier < all_keywords (Mid).keyword then
            exit when Low = Mid;
            High := Mid - 1;
         else
            exit when High = Mid;
            Low := Mid + 1;
         end if;
      end loop;
      return INVALID;
   end determine_license;


   --------------------------------------------------------------------------------------------
   --  described_option
   --------------------------------------------------------------------------------------------
   function described_option (value : String) return described_option_set
   is
      total_keywords : constant Positive :=
        described_option_set'Pos (described_option_set'Last);

      subtype keyword_string is String (1 .. 14);

      type keyword_pair is
         record
            keyword : keyword_string;
            keytype : described_option_set;
         end record;

      --  It is critical that this list be alphabetized correctly.
      all_keywords : constant array (1 .. total_keywords) of keyword_pair :=
        (
         ("AALIB         ", AALIB),
         ("ALSA          ", ALSA),
         ("ASM           ", ASM),
         ("COLORD        ", COLORD),
         ("CUPS          ", CUPS),
         ("DBUS          ", DBUS),
         ("DEBUG         ", DEBUG),
         ("DOCS          ", DOCS),
         ("FIREBIRD      ", FIREBIRD),
         ("ICONV         ", ICONV),
         ("IDN           ", IDN),
         ("IPV4          ", IPV4),
         ("IPV6          ", IPV6),
         ("JAVA          ", JAVA),
         ("LANG_CN       ", LANG_CN),
         ("LANG_KO       ", LANG_KO),
         ("LANG_RU       ", LANG_RU),
         ("LDAP          ", LDAP),
         ("LDAPS         ", LDAPS),
         ("MYSQL         ", MYSQL),
         ("NAS           ", NAS),
         ("NLS           ", NLS),
         ("OPENGL        ", OPENGL),
         ("OSS           ", OSS),
         ("PERL_534      ", PERL534),
         ("PERL_536      ", PERL536),
         ("PGSQL         ", PGSQL),
         ("PNG           ", PNG),
         ("PULSEAUDIO    ", PULSEAUDIO),
         ("PY310         ", PY310),
         ("PY311         ", PY311),
         ("READLINE      ", READLINE),
         ("RUBY30        ", RUBY30),
         ("RUBY31        ", RUBY31),
         ("RUBY32        ", RUBY32),
         ("SNDIO         ", SNDIO),
         ("SOUND         ", SOUND),
         ("SQLITE        ", SQLITE),
         ("STATIC        ", STATIC),
         ("SYSLOG        ", SYSLOG),
         ("TCL           ", TCL),
         ("TCLTK         ", TCLTK),
         ("THREADS       ", THREADS),
         ("X11           ", X11),
         ("ZLIB          ", ZLIB)
        );

      bandolier    : keyword_string := (others => LAT.Space);
      Low          : Natural := all_keywords'First;
      High         : Natural := all_keywords'Last;
      Mid          : Natural;
   begin

      if value'Length > keyword_string'Length or else
        value'Length < 3
      then
         return OPT_NOT_DEFINED;
      end if;

      bandolier (1 .. value'Length) := value;

      loop
         Mid := (Low + High) / 2;
         if bandolier = all_keywords (Mid).keyword  then
            return all_keywords (Mid).keytype;
         elsif bandolier < all_keywords (Mid).keyword then
            exit when Low = Mid;
            High := Mid - 1;
         else
            exit when High = Mid;
            Low := Mid + 1;
         end if;
      end loop;
      return OPT_NOT_DEFINED;
   end described_option;


   --------------------------------------------------------------------------------------------
   --  default_description
   --------------------------------------------------------------------------------------------
   function default_description (option : described_option_set) return String is
   begin
      case option is
         when AALIB        => return "AAlib graphics library support";
         when ALSA         => return "ALSA audio architecture support";
         when ASM          => return "Use optimized assembly code";
         when COLORD       => return "Color management via colord";
         when CUPS         => return "CUPS printing system support";
         when DBUS         => return "D-Bus IPC system support";
         when DEBUG        => return "Build with debugging support";
         when DOCS         => return "Build and install documentation";
         when FIREBIRD     => return "Firebird (Interbase) database support";
         when ICONV        => return "Encoding conversion support via iconv";
         when IDN          => return "International Domain Names support";
         when IPV4         => return "IPv4 protocol support";
         when IPV6         => return "IPv6 protocol support";
         when JAVA         => return "Java platform support";
         when LANG_CN      => return "Chinese language support";
         when LANG_KO      => return "Korean language support";
         when LANG_RU      => return "Russian language support";
         when LDAP         => return "LDAP protocol support";
         when LDAPS        => return "LDAP protocol over SSL support";
         when MYSQL        => return "MySQL database support";
         when NAS          => return "Network Audio System support";
         when NLS          => return "Native Language Support";
         when OPENGL       => return "2D/3D rendering support via OpenGL";
         when OSS          => return "Open Sound System support";
         when PERL534      => return "Build using Perl 5.34";
         when PERL536      => return "Build using Perl 5.36";
         when PGSQL        => return "PostgreSQL database support";
         when PNG          => return "PNG image format support";
         when PULSEAUDIO   => return "PulseAudio sound server support";
         when PY310        => return "Build using Python 3.10";
         when PY311        => return "Build using Python 3.11";
         when READLINE     => return "Command line editing via libreadline";
         when RUBY30       => return "Build using Ruby version 3.0";
         when RUBY31       => return "Build using Ruby version 3.1";
         when RUBY32       => return "Build using Ruby version 3.2";
         when SNDIO        => return "Sndio audio support";
         when SOUND        => return "Sound (audio) support";
         when SQLITE       => return "SQLite database support";
         when STATIC       => return "Build static executables and/or libraries";
         when SYSLOG       => return "Syslog logging support";
         when TCL          => return "Tcl scripting language support";
         when TCLTK        => return "Tcl/Tk GUI toolkit support";
         when THREADS      => return "Threading support";
         when X11          => return "X11 (graphics) support";
         when ZLIB         => return "zlib compression support";
         when OPT_NOT_DEFINED => return "dev error, OPT_NOT_DEFINED";
      end case;
   end default_description;


   --------------------------------------------------------------------------------------------
   --  post_parse_usergroup_check_passes
   --------------------------------------------------------------------------------------------
   function post_parse_usergroup_check_passes (specs : Portspecs) return Boolean
   is
      ugspkg_defined : constant Boolean := not HT.IsBlank (specs.usergroup_pkg);
   begin
      if specs.users.Is_Empty and then specs.groups.Is_Empty then
         if ugspkg_defined then
            TIO.Put_Line ("Warning: USERGROUP_SPKG is set, but GROUPS and USERS are not.");
         end if;
         return True;
      end if;
      if not ugspkg_defined then
         --  Fatal error provided by specification_parser
         return False;
      end if;
      return True;
   end post_parse_usergroup_check_passes;


   --------------------------------------------------------------------------------------------
   --  post_parse_license_check_passes
   --------------------------------------------------------------------------------------------
   function post_parse_license_check_passes (specs : Portspecs) return Boolean
   is
      procedure dump_name (position : string_crate.Cursor);
      procedure check_for_custom_name (position : string_crate.Cursor);
      procedure check_source_for_awk (position : string_crate.Cursor);

      still_good    : Boolean := True;
      found_custom  : Boolean := False;
      src_awk_good  : Boolean := True;
      lic_count     : Natural;
      solo_scheme   : constant Boolean :=  HT.equivalent (specs.lic_scheme, "solo");
      tempstorage   : string_crate.Vector;
      report_card, card2 : array (1 .. 4) of Boolean := (others => False);

      procedure dump_name (position : string_crate.Cursor)
      is
         lic_part : HT.Text := HT.SUS (HT.part_1 (HT.USS (string_crate.Element (position)), ":"));
         lpstr    : String  := HT.USS (lic_part);
         lic_type : license_type := determine_license (lpstr);
      begin
         if still_good then
            if tempstorage.Contains (lic_part) then
               TIO.Put_Line ("Duplicate LICENSE found on multiple subpackage: " & lpstr);
               still_good := False;
            else
               tempstorage.Append (lic_part);
               case lic_type is
                  when CUSTOM1 | CUSTOM2 | CUSTOM3 | CUSTOM4 =>
                     found_custom := True;
                  when others => null;
               end case;
               case lic_type is
                  when CUSTOM1 => report_card (1) := True;
                  when CUSTOM2 => report_card (2) := True;
                  when CUSTOM3 => report_card (3) := True;
                  when CUSTOM4 => report_card (4) := True;
                  when others => null;
               end case;
            end if;
         end if;
      end dump_name;

      procedure check_for_custom_name (position : string_crate.Cursor)
      is
         lic_part : HT.Text := HT.SUS (HT.part_1 (HT.USS (string_crate.Element (position)), ":"));
         lpstr    : String  := HT.USS (lic_part);
         lic_type : license_type := determine_license (lpstr);
         err_msg  : String := "Duplicate entries for LICENSE_NAME, CUSTOM";
      begin
         --  due to previous check, we already know lic_type is CUSTOM1 .. CUSTOM4
         --  We also know that all entries have matched against LICENSES
         if still_good then
            case lic_type is
               when CUSTOM1 =>
                  if card2 (1) then
                     TIO.Put_Line (err_msg & "1");
                     still_good := False;
                  end if;
                  card2 (1) := True;
                  report_card (1) := False;
            when CUSTOM2 =>
                  if card2 (2) then
                     TIO.Put_Line (err_msg & "2");
                     still_good := False;
                  end if;
                  card2 (2) := True;
                  report_card (2) := False;
            when CUSTOM3 =>
                  if card2 (3) then
                     TIO.Put_Line (err_msg & "3");
                     still_good := False;
                  end if;
                  card2 (3) := True;
                  report_card (3) := False;
            when CUSTOM4 =>
                  if card2 (4) then
                     TIO.Put_Line (err_msg & "4");
                     still_good := False;
                  end if;
                  card2 (4) := True;
                  report_card (4) := False;
            when others => null;
            end case;
         end if;
      end check_for_custom_name;

      procedure check_source_for_awk (position : string_crate.Cursor)
      is
         procedure check_src (src_pos : string_crate.Cursor);
         found_src : Boolean := False;
         lic_part : HT.Text := HT.SUS (HT.part_1 (HT.USS (string_crate.Element (position)), ":"));

         procedure check_src (src_pos : string_crate.Cursor)
         is
            lic_src : HT.Text := HT.SUS (HT.part_1 (HT.USS (string_crate.Element (src_pos)), ":"));
         begin
            if not found_src then
               found_src := HT.equivalent (lic_part, lic_src);
            end if;
         end check_src;
      begin
         if src_awk_good then
            specs.lic_source.Iterate (check_src'Access);
            if not found_src then
               TIO.Put_Line ("No LICENSE_SOURCE entry found for LICENSE_AWK " &
                               HT.USS (lic_part) & " entry");
               src_awk_good := False;
            end if;
         end if;
      end check_source_for_awk;
   begin
      if specs.licenses.Is_Empty then
         --  no license case, but make sure everything else is blank too
         if not specs.lic_names.Is_Empty or else
           not specs.lic_source.Is_Empty or else
           not specs.lic_awk.Is_Empty or else
           not specs.lic_files.Is_Empty
         then
            TIO.Put_Line ("LICENSE is empty but LICENSE_(NAMES,FILES,SOURCE,AWK) is not");
            return False;
         end if;
         if HT.IsBlank (specs.lic_scheme) or else solo_scheme then
            return True;
         else
            TIO.Put_Line ("LICENSE is empty but LICENSE_SCHEME = " & HT.USS (specs.lic_scheme));
            return False;
         end if;
      end if;

      --  Finish checking LICENSE_SCHEME
      lic_count := Natural (specs.licenses.Length);
      if HT.IsBlank (specs.lic_scheme) then
         TIO.Put_Line ("LICENSE_SCHEME must also be defined when LICENSE is defined.");
         return False;
      else
         if lic_count = 1 then
            if not solo_scheme then
               TIO.Put_Line ("LICENSE_SCHEME must be 'solo' when there is only one license.");
               return False;
            end if;
         else
            if solo_scheme then
               TIO.Put_Line ("LICENSE_SCHEME cannot be 'solo' with multiple licenses.");
               return False;
            end if;
         end if;
      end if;

      --  We don't need to check the values of LICENSE_FILE because we
      --  already validated those when the data was input.  What didn't didn't do was check that
      --  all entries were present, so all we have to do is compare totals.

      if Natural (specs.lic_files.Length) /= lic_count then
         TIO.Put_Line ("There are less LICENSE_FILES than LICENSES");
         return False;
      end if;

      --  LICENSE_NAME only has to be defined for CUSTOM1--4
      specs.licenses.Iterate (dump_name'Access);
      if found_custom then
         specs.lic_names.Iterate (check_for_custom_name'Access);
         if still_good then
            if report_card (1) or else
              report_card (2) or else
              report_card (3) or else
              report_card (4)
            then
               TIO.Put_Line ("There is at least one CUSTOMx LICENSE that doesn't have a " &
                               "defined LICENSE_NAME");
               return False;
            end if;
         end if;
      end if;

      --  If AWK is defined, SOURCE must be defined (and vice versa)
      if specs.lic_awk.Is_Empty and then not specs.lic_source.Is_Empty then
         TIO.Put_Line ("LICENSE_AWK is defined which requires LICENSE_SOURCE to be defined");
         return False;
      end if;
      if specs.lic_source.Is_Empty and then not specs.lic_awk.Is_Empty then
         TIO.Put_Line ("LICENSE_SOURCE is defined which requires LICENSE_AWK to be defined");
         return False;
      end if;

      --  Verify AWK and SOURCE have same count.
      if Natural (specs.lic_awk.Length) /= Natural (specs.lic_source.Length) then
         TIO.Put_Line ("There are not equal definitions LICENSE_AWK and LICENSE_SOURCE");
         return False;
      end if;

      --  Check each AWK has a source (error message in loop)
      specs.lic_awk.Iterate (check_source_for_awk'Access);
      if not src_awk_good then
         return False;
      end if;

      return still_good;

   end post_parse_license_check_passes;


   --------------------------------------------------------------------------------------------
   --  post_parse_option_group_size_passes
   --------------------------------------------------------------------------------------------
   function post_parse_option_group_size_passes (specs : Portspecs) return Boolean
   is
      procedure group_scan (position : list_crate.Cursor);

      all_good : Boolean := True;

      procedure group_scan (position : list_crate.Cursor) is
      begin
         if Natural (list_crate.Element (position).list.Length) < 2 then
            TIO.Put_Line ("The " & HT.USS (list_crate.Element (position).group) &
                            " group has less than 2 members");
            all_good := False;
         end if;
      end group_scan;
   begin
      specs.optgroups.Iterate (group_scan'Access);
      return all_good;
   end post_parse_option_group_size_passes;


   --------------------------------------------------------------------------------------------
   --  post_transform_option_group_defaults_passes
   --------------------------------------------------------------------------------------------
   function post_transform_option_group_defaults_passes (specs : Portspecs) return Boolean
   is
      procedure radio_scan (pos_radio : string_crate.Cursor);
      procedure restrict_scan (pos_radio : string_crate.Cursor);
      procedure group_scan (position : string_crate.Cursor);
      function display_opt_count (number : Natural) return String;

      all_good : Boolean := True;
      on_count : Natural;

      function display_opt_count (number : Natural) return String is
      begin
         if number = 1 then
            return "1 option";
         else
            return HT.int2str (number) & " options";
         end if;
      end display_opt_count;

      procedure group_scan (position : string_crate.Cursor)
      is
         option : HT.Text renames string_crate.Element (position);
      begin
         if specs.ops_helpers.Contains (option) then
            if specs.ops_helpers.Element (option).set_ON_by_default then
               on_count := on_count + 1;
            end if;
         else
            all_good := False;
            TIO.Put_Line ("option" & HT.USS (option) & " is not defined.  bug?");
            return;
         end if;
      end group_scan;

      procedure radio_scan (pos_radio : string_crate.Cursor)
      is
         group : HT.Text renames string_crate.Element (pos_radio);
      begin
         on_count := 0;
         if specs.optgroups.Contains (group) then
            specs.optgroups.Element (group).list.Iterate (group_scan'Access);
         else
            all_good := False;
            TIO.Put_Line ("radio group " & HT.USS (group) & " does not exist.  bug?");
            return;
         end if;
         if on_count /= 1 then
            all_good := False;
            TIO.Put_Line ("radio group " & HT.USS (group) & " has " & display_opt_count (on_count)
                          & " set by default, but radio groups require exactly 1");
         end if;
      end radio_scan;

      procedure restrict_scan (pos_radio : string_crate.Cursor)
      is
         group : HT.Text renames string_crate.Element (pos_radio);
      begin
         on_count := 0;
         if specs.optgroups.Contains (group) then
            specs.optgroups.Element (group).list.Iterate (group_scan'Access);
         else
            all_good := False;
            TIO.Put_Line ("restricted group " & HT.USS (group) & " does not exist.  bug?");
            return;
         end if;
         if on_count < 1 then
            all_good := False;
            TIO.Put_Line ("restricted group " & HT.USS (group) & " has no options set by " &
                          "default, but at least one is required");
         end if;
      end restrict_scan;
   begin
      specs.opt_radio.Iterate (radio_scan'Access);
      specs.opt_restrict.Iterate (restrict_scan'Access);
      return all_good;
   end post_transform_option_group_defaults_passes;


   --------------------------------------------------------------------------------------------
   --  subpackage_exists
   --------------------------------------------------------------------------------------------
   function subpackage_exists (specs : Portspecs; subpackage : String) return Boolean
   is
      procedure scan_variant    (pos_variant : string_crate.Cursor);
      procedure scan_subpackage (pos_subpkg : string_crate.Cursor);

      found : Boolean := False;

      procedure scan_variant    (pos_variant : string_crate.Cursor) is
         variant : HT.Text renames string_crate.Element (pos_variant);
      begin
         if not found then
            if specs.subpackages.Contains (variant) then
               specs.subpackages.Element (variant).list.Iterate (scan_subpackage'Access);
            end if;
         end if;
      end scan_variant;

      procedure scan_subpackage (pos_subpkg : string_crate.Cursor)
      is
         subpkg : HT.Text renames string_crate.Element (pos_subpkg);
      begin
         if not found then
            if subpackage = HT.USS (subpkg) then
               found := True;
            end if;
         end if;
      end scan_subpackage;
   begin
      specs.variants.Iterate (scan_variant'Access);
      return found;
   end subpackage_exists;


   --------------------------------------------------------------------------------------------
   --  get_license_scheme
   --------------------------------------------------------------------------------------------
   function get_license_scheme (specs : Portspecs) return String
   is
      actual : String := HT.USS (specs.lic_scheme);
   begin
      if actual = "dual" or else actual = "multi" then
         return actual;
      end if;
      return "single";
   end get_license_scheme;


   --------------------------------------------------------------------------------------------
   --  global_options_present
   --------------------------------------------------------------------------------------------
   function global_options_present (specs : Portspecs) return Boolean is
   begin
      return not specs.ops_avail.Contains (HT.SUS (options_none));
   end global_options_present;


   --------------------------------------------------------------------------------------------
   --  standard_options_present
   --------------------------------------------------------------------------------------------
   function standard_options_present (specs : Portspecs) return Boolean is
   begin
      return not specs.ops_standard.Contains (HT.SUS (options_none));
   end standard_options_present;


   --------------------------------------------------------------------------------------------
   --  module_subpackage_failed
   --------------------------------------------------------------------------------------------
   function module_subpackage_failed
     (specs        : Portspecs;
      base_module  : String;
      given_module : String) return Boolean
   is
      procedure check_variant    (position : string_crate.Cursor);
      procedure check_subpackage (position : string_crate.Cursor);
      --  Assume module starts with "terminfo" for sure.
      --  If we match any known subpackage, we pass

      candidate : HT.Text;
      matched   : Boolean := False;

      procedure check_variant (position : string_crate.Cursor)
      is
         variant : HT.Text renames string_crate.Element (position);
      begin
         if not matched then
            specs.subpackages.Element (variant).list.Iterate (check_subpackage'Access);
         end if;
      end check_variant;

      procedure check_subpackage (position : string_crate.Cursor) is
         spkg : HT.Text renames string_crate.Element (position);
      begin
         if not matched then
            if HT.equivalent (spkg, candidate) then
               matched := True;
            end if;
         end if;
      end check_subpackage;

   begin
      if not HT.leads (given_module, base_module & ":") then
         return True;
      end if;

      candidate := HT.SUS (HT.part_2 (given_module, ":"));
      specs.variants.Iterate (check_variant'Access);

      return not matched;
   end module_subpackage_failed;


   --------------------------------------------------------------------------------------------
   --  generate_github_distfile
   --------------------------------------------------------------------------------------------
   function generate_github_distfile (download_site : String) return String
   is
      gh_args    : constant String  := HT.part_2 (download_site, "/");
      num_colons : constant Natural := HT.count_char (gh_args, LAT.Colon);
      gh_ext     : constant String  := ".tar.gz";
   begin
      if num_colons < 2 then
         --  NOT EXPECTED!!!  give garbage so maintainer notices and fixes it
         return gh_args & gh_ext;
      end if;
      declare
         acct : constant String := HT.specific_field (gh_args, 1, ":");
         proj : constant String := HT.specific_field (gh_args, 2, ":");
         vers : constant String := HT.replace_all (S      => HT.specific_field (gh_args, 3, ":"),
                                                   reject => LAT.Plus_Sign,
                                                   shiny  => LAT.Hyphen);
      begin
         if vers (vers'First) = 'v' then
            return acct & LAT.Hyphen & proj & LAT.Hyphen &
              vers (vers'First + 1 .. vers'Last) & gh_ext;
         else
            return acct & LAT.Hyphen & proj & LAT.Hyphen & vers & gh_ext;
         end if;
      end;
   end generate_github_distfile;


   --------------------------------------------------------------------------------------------
   --  generate_gitlab_distfile
   --------------------------------------------------------------------------------------------
   function generate_gitlab_distfile (download_site : String) return String
   is
      lab_args    : constant String  := HT.part_2 (download_site, "/");
      num_colons  : constant Natural := HT.count_char (lab_args, LAT.Colon);
      lab_ext     : constant String  := ".tar.gz";
   begin
      if num_colons < 2 then
         --  NOT EXPECTED!!!  give garbage so maintainer notices and fixes it
         return lab_args & lab_ext;
      end if;
      declare
         acct : constant String := HT.specific_field (lab_args, 1, ":");
         proj : constant String := HT.specific_field (lab_args, 2, ":");
         vers : constant String := HT.specific_field (lab_args, 3, ":");
      begin
         return acct & LAT.Hyphen & proj & LAT.Hyphen & vers & lab_ext;
      end;
   end generate_gitlab_distfile;


   --------------------------------------------------------------------------------------------
   --  generate_crates_distfile
   --------------------------------------------------------------------------------------------
   function generate_crates_distfile (download_site : String) return String
   is
      url_args    : constant String  := HT.part_2 (download_site, "/");
      num_colons  : constant Natural := HT.count_char (url_args, LAT.Colon);
      file_ext    : constant String  := ".tar.gz";
   begin
      if num_colons < 1 then
         --  NOT EXPECTED!!!  give garbage so maintainer notices and fixes it
         return url_args & file_ext;
      end if;
      declare
         proj : constant String := HT.specific_field (url_args, 1, ":");
         vers : constant String := HT.specific_field (url_args, 2, ":");
      begin
         return proj & LAT.Hyphen & vers & file_ext;
      end;
   end generate_crates_distfile;


   --------------------------------------------------------------------------------------------
   --  post_parse_opt_desc_check_passes
   --------------------------------------------------------------------------------------------
   function post_parse_opt_desc_check_passes (specs : Portspecs) return Boolean
   is
      procedure scan (position : string_crate.Cursor);

      all_good : Boolean := True;

      procedure scan (position : string_crate.Cursor) is
      begin
         declare
            --  we only care if *any* description is defined, so check defaults first
            option   : HT.Text renames string_crate.Element (position);
            opt_name : String := HT.USS (option);
            desc_opt : described_option_set := described_option (opt_name);
            errormsg : constant String := "Option '" & opt_name & "' has no description.";
         begin
            if opt_name = options_none then
               return;
            end if;
            if desc_opt = OPT_NOT_DEFINED then
               --  No default description, so check for defined version
               if specs.ops_helpers.Contains (option) then
                  if HT.IsBlank (specs.ops_helpers.Element (option).option_description) then
                     all_good := False;
                     TIO.Put_Line (errormsg);
                  end if;
               else
                  all_good := False;
                  TIO.Put_Line (errormsg);
               end if;
            end if;
         end;
      end scan;
   begin
      specs.ops_avail.Iterate (scan'Access);
      return all_good;
   end post_parse_opt_desc_check_passes;


   --------------------------------------------------------------------------------------------
   --  info_page_check_message
   --------------------------------------------------------------------------------------------
   function info_page_check_message (specs : Portspecs; value : String) return String
   is
      nfo : constant String := "INFO entry '" & value & "' ";
   begin
      if HT.count_char (value, LAT.Colon) /= 1 then
         return nfo & "is not prefixed by a subpackage";
      end if;
      declare
         canspkg : String := HT.part_1 (value, ":");
      begin
         if not specs.subpackage_exists (canspkg) then
            declare
               otherside : String := HT.part_2 (value, ":");
            begin
               if specs.subpackage_exists (otherside) then
                  return nfo & "is reversed; the subpackage must be listed first";
               else
                  return nfo & "is not prefixed by a recognized subpackage";
               end if;
            end;
         end if;
      end;
      return "";
   end info_page_check_message;


   --------------------------------------------------------------------------------------------
   --  extra_uses_modules_sanity_check
   --------------------------------------------------------------------------------------------
   function extra_uses_modules_sanity_check_passes
     (specs  : Portspecs;
      module : String;
      errmsg : out HT.Text) return Boolean
   is
      stripped      : String  := HT.part_1 (module, ":");
      module_args   : String  := HT.specific_field (module, 2, ":");
      text_stripped : HT.Text := HT.SUS (stripped);
      comma_cnt     : Natural;
   begin
      --  Sanity check for compiler modules
      if stripped = "ada" or else
        stripped = "c++" or else
        stripped = "fortan" or else
        stripped = "cclibs"
      then
         if HT.IsBlank (module_args) then
            errmsg := HT.SUS ("subpackage not provided for " & stripped & " module");
            return False;
         else
            --  check every provided submodule
            comma_cnt := HT.count_char (module_args, ',') + 1;
            for X in Natural range 1 .. comma_cnt loop
               declare
                  spkg : constant String := HT.specific_field (module_args, X, ",");
               begin
                  if not specs.subpackage_exists (spkg) then
                     errmsg := HT.SUS (stripped & " module subpackage unrecognized: " & spkg);
                     return False;
                  end if;
               end;
            end loop;
         end if;
      end if;

      --  Don't allow macfix and libtool to be specified together (libtool brings it in)
      if (stripped = "libtool" and then specs.uses_base.Contains (HT.SUS ("macfix"))) or else
        (stripped = "macfix" and then specs.uses_base.Contains (HT.SUS ("libtool")))
      then
         errmsg := HT.SUS ("macfix and libtool USES modules detected together; remove macfix");
         return False;
      end if;
      return True;
   end extra_uses_modules_sanity_check_passes;


   --------------------------------------------------------------------------------------------
   --  port_is_generated
   --------------------------------------------------------------------------------------------
   function port_is_generated (specs : Portspecs) return Boolean is
   begin
      return specs.generated;
   end port_is_generated;


   --------------------------------------------------------------------------------------------
   --  watchdog_disabled
   --------------------------------------------------------------------------------------------
   function watchdog_disabled (specs : Portspecs) return Boolean is
   begin
      return specs.kill_watchdog;
   end watchdog_disabled;


   --------------------------------------------------------------------------------------------
   --  set_parse_error
   --------------------------------------------------------------------------------------------
   procedure set_parse_error (specs : in out Portspecs; error : String) is
   begin
      specs.parse_error := HT.SUS (error);
   end set_parse_error;


   --------------------------------------------------------------------------------------------
   --  get_parse_error
   --------------------------------------------------------------------------------------------
   function get_parse_error (specs : Portspecs) return String is
   begin
      return HT.USS (specs.parse_error);
   end get_parse_error;


   --------------------------------------------------------------------------------------------
   --  get_parse_error
   --------------------------------------------------------------------------------------------
   function definition_exists (specs : Portspecs; variable : String) return Boolean
   is
   begin
      return specs.definitions.Contains (HT.SUS (variable));
   end definition_exists;


   --------------------------------------------------------------------------------------------
   --  define
   --------------------------------------------------------------------------------------------
   procedure define (specs : in out Portspecs; variable : String; value : String) is
   begin
      if value = "" then
         raise missing_extract with "variable " & variable & " evaluates to a blank string";
      end if;
      specs.definitions.Insert (HT.SUS (variable), HT.SUS (value));
   end define;


   --------------------------------------------------------------------------------------------
   --  definition
   --------------------------------------------------------------------------------------------
   function definition (specs : Portspecs; variable : String) return String is
   begin
      return HT.USS (specs.definitions.Element (HT.SUS (variable)));
   end definition;


   --------------------------------------------------------------------------------------------
   --  no_definitions
   --------------------------------------------------------------------------------------------
   function no_definitions (specs : Portspecs) return Boolean
   is
   begin
      return specs.definitions.Is_Empty;
   end no_definitions;


   --------------------------------------------------------------------------------------------
   --  equivalent_fpc_port
   --------------------------------------------------------------------------------------------
   function equivalent_fpc_port (specs : Portspecs) return String
   is
      result : HT.Text := HT.SUS ("N/A");
      key : HT.Text := HT.SUS ("FPC_EQUIVALENT");
   begin
      if specs.catch_all.Contains (key) then
         result := specs.catch_all.Element (key).list.First_Element;
      end if;
      return HT.USS (result);
   exception
      when others =>
         return "equivalent_fpc_port/error";
   end equivalent_fpc_port;


   --------------------------------------------------------------------------------------------
   --  base_module
   --------------------------------------------------------------------------------------------
   function base_module (index : smodules) return String is
   begin
      case index is
         when 1 => return "schemas";
         when 2 => return "mime-info";
         when 3 => return "gnome-icons";
         when 4 => return "desktop-utils";
      end case;
   end base_module;


   --------------------------------------------------------------------------------------------
   --  run_dependency
   --------------------------------------------------------------------------------------------
   function run_dependency (specs : Portspecs; dependency : String) return Boolean
   is
      procedure scan1 (position1 : list_crate.Cursor);

      key   : HT.Text := HT.SUS (dependency);
      found : Boolean := False;

      procedure scan1 (position1 : list_crate.Cursor) is
      begin
         if not found then
            found := list_crate.Element (position1).list.Contains (key);
         end if;
      end scan1;
   begin
      if specs.buildrun_deps.Contains (key) or else specs.run_deps.Contains (key) then
         return True;
      end if;
      specs.extra_rundeps.Iterate (scan1'Access);
      return found;
   end run_dependency;


   --------------------------------------------------------------------------------------------
   --  get_ssl_variant
   --------------------------------------------------------------------------------------------
   function get_ssl_variant (specs : Portspecs; normal_variant : String) return String
   is
      procedure scan (position : string_crate.Cursor);
      function known_ssl_variant (candidate : String) return Boolean;

      modname    : constant String := "ssl";
      ssl_module : HT.Text := HT.SUS (modname);
      result     : HT.Text := HT.SUS (normal_variant);
      found      : Boolean := False;

      procedure scan (position : string_crate.Cursor)
      is
         value_text : HT.Text renames string_crate.Element (position);
         value      : String := HT.USS (value_text);
      begin
         if not found then
            declare
               modulestr : String := HT.part_1 (value, ":");
            begin
               if modulestr = modname then
                  found := True;
                  if HT.count_char (value, LAT.Colon) = 1 then
                     declare
                        argv       : constant String := HT.part_2 (value, ":");
                        num_commas : Natural := HT.count_char (argv, LAT.Comma);
                     begin
                        if num_commas = 0 then
                           if known_ssl_variant (argv) then
                              result := HT.SUS (argv);
                           end if;
                        else
                           for x in 1 .. num_commas + 1 loop
                              declare
                                 ax : constant String := HT.specific_field (argv, x, ",");
                              begin
                                 if known_ssl_variant (ax) then
                                    result := HT.SUS (ax);
                                    exit;
                                 end if;
                              end;
                           end loop;
                        end if;
                     end;
                  end if;
               end if;
            end;
         end if;
      end scan;

      function known_ssl_variant (candidate : String) return Boolean
      is
         OSS1 : constant String := "openssl10";
         OSS2 : constant String := "openssl11";
         OSS3 : constant String := "openssl30";
         LSS1 : constant String := "libressl";
         LSS2 : constant String := "libressl-devel";
      begin
         return
           candidate = OSS1 or else
           candidate = OSS2 or else
           candidate = OSS3 or else
           candidate = LSS1 or else
           candidate = LSS2;
      end known_ssl_variant;

   begin
      if not specs.uses_base.Contains (ssl_module)
      then
         return normal_variant;
      end if;
      specs.uses.Iterate (scan'Access);
      return HT.USS (result);
   end get_ssl_variant;


   --------------------------------------------------------------------------------------------
   --  last_catchall_key
   --------------------------------------------------------------------------------------------
   function option_already_in_group (specs : Portspecs; option_name : String) return Boolean
   is
      procedure scan1 (position1 : list_crate.Cursor);
      procedure scan2 (position2 : string_crate.Cursor);

      found : Boolean := False;

      procedure scan2 (position2 : string_crate.Cursor) is
      begin
         if not found then
            found := HT.equivalent (string_crate.Element (position2), option_name);
         end if;
      end scan2;

      procedure scan1 (position1 : list_crate.Cursor) is
      begin
         list_crate.Element (position1).list.Iterate (scan2'Access);
      end scan1;
   begin
      specs.optgroups.Iterate (scan1'Access);
      return found;
   end option_already_in_group;


   --------------------------------------------------------------------------------------------
   --  last_catchall_key
   --------------------------------------------------------------------------------------------
   function last_catchall_key (specs : Portspecs) return String is
   begin
      return HT.USS (specs.last_catchkey);
   end last_catchall_key;


   --------------------------------------------------------------------------------------------
   --  dump_specification
   --------------------------------------------------------------------------------------------
   procedure dump_specification (specs : Portspecs)
   is
      procedure print_item (position : string_crate.Cursor);
      procedure print_tagline (position : def_crate.Cursor);
      procedure print_catchall (position : def_crate.Cursor);
      procedure print_line_item (position : string_crate.Cursor);
      procedure dump (position : list_crate.Cursor);
      procedure dump_target (position : list_crate.Cursor);
      procedure dump_option (position : option_crate.Cursor);
      procedure print_vector_list (thelabel : String; thelist : spec_field);
      procedure print_group_list  (thelabel : String; thelist : spec_field);
      procedure print_single (thelabel : String; thelist : spec_field);
      procedure print_boolean (thelabel : String; thelist : spec_field);
      procedure print_opt_vector (vec : string_crate.Vector; thelabel : String);
      procedure print_define (flavor : Positive);

      procedure print_item (position : string_crate.Cursor)
      is
         index : Natural := string_crate.To_Index (position);
      begin
         if index > 1 then
            TIO.Put (" ");
         end if;
         TIO.Put (HT.USS (string_crate.Element (position)));
      end print_item;

      procedure print_tagline (position : def_crate.Cursor) is
      begin
         TIO.Put_Line ("SDESC[" & HT.USS (def_crate.Key (position)) & LAT.Right_Square_Bracket &
                         LAT.HT & LAT.HT & HT.USS (def_crate.Element (position)));
      end print_tagline;

      procedure print_catchall (position : def_crate.Cursor)
      is
         varname : String  := HT.USS (def_crate.Key (position));
         value   : String  := HT.USS (def_crate.Element (position));
         vnlen   : Natural := varname'Length + 1;
         tritab  : String (1 .. 3) := LAT.HT & LAT.HT & LAT.HT;
         frtmark : Natural := 1;
      begin
         if vnlen < 8 then
            frtmark := 3;
         elsif vnlen < 16 then
            frtmark := 2;
         end if;
         TIO.Put_Line (varname & LAT.Equals_Sign & tritab (1 .. frtmark) & value);
      end print_catchall;

      procedure print_line_item (position : string_crate.Cursor)
      is
         index : Natural := string_crate.To_Index (position);
      begin
         TIO.Put_Line (HT.USS (string_crate.Element (position)));
      end print_line_item;

      procedure dump (position : list_crate.Cursor)
      is
         NDX : String := HT.USS (list_crate.Element (position).group);
      begin
         TIO.Put ("   " & NDX);
         if NDX'Length < 5 then
            TIO.Put (LAT.HT & LAT.HT & LAT.HT);
         elsif NDX'Length < 13 then
            TIO.Put (LAT.HT & LAT.HT);
         else
            TIO.Put (LAT.HT);
         end if;
         list_crate.Element (position).list.Iterate (Process => print_item'Access);
         TIO.Put (LAT.LF);
      end dump;

      procedure dump_target (position : list_crate.Cursor)
      is
         NDX : String := HT.USS (list_crate.Element (position).group);
      begin
         TIO.Put_Line ("   " & NDX & LAT.Colon);
         list_crate.Element (position).list.Iterate (Process => print_line_item'Access);
      end dump_target;

      procedure dump_option (position : option_crate.Cursor)
      is
         rec : Option_Helper renames option_crate.Element (position);
         NDX : String := HT.USS (rec.option_name);
      begin
         TIO.Put_Line ("   " & NDX & LAT.Colon);
         TIO.Put_Line ("      BROKEN_ON=" & LAT.HT & LAT.HT & HT.USS (rec.BROKEN_ON));
         TIO.Put_Line ("      DESCRIPTION=" & LAT.HT & LAT.HT & HT.USS (rec.option_description));
         print_opt_vector (rec.BUILDRUN_DEPENDS_OFF, "BUILDRUN_DEPENDS_OFF");
         print_opt_vector (rec.BUILDRUN_DEPENDS_ON, "BUILDRUN_DEPENDS_ON");
         print_opt_vector (rec.BUILD_DEPENDS_OFF, "BUILD_DEPENDS_OFF");
         print_opt_vector (rec.BUILD_DEPENDS_ON, "BUILD_DEPENDS_ON");
         print_opt_vector (rec.BUILD_TARGET_ON, "BUILD_TARGET_ON");
         print_opt_vector (rec.CFLAGS_OFF, "CFLAGS_OFF");
         print_opt_vector (rec.CFLAGS_ON, "CFLAGS_ON");
         print_opt_vector (rec.CMAKE_ARGS_OFF, "CMAKE_ARGS_OFF");
         print_opt_vector (rec.CMAKE_ARGS_ON, "CMAKE_ARGS_ON");
         print_opt_vector (rec.CMAKE_BOOL_T_BOTH, "CMAKE_BOOL_T_BOTH");
         print_opt_vector (rec.CMAKE_BOOL_F_BOTH, "CMAKE_BOOL_F_BOTH");
         print_opt_vector (rec.CONFIGURE_ARGS_ON, "CONFIGURE_ARGS_ON");
         print_opt_vector (rec.CONFIGURE_ARGS_OFF, "CONFIGURE_ARGS_OFF");
         print_opt_vector (rec.CONFIGURE_ENABLE_BOTH, "CONFIGURE_ENABLE_BOTH");
         print_opt_vector (rec.CONFIGURE_ENV_ON, "CONFIGURE_ENV_ON");
         print_opt_vector (rec.CONFIGURE_WITH_BOTH, "CONFIGURE_WITH_BOTH");
         print_opt_vector (rec.CPPFLAGS_ON, "CPPFLAGS_ON");
         print_opt_vector (rec.CXXFLAGS_ON, "CXXFLAGS_ON");
         print_opt_vector (rec.DF_INDEX_OFF, "DF_INDEX_OFF");
         print_opt_vector (rec.DF_INDEX_ON, "DF_INDEX_ON");
         print_opt_vector (rec.EXTRA_PATCHES_ON, "EXTRA_PATCHES_ON");
         print_opt_vector (rec.EXTRACT_ONLY_ON, "EXTRACT_ONLY_ON");
         print_opt_vector (rec.GNOME_COMPONENTS_OFF, "GNOME_COMPONENTS_OFF");
         print_opt_vector (rec.GNOME_COMPONENTS_ON, "GNOME_COMPONENTS_ON");
         print_opt_vector (rec.IMPLIES_ON, "IMPLIES_ON");
         print_opt_vector (rec.INFO_OFF, "INFO_OFF");
         print_opt_vector (rec.INFO_ON, "INFO_ON");
         print_opt_vector (rec.INSTALL_TARGET_ON, "INSTALL_TARGET_ON");
         print_opt_vector (rec.KEYWORDS_ON, "KEYWORDS_ON");
         print_opt_vector (rec.LDFLAGS_ON, "LDFLAGS_ON");
         print_opt_vector (rec.MAKEFILE_OFF, "MAKEFILE_OFF");
         print_opt_vector (rec.MAKEFILE_ON, "MAKEFILE_ON");
         print_opt_vector (rec.MAKE_ARGS_OFF, "MAKE_ARGS_OFF");
         print_opt_vector (rec.MAKE_ARGS_ON, "MAKE_ARGS_ON");
         print_opt_vector (rec.MAKE_ENV_ON, "MAKE_ENV_ON");
         print_opt_vector (rec.ONLY_FOR_OPSYS_ON, "ONLY_FOR_OPSYS_ON");
         print_opt_vector (rec.PATCHFILES_ON, "PATCHFILES_ON");
         print_opt_vector (rec.PLIST_SUB_ON, "PLIST_SUB_ON");
         print_opt_vector (rec.PREVENTS_ON, "PREVENTS_ON");
         print_opt_vector (rec.QMAKE_ARGS_OFF, "QMAKE_ARGS_OFF");
         print_opt_vector (rec.QMAKE_ARGS_ON, "QMAKE_ARGS_ON");
         print_opt_vector (rec.RUN_DEPENDS_OFF, "RUN_DEPENDS_OFF");
         print_opt_vector (rec.RUN_DEPENDS_ON, "RUN_DEPENDS_ON");
         print_opt_vector (rec.SUB_FILES_OFF, "SUB_FILES_OFF");
         print_opt_vector (rec.SUB_FILES_ON, "SUB_FILES_ON");
         print_opt_vector (rec.SUB_LIST_ON, "SUB_LIST_ON");
         print_opt_vector (rec.TEST_TARGET_ON, "TEST_TARGET_ON");
         print_opt_vector (rec.USES_OFF, "USES_OFF");
         print_opt_vector (rec.USES_ON, "USES_ON");
         print_opt_vector (rec.XORG_COMPONENTS_OFF, "XORG_COMPONENTS_OFF");
         print_opt_vector (rec.XORG_COMPONENTS_ON, "XORG_COMPONENTS_ON");
      end dump_option;

      procedure print_opt_vector (vec : string_crate.Vector; thelabel : String)
      is
         --  Align at column-40
         labellen : Natural := thelabel'Length;
         num_tabs : Natural := (32 - labellen) / 8;
         extratab : String (1 .. num_tabs) := (others => LAT.HT);
      begin
         TIO.Put ("      " & thelabel & LAT.Equals_Sign & extratab);
         vec.Iterate (Process => print_item'Access);
         TIO.Put (LAT.LF);
      end print_opt_vector;

      procedure print_vector_list (thelabel : String; thelist : spec_field)
      is
         labellen : Natural := thelabel'Length;
      begin
         TIO.Put (thelabel & LAT.Equals_Sign & LAT.HT);
         if labellen < 7 then
            TIO.Put (LAT.HT & LAT.HT);
         elsif labellen < 15 then
            TIO.Put (LAT.HT);
         end if;
         case thelist is
            when sp_exc_opsys     => specs.exc_opsys.Iterate (print_item'Access);
            when sp_inc_opsys     => specs.inc_opsys.Iterate (print_item'Access);
            when sp_exc_arch      => specs.exc_arch.Iterate (print_item'Access);
            when sp_opts_avail    => specs.ops_avail.Iterate (print_item'Access);
            when sp_opts_standard => specs.ops_standard.Iterate (print_item'Access);
            when sp_df_index      => specs.df_index.Iterate (print_item'Access);
            when sp_distfiles     => specs.distfiles.Iterate (print_item'Access);
            when sp_contacts      => specs.contacts.Iterate (print_item'Access);
            when sp_variants      => specs.variants.Iterate (print_item'Access);
            when sp_keywords      => specs.keywords.Iterate (print_item'Access);
            when sp_ext_only      => specs.extract_only.Iterate (print_item'Access);
            when sp_ext_zip       => specs.extract_zip.Iterate (print_item'Access);
            when sp_ext_7z        => specs.extract_7z.Iterate (print_item'Access);
            when sp_ext_lha       => specs.extract_lha.Iterate (print_item'Access);
            when sp_ext_deb       => specs.extract_deb.Iterate (print_item'Access);
            when sp_ext_dirty     => specs.extract_dirty.Iterate (print_item'Access);
            when sp_make_args     => specs.make_args.Iterate (print_item'Access);
            when sp_make_env      => specs.make_env.Iterate (print_item'Access);
            when sp_build_target  => specs.build_target.Iterate (print_item'Access);
            when sp_cflags        => specs.cflags.Iterate (print_item'Access);
            when sp_cxxflags      => specs.cxxflags.Iterate (print_item'Access);
            when sp_cppflags      => specs.cppflags.Iterate (print_item'Access);
            when sp_ldflags       => specs.ldflags.Iterate (print_item'Access);
            when sp_patchfiles    => specs.patchfiles.Iterate (print_item'Access);
            when sp_uses          => specs.uses.Iterate (print_item'Access);
            when sp_sub_list      => specs.sub_list.Iterate (print_item'Access);
            when sp_sub_files     => specs.sub_files.Iterate (print_item'Access);
            when sp_config_args   => specs.config_args.Iterate (print_item'Access);
            when sp_config_env    => specs.config_env.Iterate (print_item'Access);
            when sp_build_deps    => specs.build_deps.Iterate (print_item'Access);
            when sp_buildrun_deps => specs.buildrun_deps.Iterate (print_item'Access);
            when sp_run_deps      => specs.run_deps.Iterate (print_item'Access);
            when sp_cmake_args    => specs.cmake_args.Iterate (print_item'Access);
            when sp_qmake_args    => specs.qmake_args.Iterate (print_item'Access);
            when sp_info          => specs.info.Iterate (print_item'Access);
            when sp_install_tgt   => specs.install_tgt.Iterate (print_item'Access);
            when sp_patch_strip   => specs.patch_strip.Iterate (print_item'Access);
            when sp_pfiles_strip  => specs.pfiles_strip.Iterate (print_item'Access);
            when sp_extra_patches => specs.extra_patches.Iterate (print_item'Access);
            when sp_plist_sub     => specs.plist_sub.Iterate (print_item'Access);
            when sp_licenses      => specs.licenses.Iterate (print_item'Access);
            when sp_lic_terms     => specs.lic_terms.Iterate (print_item'Access);
            when sp_lic_name      => specs.lic_names.Iterate (print_item'Access);
            when sp_lic_file      => specs.lic_files.Iterate (print_item'Access);
            when sp_lic_awk       => specs.lic_awk.Iterate (print_item'Access);
            when sp_lic_src       => specs.lic_source.Iterate (print_item'Access);
            when sp_users         => specs.users.Iterate (print_item'Access);
            when sp_groups        => specs.groups.Iterate (print_item'Access);
            when sp_test_tgt      => specs.test_tgt.Iterate (print_item'Access);
            when sp_test_args     => specs.test_args.Iterate (print_item'Access);
            when sp_test_env      => specs.test_env.Iterate (print_item'Access);
            when sp_mandirs       => specs.mandirs.Iterate (print_item'Access);
            when sp_broken_ssl    => specs.broken_ssl.Iterate (print_item'Access);
            when sp_broken_mysql  => specs.broken_mysql.Iterate (print_item'Access);
            when sp_broken_pgsql  => specs.broken_pgsql.Iterate (print_item'Access);
            when sp_gnome         => specs.gnome_comps.Iterate (print_item'Access);
            when sp_xorg          => specs.xorg_comps.Iterate (print_item'Access);
            when sp_sdl           => specs.sdl_comps.Iterate (print_item'Access);
            when sp_phpext        => specs.php_extensions.Iterate (print_item'Access);
            when sp_rcscript      => specs.subr_scripts.Iterate (print_item'Access);
            when sp_og_radio      => specs.opt_radio.Iterate (print_item'Access);
            when sp_og_restrict   => specs.opt_restrict.Iterate (print_item'Access);
            when sp_og_unlimited  => specs.opt_unlimited.Iterate (print_item'Access);
            when sp_cgo_cargs     => specs.cgo_conf_args.Iterate (print_item'Access);
            when sp_cgo_bargs     => specs.cgo_build_args.Iterate (print_item'Access);
            when sp_cgo_iargs     => specs.cgo_inst_args.Iterate (print_item'Access);
            when sp_cgo_feat      => specs.cgo_features.Iterate (print_item'Access);
            when others => null;
         end case;
         TIO.Put (LAT.LF);
      end print_vector_list;

      procedure print_group_list (thelabel : String; thelist : spec_field) is
      begin
         TIO.Put_Line (thelabel & LAT.Colon);
         case thelist is
            when sp_vopts            => specs.variantopts.Iterate (dump'Access);
            when sp_options_on       => specs.options_on.Iterate (dump'Access);
            when sp_broken           => specs.broken.Iterate (dump'Access);
            when sp_subpackages      => specs.subpackages.Iterate (dump'Access);
            when sp_dl_sites         => specs.dl_sites.Iterate (dump'Access);
            when sp_ext_head         => specs.extract_head.Iterate (dump'Access);
            when sp_ext_tail         => specs.extract_tail.Iterate (dump'Access);
            when sp_makefile_targets => specs.make_targets.Iterate (dump_target'Access);
            when sp_opt_helper       => specs.ops_helpers.Iterate (dump_option'Access);
            when sp_var_opsys        => specs.var_opsys.Iterate (dump'Access);
            when sp_var_arch         => specs.var_arch.Iterate (dump'Access);
            when sp_exrun            => specs.extra_rundeps.Iterate (dump'Access);
            when sp_catchall         => specs.catch_all.Iterate (dump'Access);
            when sp_opt_descr        => specs.optgroup_desc.Iterate (dump'Access);
            when sp_opt_group        => specs.optgroups.Iterate (dump'Access);
            when sp_os_bdep          => specs.opsys_b_deps.Iterate (dump'Access);
            when sp_os_rdep          => specs.opsys_r_deps.Iterate (dump'Access);
            when sp_os_brdep         => specs.opsys_br_deps.Iterate (dump'Access);
            when sp_os_uses          => specs.opsys_c_uses.Iterate (dump'Access);
            when others => null;
         end case;
      end print_group_list;

      procedure print_single (thelabel : String; thelist : spec_field)
      is
         labellen : Natural := thelabel'Length;
      begin
         TIO.Put (thelabel & LAT.Equals_Sign & LAT.HT);
         if labellen < 7 then
            TIO.Put (LAT.HT & LAT.HT);
         elsif labellen < 15 then
            TIO.Put (LAT.HT);
         end if;
         case thelist is
            when sp_namebase       => TIO.Put_Line (HT.USS (specs.namebase));
            when sp_version        => TIO.Put_Line (HT.USS (specs.version));
            when sp_revision       => TIO.Put_Line (HT.int2str (specs.revision));
            when sp_epoch          => TIO.Put_Line (HT.int2str (specs.epoch));
            when sp_opt_level      => TIO.Put_Line (HT.int2str (specs.optimizer_lvl));
            when sp_job_limit      => TIO.Put_Line (HT.int2str (specs.job_limit));
            when sp_distsubdir     => TIO.Put_Line (HT.USS (specs.dist_subdir));
            when sp_distname       => TIO.Put_Line (HT.USS (specs.distname));
            when sp_build_wrksrc   => TIO.Put_Line (HT.USS (specs.build_wrksrc));
            when sp_makefile       => TIO.Put_Line (HT.USS (specs.makefile));
            when sp_destdirname    => TIO.Put_Line (HT.USS (specs.destdirname));
            when sp_homepage       => TIO.Put_Line (HT.USS (specs.homepage));
            when sp_gnu_cfg_prefix => TIO.Put_Line (HT.USS (specs.config_prefix));
            when sp_config_script  => TIO.Put_Line (HT.USS (specs.config_script));
            when sp_config_target  => TIO.Put_Line (HT.USS (specs.config_target));
            when sp_config_wrksrc  => TIO.Put_Line (HT.USS (specs.config_wrksrc));
            when sp_patch_wrksrc   => TIO.Put_Line (HT.USS (specs.patch_wrksrc));
            when sp_must_config    => TIO.Put_Line (HT.USS (specs.config_must));
            when sp_expiration     => TIO.Put_Line (HT.USS (specs.expire_date));
            when sp_deprecated     => TIO.Put_Line (HT.USS (specs.deprecated));
            when sp_install_wrksrc => TIO.Put_Line (HT.USS (specs.install_wrksrc));
            when sp_prefix         => TIO.Put_Line (HT.USS (specs.prefix));
            when sp_ug_pkg         => TIO.Put_Line (HT.USS (specs.usergroup_pkg));
            when sp_soversion      => TIO.Put_Line (HT.USS (specs.soversion));
            when sp_lic_scheme     => TIO.Put_Line (HT.USS (specs.lic_scheme));
            when others => null;
         end case;
      end print_single;

      procedure print_boolean (thelabel : String; thelist : spec_field)
      is
         labellen : Natural := thelabel'Length;
      begin
         TIO.Put (thelabel & LAT.Equals_Sign & LAT.HT);
         if labellen < 7 then
            TIO.Put (LAT.HT & LAT.HT);
         elsif labellen < 15 then
            TIO.Put (LAT.HT);
         end if;
         case thelist is
            when sp_skip_build     => TIO.Put_Line (specs.skip_build'Img);
            when sp_skip_install   => TIO.Put_Line (specs.skip_install'Img);
            when sp_destdir_env    => TIO.Put_Line (specs.destdir_env'Img);
            when sp_single_job     => TIO.Put_Line (specs.single_job'Img);
            when sp_cfg_outsrc     => TIO.Put_Line (specs.config_outsrc'Img);
            when sp_inst_tchain    => TIO.Put_Line (specs.shift_install'Img);
            when sp_skip_ccache    => TIO.Put_Line (specs.skip_ccache'Img);
            when sp_rpath_warning  => TIO.Put_Line (specs.fatal_rpath'Img);
            when sp_debugging      => TIO.Put_Line (specs.debugging_on'Img);
            when sp_generated      => TIO.Put_Line (specs.generated'Img);
            when sp_infra          => TIO.Put_Line (specs.infrastructure'Img);
            when sp_killdog        => TIO.Put_Line (specs.kill_watchdog'Img);
            when sp_cgo_conf       => TIO.Put_Line (specs.cgo_skip_conf'Img);
            when sp_cgo_build      => TIO.Put_Line (specs.cgo_skip_build'Img);
            when sp_cgo_inst       => TIO.Put_Line (specs.cgo_skip_inst'Img);
            when others => null;
         end case;
      end print_boolean;

      procedure print_define (flavor : Positive) is
      begin
         case flavor is
            when 1 => specs.taglines.Iterate (print_tagline'Access);
            when others => null;
         end case;
      end print_define;

   begin
      print_single      ("NAMEBASE", sp_namebase);
      print_single      ("VERSION",  sp_version);
      print_single      ("REVISION", sp_revision);
      print_single      ("EPOCH",    sp_epoch);
      print_vector_list ("KEYWORDS", sp_keywords);
      print_vector_list ("VARIANTS", sp_variants);
      print_define      (1);  -- SDESC
      print_single      ("HOMEPAGE", sp_homepage);
      print_vector_list ("CONTACTS", sp_contacts);
      print_group_list  ("SITES", sp_dl_sites);
      print_vector_list ("DISTFILE", sp_distfiles);
      print_single      ("DIST_SUBDIR", sp_distsubdir);
      print_vector_list ("DF_INDEX", sp_df_index);
      print_vector_list ("PATCHFILES", sp_patchfiles);
      print_group_list  ("SPKGS", sp_subpackages);
      print_vector_list ("OPTIONS_AVAILABLE", sp_opts_avail);
      print_vector_list ("OPTIONS_STANDARD", sp_opts_standard);
      print_vector_list ("OPTGROUP_RADIO", sp_og_radio);
      print_vector_list ("OPTGROUP_RESTRICTED", sp_og_restrict);
      print_vector_list ("OPTGROUP_UNLIMITED", sp_og_unlimited);
      print_group_list  ("OPTDESCR", sp_opt_descr);
      print_group_list  ("OPTGROUP", sp_opt_group);
      print_group_list  ("VOPTS", sp_vopts);
      print_group_list  ("OPT_ON", sp_options_on);
      print_group_list  ("BROKEN", sp_broken);
      print_vector_list ("BROKEN_SSL", sp_broken_ssl);
      print_vector_list ("BROKEN_MYSQL", sp_broken_mysql);
      print_vector_list ("BROKEN_PGSQL", sp_broken_pgsql);
      print_vector_list ("ONLY_FOR_OPSYS", sp_inc_opsys);
      print_vector_list ("NOT_FOR_OPSYS", sp_exc_opsys);
      print_vector_list ("NOT_FOR_ARCH", sp_exc_arch);
      print_single      ("DEPRECATED", sp_deprecated);
      print_single      ("EXPIRATION_DATE", sp_expiration);
      print_vector_list ("BUILD_DEPENDS", sp_build_deps);
      print_vector_list ("BUILDRUN_DEPENDS", sp_buildrun_deps);
      print_vector_list ("RUN_DEPENDS", sp_run_deps);
      print_group_list  ("EXRUN", sp_exrun);
      print_group_list  ("B_DEPS", sp_os_bdep);
      print_group_list  ("BR_DEPS", sp_os_brdep);
      print_group_list  ("R_DEPS", sp_os_rdep);
      print_group_list  ("C_USES", sp_os_uses);
      print_vector_list ("USES", sp_uses);
      print_vector_list ("GNOME_COMPONENTS", sp_gnome);
      print_vector_list ("XORG_COMPONENTS", sp_xorg);
      print_vector_list ("SDL_COMPONENTS", sp_sdl);
      print_vector_list ("PHP_EXTENSIONS", sp_phpext);
      print_vector_list ("SUB_LIST", sp_sub_list);
      print_vector_list ("SUB_FILES", sp_sub_files);
      print_group_list  ("OPTION HELPERS", sp_opt_helper);

      print_single      ("DISTNAME", sp_distname);
      print_vector_list ("EXTRACT_ONLY", sp_ext_only);
      print_vector_list ("EXTRACT_WITH_UNZIP", sp_ext_zip);
      print_vector_list ("EXTRACT_WITH_7Z", sp_ext_7z);
      print_vector_list ("EXTRACT_WITH_LHA", sp_ext_lha);
      print_vector_list ("EXTRACT_DEB_PACKAGE", sp_ext_deb);
      print_vector_list ("EXTRACT_DIRTY", sp_ext_dirty);
      print_group_list  ("EXTRACT_HEAD", sp_ext_head);
      print_group_list  ("EXTRACT_TAIL", sp_ext_tail);

      print_single      ("PREFIX", sp_prefix);
      print_single      ("PATCH_WRKSRC", sp_patch_wrksrc);
      print_vector_list ("EXTRA_PATCHES", sp_extra_patches);
      print_vector_list ("PATCH_STRIP", sp_patch_strip);
      print_vector_list ("PATCHFILES_STRIP", sp_pfiles_strip);

      print_single      ("MUST_CONFIGURE", sp_must_config);
      print_single      ("CONFIGURE_WRKSRC", sp_config_wrksrc);
      print_single      ("CONFIGURE_TARGET", sp_config_target);
      print_single      ("CONFIGURE_SCRIPT", sp_config_script);
      print_boolean     ("CONFIGURE_OUTSOURCE", sp_cfg_outsrc);
      print_vector_list ("CONFIGURE_ARGS", sp_config_args);
      print_vector_list ("CONFIGURE_ENV", sp_config_env);
      print_single      ("GNU_CONFIGURE_PREFIX", sp_gnu_cfg_prefix);
      print_boolean     ("RPATH_CHECK_FATAL", sp_rpath_warning);

      print_boolean     ("SKIP_BUILD", sp_skip_build);
      print_boolean     ("SKIP_INSTALL", sp_skip_install);
      print_boolean     ("SKIP_CCACHE", sp_skip_ccache);
      print_boolean     ("INSTALL_REQ_TOOLCHAIN", sp_inst_tchain);
      print_boolean     ("SINGLE_JOB", sp_single_job);
      print_boolean     ("SET_DEBUGGING_ON", sp_debugging);
      print_boolean     ("DESTDIR_VIA_ENV", sp_destdir_env);
      print_boolean     ("iNFRASTRUCTURE", sp_infra);
      print_boolean     ("BLOCK_WATCHDOG", sp_killdog);
      print_single      ("BUILD_WRKSRC", sp_build_wrksrc);
      print_single      ("MAKEFILE", sp_makefile);
      print_single      ("DESTDIRNAME", sp_destdirname);
      print_vector_list ("MAKE_ARGS", sp_make_args);
      print_vector_list ("MAKE_ENV", sp_make_env);
      print_vector_list ("BUILD_TARGET", sp_build_target);
      print_single      ("SOVERSION", sp_soversion);
      print_single      ("OPTIMIZER_LEVEL", sp_opt_level);
      print_vector_list ("CFLAGS", sp_cflags);
      print_vector_list ("CXXFLAGS", sp_cxxflags);
      print_vector_list ("CPPFLAGS", sp_cppflags);
      print_vector_list ("LDFLAGS", sp_ldflags);
      print_vector_list ("CMAKE_ARGS", sp_cmake_args);
      print_vector_list ("QMAKE_ARGS", sp_qmake_args);
      print_vector_list ("INFO", sp_info);
      print_vector_list ("INSTALL_TARGET", sp_install_tgt);
      print_single      ("INSTALL_WRKSRC", sp_install_wrksrc);
      print_vector_list ("PLIST_SUB", sp_plist_sub);
      print_vector_list ("LICENSES", sp_licenses);
      print_vector_list ("LICENSE_TERMS", sp_lic_terms);
      print_vector_list ("LICENSE_NAME", sp_lic_name);
      print_vector_list ("LICENSE_FILE", sp_lic_file);
      print_vector_list ("LICENSE_AWK", sp_lic_awk);
      print_vector_list ("LICENSE_SOURCE", sp_lic_src);
      print_single      ("LICENSE_SCHEME", sp_lic_scheme);
      print_vector_list ("USERS", sp_users);
      print_vector_list ("GROUPS", sp_groups);
      print_single      ("USERGROUP_SPKG", sp_ug_pkg);
      print_vector_list ("MANDIRS", sp_mandirs);
      print_group_list  ("CATCHALL", sp_catchall);
      print_group_list  ("VAR_OPSYS", sp_var_opsys);
      print_group_list  ("VAR_ARCH", sp_var_arch);
      print_vector_list ("TEST_TARGET", sp_test_tgt);
      print_vector_list ("TEST_ARGS", sp_test_args);
      print_vector_list ("TEST_ENV", sp_test_env);
      print_vector_list ("RC_SUBR", sp_rcscript);
      print_boolean     ("GENERATED", sp_generated);
      print_single      ("MAKE_JOBS_NUMBER_LIMIT", sp_job_limit);

      print_group_list  ("Makefile Targets", sp_makefile_targets);

      print_boolean     ("CARGO_CONFIGURE", sp_cgo_conf);
      print_boolean     ("CARGO_BUILD", sp_cgo_build);
      print_boolean     ("CARGO_INSTALL", sp_cgo_inst);
      print_vector_list ("CARGO_CONFIG_ARGS", sp_cgo_cargs);
      print_vector_list ("CARGO_BUILD_ARGS", sp_cgo_bargs);
      print_vector_list ("CARGO_INSTALL_ARGS", sp_cgo_iargs);
      print_vector_list ("CARGO_FEATURES", sp_cgo_feat);

   end dump_specification;

end Port_Specification;
