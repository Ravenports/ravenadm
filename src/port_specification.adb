--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions; use Definitions;
with Utilities;
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
   procedure initialize (specs : out Portspecs) is
   begin
      specs.namebase     := HT.blank;
      specs.version      := HT.blank;
      specs.revision     := 0;
      specs.epoch        := 0;
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
      specs.extract_dirty.Clear;
      specs.extract_head.Clear;
      specs.extract_tail.Clear;
      specs.distname     := HT.blank;

      specs.config_args.Clear;
      specs.config_env.Clear;

      specs.skip_build    := False;
      specs.skip_install  := False;
      specs.destdir_env   := False;
      specs.single_job    := False;
      specs.shift_install := False;
      specs.build_wrksrc  := HT.blank;
      specs.makefile      := HT.blank;
      specs.destdirname   := HT.blank;
      specs.make_env.Clear;
      specs.make_args.Clear;
      specs.build_target.Clear;
      specs.build_deps.Clear;
      specs.buildrun_deps.Clear;
      specs.run_deps.Clear;
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
      specs.apply_f10_fix  := False;
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

      specs.licenses.Clear;
      specs.users.Clear;
      specs.groups.Clear;
      specs.catch_all.Clear;
      specs.pkg_notes.Clear;

      specs.last_set := so_initialized;
   end initialize;


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
            if specs.variants.Is_Empty and then
              value /= variant_standard
            then
               raise wrong_value with "First variant must be '" & variant_standard & "'";
            end if;
            if value'Length > 15 then
               raise wrong_value with "'" & value & "' value is too long (15-char limit)";
            end if;
            if specs.variants.Contains (text_value) then
               raise dupe_list_value with value;
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
               raise wrong_value with "No download group prefix present in distfile";
            end if;
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
            end if;
            specs.ops_standard.Append (text_value);
            specs.last_set := so_opts_std;
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
            specs.patch_strip.Append (text_value);
         when sp_patch_strip =>
            verify_entry_is_post_options;
            specs.patch_strip.Append (text_value);
         when sp_extra_patches =>
            verify_entry_is_post_options;
            specs.extra_patches.Append (text_value);
         when sp_plist_sub =>
            verify_entry_is_post_options;
            specs.plist_sub.Append (text_value);
         when sp_licenses =>
            verify_entry_is_post_options;
            specs.licenses.Append (text_value);
         when sp_users =>
            verify_entry_is_post_options;
            specs.users.Append (text_value);
         when sp_groups =>
            verify_entry_is_post_options;
            specs.groups.Append (text_value);
         when sp_build_deps | sp_buildrun_deps | sp_run_deps =>
            verify_entry_is_post_options;
            if not valid_dependency_format (value) then
               raise wrong_value with "invalid dependency format '" & value & "'";
            end if;
            if specs.build_deps.Contains (text_value) or else
              specs.buildrun_deps.Contains (text_value) or else
              specs.run_deps.Contains (text_value)
            then
               raise wrong_value with "Duplicate dependency '" & value & "'";
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
            if specs.uses.Contains (text_value) then
               raise wrong_value with "Duplicate USES module '" & value & "'";
            end if;
            declare
               stripped      : String  := HT.part_1 (value, ":");
               text_stripped : HT.Text := HT.SUS (stripped);
            begin
               specs.uses.Append (text_value);
               if not specs.uses_base.Contains (text_stripped) then
                  specs.uses_base.Append (text_stripped);
               end if;
            end;
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
            if specs.dl_sites.Contains (text_group) then
               raise dupe_list_value with group;
            end if;
            specs.dl_sites.Insert (text_group, initial_rec);
            specs.last_set := so_dl_groups;
         when sp_subpackages =>
            --  variant, order, length and uniqueness already checked
            --  don't updatee last_set either
            specs.subpackages.Insert (text_group, initial_rec);
         when sp_vopts =>
            --  variant, order, length and uniqueness already checked
            --  don't updatee last_set either
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
            if specs.catch_all.Contains (text_key) then
               raise wrong_value with "duplicate definition: " & key & "=" & value;
            end if;
            specs.catch_all.Insert (text_key, text_value);
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
         when sp_apply_f10_fix =>
            specs.apply_f10_fix := value;
         when sp_inst_tchain =>
            specs.shift_install := value;
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
            when build_depends_on =>
               Element.BUILD_DEPENDS_ON.Append (value_text);
            when build_target_on =>
               Element.BUILD_TARGET_ON.Append (value_text);
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
            when configure_env_on =>
               Element.CONFIGURE_ENV_ON.Append (value_text);
            when configure_with_both =>
               Element.CONFIGURE_WITH_BOTH.Append (value_text);
            when cppflags_on =>
               Element.CPPFLAGS_ON.Append (value_text);
            when cxxflags_on =>
               Element.CXXFLAGS_ON.Append (value_text);
            when description =>
               Element.option_description := value_text;
            when df_index_on =>
               Element.DF_INDEX_ON.Append (value_text);
            when extra_patches_on =>
               Element.EXTRA_PATCHES_ON.Append (value_text);
            when extract_only_on  =>
               Element.EXTRACT_ONLY_ON.Append (value_text);
            when gh_account_on =>
               Element.GH_ACCOUNT_ON.Append (value_text);
            when gh_project_on =>
               Element.GH_PROJECT_ON.Append (value_text);
            when gh_subdir_on =>
               Element.GH_SUBDIR_ON.Append (value_text);
            when gh_tagname_on =>
               Element.GH_TAGNAME_ON.Append (value_text);
            when gh_tuple_on =>
               Element.GH_TUPLE_ON.Append (value_text);
            when implies_on =>
               Element.IMPLIES_ON.Append (value_text);
            when info_on =>
               Element.INFO_ON.Append (value_text);
            when install_target_on =>
               Element.INSTALL_TARGET_ON.Append (value_text);
            when keywords_on =>
               Element.KEYWORDS_ON.Append (value_text);
            when ldflags_on =>
               Element.LDFLAGS_ON.Append (value_text);
            when buildrun_depends_on =>
               Element.BUILDRUN_DEPENDS_ON.Append (value_text);
            when make_args_on =>
               Element.MAKE_ARGS_ON.Append (value_text);
            when make_env_on =>
               Element.MAKE_ENV_ON.Append (value_text);
            when patchfiles_on =>
               Element.PATCHFILES_ON.Append (value_text);
            when plist_sub_on =>
               Element.PLIST_SUB_ON.Append (value_text);
            when prevents_on =>
               Element.PREVENTS_ON.Append (value_text);
            when qmake_off =>
               Element.QMAKE_OFF.Append (value_text);
            when qmake_on =>
               Element.QMAKE_ON.Append (value_text);
            when run_depends_on =>
               Element.RUN_DEPENDS_ON.Append (value_text);
            when sub_files_on =>
               Element.SUB_FILES_ON.Append (value_text);
            when sub_list_on =>
               Element.SUB_LIST_ON.Append (value_text);
            when test_target_on =>
               Element.TEST_TARGET_ON.Append (value_text);
            when uses_on =>
               Element.USES_ON.Append (value_text);
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
              gh_subdir_on | gh_project_on | gh_account_on | gh_tagname_on | gh_tuple_on |
              implies_on | info_on | install_target_on | keywords_on  | buildrun_depends_on |
              patchfiles_on | prevents_on | run_depends_on | sub_files_on | test_target_on |
              uses_on =>
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
         when broken_on | build_target_on | cflags_on | cmake_args_off | cmake_args_on |
              cmake_bool_f_both | cmake_bool_t_both | configure_args_off | configure_args_on |
              configure_enable_both | configure_env_on | configure_with_both |
              cppflags_on | cxxflags_on | extra_patches_on | gh_tuple_on | gh_tagname_on |
              gh_account_on | gh_project_on | gh_subdir_on | info_on | install_target_on |
              ldflags_on | make_args_on | make_env_on | patchfiles_on | plist_sub_on |
              qmake_on | qmake_off | sub_files_on | sub_list_on | test_target_on | description =>
            --  No validation required
            null;
         when build_depends_on | buildrun_depends_on | run_depends_on =>
            if not valid_dependency_format (value) then
               raise wrong_value with "invalid dependency format '" & value & "'";
            end if;
         when df_index_on =>
            if not specs.dist_index_is_valid (value) then
               raise wrong_value with "distfile index '" & value & "' is not valid";
            end if;
            if specs.ops_helpers.Element (option_text).DF_INDEX_ON.Contains (value_text) then
               raise dupe_list_value with value;
            end if;
         when extract_only_on =>
            if not specs.dist_index_is_valid (value) then
               raise wrong_value with "distfile index '" & value & "' is not valid";
            end if;
            if specs.ops_helpers.Element (option_text).EXTRACT_ONLY_ON.Contains (value_text) then
               raise dupe_list_value with value;
            end if;
         when implies_on =>
            if not specs.ops_avail.Contains (value_text) then
               raise wrong_value with "Not a defined option: '" & value & "'";
            end if;
            if specs.ops_helpers.Element (option_text).IMPLIES_ON.Contains (value_text) then
               raise dupe_list_value with value;
            end if;
         when prevents_on =>
            if not specs.ops_avail.Contains (value_text) then
               raise wrong_value with "Not a defined option: '" & value & "'";
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
         when uses_on =>
            if not valid_uses_module (value) then
               raise wrong_value with "USES '" & value & "' is not recognized";
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
            when build_depends_on      => return rec.BUILD_DEPENDS_ON.Is_Empty;
            when build_target_on       => return rec.BUILD_TARGET_ON.Is_Empty;
            when cflags_on             => return rec.CFLAGS_ON.Is_Empty;
            when cmake_args_off        => return rec.CMAKE_ARGS_OFF.Is_Empty;
            when cmake_args_on         => return rec.CMAKE_ARGS_ON.Is_Empty;
            when cmake_bool_f_both     => return rec.CMAKE_BOOL_F_BOTH.Is_Empty;
            when cmake_bool_t_both     => return rec.CMAKE_BOOL_T_BOTH.Is_Empty;
            when configure_args_off    => return rec.CONFIGURE_ARGS_OFF.Is_Empty;
            when configure_args_on     => return rec.CONFIGURE_ARGS_ON.Is_Empty;
            when configure_enable_both => return rec.CONFIGURE_ENABLE_BOTH.Is_Empty;
            when configure_env_on      => return rec.CONFIGURE_ENV_ON.Is_Empty;
            when configure_with_both   => return rec.CONFIGURE_WITH_BOTH.Is_Empty;
            when cppflags_on           => return rec.CPPFLAGS_ON.Is_Empty;
            when cxxflags_on           => return rec.CXXFLAGS_ON.Is_Empty;
            when df_index_on           => return rec.DF_INDEX_ON.Is_Empty;
            when description           => return HT.IsBlank (rec.option_description);
            when extra_patches_on      => return rec.EXTRA_PATCHES_ON.Is_Empty;
            when extract_only_on       => return rec.EXTRACT_ONLY_ON.Is_Empty;
            when gh_account_on         => return rec.GH_ACCOUNT_ON.Is_Empty;
            when gh_project_on         => return rec.GH_PROJECT_ON.Is_Empty;
            when gh_subdir_on          => return rec.GH_SUBDIR_ON.Is_Empty;
            when gh_tagname_on         => return rec.GH_TAGNAME_ON.Is_Empty;
            when gh_tuple_on           => return rec.GH_TUPLE_ON.Is_Empty;
            when implies_on            => return rec.IMPLIES_ON.Is_Empty;
            when info_on               => return rec.INFO_ON.Is_Empty;
            when install_target_on     => return rec.INSTALL_TARGET_ON.Is_Empty;
            when keywords_on           => return rec.KEYWORDS_ON.Is_Empty;
            when ldflags_on            => return rec.LDFLAGS_ON.Is_Empty;
            when buildrun_depends_on   => return rec.BUILDRUN_DEPENDS_ON.Is_Empty;
            when make_args_on          => return rec.MAKE_ARGS_ON.Is_Empty;
            when make_env_on           => return rec.MAKE_ENV_ON.Is_Empty;
            when patchfiles_on         => return rec.PATCHFILES_ON.Is_Empty;
            when plist_sub_on          => return rec.PLIST_SUB_ON.Is_Empty;
            when prevents_on           => return rec.PREVENTS_ON.Is_Empty;
            when qmake_off             => return rec.QMAKE_OFF.Is_Empty;
            when qmake_on              => return rec.QMAKE_ON.Is_Empty;
            when run_depends_on        => return rec.RUN_DEPENDS_ON.Is_Empty;
            when sub_files_on          => return rec.SUB_FILES_ON.Is_Empty;
            when sub_list_on           => return rec.SUB_LIST_ON.Is_Empty;
            when test_target_on        => return rec.TEST_TARGET_ON.Is_Empty;
            when uses_on               => return rec.USES_ON.Is_Empty;
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
         when others => return False;
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
      mask    : String  := word;
      Qopened : Boolean := False;
   begin
      for x in mask'Range loop
         if mask (x) = LAT.Quotation then
            Qopened := not Qopened;
         elsif mask (x) = LAT.Space then
            if Qopened then
               mask (x) := 'X';
            end if;
         end if;
      end loop;
      return HT.contains (S => mask, fragment => " ");
   end contains_nonquoted_spaces;


   --------------------------------------------------------------------------------------------
   --  adjust_defaults_port_parse
   --------------------------------------------------------------------------------------------
   procedure adjust_defaults_port_parse (specs : in out Portspecs)
   is
      procedure grow (Key : HT.Text; Element : in out group_list);

      empty_comment : HT.Text := HT.SUS ("# empty");

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
      if specs.df_index.Is_Empty then
         specs.df_index.Append (HT.SUS ("1"));
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
   --  get_field_value
   --------------------------------------------------------------------------------------------
   function get_field_value (specs : Portspecs; field : spec_field) return String
   is
      procedure concat (position : string_crate.Cursor);
      procedure dump_option (position : option_crate.Cursor);

      joined : HT.Text;

      procedure concat (position : string_crate.Cursor) is
      begin
         if not HT.IsBlank (joined) then
            HT.SU.Append (joined, ", ");
         end if;
         HT.SU.Append (joined, string_crate.Element (position));
      end concat;

      procedure dump_option (position : option_crate.Cursor)
      is
         rec : Option_Helper renames option_crate.Element (position);
         optname : String := HT.USS (rec.option_name);
      begin
         if optname = "none" then
            return;
         end if;
         if rec.currently_set_ON then
            HT.SU.Append (joined, " " & optname & ": on,");
         else
            HT.SU.Append (joined, " " & optname & ": off,");
         end if;
      end dump_option;
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
         when sp_contacts   =>
            specs.contacts.Iterate (concat'Access);
            return HT.USS (joined);
         when sp_keywords =>
            specs.keywords.Iterate (concat'Access);
            return HT.USS (joined);
         when sp_licenses =>
            specs.licenses.Iterate (concat'Access);
            return HT.USS (joined);
         when sp_users =>
            specs.users.Iterate (concat'Access);
            return HT.USS (joined);
         when sp_groups =>
            specs.groups.Iterate (concat'Access);
            return HT.USS (joined);
         when sp_opt_helper =>
            specs.ops_helpers.Iterate (dump_option'Access);
            return HT.USS (joined);
         when sp_variants =>
            specs.variants.Iterate (concat'Access);
            return HT.USS (joined);
         when others =>
            raise wrong_type with field'Img;
      end case;
   end get_field_value;


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
   begin
      case field is
         when sp_build_deps    => specs.build_deps.Iterate (scan'Access);
         when sp_buildrun_deps => specs.buildrun_deps.Iterate (scan'Access);
         when sp_run_deps      => specs.run_deps.Iterate (scan'Access);
         when sp_opts_standard => specs.ops_standard.Iterate (scan'Access);
         when sp_opts_avail    => specs.ops_avail.Iterate (scan'Access);
         when sp_variants      => specs.variants.Iterate (scan'Access);
         when sp_notes         => specs.pkg_notes.Iterate (scan_note'Access);
         when others =>
            raise wrong_type with field'Img;
      end case;
      return HT.USS (result);
   end get_list_item;


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
   --  keyword_is_valid
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
      return False;
   end invalid_namebase;


   --------------------------------------------------------------------------------------------
   --  valid_uses_module
   --------------------------------------------------------------------------------------------
   function options_summary (specs : Portspecs) return String
   is
      package sorter is new string_crate.Generic_Sorting ("<" => HT.SU."<");

      procedure scan (position : option_crate.Cursor);
      procedure format (position : string_crate.Cursor);

      tempstore : string_crate.Vector;
      block : HT.Text;

      procedure scan (position : option_crate.Cursor)
      is
         rec : Option_Helper renames option_crate.Element (position);
      begin
         tempstore.Append (rec.option_name);
      end scan;

      procedure format (position : string_crate.Cursor)
      is
         optname_text : HT.Text renames string_crate.Element (position);
         optname : String := HT.USS (optname_text);
         curval  : Boolean := specs.ops_helpers.Element (optname_text).currently_set_ON;
         desc    : String  := HT.USS (specs.ops_helpers.Element (optname_text).option_description);
         --  The option name is limited to 14 characters.  Format:
         --  123456789-12345678-1234
         --  OPTION_NAMEXXX  OFF  Description ...
         --  OPTION_2        ON   Description ...
         part1   : String (1 .. 16) := (others => ' ');
         part2   : String (1 .. 5);
      begin
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
   function combined_dependency_origins (specs : Portspecs) return String
   is
      procedure scan  (position : string_crate.Cursor);
      procedure print (position : string_crate.Cursor);

      combined : string_crate.Vector;
      result   : HT.Text;

      procedure scan (position : string_crate.Cursor)
      is
         text_value : HT.Text renames string_crate.Element (position);
      begin
         if not combined.Contains (text_value) then
            combined.Append (text_value);
         end if;
      end scan;

      procedure print (position : string_crate.Cursor)
      is
         text_value : HT.Text renames string_crate.Element (position);
      begin
         HT.SU.Append (result, HT.USS (text_value) & LAT.LF);
      end print;

   begin
      specs.build_deps.Iterate (scan'Access);
      specs.buildrun_deps.Iterate (scan'Access);
      specs.run_deps.Iterate (scan'Access);
      combined.Iterate (print'Access);
      return HT.USS (result);
   end combined_dependency_origins;


   --------------------------------------------------------------------------------------------
   --  combined_run_dependency_origins
   --------------------------------------------------------------------------------------------
   function combined_run_dependency_origins (specs : Portspecs) return String
   is
      procedure scan  (position : string_crate.Cursor);
      procedure print (position : string_crate.Cursor);

      combined : string_crate.Vector;
      result   : HT.Text;

      procedure scan (position : string_crate.Cursor)
      is
         text_value : HT.Text renames string_crate.Element (position);
      begin
         if not combined.Contains (text_value) then
            combined.Append (text_value);
         end if;
      end scan;

      procedure print (position : string_crate.Cursor)
      is
         text_value : HT.Text renames string_crate.Element (position);
      begin
         HT.SU.Append (result, HT.USS (text_value) & LAT.LF);
      end print;
   begin
      specs.buildrun_deps.Iterate (scan'Access);
      specs.run_deps.Iterate (scan'Access);
      combined.Iterate (print'Access);
      return HT.USS (result);
   end combined_run_dependency_origins;


   --------------------------------------------------------------------------------------------
   --  valid_uses_module
   --------------------------------------------------------------------------------------------
   function valid_uses_module (value : String) return Boolean
   is
      total_modules : constant Positive := 5;

      subtype uses_string is String (1 .. 12);

      --  Keep in alphabetical order for future conversion to binary search
      all_keywords : constant array (1 .. total_modules) of uses_string :=
        (
         "cpe         ",
         "gettext     ",
         "gmake       ",
         "iconv       ",
         "libtool     "
        );
      bandolier : uses_string := (others => ' ');

   begin
      if value'Length > uses_string'Length then
         return False;
      end if;

      bandolier (1 .. value'Length) := value;

      for index in all_keywords'Range loop
         if all_keywords (index) = bandolier then
            return True;
         end if;
      end loop;

      return False;
   end valid_uses_module;


   --------------------------------------------------------------------------------------------
   --  keyword_is_valid
   --------------------------------------------------------------------------------------------
   function keyword_is_valid (keyword : String) return Boolean
   is
      total_keywords : constant Positive := 66;

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
         "german       ",
         "graphics     ",
         "irc          ",
         "italian      ",
         "japanese     ",
         "java         ",
         "javascript   ",
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
         "scheme       ",
         "science      ",
         "security     ",
         "shells       ",
         "spanish      ",
         "sysutils     ",
         "textproc     ",
         "vietnamese   ",
         "www          ",
         "x11          ",
         "x11_clocks   ",
         "x11_drivers  ",
         "x11_fm       ",
         "x11_fonts    ",
         "x11_servers  ",
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
         print_opt_vector (rec.BUILD_DEPENDS_ON, "BUILD_DEPENDS_ON");
         print_opt_vector (rec.BUILD_TARGET_ON, "BUILD_TARGET_ON");
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
         print_opt_vector (rec.DF_INDEX_ON, "DF_INDEX_ON");
         print_opt_vector (rec.EXTRA_PATCHES_ON, "EXTRA_PATCHES_ON");
         print_opt_vector (rec.EXTRACT_ONLY_ON, "EXTRACT_ONLY_ON");
         print_opt_vector (rec.GH_ACCOUNT_ON, "GH_ACCOUNT_ON");
         print_opt_vector (rec.GH_PROJECT_ON, "GH_PROJECT_ON");
         print_opt_vector (rec.GH_SUBDIR_ON, "GH_SUBDIR_ON");
         print_opt_vector (rec.GH_TAGNAME_ON, "GH_TAGNAME_ON");
         print_opt_vector (rec.GH_TUPLE_ON, "GH_TUPLE_ON");
         print_opt_vector (rec.IMPLIES_ON, "IMPLIES_ON");
         print_opt_vector (rec.INFO_ON, "INFO_ON");
         print_opt_vector (rec.INSTALL_TARGET_ON, "INSTALL_TARGET_ON");
         print_opt_vector (rec.KEYWORDS_ON, "KEYWORDS_ON");
         print_opt_vector (rec.LDFLAGS_ON, "LDFLAGS_ON");
         print_opt_vector (rec.BUILDRUN_DEPENDS_ON, "BUILDRUN_DEPENDS_ON");
         print_opt_vector (rec.MAKE_ARGS_ON, "MAKE_ARGS_ON");
         print_opt_vector (rec.MAKE_ENV_ON, "MAKE_ENV_ON");
         print_opt_vector (rec.PATCHFILES_ON, "PATCHFILES_ON");
         print_opt_vector (rec.PLIST_SUB_ON, "PLIST_SUB_ON");
         print_opt_vector (rec.PREVENTS_ON, "PREVENTS_ON");
         print_opt_vector (rec.QMAKE_OFF, "QMAKE_OFF");
         print_opt_vector (rec.QMAKE_ON, "QMAKE_ON");
         print_opt_vector (rec.RUN_DEPENDS_ON, "RUN_DEPENDS_ON");
         print_opt_vector (rec.SUB_FILES_ON, "SUB_FILES_ON");
         print_opt_vector (rec.SUB_LIST_ON, "SUB_LIST_ON");
         print_opt_vector (rec.TEST_TARGET_ON, "TEST_TARGET_ON");
         print_opt_vector (rec.USES_ON, "USES_ON");
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
            when sp_exc_opsys     => specs.exc_opsys.Iterate (Process => print_item'Access);
            when sp_inc_opsys     => specs.inc_opsys.Iterate (Process => print_item'Access);
            when sp_exc_arch      => specs.exc_arch.Iterate (Process => print_item'Access);
            when sp_opts_avail    => specs.ops_avail.Iterate (Process => print_item'Access);
            when sp_opts_standard => specs.ops_standard.Iterate (Process => print_item'Access);
            when sp_df_index      => specs.df_index.Iterate (Process => print_item'Access);
            when sp_distfiles     => specs.distfiles.Iterate (Process => print_item'Access);
            when sp_contacts      => specs.contacts.Iterate (Process => print_item'Access);
            when sp_variants      => specs.variants.Iterate (Process => print_item'Access);
            when sp_keywords      => specs.keywords.Iterate (Process => print_item'Access);
            when sp_ext_only      => specs.extract_only.Iterate (Process => print_item'Access);
            when sp_ext_zip       => specs.extract_zip.Iterate (Process => print_item'Access);
            when sp_ext_7z        => specs.extract_7z.Iterate (Process => print_item'Access);
            when sp_ext_lha       => specs.extract_lha.Iterate (Process => print_item'Access);
            when sp_ext_dirty     => specs.extract_dirty.Iterate (Process => print_item'Access);
            when sp_make_args     => specs.make_args.Iterate (Process => print_item'Access);
            when sp_make_env      => specs.make_env.Iterate (Process => print_item'Access);
            when sp_build_target  => specs.build_target.Iterate (Process => print_item'Access);
            when sp_cflags        => specs.cflags.Iterate (Process => print_item'Access);
            when sp_cxxflags      => specs.cxxflags.Iterate (Process => print_item'Access);
            when sp_cppflags      => specs.cppflags.Iterate (Process => print_item'Access);
            when sp_ldflags       => specs.ldflags.Iterate (Process => print_item'Access);
            when sp_patchfiles    => specs.patchfiles.Iterate (Process => print_item'Access);
            when sp_uses          => specs.uses.Iterate (Process => print_item'Access);
            when sp_sub_list      => specs.sub_list.Iterate (Process => print_item'Access);
            when sp_sub_files     => specs.sub_files.Iterate (Process => print_item'Access);
            when sp_config_args   => specs.config_args.Iterate (Process => print_item'Access);
            when sp_config_env    => specs.config_env.Iterate (Process => print_item'Access);
            when sp_build_deps    => specs.build_deps.Iterate (Process => print_item'Access);
            when sp_buildrun_deps => specs.buildrun_deps.Iterate (Process => print_item'Access);
            when sp_run_deps      => specs.run_deps.Iterate (Process => print_item'Access);
            when sp_cmake_args    => specs.cmake_args.Iterate (Process => print_item'Access);
            when sp_qmake_args    => specs.qmake_args.Iterate (Process => print_item'Access);
            when sp_info          => specs.info.Iterate (Process => print_item'Access);
            when sp_install_tgt   => specs.install_tgt.Iterate (Process => print_item'Access);
            when sp_patch_strip   => specs.patch_strip.Iterate (Process => print_item'Access);
            when sp_pfiles_strip  => specs.pfiles_strip.Iterate (Process => print_item'Access);
            when sp_extra_patches => specs.extra_patches.Iterate (Process => print_item'Access);
            when sp_plist_sub     => specs.plist_sub.Iterate (Process => print_item'Access);
            when sp_licenses      => specs.licenses.Iterate (Process => print_item'Access);
            when sp_users         => specs.users.Iterate (Process => print_item'Access);
            when sp_groups        => specs.groups.Iterate (Process => print_item'Access);
            when others => null;
         end case;
         TIO.Put (LAT.LF);
      end print_vector_list;

      procedure print_group_list (thelabel : String; thelist : spec_field) is
      begin
         TIO.Put_Line (thelabel & LAT.Colon);
         case thelist is
            when sp_vopts            => specs.variantopts.Iterate (Process => dump'Access);
            when sp_options_on       => specs.options_on.Iterate (Process => dump'Access);
            when sp_broken           => specs.broken.Iterate (Process => dump'Access);
            when sp_subpackages      => specs.subpackages.Iterate (Process => dump'Access);
            when sp_dl_sites         => specs.dl_sites.Iterate (Process => dump'Access);
            when sp_ext_head         => specs.extract_head.Iterate (Process => dump'Access);
            when sp_ext_tail         => specs.extract_tail.Iterate (Process => dump'Access);
            when sp_makefile_targets => specs.make_targets.Iterate (Process => dump_target'Access);
            when sp_opt_helper       => specs.ops_helpers.Iterate (Process => dump_option'Access);
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
            when sp_apply_f10_fix  => TIO.Put_Line (specs.apply_f10_fix'Img);
            when sp_cfg_outsrc     => TIO.Put_Line (specs.config_outsrc'Img);
            when sp_inst_tchain    => TIO.Put_Line (specs.shift_install'Img);
            when others => null;
         end case;
      end print_boolean;

      procedure print_define (flavor : Positive) is
      begin
         case flavor is
            when 1 => specs.taglines.Iterate (Process => print_tagline'Access);
            when 2 => specs.catch_all.Iterate (Process => print_catchall'Access);
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
      print_group_list  ("VOPTS", sp_vopts);
      print_group_list  ("OPT_ON", sp_options_on);
      print_group_list  ("BROKEN", sp_broken);
      print_vector_list ("ONLY_FOR_OPSYS", sp_inc_opsys);
      print_vector_list ("NOT_FOR_OPSYS", sp_exc_opsys);
      print_vector_list ("NOT_FOR_ARCH", sp_exc_arch);
      print_single      ("DEPRECATED", sp_deprecated);
      print_single      ("EXPIRATION_DATE", sp_expiration);
      print_vector_list ("BUILD_DEPENDS", sp_build_deps);
      print_vector_list ("BUILDRUN_DEPENDS", sp_buildrun_deps);
      print_vector_list ("RUN_DEPENDS", sp_run_deps);
      print_vector_list ("USES", sp_uses);
      print_vector_list ("SUB_LIST", sp_sub_list);
      print_vector_list ("SUB_FILES", sp_sub_files);
      print_group_list  ("OPTION HELPERS", sp_opt_helper);

      print_single      ("DISTNAME", sp_distname);
      print_vector_list ("EXTRACT_ONLY", sp_ext_only);
      print_vector_list ("EXTRACT_WITH_UNZIP", sp_ext_zip);
      print_vector_list ("EXTRACT_WITH_7Z", sp_ext_7z);
      print_vector_list ("EXTRACT_WITH_LHA", sp_ext_lha);
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
      print_boolean     ("APPLY_F10_FIX", sp_apply_f10_fix);

      print_boolean     ("SKIP_BUILD", sp_skip_build);
      print_boolean     ("SKIP_INSTALL", sp_skip_install);
      print_boolean     ("INSTALL_REQ_TOOLCHAIN", sp_inst_tchain);
      print_boolean     ("SINGLE_JOB", sp_single_job);
      print_boolean     ("DESTDIR_VIA_ENV", sp_destdir_env);
      print_single      ("BUILD_WRKSRC", sp_build_wrksrc);
      print_single      ("MAKEFILE", sp_makefile);
      print_single      ("DESTDIRNAME", sp_destdirname);
      print_vector_list ("MAKE_ARGS", sp_make_args);
      print_vector_list ("MAKE_ENV", sp_make_env);
      print_vector_list ("BUILD_TARGET", sp_build_target);
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
      print_vector_list ("USERS", sp_users);
      print_vector_list ("GROUPS", sp_groups);
      print_define      (2);  -- catchall

      print_group_list  ("Makefile Targets", sp_makefile_targets);

   end dump_specification;

end Port_Specification;
