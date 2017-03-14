--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Utilities;
with File_Operations;
with Package_Manifests;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Exceptions;

package body Specification_Parser is

   package UTL renames Utilities;
   package FOP renames File_Operations;
   package MAN renames Package_Manifests;
   package LAT renames Ada.Characters.Latin_1;
   package AS  renames Ada.Strings;
   package EX  renames Ada.Exceptions;

   --------------------------------------------------------------------------------------------
   --  parse_specification_file
   --------------------------------------------------------------------------------------------
   procedure parse_specification_file
     (dossier         : String;
      specification   : out PSP.Portspecs;
      success         : out Boolean;
      opsys_focus     : supported_opsys;
      stop_at_targets : Boolean;
      extraction_dir  : String := "")
   is
      spec : PSP.Portspecs renames specification;

      contents      : constant String  := FOP.get_file_contents (dossier);
      stop_at_files : constant Boolean := (extraction_dir = "");
      match_opsys   : constant String  := UTL.lower_opsys (opsys_focus);
      markers       : HT.Line_Markers;
      linenum       : Natural := 0;
      seen_namebase : Boolean := False;
      line_array    : spec_array;
      line_singlet  : spec_singlet;
      line_target   : spec_target;
      line_option   : PSP.spec_option;
      line_file     : Boolean;
      last_array    : spec_array      := not_array;
      last_singlet  : spec_singlet    := not_singlet;
      last_option   : PSP.spec_option := PSP.not_helper_format;
      last_seen     : type_category   := cat_none;
      last_df       : Integer := 0;
      last_index    : HT.Text;
      last_optindex : HT.Text;
      seen_singlet  : array (spec_singlet)    of Boolean := (others => False);
      quad_tab      : Boolean;

      use type PSP.spec_option;
   begin
      success := False;
      specification.initialize;
      HT.initialize_markers (contents, markers);
      loop
         exit when not HT.next_line_present (contents, markers);
         quad_tab := False;
         linenum := linenum + 1;
         declare
            line : constant String := HT.extract_line (contents, markers);
            LN   : constant String := "Line" & linenum'Img & ": ";
         begin
            if HT.IsBlank (line) then
               goto line_done;
            end if;
            if HT.trailing_whitespace_present (line) then
               last_parse_error := HT.SUS (LN & "Detected trailing white space");
               exit;
            end if;
            if HT.trapped_space_character_present (line) then
               last_parse_error := HT.SUS (LN & "Detected trapped space before hard tab");
               exit;
            end if;
            if line (line'First) = '#' then
               if line'Length = 1 then
                  goto line_done;
               end if;
               if line'Length > 79 then
                  last_parse_error := HT.SUS (LN & "Comment length exceeds 79 columns");
                  exit;
               end if;
               if line (line'First + 1) /= LAT.Space then
                  last_parse_error := HT.SUS (LN & "Space does not follow hash in comment");
                  exit;
               end if;
               goto line_done;
            end if;

            line_target := determine_target (specification, line, last_seen);
            if line_target = bad_target then
               last_parse_error := HT.SUS (LN & "Make target detected, but not recognized");
               exit;
            end if;

            --  Short-circuit the rest if the first character is a tab.
            if line_target = not_target or else
              line (line'First) = LAT.HT
            then
               line_file := is_file_capsule (line);
               if not line_file then
                  line_option := determine_option (line);
                  if line_option = PSP.not_supported_helper then
                     last_parse_error := HT.SUS (LN & "Option format, but helper not recognized.");
                     exit;
                  end if;
                  if line_option = PSP.not_helper_format then
                     line_array := determine_array (line);
                     if line_array = not_array then
                        line_singlet := determine_singlet (line);
                        if line_singlet /= not_singlet and then
                          seen_singlet (line_singlet)
                        then
                           last_parse_error :=
                             HT.SUS (LN & "variable previously defined (use triple tab)");
                           exit;
                        end if;
                        seen_singlet (line_singlet) := True;
                     else
                        line_singlet := not_singlet;
                     end if;
                  else
                     line_array   := not_array;
                     line_singlet := not_singlet;
                  end if;
               else
                  line_option  := PSP.not_helper_format;
                  line_array   := not_array;
                  line_singlet := not_singlet;
               end if;
            else
               line_file    := False;
               line_option  := PSP.not_helper_format;
               line_array   := not_array;
               line_singlet := not_singlet;
            end if;

            if not line_file and then
              line_target = not_target and then
              line_option = PSP.not_helper_format and then
              line_array = not_array and then
              line_singlet = not_singlet
            then
               if (
                   last_seen = cat_option and then
                   line'Length > 4 and then
                   line (line'First .. line'First + 3) = LAT.HT & LAT.HT & LAT.HT & LAT.HT
                  ) or else
                 (line'Length > 3 and then
                  line (line'First .. line'First + 2) = LAT.HT & LAT.HT & LAT.HT)
               then
                  case last_seen is
                     when cat_array   => line_array   := last_array;
                     when cat_singlet => line_singlet := last_singlet;
                     when cat_none    => null;
                     when cat_target  => null;
                     when cat_file    => null;
                     when cat_option  => line_option  := last_option;
                                         quad_tab := True;
                  end case;
               else
                  last_parse_error := HT.SUS (LN & "Parse failed, content unrecognized.");
                  exit;
               end if;
            end if;

            begin
               if line_array /= not_array then
                  declare
                     tvalue   : String  := retrieve_single_value (line);
                     defkey   : HT.Text := retrieve_key (line, last_index);
                     defvalue : HT.Text := HT.SUS (tvalue);
                     tkey     : String  := HT.USS (defkey);
                  begin
                     last_index := defkey;
                     case line_array is
                        when def =>
                           if seen_namebase then
                              last_parse_error := HT.SUS (LN & "DEF can't appear after NAMEBASE");
                              exit;
                           end if;
                           if spec_definitions.Contains (defkey) then
                              raise duplicate_key with HT.USS (defkey);
                           else
                              spec_definitions.Insert (Key      => defkey,
                                                       New_Item => defvalue);
                           end if;
                        when sdesc =>
                           if HT.SU.Length (defvalue) > 50 then
                              last_parse_error := HT.SUS (LN & "SDESC longer than 50 chars");
                              exit;
                           end if;
                           if HT.SU.Length (defvalue) < 12 then
                              last_parse_error := HT.SUS (LN & "SDESC does not meet " &
                                                            "12-character length minimum");
                              exit;
                           end if;
                           declare
                              onestr : String (1 .. 1);
                              trestr : String (1 .. 3);
                           begin
                              onestr (1) := tvalue (tvalue'First);
                              if onestr /= HT.uppercase (onestr) then
                                 last_parse_error := HT.SUS (LN & "SDESC does not start with " &
                                                               "a capital letter");
                                 exit;
                              end if;
                              if tvalue (tvalue'Last) = LAT.Full_Stop then
                                 last_parse_error := HT.SUS (LN & "SDESC ends in a period");
                                 exit;
                              end if;
                              trestr := tvalue (tvalue'First .. tvalue'First + 2);
                              if trestr = "An " or else
                                trestr (1 .. 2) = "A "
                              then
                                 last_parse_error := HT.SUS (LN & "SDESC starts with an " &
                                                               "indefinite article");
                                 exit;
                              end if;
                              if specification.variant_exists (tkey) then
                                 specification.append_array (field => PSP.sp_taglines,
                                                             key   => tkey,
                                                             value => tvalue,
                                                             allow_spaces => True);
                              else
                                 last_parse_error := HT.SUS (LN & "variant '" & tkey &
                                                               "' was not previously defined.");
                                 exit;
                              end if;
                           end;
                        when distfile =>
                           declare
                              new_index : Integer := Integer'Value (tkey);
                           begin
                              if new_index /= last_df + 1 then
                                 last_parse_error := HT.SUS (LN & "'" & tkey & "' index is " &
                                                               "not in order 1,2,3,..,n");
                                 exit;
                              end if;
                              last_df := new_index;
                           exception
                              when Constraint_Error =>
                                 last_parse_error := HT.SUS (LN & "'" & tkey & "' index is " &
                                                               "not an integer as required.");
                                 exit;
                           end;
                           specification.append_list (PSP.sp_distfiles, tvalue);
                        when sites =>
                           if tkey = dlgroup_none then
                              last_parse_error := HT.SUS (LN & "cannot set site group to '" &
                                                            dlgroup_none & "'");
                              exit;
                           else
                              build_group_list (spec  => specification,
                                                field => PSP.sp_dl_sites,
                                                key   => tkey,
                                                value => tvalue);
                           end if;
                        when spkgs =>
                           build_group_list (spec  => specification,
                                             field => PSP.sp_subpackages,
                                             key   => tkey,
                                             value => tvalue);
                        when vopts =>
                           build_group_list (spec  => specification,
                                             field => PSP.sp_vopts,
                                             key   => tkey,
                                             value => tvalue);
                        when ext_head =>
                           specification.append_array (field        => PSP.sp_ext_head,
                                                       key          => tkey,
                                                       value        => tvalue,
                                                       allow_spaces => True);
                        when ext_tail =>
                           specification.append_array (field        => PSP.sp_ext_tail,
                                                       key          => tkey,
                                                       value        => tvalue,
                                                       allow_spaces => True);
                        when option_on =>
                           if tkey = options_all or else
                             UTL.valid_lower_opsys (tkey) or else
                             UTL.valid_cpu_arch (tkey)
                           then
                              build_group_list (spec  => specification,
                                                field => PSP.sp_options_on,
                                                key   => tkey,
                                                value => tvalue);
                           else
                              last_parse_error :=
                                HT.SUS (LN & "group '" & tkey &
                                          "' is not valid (all, <opsys>, <arch>)");
                              exit;
                           end if;
                        when broken =>
                           if tkey = broken_all or else
                             UTL.valid_lower_opsys (tkey) or else
                             UTL.valid_cpu_arch (tkey)
                           then
                              specification.append_array (field        => PSP.sp_broken,
                                                          key          => tkey,
                                                          value        => tvalue,
                                                          allow_spaces => True);
                           else
                              last_parse_error :=
                                HT.SUS (LN & "group '" & tkey &
                                          "' is not valid (all, <opsys>, <arch>)");
                              exit;
                           end if;
                        when not_array => null;
                     end case;
                  end;
                  last_array := line_array;
                  last_seen  := cat_array;
               end if;

               if line_singlet /= not_singlet then
                  case line_singlet is
                     when namebase =>
                        seen_namebase := True;
                        build_string (spec, PSP.sp_namebase, line);
                     when version          => build_string (spec, PSP.sp_version, line);
                     when dist_subdir      => build_string (spec, PSP.sp_distsubdir, line);
                     when distname         => build_string (spec, PSP.sp_distname, line);
                     when build_wrksrc     => build_string (spec, PSP.sp_build_wrksrc, line);
                     when patch_wrksrc     => build_string (spec, PSP.sp_patch_wrksrc, line);
                     when configure_wrksrc => build_string (spec, PSP.sp_config_wrksrc, line);
                     when install_wrksrc   => build_string (spec, PSP.sp_install_wrksrc, line);
                     when makefile         => build_string (spec, PSP.sp_makefile, line);
                     when destdirname      => build_string (spec, PSP.sp_destdirname, line);
                     when homepage         => build_string (spec, PSP.sp_homepage, line);
                     when configure_script => build_string (spec, PSP.sp_config_script, line);
                     when gnu_cfg_prefix   => build_string (spec, PSP.sp_gnu_cfg_prefix, line);
                     when must_configure   => build_string (spec, PSP.sp_must_config, line);
                     when deprecated       => build_string (spec, PSP.sp_deprecated, line);
                     when expiration       => build_string (spec, PSP.sp_expiration, line);
                     when revision         => set_natural (spec, PSP.sp_revision, line);
                     when epoch            => set_natural (spec, PSP.sp_epoch, line);
                     when opt_level        => set_natural (spec, PSP.sp_opt_level, line);
                     when skip_build       => set_boolean (spec, PSP.sp_skip_build, line);
                     when skip_install     => set_boolean (spec, PSP.sp_skip_install, line);
                     when single_job       => set_boolean (spec, PSP.sp_single_job, line);
                     when destdir_env      => set_boolean (spec, PSP.sp_destdir_env, line);
                     when config_outsource => set_boolean (spec, PSP.sp_cfg_outsrc, line);
                     when keywords         => build_list (spec, PSP.sp_keywords, line);
                     when variants         => build_list (spec, PSP.sp_variants, line);
                     when contacts         => build_list (spec, PSP.sp_contacts, line);
                     when dl_groups        => build_list (spec, PSP.sp_dl_groups, line);
                     when df_index         => build_list (spec, PSP.sp_df_index, line);
                     when opt_avail        => build_list (spec, PSP.sp_opts_avail, line);
                     when opt_standard     => build_list (spec, PSP.sp_opts_standard, line);
                     when exc_opsys        => build_list (spec, PSP.sp_exc_opsys, line);
                     when inc_opsys        => build_list (spec, PSP.sp_inc_opsys, line);
                     when exc_arch         => build_list (spec, PSP.sp_exc_arch, line);
                     when ext_only         => build_list (spec, PSP.sp_ext_only, line);
                     when ext_zip          => build_list (spec, PSP.sp_ext_zip, line);
                     when ext_7z           => build_list (spec, PSP.sp_ext_7z, line);
                     when ext_lha          => build_list (spec, PSP.sp_ext_lha, line);
                     when ext_dirty        => build_list (spec, PSP.sp_ext_dirty, line);
                     when make_args        => build_list (spec, PSP.sp_make_args, line);
                     when make_env         => build_list (spec, PSP.sp_make_env, line);
                     when build_target     => build_list (spec, PSP.sp_build_target, line);
                     when cflags           => build_list (spec, PSP.sp_cflags, line);
                     when cxxflags         => build_list (spec, PSP.sp_cxxflags, line);
                     when cppflags         => build_list (spec, PSP.sp_cppflags, line);
                     when ldflags          => build_list (spec, PSP.sp_ldflags, line);
                     when patchfiles       => build_list (spec, PSP.sp_patchfiles, line);
                     when uses             => build_list (spec, PSP.sp_uses, line);
                     when sub_files        => build_list (spec, PSP.sp_sub_files, line);
                     when sub_list         => build_list (spec, PSP.sp_sub_list, line);
                     when config_args      => build_list (spec, PSP.sp_config_args, line);
                     when config_env       => build_list (spec, PSP.sp_config_env, line);
                     when configure_target => build_list (spec, PSP.sp_config_target, line);
                     when build_deps       => build_list (spec, PSP.sp_build_deps, line);
                     when lib_deps         => build_list (spec, PSP.sp_lib_deps, line);
                     when run_deps         => build_list (spec, PSP.sp_run_deps, line);
                     when cmake_args       => build_list (spec, PSP.sp_cmake_args, line);
                     when qmake_args       => build_list (spec, PSP.sp_qmake_args, line);
                     when info             => build_list (spec, PSP.sp_info, line);
                     when install_tgt      => build_list (spec, PSP.sp_install_tgt, line);
                     when apply_10_fix     => build_list (spec, PSP.sp_apply_f10_fix, line);
                     when patch_strip      => build_list (spec, PSP.sp_patch_strip, line);
                     when extra_patches    => build_list (spec, PSP.sp_extra_patches, line);
                     when patchfiles_strip => build_list (spec, PSP.sp_pfiles_strip, line);
                     when plist_sub        => build_list (spec, PSP.sp_plist_sub, line);
                     when not_singlet      => null;
                  end case;
                  last_singlet := line_singlet;
                  last_seen := cat_singlet;
               end if;

               if line_target /= not_target then
                  if stop_at_targets then
                     exit;
                  end if;
                  case line_target is
                     when target_title =>
                        declare
                           target : String := line (line'First .. line'Last - 1);
                        begin
                           if specification.group_exists (PSP.sp_makefile_targets, target) then
                              last_parse_error := HT.SUS (LN & "Duplicate makefile target '"
                                                          & target & "' detected.");
                              exit;
                           end if;
                           specification.establish_group (PSP.sp_makefile_targets, target);
                           last_index := HT.SUS (target);
                        end;
                     when target_body  =>
                        specification.append_array (field        => PSP.sp_makefile_targets,
                                                    key          => HT.USS (last_index),
                                                    value        => line,
                                                    allow_spaces => True);
                     when bad_target   => null;
                     when not_target   => null;
                  end case;
                  last_seen := cat_target;
               end if;

               if line_option /= PSP.not_helper_format then
                  declare
                     option_name : String := extract_option_name (spec, line, last_optindex);
                  begin
                     if option_name = "" then
                        last_parse_error :=
                          HT.SUS (LN & "Valid helper, but option has never been defined " &
                                    "(also seen when continuation line doesn't start with " &
                                    "5 tabs)");
                        exit;
                     end if;
                     if not quad_tab and then
                       not specification.option_helper_unset (field  => line_option,
                                                              option => option_name)
                     then
                        last_parse_error :=
                          HT.SUS (LN & "option helper previously defined (use quadruple tab)");
                        exit;
                     end if;
                     build_list (specification, line_option, option_name, line);
                     last_optindex := HT.SUS (option_name);
                  end;
                  last_option := line_option;
                  last_seen   := cat_option;
               end if;

               if line_file then
                  if stop_at_files then
                     exit;
                  end if;
                  declare
                     filename : String  := retrieve_file_name (line);
                     filesize : Natural := retrieve_file_size (line);
                     fileguts : String  := HT.extract_file (contents, markers, filesize);
                     subdir   : String  := HT.partial_search (filename, 0, "/");
                     --  only acceptable subdir:
                     --     1. ""
                     --     2. "patches"
                     --     3. "files"
                     --     4. "scripts"
                     --     5. current lower opsys (saves to "opsys")
                  begin
                     if subdir = "" or else
                       subdir = "patches" or else
                       subdir = "files" or else
                       subdir = "scripts" or else
                       subdir = "descriptions"
                     then
                        FOP.create_subdirectory (extraction_dir, subdir);
                        FOP.dump_contents_to_file (fileguts, extraction_dir & "/" & filename);
                     elsif subdir = match_opsys then
                        FOP.create_subdirectory (extraction_dir, "opsys");
                        declare
                           newname : String :=
                             extraction_dir & "/opsys/" & HT.part_2 (filename, "/");
                        begin
                           FOP.dump_contents_to_file (fileguts, newname);
                        end;
                     elsif subdir = "manifests" then
                        FOP.create_subdirectory (extraction_dir, subdir);
                        MAN.decompress_manifest
                          (compressed_string => fileguts,
                           save_to_file      => MAN.Filename (extraction_dir & "/" & filename));
                     end if;
                  end;
               end if;

            exception
               when F1 : PSP.misordered =>
                  last_parse_error := HT.SUS (LN & "Field " & EX.Exception_Message (F1) &
                                                " appears out of order");
                  exit;
               when F2 : PSP.contains_spaces =>
                  last_parse_error := HT.SUS (LN & "Multiple values found");
                  exit;
               when F3 : PSP.wrong_type =>
                  last_parse_error := HT.SUS (LN & "Field " & EX.Exception_Message (F3) &
                                                " DEV ISSUE: matched to wrong type");
                  exit;
               when F4 : PSP.wrong_value =>
                  last_parse_error := HT.SUS (LN & EX.Exception_Message (F4));
                  exit;
               when F5 : mistabbed =>
                  last_parse_error := HT.SUS (LN & "value not aligned to column-24 (tab issue)");
                  exit;
               when F6 : missing_definition =>
                  last_parse_error := HT.SUS (LN & "Variable expansion: definition missing. " &
                                                EX.Exception_Message (F6));
                  exit;
               when F7 : extra_spaces =>
                  last_parse_error := HT.SUS (LN & "extra spaces detected between list items.");
                  exit;
               when F8 : expansion_too_long =>
                  last_parse_error := HT.SUS (LN & "expansion exceeds 512-char maximum.");
                  exit;
               when F9 : duplicate_key =>
                  last_parse_error := HT.SUS (LN & "array key '" & EX.Exception_Message (F9) &
                                                "' duplicated.");
                  exit;
               when FA : PSP.dupe_spec_key =>
                  last_parse_error := HT.SUS (LN & EX.Exception_Message (FA) &
                                                " key duplicated.");
                  exit;
               when FB : generic_format =>
                  last_parse_error := HT.SUS (LN & EX.Exception_Message (FB));
                  exit;
               when FC : PSP.missing_group =>
                  last_parse_error := HT.SUS (LN & EX.Exception_Message (FC) &
                                                " group has not yet been established.");
               when FD : PSP.dupe_list_value =>
                  last_parse_error := HT.SUS (LN & "list item '" & EX.Exception_Message (FD) &
                                                "' is duplicate.");
                  exit;
               when FE : mistabbed_40 =>
                  last_parse_error :=
                    HT.SUS (LN & "option value not aligned to column-40 (tab issue)");
                  exit;
            end;
            <<line_done>>
         end;
      end loop;
      if HT.IsBlank (last_parse_error) then
         last_parse_error := late_validity_check_error (specification);
         if HT.IsBlank (last_parse_error) then
            specification.adjust_defaults_port_parse;
            success := True;
         end if;
      end if;
   exception
      when FOP.file_handling =>
         success := False;
         last_parse_error := HT.SUS ("Failed to dump contents of " & dossier);
   end parse_specification_file;


   --------------------------------------------------------------------------------------------
   --  get_parse_error
   --------------------------------------------------------------------------------------------
   function get_parse_error return String is
   begin
      return HT.USS (last_parse_error);
   end get_parse_error;


   --------------------------------------------------------------------------------------------
   --  expand_value
   --------------------------------------------------------------------------------------------
   function expand_value (value : String) return String
   is
      function translate (variable : String) return String;
      function will_fit  (CL, CR : Natural; new_text : String) return Boolean;
      procedure exchange (CL, CR : Natural; new_text : String);
      function modify    (curvalue : HT.Text;
                          modifier : String;
                          valid    : out Boolean) return HT.Text;

      canvas       : String (1 .. 512);
      found        : Boolean;
      front_marker : Natural;
      curly_left   : Natural;
      curly_right  : Natural;
      trans_error  : Natural;
      delimiter    : HT.Text := HT.SUS ("/");

      function will_fit (CL, CR : Natural; new_text : String) return Boolean
      is
         old_length : Natural := CR - CL + 1;
         new_marker : Natural := front_marker - old_length + new_text'Length;
      begin
         return (new_marker < canvas'Length);
      end will_fit;

      procedure exchange (CL, CR : Natural; new_text : String)
      is
         old_length : Natural := CR - CL + 1;
      begin
         if old_length = new_text'Length then
            canvas (CL .. CR) := new_text;
            return;
         end if;

         declare
            final_segment : String := canvas (CR + 1 .. front_marker);
         begin
            --  if old_length < new_text'Length then
            --     Shift later text to the right from the end (expand)
            --  else
            --     Shift later text to the left from the beginning (shrink)

            front_marker := front_marker + (new_text'Length - old_length);
            canvas (CL .. front_marker) := new_text & final_segment;
         end;
      end exchange;

      function translate (variable : String) return String
      is
         basename   : HT.Text;
         result     : HT.Text := HT.blank;
         colon_pos  : Natural := AS.Fixed.Index (variable, ":");
         colon_next : Natural;
         end_mark   : Natural;
         found      : Boolean;
         valid      : Boolean;
      begin
         if colon_pos = 0 then
            basename := HT.SUS (variable);
         else
            basename := HT.SUS (variable (variable'First .. colon_pos - 1));
         end if;

         if spec_definitions.Contains (basename) then
            result := spec_definitions.Element (basename);
         else
            trans_error := 1;  -- missing_definition
            return "";
         end if;

         loop
            exit when colon_pos = 0;
            colon_next := colon_pos + 1;
            found := False;
            loop
               exit when colon_next > variable'Last;
               if variable (colon_next) = LAT.Colon then
                  found := True;
                  exit;
               end if;
               colon_next := colon_next + 1;
            end loop;
            if found then
               end_mark := colon_next - 1;
            else
               end_mark := variable'Last;
               colon_next := 0;
            end if;
            if end_mark = colon_pos then
               trans_error := 2;  --  bad_modifier
               return "";
            end if;
            result := modify (result, variable (colon_pos + 1 .. end_mark), valid);
            if not valid then
               trans_error := 2;  --  bad_modifier
               return "";
            end if;
            colon_pos := colon_next;
         end loop;
         trans_error := 0;
         return HT.USS (result);
      end translate;

      function modify (curvalue : HT.Text;
                       modifier : String;
                       valid    : out Boolean) return HT.Text
      is
         ml : Natural := modifier'Length;
         dot : HT.Text := HT.SUS (".");
      begin
         valid := True;
         if modifier = "LC" then
            return HT.lowercase (curvalue);
         elsif modifier = "UC" then
            return HT.uppercase (curvalue);
         elsif modifier = "H" then
            return HT.head (curvalue, delimiter);
         elsif modifier = "T" then
            return HT.tail (curvalue, delimiter);
         elsif modifier = "R" then
            return HT.head (curvalue, dot);
         elsif modifier = "E" then
            return HT.tail (curvalue, dot);
         elsif ml >= 6 and then
           modifier (modifier'First .. modifier'First + 3) = "DL=" & LAT.Quotation and then
           modifier (modifier'Last) = LAT.Quotation
         then
            delimiter := HT.SUS (modifier (modifier'First + 4 .. modifier'Last - 1));
            return curvalue;
         elsif ml >= 5 and then modifier (modifier'First) = 'S' then
            declare
               separator  : Character;
               position_2 : Natural := 0;
               position_3 : Natural := 0;
               repeat     : Boolean := False;
               its_bad    : Boolean := False;
               new_value  : HT.Text := curvalue;
            begin
               if modifier (modifier'First + 1) = LAT.Solidus or else
                 modifier (modifier'First + 1) = LAT.Vertical_Line
               then
                  separator := modifier (modifier'First + 1);
                  for arrow in Natural range modifier'First + 2 .. modifier'Last loop
                     if modifier (arrow) = separator then
                        if position_2 = 0 then
                           position_2 := arrow;
                        elsif position_3 = 0 then
                           position_3 := arrow;
                        else
                           its_bad := True;
                        end if;
                     end if;
                  end loop;
                  if position_3 = 0 then
                     its_bad := True;
                  else
                     if position_3 < modifier'Last then
                        if (position_3 = modifier'Last - 1) and then
                          modifier (modifier'Last) = 'g'
                        then
                           repeat := True;
                        else
                           its_bad := True;
                        end if;
                     end if;
                  end if;
                  if not its_bad then
                     declare
                        oldst : String := modifier (modifier'First + 2 .. position_2 - 1);
                        newst : String := modifier (position_2 + 1 .. position_3 - 1);
                     begin
                        loop
                           exit when not HT.contains (new_value, oldst);
                           new_value := HT.replace_substring (US         => new_value,
                                                              old_string => oldst,
                                                              new_string => newst);
                           exit when not repeat;
                        end loop;
                     end;
                     return new_value;
                  end if;
               end if;
            end;
         end if;
         valid := False;
         return HT.blank;
      end modify;

   begin
      if not HT.contains (S => value, fragment => "${") then
         return value;
      end if;
      if value'Length > canvas'Length then
         raise expansion_too_long;
      end if;
      front_marker := value'Length;
      canvas (1 .. front_marker) := value;
      loop
         curly_left := AS.Fixed.Index (canvas (1 .. front_marker), "${");
         if curly_left = 0 then
            return canvas (1 .. front_marker);
         end if;
         found := False;
         curly_right := curly_left + 2;
         loop
            exit when curly_right > front_marker;
            if canvas (curly_right) = LAT.Right_Curly_Bracket then
               found := True;
               exit;
            end if;
            curly_right := curly_right + 1;
         end loop;
         if found then
            if curly_right - curly_left - 2 = 0 then
               raise missing_definition with "zero-length variable name";
            end if;
            declare
               expanded : String := translate (canvas (curly_left + 2 .. curly_right - 1));
            begin
               if trans_error = 0 then
                  if will_fit (curly_left, curly_right, expanded) then
                     exchange (curly_left, curly_right, expanded);
                  else
                     raise expansion_too_long;
                  end if;
               elsif trans_error = 1 then
                  raise missing_definition;
               elsif trans_error = 2 then
                  raise bad_modifier;
               end if;
            end;
         end if;
      end loop;
   end expand_value;


   --------------------------------------------------------------------------------------------
   --  determine_array
   --------------------------------------------------------------------------------------------
   function determine_array (line : String) return spec_array
   is
      function known (index : Positive) return Boolean;

      total_arrays : constant Positive := 10;

      type array_pair is
         record
            varname : String (1 .. 12);
            len     : Natural;
            sparray : spec_array;
         end record;

      --  Keep in alphabetical order for future conversion to binary search
      all_arrays : constant array (1 .. total_arrays) of array_pair :=
        (
         ("BROKEN      ",  6, broken),
         ("DEF         ",  3, def),
         ("DISTFILE    ",  8, distfile),
         ("EXTRACT_HEAD", 12, ext_head),
         ("EXTRACT_TAIL", 12, ext_tail),
         ("OPT_ON      ",  6, option_on),
         ("SDESC       ",  5, sdesc),
         ("SITES       ",  5, sites),
         ("SPKGS       ",  5, spkgs),
         ("VOPTS       ",  5, vopts)
        );

      function known (index : Positive) return Boolean
      is
         len   : Natural renames all_arrays (index).len;
         apple : constant String  := all_arrays (index).varname (1 .. len);
      begin
         return line'Length > len + 6 and then
           line (line'First .. line'First + len) = apple & LAT.Left_Square_Bracket;
      end known;
   begin
      if not HT.contains (S => line, fragment => "]=" & LAT.HT) then
         return not_array;
      end if;
      for index in all_arrays'Range loop
         if known (index) then
            return all_arrays (index).sparray;
         end if;
      end loop;
      return not_array;

   end determine_array;


   --------------------------------------------------------------------------------------------
   --  determine_singlet
   --------------------------------------------------------------------------------------------
   function determine_singlet (line : String) return spec_singlet
   is
      function nailed    (index : Natural) return Boolean;
      function less_than (index : Natural) return Boolean;

      total_singlets : constant Positive := 65;

      type singlet_pair is
         record
            varname : String (1 .. 22);
            len     : Natural;
            singlet : spec_singlet;
         end record;

      --  It is critical that this list be alphabetized correctly.
      all_singlets : constant array (1 .. total_singlets) of singlet_pair :=
        (
         ("APPLY_F10_FIX         ", 13, apply_10_fix),
         ("BUILD_DEPENDS         ", 13, build_deps),
         ("BUILD_TARGET          ", 12, build_target),
         ("BUILD_WRKSRC          ", 12, build_wrksrc),
         ("CFLAGS                ",  6, cflags),
         ("CMAKE_ARGS            ", 10, cmake_args),
         ("CONFIGURE_ARGS        ", 14, config_args),
         ("CONFIGURE_ENV         ", 13, config_env),
         ("CONFIGURE_OUTSOURCE   ", 19, config_outsource),
         ("CONFIGURE_SCRIPT      ", 16, configure_script),
         ("CONFIGURE_TARGET      ", 16, configure_target),
         ("CONFIGURE_WRKSRC      ", 16, configure_wrksrc),
         ("CONTACT               ",  7, contacts),
         ("CPPFLAGS              ",  8, cppflags),
         ("CXXFLAGS              ",  8, cxxflags),
         ("DEPRECATED            ", 10, deprecated),
         ("DESTDIRNAME           ", 11, destdirname),
         ("DESTDIR_VIA_ENV       ", 15, destdir_env),
         ("DF_INDEX              ",  8, df_index),
         ("DISTNAME              ",  8, distname),
         ("DIST_SUBDIR           ", 11, dist_subdir),
         ("DOWNLOAD_GROUPS       ", 15, dl_groups),
         ("EPOCH                 ",  5, epoch),
         ("EXPIRATION_DATE       ", 15, expiration),
         ("EXTRACT_DIRTY         ", 13, ext_dirty),
         ("EXTRACT_ONLY          ", 12, ext_only),
         ("EXTRACT_WITH_7Z       ", 15, ext_7z),
         ("EXTRACT_WITH_LHA      ", 16, ext_lha),
         ("EXTRACT_WITH_UNZIP    ", 18, ext_zip),
         ("EXTRA_PATCHES         ", 13, extra_patches),
         ("GNU_CONFIGURE_PREFIX  ", 20, gnu_cfg_prefix),
         ("HOMEPAGE              ",  8, homepage),
         ("INFO                  ",  4, info),
         ("INSTALL_TARGET        ", 14, install_tgt),
         ("INSTALL_WRKSRC        ", 14, install_wrksrc),
         ("KEYWORDS              ",  8, keywords),
         ("LDFLAGS               ",  7, ldflags),
         ("LIB_DEPENDS           ", 11, lib_deps),
         ("MAKEFILE              ",  8, makefile),
         ("MAKE_ARGS             ",  9, make_args),
         ("MAKE_ENV              ",  8, make_env),
         ("MUST_CONFIGURE        ", 14, must_configure),
         ("NAMEBASE              ",  8, namebase),
         ("NOT_FOR_ARCH          ", 12, exc_arch),
         ("NOT_FOR_OPSYS         ", 13, exc_opsys),
         ("ONLY_FOR_OPSYS        ", 14, inc_opsys),
         ("OPTIMIZER_LEVEL       ", 15, opt_level),
         ("OPTIONS_AVAILABLE     ", 17, opt_avail),
         ("OPTIONS_STANDARD      ", 16, opt_standard),
         ("PATCHFILES            ", 10, patchfiles),
         ("PATCHFILES_STRIP      ", 16, patchfiles_strip),
         ("PATCH_STRIP           ", 11, patch_strip),
         ("PATCH_WRKSRC          ", 12, patch_wrksrc),
         ("PLIST_SUB             ",  9, plist_sub),
         ("QMAKE_ARGS            ", 10, qmake_args),
         ("REVISION              ",  8, revision),
         ("RUN_DEPENDS           ", 11, run_deps),
         ("SINGLE_JOB            ", 10, single_job),
         ("SKIP_BUILD            ", 10, skip_build),
         ("SKIP_INSTALL          ", 12, skip_install),
         ("SUB_FILES             ",  9, sub_files),
         ("SUB_LIST              ",  8, sub_list),
         ("USES                  ",  4, uses),
         ("VARIANTS              ",  8, variants),
         ("VERSION               ",  7, version)
        );

      function nailed (index : Natural) return Boolean
      is
         len : Natural renames all_singlets (index).len;
         apple : constant String  := all_singlets (index).varname (1 .. len) & LAT.Equals_Sign;
      begin
         return line'Length > len + 2 and then
           line (line'First .. line'First + len) = apple;
      end nailed;

      function less_than (index : Natural) return Boolean
      is
         len : Natural renames all_singlets (index).len;
         apple : constant String  := all_singlets (index).varname (1 .. len) & LAT.Equals_Sign;
      begin
         if line'Length > len + 2 then
            return line (line'First .. line'First + len) < apple;
         else
            return line < apple;
         end if;
      end less_than;

      Low  : Natural := all_singlets'First;
      High : Natural := all_singlets'Last;
      Mid  : Natural;
   begin
      if not HT.contains (S => line, fragment => "=" & LAT.HT) then
         return not_singlet;
      end if;

      loop
         Mid := (Low + High) / 2;
         if nailed (Mid) then
            return  all_singlets (Mid).singlet;
         elsif less_than (Mid) then
            exit when Low = Mid;
            High := Mid - 1;
         else
            exit when High = Mid;
            Low := Mid + 1;
         end if;
      end loop;
      return not_singlet;

   end determine_singlet;


   --------------------------------------------------------------------------------------------
   --  determine_option
   --------------------------------------------------------------------------------------------
   function determine_option (line : String) return PSP.spec_option
   is
      total_helpers : constant Positive := 40;

      subtype helper_string is String (1 .. 21);

      type helper_pair is
         record
            varname : helper_string;
            singlet : PSP.spec_option;
         end record;

      --  It is critical that this list be alphabetized correctly.
      all_helpers : constant array (1 .. total_helpers) of helper_pair :=
        (
         ("BROKEN_ON            ", PSP.broken_on),
         ("BUILD_DEPENDS_ON     ", PSP.build_depends_on),
         ("BUILD_TARGET_ON      ", PSP.build_target_on),
         ("CFLAGS_ON            ", PSP.cflags_on),
         ("CMAKE_ARGS_OFF       ", PSP.cmake_args_off),
         ("CMAKE_ARGS_ON        ", PSP.cmake_args_on),
         ("CMAKE_BOOL_F_BOTH    ", PSP.cmake_bool_f_both),
         ("CMAKE_BOOL_T_BOTH    ", PSP.cmake_bool_t_both),
         ("CONFIGURE_ARGS_OFF   ", PSP.configure_args_off),
         ("CONFIGURE_ARGS_ON    ", PSP.configure_args_on),
         ("CONFIGURE_ENABLE_BOTH", PSP.configure_enable_both),
         ("CONFIGURE_ENV_ON     ", PSP.configure_env_on),
         ("CONFIGURE_WITH_BOTH  ", PSP.configure_with_both),
         ("CPPFLAGS_ON          ", PSP.cppflags_on),
         ("DF_INDEX_ON          ", PSP.df_index_on),
         ("EXTRACT_ONLY_ON      ", PSP.extra_patches_on),
         ("EXTRA_PATCHES_ON     ", PSP.extra_patches_on),
         ("GH_ACCOUNT_ON        ", PSP.gh_account_on),
         ("GH_PROJECT_ON        ", PSP.gh_project_on),
         ("GH_SUBDIR_ON         ", PSP.gh_subdir_on),
         ("GH_TAGNAME_ON        ", PSP.gh_tagname_on),
         ("GH_TUPLE_ON          ", PSP.gh_tuple_on),
         ("IMPLIES_ON           ", PSP.implies_on),
         ("INFO_ON              ", PSP.info_on),
         ("INSTALL_TARGET_ON    ", PSP.install_target_on),
         ("KEYWORDS_ON          ", PSP.keywords_on),
         ("LDFLAGS_ON           ", PSP.ldflags_on),
         ("LIB_DEPENDS_ON       ", PSP.lib_depends_on),
         ("MAKE_ARGS_ON         ", PSP.make_args_on),
         ("MAKE_ENV_ON          ", PSP.make_env_on),
         ("PATCHFILES_ON        ", PSP.patchfiles_on),
         ("PLIST_SUB_ON         ", PSP.plist_sub_on),
         ("PREVENTS_ON          ", PSP.prevents_on),
         ("QMAKE_OFF            ", PSP.qmake_off),
         ("QMAKE_ON             ", PSP.qmake_on),
         ("RUN_DEPENDS_ON       ", PSP.run_depends_on),
         ("SUB_FILES_ON         ", PSP.sub_files_on),
         ("SUB_LIST_ON          ", PSP.sub_list_on),
         ("TEST_TARGET_ON       ", PSP.test_target_on),
         ("USES_ON              ", PSP.uses_on)
        );

      end_opt_name : Natural;
      end_varname  : Natural;
      testword_len : Natural;
      bandolier    : helper_string := (others => LAT.Space);
      Low          : Natural := all_helpers'First;
      High         : Natural := all_helpers'Last;
      Mid          : Natural;

   begin
      if line (line'First) /= LAT.Left_Square_Bracket then
         return PSP.not_helper_format;
      end if;

      end_opt_name := AS.Fixed.Index (Source => line, Pattern => "].");
      if end_opt_name = 0 then
         return PSP.not_helper_format;
      end if;
      end_varname := AS.Fixed.Index (Source => line, Pattern => "=");
      if end_varname = 0 or else end_varname < end_opt_name then
         return PSP.not_helper_format;
      end if;

      testword_len := end_varname - end_opt_name - 2;
      if testword_len < 6 or else testword_len > helper_string'Length then
         return PSP.not_supported_helper;
      end if;

      bandolier (1 .. testword_len) := line (end_opt_name + 2 .. end_varname - 1);

      loop
         Mid := (Low + High) / 2;
         if bandolier = all_helpers (Mid).varname then
            return all_helpers (Mid).singlet;
         elsif bandolier < all_helpers (Mid).varname then
            exit when Low = Mid;
            High := Mid - 1;
         else
            exit when High = Mid;
            Low := Mid + 1;
         end if;
      end loop;
      return PSP.not_supported_helper;

   end determine_option;


   --------------------------------------------------------------------------------------------
   --  retrieve_single_value
   --------------------------------------------------------------------------------------------
   function retrieve_single_value (line : String) return String
   is
      wrkstr : String (1 .. line'Length) := line;
      equals : Natural := AS.Fixed.Index (wrkstr, LAT.Equals_Sign & LAT.HT);
      c81624 : Natural := ((equals / 8) + 1) * 8;
      --  f(4)  =  8    ( 2 ..  7)
      --  f(8)  = 16;   ( 8 .. 15)
      --  f(18) = 24;   (16 .. 23)
      --  We are looking for an exact number of tabs starting at equals + 2:
      --  if c81624 = 8, then we need 2 tabs.  IF it's 16 then we need 1 tab,
      --  if it's 24 then there can be no tabs, and if it's higher, that's a problem.
   begin
      if equals = 0 then
         --  Support triple-tab line too.
         if wrkstr'Length > 3 and then
           wrkstr (wrkstr'First .. wrkstr'First + 2) = LAT.HT & LAT.HT & LAT.HT
         then
            equals := wrkstr'First + 1;
            c81624 := 24;
         else
            raise missing_definition with "No triple-tab or equals+tab detected.";
         end if;
      end if;
      if c81624 > 24 then
         raise mistabbed;
      end if;
      declare
         rest : constant String := wrkstr (equals + 2 .. wrkstr'Last);
         contig_tabs : Natural := 0;
         arrow : Natural := rest'First;
      begin
         loop
            exit when arrow > rest'Last;
            exit when rest (arrow) /= LAT.HT;
            contig_tabs := contig_tabs + 1;
            arrow := arrow + 1;
         end loop;
         if ((c81624 = 8) and then (contig_tabs /= 2)) or else
           ((c81624 = 16) and then (contig_tabs /= 1)) or else
           ((c81624 = 24) and then (contig_tabs /= 0))
         then
            raise mistabbed;
         end if;
         return expand_value (rest (rest'First + contig_tabs .. rest'Last));
      end;
   end retrieve_single_value;


   --------------------------------------------------------------------------------------------
   --  retrieve_single_integer
   --------------------------------------------------------------------------------------------
   function retrieve_single_integer (line : String) return Natural
   is
      result   : Natural;
      strvalue : constant String := retrieve_single_value (line);
      --  let any exceptions cascade
   begin
      result := Integer'Value (strvalue);
      return result;
   exception
      when Constraint_Error =>
         raise integer_expected;
   end retrieve_single_integer;


   --------------------------------------------------------------------------------------------
   --  retrieve_key
   --------------------------------------------------------------------------------------------
   function retrieve_key (line : String; previous_index : HT.Text) return HT.Text
   is
      LB : Natural := AS.Fixed.Index (line, "[");
      RB : Natural := AS.Fixed.Index (line, "]");
   begin
      if line'Length > 3 and then
        line (line'First .. line'First + 2) = LAT.HT & LAT.HT & LAT.HT
      then
         return previous_index;
      end if;
      if LB = 0 or else
        RB = 0 or else
        RB <= LB + 1
      then
         return HT.SUS ("BOGUS");  --  should be impossible
      end if;
      return HT.SUS (line (LB + 1 .. RB - 1));
   end retrieve_key;


   --------------------------------------------------------------------------------------------
   --  set_boolean
   --------------------------------------------------------------------------------------------
   procedure set_boolean (spec : in out PSP.Portspecs; field : PSP.spec_field; line : String)
   is
      value : String := retrieve_single_value (line);
   begin
      if value = boolean_yes then
         spec.set_boolean (field, True);
      else
         raise PSP.wrong_value with "boolean variables may only be set to '" & boolean_yes
           & "' (was given '" & value & "')";
      end if;
   end set_boolean;


   --------------------------------------------------------------------------------------------
   --  set_natural
   --------------------------------------------------------------------------------------------
   procedure set_natural (spec : in out PSP.Portspecs; field : PSP.spec_field; line : String) is
   begin
       spec.set_natural_integer (field, retrieve_single_integer (line));
   end set_natural;


   --------------------------------------------------------------------------------------------
   --  build_string
   --------------------------------------------------------------------------------------------
   procedure build_string (spec : in out PSP.Portspecs; field : PSP.spec_field; line : String) is
   begin
      spec.set_single_string (field, retrieve_single_value (line));
   end build_string;


   --------------------------------------------------------------------------------------------
   --  build_list
   --------------------------------------------------------------------------------------------
   procedure build_list (spec : in out PSP.Portspecs; field : PSP.spec_field; line : String)
   is
      procedure insert_item (data : String);

      arrow      : Natural;
      word_start : Natural;
      strvalue   : constant String := retrieve_single_value (line);
      mask       : String := strvalue;
      Qopened    : Boolean := False;
      --  let any exceptions cascade

      procedure insert_item (data : String) is
      begin
         case field is
            when PSP.sp_dl_groups =>
               spec.establish_group (field, data);
            when PSP.sp_variants =>
               spec.append_list (field, data);
               spec.establish_group (PSP.sp_subpackages, data);
               if data /= variant_standard then
                  spec.establish_group (PSP.sp_vopts, data);
               end if;
            when others =>
               spec.append_list (field, data);
         end case;
      end insert_item;
   begin
      --  Handle single item case
      if not HT.contains (S => strvalue, fragment => " ") then
         insert_item (strvalue);
         return;
      end if;

      --  Check for multiple space error or leading space error
      --  We start by masking all spaces between quotations so we can accurately detect them
      for x in mask'Range loop
         if mask (x) = LAT.Quotation then
            Qopened := not Qopened;
         elsif mask (x) = LAT.Space then
            if Qopened then
               mask (x) := 'X';
            end if;
         end if;
      end loop;
      if HT.contains (S => mask, fragment => "  ") or else
        mask (mask'First) = LAT.Space
      then
         raise extra_spaces;
      end if;

      --  Now we have multiple list items separated by single spaces
      --  We know the original line has no trailing spaces too, btw.
      word_start := strvalue'First;
      arrow := word_start;
      loop
         exit when arrow > strvalue'Last;
         if mask (arrow) = LAT.Space then
            insert_item (strvalue (word_start .. arrow - 1));
            word_start := arrow + 1;
         end if;
         arrow := arrow + 1;
      end loop;
      insert_item (strvalue (word_start .. strvalue'Last));

   end build_list;


   --------------------------------------------------------------------------------------------
   --  build_group_list
   --------------------------------------------------------------------------------------------
   procedure build_group_list
     (spec  : in out PSP.Portspecs;
      field : PSP.spec_field;
      key   : String;
      value : String)
   is
      procedure insert_item (data : String);

      arrow      : Natural;
      word_start : Natural;
      mask       : String := value;
      Qopened    : Boolean := False;

      procedure insert_item (data : String) is
      begin
         spec.append_array (field        => field,
                                     key          => key,
                                     value        => data,
                                     allow_spaces => False);
      end insert_item;
   begin
      --  Handle single item case
      if not HT.contains (S => value, fragment => " ") then
         insert_item (value);
         return;
      end if;

      --  Check for multiple space error or leading space error
      --  We start by masking all spaces between quotations so we can accurately detect them
      for x in mask'Range loop
         if mask (x) = LAT.Quotation then
            Qopened := not Qopened;
         elsif mask (x) = LAT.Space then
            if Qopened then
               mask (x) := 'X';
            end if;
         end if;
      end loop;
      if HT.contains (S => mask, fragment => "  ") or else
        mask (mask'First) = LAT.Space
      then
         raise extra_spaces;
      end if;

      --  Now we have multiple list items separated by single spaces
      --  We know the original line has no trailing spaces too, btw.
      word_start := value'First;
      arrow := word_start;
      loop
         exit when arrow > value'Last;
         if mask (arrow) = LAT.Space then
            insert_item (value (word_start .. arrow - 1));
            word_start := arrow + 1;
         end if;
         arrow := arrow + 1;
      end loop;
      insert_item (value (word_start .. value'Last));

   end build_group_list;


   --------------------------------------------------------------------------------------------
   --  passed_late_validity_checks
   --------------------------------------------------------------------------------------------
   function late_validity_check_error (spec : PSP.Portspecs) return HT.Text
   is
      variant_check_result : String := spec.check_variants;
   begin
      if variant_check_result /= "" then
         return HT.SUS ("Variant '" & HT.part_1 (variant_check_result, ":") &
                          "' is missing the required '" & HT.part_2 (variant_check_result, ":") &
                          "' option configuration.");
      end if;
      if not spec.deprecation_valid then
         return HT.SUS ("DEPRECATED and EXPIRATION must both be set");
      end if;
      return HT.blank;
   end late_validity_check_error;


   --------------------------------------------------------------------------------------------
   --  determine_target
   --------------------------------------------------------------------------------------------
   function determine_target
      (spec      : PSP.Portspecs;
       line      : String;
       last_seen : type_category) return spec_target
   is
      function active_prefix return String;
      function extract_option (prefix, line : String) return String;

      lead_pre  : Boolean := False;
      lead_do   : Boolean := False;
      lead_post : Boolean := False;
      fetch     : constant String := "fetch";
      extract   : constant String := "extract";
      patch     : constant String := "patch";
      configure : constant String := "configure";
      build     : constant String := "build";
      install   : constant String := "install";
      opt_on    : constant String := "-ON:";
      opt_off   : constant String := "-OFF:";
      pre_pre   : constant String := "pre-";
      pre_do    : constant String := "do-";
      pre_post  : constant String := "post-";

      function active_prefix return String is
      begin
         if lead_pre then
            return pre_pre;
         elsif lead_do then
            return pre_do;
         else
            return pre_post;
         end if;
      end active_prefix;

      function extract_option (prefix, line : String) return String
      is
         function first_set_successful (substring : String) return Boolean;
         --  Given: Line terminates in "-ON:" or "-OFF:"
         last  : Natural;
         first : Natural := 0;

         function first_set_successful (substring : String) return Boolean is
         begin
            if HT.leads (line, substring) then
               first := line'First + substring'Length;
               return True;
            else
               return False;
            end if;
         end first_set_successful;
      begin
         if HT.trails (line, opt_on) then
            last := line'Last - opt_on'Length;
         else
            last := line'Last - opt_off'Length;
         end if;
         if first_set_successful (prefix & fetch & LAT.Hyphen) or else
           first_set_successful (prefix & extract & LAT.Hyphen) or else
           first_set_successful (prefix & patch & LAT.Hyphen) or else
           first_set_successful (prefix & configure & LAT.Hyphen) or else
           first_set_successful (prefix & build & LAT.Hyphen) or else
           first_set_successful (prefix & install & LAT.Hyphen)
         then
            return line (first .. last);
         else
            return "";
         end if;
      end extract_option;
   begin
      if last_seen = cat_target then
         --  If the line starts with a period or if it has a single tab, then mark it as
         --  as a target body and leave.  We don't need to check more.
         if line (line'First) = LAT.Full_Stop or else
           line (line'First) = LAT.HT
         then
            return target_body;
         end if;
      end if;

      --  Check if line has format of a target (ends in a colon)
      if not HT.trails (line, ":") then
         return not_target;
      end if;

      --  From this point forward, we're either a target_title or bad_target

      lead_pre := HT.leads (line, pre_pre);
      if not lead_pre then
         lead_do := HT.leads (line, pre_do);
         if not lead_do then
            lead_post := HT.leads (line, pre_post);
            if not lead_post then
               return bad_target;
            end if;
         end if;
      end if;

      declare
         prefix : constant String := active_prefix;
      begin

         --  Handle pre-, do-, post- target overrides
         if line = prefix & fetch & LAT.Colon or else
           line = prefix & fetch & LAT.Colon or else
           line = prefix & extract & LAT.Colon or else
           line = prefix & patch & LAT.Colon or else
           line = prefix & configure & LAT.Colon or else
           line = prefix & build & LAT.Colon or else
           line = prefix & install & LAT.Colon
         then
            return target_title;
         end if;

         --  Opsys also applies to pre-, do-, and post-
         for opsys in supported_opsys'Range loop
            declare
               lowsys : String := '-' & UTL.lower_opsys (opsys) & LAT.Colon;
            begin
               if line = prefix & fetch & lowsys or else
                 line = prefix & extract & lowsys or else
                 line = prefix & patch & lowsys or else
                 line = prefix & configure & lowsys or else
                 line = prefix & build & lowsys or else
                 line = prefix & install & lowsys
               then
                  return target_title;
               end if;
            end;
         end loop;

         --  The only targets left to check are options which end in "-ON:" and "-OFF:".
         --  If these suffices aren't found, it's a bad target.
         if not HT.trails (line, opt_on) and then
           not HT.trails (line, opt_off)
         then
            return bad_target;
         end if;

         declare
            option_name : String := extract_option (prefix, line);
         begin
            if spec.option_exists (option_name) then
               return target_title;
            else
               return bad_target;
            end if;
         end;
      end;
   end determine_target;


   --------------------------------------------------------------------------------------------
   --  extract_option_name
   --------------------------------------------------------------------------------------------
   function extract_option_name
     (spec      : PSP.Portspecs;
      line      : String;
      last_name : HT.Text) return String
   is
      --  Already known: first character = "]" and there's "]." present
      candidate : String := HT.partial_search (fullstr    => line,
                                               offset     => 1,
                                               end_marker => "].");
      tabs5 : String (1 .. 5) := (others => LAT.HT);
   begin
      if candidate = "" and then
        line'Length > 5 and then
        line (line'First .. line'First + 4) = tabs5
      then
         return HT.USS (last_name);
      end if;

      if spec.option_exists (candidate) then
         return candidate;
      else
         return "";
      end if;
   end extract_option_name;


   --------------------------------------------------------------------------------------------
   --  build_list
   --------------------------------------------------------------------------------------------
   procedure build_list
     (spec   : in out PSP.Portspecs;
      field  : PSP.spec_option;
      option : String;
      line   : String)
   is
      procedure insert_item (data : String);

      arrow      : Natural;
      word_start : Natural;
      strvalue   : constant String := retrieve_single_option_value (line);
      mask       : String := strvalue;
      Qopened    : Boolean := False;
      --  let any exceptions cascade

      procedure insert_item (data : String) is
      begin
         spec.build_option_helper (field  => field,
                                            option => option,
                                            value  => data);
      end insert_item;

      use type PSP.spec_option;
   begin
      if field = PSP.broken_on then
         spec.build_option_helper (field  => field,
                                            option => option,
                                            value  => strvalue);
         return;
      end if;

      --  Handle single item case
      if not HT.contains (S => strvalue, fragment => " ") then
         insert_item (strvalue);
         return;
      end if;

      --  Check for multiple space error or leading space error
      --  We start by masking all spaces between quotations so we can accurately detect them
      for x in mask'Range loop
         if mask (x) = LAT.Quotation then
            Qopened := not Qopened;
         elsif mask (x) = LAT.Space then
            if Qopened then
               mask (x) := 'X';
            end if;
         end if;
      end loop;
      if HT.contains (S => mask, fragment => "  ") or else
        mask (mask'First) = LAT.Space
      then
         raise extra_spaces;
      end if;

      --  Now we have multiple list items separated by single spaces
      --  We know the original line has no trailing spaces too, btw.
      word_start := strvalue'First;
      arrow := word_start;
      loop
         exit when arrow > strvalue'Last;
         if mask (arrow) = LAT.Space then
            insert_item (strvalue (word_start .. arrow - 1));
            word_start := arrow + 1;
         end if;
         arrow := arrow + 1;
      end loop;
      insert_item (strvalue (word_start .. strvalue'Last));

   end build_list;


   --------------------------------------------------------------------------------------------
   --  retrieve_single_option_value
   --------------------------------------------------------------------------------------------
   function retrieve_single_option_value (line : String) return String
   is
      wrkstr : String (1 .. line'Length) := line;
      equals : Natural := AS.Fixed.Index (wrkstr, LAT.Equals_Sign & LAT.HT);
      c81624 : Natural := ((equals / 8) + 1) * 8;
      tabs5  : String (1 .. 5) := (others => LAT.HT);
      --  f(4)  =  8    ( 2 ..  7)
      --  f(8)  = 16;   ( 8 .. 15)
      --  f(18) = 24;   (16 .. 23)
      --  We are looking for an exact number of tabs starting at equals + 2:
      --  if c81624 = 8, then we need 2 tabs.  IF it's 16 then we need 1 tab,
      --  if it's 24 then there can be no tabs, and if it's higher, that's a problem.
   begin
      if equals = 0 then
         --  Support quadruple-tab line too.
         if wrkstr'Length > 5 and then
           wrkstr (wrkstr'First .. wrkstr'First + 4) = tabs5
         then
            equals := wrkstr'First + 3;
            c81624 := 40;
         else
            raise missing_definition with "No quintuple-tab or equals+tab detected.";
         end if;
      end if;
      if c81624 > 40 then
         raise mistabbed_40;
      end if;
      declare
         rest : constant String := wrkstr (equals + 2 .. wrkstr'Last);
         contig_tabs : Natural := 0;
         arrow : Natural := rest'First;
      begin
         loop
            exit when arrow > rest'Last;
            exit when rest (arrow) /= LAT.HT;
            contig_tabs := contig_tabs + 1;
            arrow := arrow + 1;
         end loop;
         if ((c81624 = 8) and then (contig_tabs /= 4)) or else
           ((c81624 = 16) and then (contig_tabs /= 3)) or else
           ((c81624 = 24) and then (contig_tabs /= 2)) or else
           ((c81624 = 32) and then (contig_tabs /= 1)) or else
           ((c81624 = 40) and then (contig_tabs /= 0))
         then
            raise mistabbed_40;
         end if;
         return expand_value (rest (rest'First + contig_tabs .. rest'Last));
      end;
   end retrieve_single_option_value;


   --------------------------------------------------------------------------------------------
   --  is_file_capsule
   --------------------------------------------------------------------------------------------
   function is_file_capsule (line : String) return Boolean
   is
      --  format: [FILE:XXXX:filename]
      dummy : Integer;
   begin
      if line (line'Last) /= LAT.Right_Square_Bracket then
         return False;
      end if;
      if not HT.leads (line, "[FILE:") then
         return False;
      end if;
      if HT.count_char (line, LAT.Colon) /= 2 then
         return False;
      end if;
      dummy := Integer'Value (HT.partial_search (line, 6, ":"));
      return True;
   exception
      when others =>
         return False;
   end is_file_capsule;


   --------------------------------------------------------------------------------------------
   --  retrieve_file_size
   --------------------------------------------------------------------------------------------
   function retrieve_file_size (capsule_label : String) return Natural
   is
      result : Natural;
   begin
      result := Integer'Value (HT.partial_search (capsule_label, 6, ":"));
      if result > 0 then
         return result;
      else
         return 0;
      end if;
   exception
      when others =>
         return 0;
   end retrieve_file_size;


   --------------------------------------------------------------------------------------------
   --  retrieve_file_name
   --------------------------------------------------------------------------------------------
   function retrieve_file_name (capsule_label : String) return String is
   begin
      return HT.part_2 (HT.partial_search (capsule_label, 6, "]"), ":");
   end retrieve_file_name;


end Specification_Parser;
