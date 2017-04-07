--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions; use Definitions;

with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Text_IO;
with Package_Manifests;
with File_Operations;
with Utilities;

package body Port_Specification.Buildsheet is

   package TIO renames Ada.Text_IO;
   package DIR renames Ada.Directories;
   package LAT renames Ada.Characters.Latin_1;
   package MAN renames Package_Manifests;
   package FOP renames File_Operations;
   package UTL renames Utilities;

   --------------------------------------------------------------------------------------------
   --  generator
   --------------------------------------------------------------------------------------------
   procedure generator
     (specs       : Portspecs;
      ravensrcdir : String;
      output_file : String)
   is
      procedure send (data : String; use_put : Boolean := False);
      procedure send (varname : String; value, default : Integer);
      procedure send (varname, value : String);
      procedure send (varname : String; value : HT.Text);
      procedure send (varname : String; crate : string_crate.Vector; flavor : Positive);
      procedure send (varname : String; crate : def_crate.Map);
      procedure send (varname : String; crate : list_crate.Map);
      procedure send (varname : String; value : Boolean; dummy : Boolean);
      procedure send_options;
      procedure send_targets;
      procedure send_descriptions;
      procedure send_scripts;
      procedure send_manifests;
      procedure send_download_groups;
      procedure print_item     (position : string_crate.Cursor);
      procedure print_item40   (position : string_crate.Cursor);
      procedure print_straight (position : string_crate.Cursor);
      procedure print_adjacent (position : string_crate.Cursor);
      procedure dump_vardesc   (position : string_crate.Cursor);
      procedure dump_vardesc2  (position : string_crate.Cursor);
      procedure dump_manifest  (position : string_crate.Cursor);
      procedure dump_manifest2 (position : string_crate.Cursor);
      procedure dump_sdesc     (position : def_crate.Cursor);
      procedure dump_sites     (position : list_crate.Cursor);
      procedure dump_distfiles (position : string_crate.Cursor);
      procedure dump_targets   (position : list_crate.Cursor);
      procedure dump_helper (option_name : String; crate : string_crate.Vector; helper : String);
      procedure expand_option_record (position : option_crate.Cursor);
      procedure blank_line;
      procedure send_file      (filename : String);
      procedure send_plist     (filename : String);
      procedure send_directory (dirname  : String; pattern : String := "");
      procedure send_catchall;

      write_to_file   : constant Boolean := (output_file /= "");
      makefile_handle : TIO.File_Type;
      varname_prefix  : HT.Text;
      save_variant    : HT.Text;
      current_len     : Natural;
      currently_blank : Boolean := True;
      desc_prefix     : constant String := "descriptions/desc.";
      plist_prefix    : constant String := "manifests/plist.";
      distinfo        : constant String := "distinfo";
      temp_storage    : string_crate.Vector;

      procedure send (data : String;  use_put : Boolean := False) is
      begin
         if write_to_file then
            if use_put then
               TIO.Put (makefile_handle, data);
            else
               TIO.Put_Line (makefile_handle, data);
            end if;
         else
            if use_put then
               TIO.Put (data);
            else
               TIO.Put_Line (data);
            end if;
         end if;
         if data /= "" then
            currently_blank := False;
         end if;
      end send;

      procedure send (varname, value : String) is
      begin
         if value /= "" then
            send (align24 (varname & LAT.Equals_Sign) & value);
         end if;
      end send;

      procedure send (varname : String; value : HT.Text) is
      begin
         if not HT.IsBlank (value) then
            send (align24 (varname & LAT.Equals_Sign) & HT.USS (value));
         end if;
      end send;

      procedure send (varname : String; value, default : Integer) is
      begin
         if value /= default then
            send (align24 (varname & LAT.Equals_Sign) & HT.int2str (value));
         end if;
      end send;

      procedure send (varname : String; crate : string_crate.Vector; flavor : Positive) is
      begin
         if crate.Is_Empty then
            return;
         end if;
         case flavor is
            when 1 =>
               send (align24 (varname & "="), True);
               crate.Iterate (Process => print_item'Access);
            when 2 =>
               current_len := 0;
               send (align24 (varname & "="), True);
               crate.Iterate (Process => print_adjacent'Access);
               send ("");
            when 3 =>
               varname_prefix := HT.SUS (varname);
               crate.Iterate (Process => dump_distfiles'Access);
            when others =>
               null;
         end case;
      end send;

      procedure send (varname : String; crate : def_crate.Map) is
      begin
         varname_prefix := HT.SUS (varname);
         crate.Iterate (Process => dump_sdesc'Access);
      end send;

      procedure send (varname : String; crate : list_crate.Map) is
      begin
         varname_prefix := HT.SUS (varname);
         crate.Iterate (Process => dump_sites'Access);
      end send;

      procedure send (varname : String; value : Boolean; dummy : Boolean) is
      begin
         if value then
            send (align24 (varname & LAT.Equals_Sign) & "yes");
         end if;
      end send;

      procedure print_item (position : string_crate.Cursor)
      is
         index : Natural := string_crate.To_Index (position);
         item  : String  := HT.USS (string_crate.Element (position));
      begin
         if index = 1 then
            send (item);
         else
            send (LAT.HT & LAT.HT & LAT.HT & item);
         end if;
      end print_item;

      procedure print_item40 (position : string_crate.Cursor)
      is
         index : Natural := string_crate.To_Index (position);
         item  : String  := HT.USS (string_crate.Element (position));
      begin
         if index = 1 then
            send (item);
         else
            send (LAT.HT & LAT.HT & LAT.HT & LAT.HT & LAT.HT & item);
         end if;
      end print_item40;

      procedure print_adjacent (position : string_crate.Cursor)
      is
         index : Natural := string_crate.To_Index (position);
         item  : String  := HT.USS (string_crate.Element (position));
         len   : Natural := item'Length;
      begin
         if current_len + len + 1 > 76 then
            current_len := 0;
            send ("");
            send (LAT.HT & LAT.HT & LAT.HT, True);
         end if;
         if current_len > 0 then
            send (" ", True);
            current_len := current_len + 1;
         end if;
         send (item, True);
         current_len := current_len + len;
      end print_adjacent;

      procedure print_straight (position : string_crate.Cursor)
      is
         item  : String  := HT.USS (string_crate.Element (position));
      begin
         send (item);
      end print_straight;

      procedure dump_sdesc (position : def_crate.Cursor)
      is
         varname : String := HT.USS (varname_prefix)  & LAT.Left_Square_Bracket &
                   HT.USS (def_crate.Key (position)) & LAT.Right_Square_Bracket & LAT.Equals_Sign;
      begin
         send (align24 (varname) & HT.USS (def_crate.Element (position)));
      end dump_sdesc;

      procedure dump_sites (position : list_crate.Cursor)
      is
         rec     : group_list renames list_crate.Element (position);
         varname : String := HT.USS (varname_prefix)  & LAT.Left_Square_Bracket &
                   HT.USS (rec.group) & LAT.Right_Square_Bracket & LAT.Equals_Sign;
      begin
         if not rec.list.Is_Empty then
            send (align24 (varname), True);
            rec.list.Iterate (Process => print_item'Access);
         end if;
      end dump_sites;

      procedure dump_distfiles (position : string_crate.Cursor)
      is
         index : Natural := string_crate.To_Index (position);
         NDX   : String  := HT.USS (varname_prefix)  & LAT.Left_Square_Bracket &
           HT.int2str (index) & LAT.Right_Square_Bracket & LAT.Equals_Sign;
      begin
         send (align24 (NDX) & HT.USS (string_crate.Element (position)));
      end dump_distfiles;

      procedure blank_line is
      begin
         if not currently_blank then
            send ("");
         end if;
         currently_blank := True;
      end blank_line;

      procedure send_targets is
      begin
         specs.make_targets.Iterate (Process => dump_targets'Access);
      end send_targets;

      procedure dump_targets (position : list_crate.Cursor)
      is
         rec    : group_list renames list_crate.Element (position);
         target : String := HT.USS (rec.group) & LAT.Colon;
      begin
         blank_line;
         send (target);
         rec.list.Iterate (Process => print_straight'Access);
      end dump_targets;

      procedure dump_helper (option_name : String; crate : string_crate.Vector; helper : String)
      is
      begin
         if not crate.Is_Empty then
            send (align40 (LAT.Left_Square_Bracket & option_name & "]." &
                    helper & LAT.Equals_Sign), True);
            crate.Iterate (Process => print_item40'Access);
         end if;
      end dump_helper;

      procedure expand_option_record (position : option_crate.Cursor)
      is
         rec  : Option_Helper renames option_crate.Element (position);
         name : String := HT.USS (rec.option_name);
      begin
         blank_line;
         if not HT.IsBlank (rec.option_description) then
            send (align40 (LAT.Left_Square_Bracket & name & "].DESCRIPTION=") &
                    HT.USS (rec.option_description));
         end if;
         if not HT.IsBlank (rec.BROKEN_ON) then
            send (align40 (LAT.Left_Square_Bracket & name & "].BROKEN_ON=") &
                    HT.USS (rec.BROKEN_ON));
         end if;
         dump_helper (name, rec.BUILDRUN_DEPENDS_OFF, "BUILDRUN_DEPENDS_OFF");
         dump_helper (name, rec.BUILDRUN_DEPENDS_ON, "BUILDRUN_DEPENDS_ON");
         dump_helper (name, rec.BUILD_DEPENDS_OFF, "BUILD_DEPENDS_OFF");
         dump_helper (name, rec.BUILD_DEPENDS_ON, "BUILD_DEPENDS_ON");
         dump_helper (name, rec.BUILD_TARGET_ON, "BUILD_TARGET_ON");
         dump_helper (name, rec.CFLAGS_OFF, "CFLAGS_OFF");
         dump_helper (name, rec.CFLAGS_ON, "CFLAGS_ON");
         dump_helper (name, rec.CMAKE_ARGS_OFF, "CMAKE_ARGS_OFF");
         dump_helper (name, rec.CMAKE_ARGS_ON, "CMAKE_ARGS_ON");
         dump_helper (name, rec.CMAKE_BOOL_T_BOTH, "CMAKE_BOOL_T_BOTH");
         dump_helper (name, rec.CMAKE_BOOL_F_BOTH, "CMAKE_BOOL_F_BOTH");
         dump_helper (name, rec.CONFIGURE_ARGS_OFF, "CONFIGURE_ARGS_OFF");
         dump_helper (name, rec.CONFIGURE_ARGS_ON, "CONFIGURE_ARGS_ON");
         dump_helper (name, rec.CONFIGURE_ENABLE_BOTH, "CONFIGURE_ENABLE_BOTH");
         dump_helper (name, rec.CONFIGURE_ENV_ON, "CONFIGURE_ENV_ON");
         dump_helper (name, rec.CONFIGURE_WITH_BOTH, "CONFIGURE_WITH_BOTH");
         dump_helper (name, rec.CPPFLAGS_ON, "CPPFLAGS_ON");
         dump_helper (name, rec.CXXFLAGS_ON, "CXXFLAGS_ON");
         dump_helper (name, rec.DF_INDEX_ON, "DF_INDEX_ON");
         dump_helper (name, rec.EXTRA_PATCHES_ON, "EXTRA_PATCHES_ON");
         dump_helper (name, rec.EXTRACT_ONLY_ON, "EXTRACT_ONLY_ON");
         dump_helper (name, rec.IMPLIES_ON, "IMPLIES_ON");
         dump_helper (name, rec.INFO_ON, "INFO_ON");
         dump_helper (name, rec.INSTALL_TARGET_ON, "INSTALL_TARGET_ON");
         dump_helper (name, rec.KEYWORDS_ON, "KEYWORDS_ON");
         dump_helper (name, rec.LDFLAGS_ON, "LDFLAGS_ON");
         dump_helper (name, rec.MAKEFILE_OFF, "MAKEFILE_OFF");
         dump_helper (name, rec.MAKEFILE_ON, "MAKEFILE_ON");
         dump_helper (name, rec.MAKE_ARGS_ON, "MAKE_ARGS_ON");
         dump_helper (name, rec.MAKE_ENV_ON, "MAKE_ENV_ON");
         dump_helper (name, rec.PATCHFILES_ON, "PATCHFILES_ON");
         dump_helper (name, rec.PLIST_SUB_ON, "PLIST_SUB_ON");
         dump_helper (name, rec.PREVENTS_ON, "PREVENTS_ON");
         dump_helper (name, rec.QMAKE_OFF, "QMAKE_OFF");
         dump_helper (name, rec.QMAKE_ON, "QMAKE_ON");
         dump_helper (name, rec.RUN_DEPENDS_OFF, "RUN_DEPENDS_OFF");
         dump_helper (name, rec.RUN_DEPENDS_ON, "RUN_DEPENDS_ON");
         dump_helper (name, rec.SUB_FILES_ON, "SUB_FILES_ON");
         dump_helper (name, rec.SUB_LIST_ON, "SUB_LIST_ON");
         dump_helper (name, rec.TEST_TARGET_ON, "TEST_TARGET_ON");
         dump_helper (name, rec.USES_OFF, "USES_OFF");
         dump_helper (name, rec.USES_ON, "USES_ON");
      end expand_option_record;

      procedure send_options is
      begin
         specs.ops_helpers.Iterate (Process => expand_option_record'Access);
      end send_options;

      procedure send_file (filename : String)
      is
         abspath : constant String := ravensrcdir & "/" & filename;
      begin
         if DIR.Exists (abspath) then
            declare
               contents : constant String := FOP.get_file_contents (abspath);
            begin
               blank_line;
               send ("[FILE:" & HT.int2str (contents'Length) & LAT.Colon & filename &
                       LAT.Right_Square_Bracket);
               send (contents);
            end;
         end if;
      end send_file;

      procedure send_plist (filename : String)
      is
         abspath : constant String := ravensrcdir & "/" & filename;
      begin
         if DIR.Exists (abspath) then
            declare
               contents : constant String := MAN.compress_manifest (MAN.Filename (abspath));
            begin
               blank_line;
               send ("[FILE:" & HT.int2str (contents'Length) & LAT.Colon & filename &
                       LAT.Right_Square_Bracket);
               send (contents);
            end;
         end if;
      end send_plist;

      procedure dump_vardesc2  (position : string_crate.Cursor)
      is
         item : HT.Text renames string_crate.Element (position);
         subpkg : String := HT.USS (item);
      begin
         send_file (desc_prefix & subpkg & LAT.Full_Stop & HT.USS (varname_prefix));
         if DIR.Exists (ravensrcdir & "/" & desc_prefix & subpkg) and then
           not temp_storage.Contains (item)
         then
            temp_storage.Append (item);
            send_file (desc_prefix & subpkg);
         end if;
      end dump_vardesc2;

      procedure dump_vardesc (position : string_crate.Cursor) is
      begin
         varname_prefix := string_crate.Element (position);
         specs.subpackages.Element (varname_prefix).list.Iterate (dump_vardesc2'Access);
      end dump_vardesc;

      procedure send_descriptions is
      begin
         specs.variants.Iterate (Process => dump_vardesc'Access);
         temp_storage.Clear;
      end send_descriptions;

      procedure send_scripts
      is
         function get_phasestr (index : Positive) return String;
         function get_prefix   (index : Positive) return String;
         function get_phasestr (index : Positive) return String is
         begin
            case index is
               when 1 => return "fetch";
               when 2 => return "extract";
               when 3 => return "patch";
               when 4 => return "configure";
               when 5 => return "build";
               when 6 => return "install";
               when others => return "";
            end case;
         end get_phasestr;
         function get_prefix (index : Positive) return String is
         begin
            case index is
               when 1 => return "pre-";
               when 2 => return "post-";
               when others => return "";
            end case;
         end get_prefix;
      begin
         for phase in Positive range 1 .. 6 loop
            for prefix in Positive range 1 .. 2 loop
               declare
                  target : String := get_prefix (prefix) & get_phasestr (phase) & "-script";
               begin
                  send_file ("scripts/" & target);
               end;
            end loop;
         end loop;
      end send_scripts;

      procedure send_directory (dirname : String; pattern : String := "")
      is
         procedure dump_file (cursor : string_crate.Cursor);

         Search  : DIR.Search_Type;
         Dir_Ent : DIR.Directory_Entry_Type;
         bucket  : string_crate.Vector;
         abspath : constant String := ravensrcdir & "/" & dirname;
         filter  : constant DIR.Filter_Type := (DIR.Directory     => False,
                                                DIR.Ordinary_File => True,
                                                DIR.Special_File  => False);

         procedure dump_file (cursor : string_crate.Cursor)
         is
            filename : String := HT.USS (string_crate.Element (cursor));
         begin
            send_file (dirname & "/" & filename);
         end dump_file;
      begin
         if not DIR.Exists (abspath) then
            return;
         end if;

         DIR.Start_Search (Search    => Search,
                           Directory => abspath,
                           Pattern   => pattern,
                           Filter    => filter);

         while DIR.More_Entries (Search => Search) loop
            DIR.Get_Next_Entry (Search => Search, Directory_Entry => Dir_Ent);
            bucket.Append (HT.SUS (DIR.Simple_Name (Dir_Ent)));
         end loop;
         DIR.End_Search (Search);
         sorter.Sort (Container => bucket);
         bucket.Iterate (Process => dump_file'Access);
      end send_directory;

      procedure dump_manifest2  (position : string_crate.Cursor)
      is
         item : HT.Text renames string_crate.Element (position);
         subpkg : String := HT.USS (item);
         fullkey   : HT.Text;
         fullplist : String := plist_prefix & subpkg & "." & HT.USS (save_variant);
      begin
         if DIR.Exists (ravensrcdir & "/" & fullplist) then
            fullkey := HT.SUS (subpkg & "." & HT.USS (save_variant));
            if not temp_storage.Contains (fullkey) then
               temp_storage.Append (fullkey);
               send_plist (fullplist);
            end if;
         else
            if DIR.Exists (ravensrcdir & "/" & plist_prefix & subpkg) and then
              not temp_storage.Contains (item)
            then
               temp_storage.Append (item);
               send_plist (plist_prefix & subpkg);
            end if;
         end if;
      end dump_manifest2;

      procedure dump_manifest (position : string_crate.Cursor)
      is
         variant : HT.Text renames string_crate.Element (position);
      begin
         save_variant := variant;
         specs.subpackages.Element (variant).list.Iterate (dump_manifest2'Access);
      end dump_manifest;

      procedure send_manifests
      is
         --  Manifests are subpackage-based
         --  Not having a subpackage manifest is ok.
         --  Subpackaegs typically missing: docs, examples, complete (Metaport)
      begin
         specs.variants.Iterate (Process => dump_manifest'Access);
         temp_storage.Clear;
      end send_manifests;

      procedure send_download_groups
      is
         --  The first group must be either "main" or "none"
         procedure dump_group (position : list_crate.Cursor);
         procedure dump_main (position : list_crate.Cursor);

         procedure dump_main (position : list_crate.Cursor)
         is
            rec : group_list renames list_crate.Element (position);
         begin
            if HT.equivalent (rec.group, dlgroup_main) or else
              HT.equivalent (rec.group, dlgroup_none)
            then
               send (HT.USS (rec.group), True);
            end if;
         end dump_main;

         procedure dump_group (position : list_crate.Cursor)
         is
            rec : group_list renames list_crate.Element (position);
         begin
            if not HT.equivalent (rec.group, dlgroup_main) and then
              not HT.equivalent (rec.group, dlgroup_none)
            then
              send (" " & HT.USS (rec.group), True);
            end if;
         end dump_group;
      begin
         send (align24 ("DOWNLOAD_GROUPS="), True);
         specs.dl_sites.Iterate (Process => dump_main'Access);
         specs.dl_sites.Iterate (Process => dump_group'Access);
         send ("");
      end send_download_groups;

      procedure send_catchall
      is
         procedure scan    (position : def_crate.Cursor);
         procedure dump_nv (position : string_crate.Cursor);

         temp_storage : string_crate.Vector;

         procedure scan (position : def_crate.Cursor) is
         begin
            temp_storage.Append (def_crate.Key (position));
         end scan;

         procedure dump_nv (position : string_crate.Cursor)
         is
            text_key : HT.Text renames string_crate.Element (position);

            keystr : String := HT.USS (text_key) & LAT.Equals_Sign;
            valstr : String := HT.USS (specs.catch_all.Element (text_key));
         begin
            send (align24 (keystr) & valstr);
         end dump_nv;
      begin
         specs.catch_all.Iterate (scan'Access);
         sorter.Sort (temp_storage);
         temp_storage.Iterate (dump_nv'Access);
      end send_catchall;

   begin
      if write_to_file then
        TIO.Create (File => makefile_handle,
                    Mode => TIO.Out_File,
                    Name => output_file);
      end if;

      send ("# Buildsheet autogenerated by ravenadm tool -- Do not edit." & LAT.LF);

      send ("NAMEBASE",             specs.namebase);
      send ("VERSION",              specs.version);
      send ("REVISION",             specs.revision, 0);
      send ("EPOCH",                specs.epoch, 0);
      send ("KEYWORDS",             specs.keywords, 2);
      send ("VARIANTS",             specs.variants, 2);
      send ("SDESC",                specs.taglines);
      send ("HOMEPAGE",             specs.homepage);
      send ("CONTACT",              specs.contacts, 1);
      blank_line;

      send_download_groups;
      send ("SITES",                specs.dl_sites);
      send ("DISTFILE",             specs.distfiles, 3);
      send ("DIST_SUBDIR",          specs.dist_subdir);
      send ("DF_INDEX",             specs.df_index, 2);
      send ("SPKGS",                specs.subpackages);

      blank_line;
      send ("OPTIONS_AVAILABLE",    specs.ops_avail, 2);
      send ("OPTIONS_STANDARD",     specs.ops_standard, 2);
      send ("VOPTS",                specs.variantopts);
      send ("OPT_ON",               specs.options_on);
      blank_line;
      send ("BROKEN",               specs.broken);
      send ("NOT_FOR_OPSYS",        specs.exc_opsys, 2);
      send ("ONLY_FOR_OPSYS",       specs.inc_opsys, 2);
      send ("NOT_FOR_ARCH",         specs.exc_arch, 2);
      send ("DEPRECATED",           specs.deprecated);
      send ("EXPIRATION_DATE",      specs.expire_date);
      blank_line;
      send ("BUILD_DEPENDS",        specs.build_deps, 1);
      send ("BUILDRUN_DEPENDS",     specs.buildrun_deps, 1);
      send ("RUN_DEPENDS",          specs.run_deps, 1);
      send ("EXRUN",                specs.extra_rundeps);
      send ("USES",                 specs.uses, 2);
      blank_line;
      send ("DISTNAME",             specs.distname);
      send ("EXTRACT_DIRTY",        specs.extract_dirty, 2);
      send ("EXTRACT_ONLY",         specs.extract_only, 2);
      send ("EXTRACT_WITH_UNZIP",   specs.extract_zip, 2);
      send ("EXTRACT_WITH_7Z",      specs.extract_7z, 2);
      send ("EXTRACT_WITH_LHA",     specs.extract_lha, 2);
      send ("EXTRACT_HEAD",         specs.extract_head);
      send ("EXTRACT_TAIL",         specs.extract_tail);
      blank_line;
      send ("LICENSE",              specs.licenses, 2);
      send ("LICENSE_NAME",         specs.lic_names, 1);
      send ("LICENSE_SCHEME",       specs.lic_scheme);
      send ("LICENSE_FILE",         specs.lic_files, 1);
      send ("PREFIX",               specs.prefix);
      send ("INFO",                 specs.info, 1);
      send_catchall;
      send ("SKIP_CCACHE",          specs.skip_ccache, True);
      blank_line;
      send ("PATCH_WRKSRC",         specs.patch_wrksrc);
      send ("PATCHFILES",           specs.patchfiles, 1);
      send ("EXTRA_PATCHES",        specs.extra_patches, 1);
      send ("PATCH_STRIP",          specs.patch_strip, 2);
      send ("PATCHFILES_STRIP",     specs.patchfiles, 2);
      blank_line;
      send ("MUST_CONFIGURE",       specs.config_must);
      send ("GNU_CONFIGURE_PREFIX", specs.config_prefix);
      send ("CONFIGURE_OUTSOURCE",  specs.config_outsrc, True);
      send ("CONFIGURE_WRKSRC",     specs.config_wrksrc);
      send ("CONFIGURE_SCRIPT",     specs.config_script);
      send ("CONFIGURE_TARGET",     specs.config_target);
      send ("CONFIGURE_ARGS",       specs.config_args, 1);
      send ("CONFIGURE_ENV",        specs.config_env, 1);
      send ("APPLY_F10_FIX",        specs.apply_f10_fix, True);
      blank_line;
      send ("SKIP_BUILD",           specs.skip_build, True);
      send ("BUILD_WRKSRC",         specs.build_wrksrc);
      send ("BUILD_TARGET",         specs.build_target, 2);
      send ("MAKEFILE",             specs.makefile);
      send ("MAKE_ARGS",            specs.make_args, 1);
      send ("MAKE_ENV",             specs.make_env, 1);
      send ("DESTDIRNAME",          specs.destdirname);
      send ("DESTDIR_VIA_ENV",      specs.destdir_env, True);
      send ("SINGLE_JOB",           specs.single_job, True);
      blank_line;
      send ("SKIP_INSTALL",         specs.skip_install, True);
      send ("INSTALL_WRKSRC",       specs.install_wrksrc);
      send ("INSTALL_TARGET",       specs.install_tgt, 2);
      send ("INSTALL_REQ_TOOLCHAIN", specs.shift_install, True);
      send ("MANDIRS",              specs.mandirs, 1);
      send ("PLIST_SUB",            specs.plist_sub, 1);
      send ("SUB_FILES",            specs.sub_files, 1);
      send ("SUB_LIST",             specs.sub_list, 1);
      blank_line;
      send ("CFLAGS",               specs.cflags, 1);
      send ("CXXFLAGS",             specs.cxxflags, 1);
      send ("CPPFLAGS",             specs.cppflags, 1);
      send ("LDFLAGS",              specs.ldflags, 1);
      send ("OPTIMIZER_LEVEL",      specs.optimizer_lvl, 2);
      send ("TEST_TARGET",          specs.test_tgt, 2);
      send ("VAR_OPSYS",            specs.var_opsys);
      send ("VAR_ARCH",             specs.var_arch);

      --  TODO
      --  MANPREFIX[x] (needs imp, doc)
      --  RC_SUBR (array spkg), (needs imp, doc)
      --  WITH_DEBUG (?)
      --  DEBUG_FLAGS (?)

      send_options;
      send_targets;
      send_descriptions;
      send_file (distinfo);
      send_manifests;
      send_scripts;
      send_directory ("patches", "patch-*");
      send_directory ("files", "");
      for opsys in supported_opsys'Range loop
         send_directory (UTL.lower_opsys (opsys), "");
      end loop;

      if write_to_file then
         TIO.Close (makefile_handle);
      end if;
   exception
      when others =>
         if TIO.Is_Open (makefile_handle) then
            TIO.Close (makefile_handle);
         end if;

   end generator;


   --------------------------------------------------------------------------------------------
   --  align24
   --------------------------------------------------------------------------------------------
   function align24 (payload : String) return String
   is
      len : Natural := payload'Length;
   begin
      if len < 8 then
         return payload & LAT.HT & LAT.HT & LAT.HT;
      elsif len < 16 then
         return payload & LAT.HT & LAT.HT;
      elsif len < 24 then
         return payload & LAT.HT;
      else
         return payload;
      end if;
   end align24;


   --------------------------------------------------------------------------------------------
   --  align40
   --------------------------------------------------------------------------------------------
   function align40 (payload : String) return String
   is
      len : Natural := payload'Length;
   begin
      if len < 8 then
         return payload & LAT.HT & LAT.HT & LAT.HT & LAT.HT & LAT.HT;
      elsif len < 16 then
         return payload & LAT.HT & LAT.HT & LAT.HT & LAT.HT;
      elsif len < 24 then
         return payload & LAT.HT & LAT.HT & LAT.HT;
      elsif len < 32 then
         return payload & LAT.HT & LAT.HT;
      elsif len < 40 then
         return payload & LAT.HT;
      else
         return payload;
      end if;
   end align40;


   --------------------------------------------------------------------------------------------
   --  print_specification_template
   --------------------------------------------------------------------------------------------
   procedure print_specification_template (dump_to_file : Boolean)
   is
      tab : constant Character := LAT.HT;
      CR  : constant Character := LAT.LF;

      part1 : constant String :=
        "# DEF[PORTVERSION]=" & tab & "1.00" & CR &
        "# ----------------------------------------------------------------------------";

      part2 : constant String := CR &
        "NAMEBASE=" & tab & tab & "..." & CR &
        "VERSION=" & tab & tab & "${PORTVERSION}" & CR &
        "KEYWORDS=" & tab & tab & "..." & CR &
        "VARIANTS=" & tab & tab & "standard" & CR &
        "SDESC[standard]=" & tab & "..." & CR &
        "HOMEPAGE=" & tab & tab & "none" & CR &
        "CONTACT=" & tab & tab & "Jay_Leno[jay@aarp.org]" & CR & CR &
        "DOWNLOAD_GROUPS=" & tab & "main" & CR &
        "SITES[main]=" & tab & tab & "http://www.example.com/" & CR &
        "DISTFILE[1]=" & tab & tab & "something.tar.gz:main" & CR & CR &
        "SPKGS[standard]=" & tab & "single" & CR & CR &
        "OPTIONS_AVAILABLE=" & tab & "none" & CR &
        "OPTIONS_STANDARD=" & tab & "none";

      template : TIO.File_Type;

   begin
      if dump_to_file then
         TIO.Create (File => template,
                     Mode => TIO.Out_File,
                     Name => "specification");
         TIO.Put_Line (template, part1);
         TIO.Put_Line (template, part2);
         TIO.Close (template);
      else
         TIO.Put_Line (part1);
         TIO.Put_Line (part2);
      end if;
   exception
      when others =>
         if TIO.Is_Open (template) then
            TIO.Close (template);
         end if;
   end print_specification_template;

end Port_Specification.Buildsheet;
