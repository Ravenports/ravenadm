--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Utilities;
with Ada.Text_IO;
with Ada.Characters.Latin_1;

package body Port_Specification.Makefile is

   package UTL renames Utilities;
   package TIO renames Ada.Text_IO;
   package LAT renames Ada.Characters.Latin_1;

   --------------------------------------------------------------------------------------------
   --  generator
   --------------------------------------------------------------------------------------------
   procedure generator
     (specs         : Portspecs;
      variant       : String;
      opsys         : supported_opsys;
      arch          : supported_arch;
      output_file   : String
     )
   is
      procedure send (data : String; use_put : Boolean := False);
      procedure send (varname, value : String);
      procedure send (varname : String; value : HT.Text);
      procedure send (varname : String; crate : string_crate.Vector; flavor : Positive);
      procedure send (varname : String; crate : list_crate.Map; flavor : Positive);
      procedure send (varname : String; value : Boolean; dummy : Boolean);
      procedure send (varname : String; value, default : Integer);
      procedure print_item (position : string_crate.Cursor);
      procedure print_module (position : string_crate.Cursor);
      procedure dump_list (position : list_crate.Cursor);
      procedure dump_variant_index (position : list_crate.Cursor);
      procedure dump_distfiles (position : string_crate.Cursor);
      procedure dump_makesum   (position : string_crate.Cursor);
      procedure dump_ext_zip   (position : string_crate.Cursor);
      procedure dump_ext_7z    (position : string_crate.Cursor);
      procedure dump_ext_lha   (position : string_crate.Cursor);
      procedure dump_line      (position : string_crate.Cursor);
      procedure dump_extract_head_tail (position : list_crate.Cursor);
      procedure dump_dirty_extract (position : string_crate.Cursor);
      procedure dump_standard_target (target : String);
      procedure dump_opsys_target    (target : String);
      procedure dump_option_target   (target : String);
      procedure dump_broken;
      procedure dump_catchall;
      procedure dump_has_configure   (value  : HT.Text);
      procedure dump_distname;
      procedure dump_license;
      procedure dump_info;
      procedure dump_conditional_vars;

      write_to_file   : constant Boolean := (output_file /= "");
      makefile_handle : TIO.File_Type;
      varname_prefix  : HT.Text;

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
      end send;

      procedure send (varname, value : String) is
      begin
         send (varname & LAT.Equals_Sign & value);
      end send;

      procedure send (varname : String; value : HT.Text) is
      begin
         if not HT.IsBlank (value) then
            send (varname & LAT.Equals_Sign & HT.USS (value));
         end if;
      end send;

      procedure send (varname : String; value : Boolean; dummy : Boolean) is
      begin
         if value then
            send (varname & LAT.Equals_Sign & "yes");
         end if;
      end send;

      procedure send (varname : String; value, default : Integer) is
      begin
         if value /= default then
            send (varname & LAT.Equals_Sign & HT.int2str (value));
         end if;
      end send;

      procedure send (varname : String; crate : string_crate.Vector; flavor : Positive) is
      begin
         if crate.Is_Empty then
            return;
         end if;
         case flavor is
            when 1 =>
               send (varname & "=", True);
               crate.Iterate (Process => print_item'Access);
               send ("");
            when 2 =>
               varname_prefix := HT.SUS (varname);
               crate.Iterate (Process => dump_distfiles'Access);
            when 3 =>
               crate.Iterate (Process => dump_ext_zip'Access);
            when 4 =>
               crate.Iterate (Process => dump_ext_7z'Access);
            when 5 =>
               crate.Iterate (Process => dump_ext_lha'Access);
            when 7 =>
               crate.Iterate (Process => dump_dirty_extract'Access);
            when 9 =>
               send (varname & "=", True);
               crate.Iterate (Process => dump_makesum'Access);
               send ("");
            when 10 =>
               send (varname & "=", True);
               crate.Iterate (Process => print_module'Access);
               send ("");
            when others =>
               raise dev_error;
         end case;
      end send;

      procedure send (varname : String; crate : list_crate.Map; flavor : Positive) is
      begin
         varname_prefix := HT.SUS (varname);
         case flavor is
            when 1 => crate.Iterate (Process => dump_list'Access);
            when 6 => crate.Iterate (Process => dump_extract_head_tail'Access);
            when 8 => crate.Iterate (Process => dump_variant_index'Access);
            when others =>
               raise dev_error;
         end case;
      end send;

      procedure dump_list (position : list_crate.Cursor)
      is
         NDX : String := HT.USS (varname_prefix)  & "_" &
                         HT.USS (list_crate.Element (position).group) & LAT.Equals_Sign;
      begin
         send (NDX, True);
         list_crate.Element (position).list.Iterate (Process => print_item'Access);
         send ("");
      end dump_list;

      procedure dump_variant_index (position : list_crate.Cursor)
      is
         index : String := HT.USS (list_crate.Element (position).group);
      begin
         if index = variant then
            send (HT.USS (varname_prefix) & LAT.Equals_Sign, True);
            list_crate.Element (position).list.Iterate (Process => print_item'Access);
            send ("");
         end if;
      end dump_variant_index;

      procedure dump_extract_head_tail (position : list_crate.Cursor)
      is
         NDX : String := HT.USS (varname_prefix)  & "_" &
                         HT.USS (list_crate.Element (position).group) & LAT.Equals_Sign;
      begin
         if not list_crate.Element (position).list.Is_Empty then
            send (NDX, True);
            list_crate.Element (position).list.Iterate (Process => print_item'Access);
            send ("");
         end if;
      end dump_extract_head_tail;

      procedure print_item (position : string_crate.Cursor)
      is
         index : Natural := string_crate.To_Index (position);
      begin
         if index > 1 then
            send (" ", True);
         end if;
         send (HT.USS (string_crate.Element (position)), True);
      end print_item;

      procedure print_module (position : string_crate.Cursor)
      is
         --  Some modules are implemented entirely in ravenadm, so don't output them
         --  to the makefile which add unnecessary includes commands.
         module : String := HT.USS (string_crate.Element (position));
      begin
         if module = "cpe" or else
           module = "pkgconfig" or else
           module = "gettext-runtime" or else
           module = "gettext-tools"
         then
            null;
         else
            send (module & " ", True);
         end if;
      end print_module;

      procedure dump_line (position : string_crate.Cursor) is
      begin
         send (HT.USS (string_crate.Element (position)));
      end dump_line;

      procedure dump_distfiles (position : string_crate.Cursor)
      is
         index : String := HT.int2str (string_crate.To_Index (position));
         pload : String := HT.USS (string_crate.Element (position));
         NDX   : String := HT.USS (varname_prefix)  & "_" & index & LAT.Equals_Sign;
      begin
         if HT.leads (pload, "generated:") then
            declare
               group  : String := HT.part_2 (pload, ":");
               dlsite : String :=
                 HT.USS (specs.dl_sites.Element (HT.SUS (group)).list.First_Element);
            begin
               if HT.leads (dlsite, "GITHUB/") or else
                 HT.leads (dlsite, "GH/")
               then
                  send (NDX & generate_github_distfile (dlsite) & ":" & group);
                  return;
               else
                  --  seems like a mistake, fall through
                  null;
               end if;
            end;
         end if;
         send (NDX & pload);
      end dump_distfiles;

      procedure dump_distname is
      begin
         if not HT.IsBlank (specs.distname) then
            send ("DISTNAME", specs.distname);
         else
            if HT.equivalent (list_crate.Element (specs.dl_sites.First).group, dlgroup_none) then
               return;
            end if;
            declare
               first_dlsite : constant String :=
                 HT.USS (specs.dl_sites.Element (HT.SUS (dlgroup_main)).list.First_Element);
            begin
               if HT.leads (first_dlsite, "GITHUB/") or else
                 HT.leads (first_dlsite, "GH/")
               then
                  send ("DISTNAME", generate_github_distname (first_dlsite));
               end if;
            end;
         end if;
      end dump_distname;


      procedure dump_makesum (position : string_crate.Cursor)
      is
         index : Natural := string_crate.To_Index (position);
      begin
         send (HT.int2str (index) & " ", True);
      end dump_makesum;

      procedure dump_ext_zip (position : string_crate.Cursor)
      is
         N : String := HT.USS (string_crate.Element (position));
      begin
         send ("EXTRACT_HEAD_" & N & "=/usr/bin/unzip -qo");
         send ("EXTRACT_TAIL_" & N & "=-d ${EXTRACT_WRKDIR_" & N & "}");
      end dump_ext_zip;

      procedure dump_ext_7z (position : string_crate.Cursor)
      is
         N : String := HT.USS (string_crate.Element (position));
      begin
         send ("EXTRACT_HEAD_" & N & "=7z x -bd -y -o${EXTRACT_WRKDIR_" & N & "} >/dev/null");
         send ("EXTRACT_TAIL_" & N & "=# empty");
      end dump_ext_7z;

      procedure dump_ext_lha (position : string_crate.Cursor)
      is
         N : String := HT.USS (string_crate.Element (position));
      begin
         send ("EXTRACT_HEAD_" & N & "=lha xfpw=${EXTRACT_WRKDIR_" & N & "}");
         send ("EXTRACT_TAIL_" & N & "=# empty");
      end dump_ext_lha;

      procedure dump_dirty_extract (position : string_crate.Cursor)
      is
         N : String := HT.USS (string_crate.Element (position));
      begin
         send ("DIRTY_EXTRACT_" & N & "=yes");
      end dump_dirty_extract;

      procedure dump_standard_target (target : String)
      is
         target_text : HT.Text := HT.SUS (target);
      begin
         if specs.make_targets.Contains (target_text) then
            send ("");
            send (target & LAT.Colon);
            specs.make_targets.Element (target_text).list.Iterate (Process => dump_line'Access);
         end if;
      end dump_standard_target;

      procedure dump_opsys_target (target : String)
      is
         os_target  : HT.Text := HT.SUS (target & LAT.Hyphen & UTL.lower_opsys (opsys));
         std_target : String := target & "-opsys";
      begin
         if specs.make_targets.Contains (os_target) then
            send ("");
            send (std_target & LAT.Colon);
            send (LAT.HT & "# " & UTL.mixed_opsys (opsys) & "-specific");
            specs.make_targets.Element (os_target).list.Iterate (Process => dump_line'Access);
         end if;
      end dump_opsys_target;

      procedure dump_option_target (target : String)
      is
         procedure precheck (position : string_crate.Cursor);
         procedure check (position : string_crate.Cursor);

         --  Logic: considered "used" if option is OFF and <target>-<OPTION NAME>-OFF is set.
         --         vice versa with "ON"
         --  Iterate for each option.
         --  If precheck indicates use, iterate again with check to write out the target.

         target_used : Boolean := False;
         std_target  : String := target & "-option";

         procedure precheck (position : string_crate.Cursor)
         is
            base   : String  := HT.USS (string_crate.Element (position));
            WON    : HT.Text := HT.SUS (target & "-" & base & "-ON");
            WOFF   : HT.Text := HT.SUS (target & "-" & base & "-OFF");
            opt_on : Boolean := specs.option_current_setting (base);
         begin
            if not target_used then
               if opt_on then
                  if specs.make_targets.Contains (WON) then
                     target_used := True;
                  end if;
               else
                  if specs.make_targets.Contains (WOFF) then
                     target_used := True;
                  end if;
               end if;
            end if;
         end precheck;

         procedure check (position : string_crate.Cursor)
         is
            base   : String  := HT.USS (string_crate.Element (position));
            WON    : HT.Text := HT.SUS (target & "-" & base & "-ON");
            WOFF   : HT.Text := HT.SUS (target & "-" & base & "-OFF");
            opt_on : Boolean := specs.option_current_setting (base);
         begin
            if opt_on then
               if specs.make_targets.Contains (WON) then
                  send (LAT.HT & "# " & base & " option ON");
                  specs.make_targets.Element (WON).list.Iterate (Process => dump_line'Access);
               end if;
            else
               if specs.make_targets.Contains (WOFF) then
                  send (LAT.HT & "# " & base & " option OFF");
                  specs.make_targets.Element (WOFF).list.Iterate (Process => dump_line'Access);
               end if;
            end if;
         end check;
      begin
         specs.ops_avail.Iterate (Process => precheck'Access);
         if target_used then
            send ("");
            send (std_target & LAT.Colon);
            specs.ops_avail.Iterate (Process => check'Access);
         end if;
      end dump_option_target;

      procedure dump_broken
      is
         procedure precheck    (position : list_crate.Cursor);
         procedure check       (position : list_crate.Cursor);
         procedure send_prefix (reason_number : Natural);
         procedure send_reason (reason_number : Natural; reason : String);

         num_reasons : Natural := 0;
         curnum      : Natural := 1;
         cpu_ia64    : constant String := UTL.cpu_arch (x86_64) & "_";
         cpu_ia32    : constant String := UTL.cpu_arch (i386) & "_";
         cpu_armv8   : constant String := UTL.cpu_arch (aarch64) & "_";
         separator   : constant String := ": ";
         varname     : constant String := "IGNORE=";

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

         procedure check (position : list_crate.Cursor)
         is
            procedure check_list (position : string_crate.Cursor);

            broken_Key : String := HT.USS (list_crate.Element (position).group);

            procedure check_list (position : string_crate.Cursor)
            is
               reason     : String  := HT.USS (string_crate.Element (position));
               used       : Boolean := False;
            begin
               if broken_Key = broken_all then
                  send_prefix (curnum);
                  send_reason (curnum, reason);
                  curnum := curnum + 1;
               end if;
            end check_list;
         begin
            list_crate.Element (position).list.Iterate (Process => check_list'Access);
         end check;

         procedure send_prefix (reason_number : Natural) is
         begin
            if num_reasons > 1 then
               send ("[Reason " & HT.int2str (reason_number) & "] ", True);
            end if;
         end send_prefix;

         procedure send_reason (reason_number : Natural; reason : String) is
         begin
            if reason_number = num_reasons then
               send (reason);
            else
               send (reason & " \");
            end if;
         end send_reason;

      begin
         specs.broken.Iterate (Process => precheck'Access);
         if num_reasons > 0 then
            if num_reasons > 1 then
               send (varname & "\");
            else
               send (varname, True);
            end if;
            specs.broken.Iterate (Process => check'Access);
         end if;
      end dump_broken;

      procedure dump_has_configure (value : HT.Text)
      is
         valuestr : String := HT.USS (value);
      begin
         if valuestr = boolean_yes then
            send ("HAS_CONFIGURE=yes");
         elsif valuestr = "gnu" then
            send ("GNU_CONFIGURE=yes");
         end if;
      end dump_has_configure;

      procedure dump_catchall
      is
         procedure dump_nv (position : def_crate.Cursor);
         procedure dump_nv (position : def_crate.Cursor)
         is
            keystr : String := HT.USS (def_crate.Key (position));
            valstr : String := HT.USS (def_crate.Element (position));
         begin
            send (keystr, valstr);
         end dump_nv;
      begin
         specs.catch_all.Iterate (dump_nv'Access);
      end dump_catchall;

      procedure dump_info
      is
         procedure scan (position : string_crate.Cursor);
         procedure print (position : list_crate.Cursor);

         tempstore : list_crate.Map;

         procedure scan (position : string_crate.Cursor)
         is
            procedure update (Key : HT.Text; Element : in out group_list);

            value  : String := HT.USS (string_crate.Element (position));
            newkey : HT.Text := HT.SUS (HT.part_1 (value, ":"));
            newval : HT.Text := HT.SUS (HT.part_2 (value, ":"));

            procedure update (Key : HT.Text; Element : in out group_list) is
            begin
               Element.list.Append (newval);
            end update;
         begin
            if not tempstore.Contains (newkey) then
               declare
                  newrec : group_list;
               begin
                  newrec.group := newkey;
                  tempstore.Insert (newkey, newrec);
               end;
            end if;
            tempstore.Update_Element (Position => tempstore.Find (newkey),
                                      Process  => update'Access);
         end scan;

         procedure print (position : list_crate.Cursor)
         is
            procedure print_page (position : string_crate.Cursor);

            rec : group_list renames list_crate.Element (position);

            procedure print_page (position : string_crate.Cursor) is
            begin
               send (" " & HT.USS (string_crate.Element (position)), True);
            end print_page;
         begin
            send ("INFO_" & HT.USS (rec.group) & LAT.Equals_Sign, True);
            rec.list.Iterate (print_page'Access);
            send ("");
         end print;
      begin
         specs.info.Iterate (scan'Access);
         tempstore.Iterate (print'Access);
      end dump_info;

      procedure dump_conditional_vars
      is
         procedure print_var (position : string_crate.Cursor);

         key_opsys : HT.Text := HT.SUS (UTL.lower_opsys (opsys));
         key_arch  : HT.Text := HT.SUS (UTL.cpu_arch (arch));

         procedure print_var (position : string_crate.Cursor)
         is
            full : String := HT.USS (string_crate.Element (position));
            varname : String := HT.part_1 (full, "=");
            varval  : String := HT.part_2 (full, "=");
         begin
            send (varname & "+=" & varval);
         end print_var;
      begin
         if specs.var_opsys.Contains (key_opsys) then
            specs.var_opsys.Element (key_opsys).list.Iterate (print_var'Access);
         end if;
         if specs.var_arch.Contains (key_arch) then
            specs.var_arch.Element (key_arch).list.Iterate (print_var'Access);
         end if;
      end dump_conditional_vars;

      procedure dump_license is
         procedure dump_lic_file (position : string_crate.Cursor);
         procedure dump_spkg_licenses (position : string_crate.Cursor);

         procedure dump_lic_file (position : string_crate.Cursor)
         is
            value : String := HT.USS (string_crate.Element (position));
            lic   : String := HT.part_1 (value, ":");
            path  : String := HT.part_2 (value, ":");
         begin
            send ("LICENSE_FILE_" & lic & LAT.Equals_Sign & path);
         end dump_lic_file;

         procedure dump_spkg_licenses (position : string_crate.Cursor)
         is
            procedure search (name_pos : string_crate.Cursor);

            value : String  := HT.USS (string_crate.Element (position));
            lic   : String  := HT.part_1 (value, ":");
            spkg  : String  := HT.part_2 (value, ":");
            LNAME : String  := "LICENSE_NAME_" & lic & LAT.Equals_Sign;
            ltype : license_type := determine_license (lic);
            cname : HT.Text;

            procedure search (name_pos : string_crate.Cursor)
            is
               inner_value : String  := HT.USS (string_crate.Element (position));
               inner_lic   : String  := HT.part_1 (inner_value, ":");
               inner_desc  : String  := HT.part_2 (inner_value, ":");
            begin
               if inner_lic = lic  then
                  cname := HT.SUS (inner_desc);
               end if;
            end search;
         begin
            send ("LICENSE_" & spkg & LAT.Plus_Sign & LAT.Equals_Sign & lic);
            case ltype is
               when CUSTOM1 | CUSTOM2 | CUSTOM3 | CUSTOM4 =>
                  specs.lic_names.Iterate (search'Access);
                  send (LNAME & HT.USS (cname));
               when others =>
                  send (LNAME & LAT.Quotation & standard_license_names (ltype) & LAT.Quotation);
            end case;
         end dump_spkg_licenses;
      begin
         if specs.licenses.Is_Empty then
            return;
         else
            send ("LICENSE_SET=yes");
         end if;
         send ("LICENSE_SCHEME=" & HT.USS (specs.lic_scheme));
         specs.lic_files.Iterate (dump_lic_file'Access);
         specs.licenses.Iterate (dump_spkg_licenses'Access);
      end dump_license;

   begin
      if not specs.variant_exists (variant) then
         TIO.Put_Line ("Error : Variant '" & variant & "' does not exist!");
         return;
      end if;

      if write_to_file then
        TIO.Create (File => makefile_handle,
                    Mode => TIO.Out_File,
                    Name => output_file);
      end if;

      --  The pkg manifests did use many of these, but they will be generated by ravenadm

      send ("# Makefile has been autogenerated by ravenadm tool" & LAT.LF);

      send ("NAMEBASE",         HT.USS (specs.namebase));
      send ("VERSION",          HT.USS (specs.version));
      send ("REVISION",         specs.revision, 0);
      send ("EPOCH",            specs.epoch, 0);
      send ("VARIANT",          variant);
      send ("DL_SITES",         specs.dl_sites, 1);
      send ("DISTFILE",         specs.distfiles, 2);
      send ("DIST_SUBDIR",      specs.dist_subdir);
      dump_distname;
      send ("MAKESUM_INDEX",    specs.distfiles, 9);
      send ("DF_INDEX",         specs.df_index, 1);
      send ("SUBPACKAGES",      specs.subpackages, 8);
      send ("EXTRACT_ONLY",     specs.extract_only, 1);
      send ("DIRTY_EXTRACT",    specs.extract_dirty, 7);
      send ("ZIP-EXTRACT",      specs.extract_zip, 3);
      send ("7Z-EXTRACT",       specs.extract_7z, 4);
      send ("LHA-EXTRACT",      specs.extract_lha, 5);
      send ("EXTRACT_HEAD",     specs.extract_head, 6);
      send ("EXTRACT_TAIL",     specs.extract_tail, 6);
      dump_broken;
      send ("USES",             specs.uses, 10);
      dump_license;
      dump_info;
      dump_catchall;
      send ("NO_CCACHE",        specs.skip_ccache, True);
      send ("PATCH_WRKSRC",     specs.patch_wrksrc);

      dump_has_configure (specs.config_must);
      send ("GNU_CONFIGURE_PREFIX", specs.config_prefix);
      send ("CONFIGURE_OUTSOURCE",  specs.config_outsrc, True);
      send ("CONFIGURE_WRKSRC",     specs.config_wrksrc);
      send ("CONFIGURE_SCRIPT",     specs.config_script);
      send ("CONFIGURE_TARGET",     specs.config_target);
      send ("CONFIGURE_ARGS",       specs.config_args, 1);
      send ("CONFIGURE_ENV",        specs.config_env, 1);
      send ("APPLY_F10_FIX",        specs.apply_f10_fix, True);

      send ("NO_BUILD",         specs.skip_build, True);
      send ("BUILD_WRKSRC",     specs.build_wrksrc);
      send ("BUILD_TARGET",     specs.build_target, 1);
      send ("NO_INSTALL",       specs.skip_install, True);
      send ("INSTALL_REQ_TOOLCHAIN", specs.shift_install, True);
      send ("INSTALL_WRKSRC",   specs.install_wrksrc);
      send ("INSTALL_TARGET",   specs.install_tgt, 1);
      send ("PLIST_SUB",        specs.plist_sub, 1);
      send ("SUB_FILES",        specs.sub_files, 1);
      send ("SUB_LIST",         specs.sub_list, 1);
      send ("MANDIRS",          specs.mandirs, 1);
      send ("MAKEFILE",         specs.makefile);
      send ("MAKE_ENV",         specs.make_env, 1);
      send ("MAKE_ARGS",        specs.make_args, 1);
      send ("OPTIMIZER_LEVEL",  specs.optimizer_lvl, 2);
      send ("CFLAGS",           specs.cflags, 1);
      send ("CXXFLAGS",         specs.cxxflags, 1);
      send ("CPPFLAGS",         specs.cppflags, 1);
      send ("LDFLAGS",          specs.ldflags, 1);
      dump_conditional_vars;
      send ("SINGLE_JOB",       specs.single_job, True);
      send ("DESTDIR_VIA_ENV",  specs.destdir_env, True);
      send ("DESTDIRNAME",      specs.destdirname);
      send ("TEST_TARGET",      specs.test_tgt, 1);

      declare
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
               when 2 => return "do-";
               when 3 => return "post-";
               when others => return "";
            end case;
         end get_prefix;
      begin
         for phase in Positive range 1 .. 6 loop
            for prefix in Positive range 1 .. 3 loop
               declare
                  target : String := get_prefix (prefix) & get_phasestr (phase);
               begin
                  dump_standard_target (target);
                  dump_opsys_target    (target);
                  dump_option_target   (target);
               end;
            end loop;
         end loop;
      end;

      send (LAT.LF & ".include " & LAT.Quotation & "/xports/Mk/raven.mk" & LAT.Quotation);

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
   --  generate_github_distfile
   --------------------------------------------------------------------------------------------
   function generate_github_distname (download_site : String) return String
   is
      gh_args    : constant String  := HT.part_2 (download_site, "/");
      num_colons : constant Natural := HT.count_char (gh_args, LAT.Colon);
   begin
      if num_colons < 2 then
         --  NOT EXPECTED!!!  give garbage so maintainer notices and fixes it
         return gh_args;
      end if;
      declare
         proj : constant String := HT.specific_field (gh_args, 2, ":");
         vers : constant String := HT.replace_all (S      => HT.specific_field (gh_args, 3, ":"),
                                                   reject => LAT.Plus_Sign,
                                                   shiny  => LAT.Hyphen);
      begin
         if vers (vers'First) = 'v' then
            return proj & LAT.Hyphen & vers (vers'First + 1 .. vers'Last);
         else
            return proj & LAT.Hyphen & vers;
         end if;
      end;
   end generate_github_distname;


   --------------------------------------------------------------------------------------------
   --  standard_license_names
   --------------------------------------------------------------------------------------------
   function standard_license_names (license : license_type) return String
   is
      AGPL  : constant String := "GNU Affero General Public License version ";
      GPL   : constant String := "GNU General Public License version ";
      LGPL  : constant String := "GNU Library General Public License version ";
      RLE3  : constant String := "GNU GPL version 3 Runtime Library Exception";
      AL    : constant String := "Apache License ";
      ART   : constant String := "Artistic License version ";
      later : constant String := " (or later)";
   begin
      case license is
         when CUSTOM1 | CUSTOM2 | CUSTOM3 | CUSTOM4 => return "Don't use for custom licenses";
         when INVALID    => return "error, invalid license";

         when AGPLv3     => return AGPL & "3";
         when AGPLv3x    => return AGPL & "3" & later;
         when APACHE10   => return AL & "1.0";
         when APACHE11   => return AL & "1.1";
         when APACHE20   => return AL & "2.0";
         when ART10      => return ART & "1.0";
         when ART20      => return ART & "2.0";
         when ARTPERL10  => return "Artistic License (perl) version 1.0";
         when BSD2CLAUSE => return "BSD 2-clause 'Simplified' License";
         when BSD3CLAUSE => return "BSD 3-clause 'New' or 'Revised' License";
         when BSD4CLAUSE => return "BSD 4-clause 'Original' or 'Old' License";
         when GPLv1      => return GPL & "1";
         when GPLv1x     => return GPL & "1" & later;
         when GPLv2      => return GPL & "2";
         when GPLv2x     => return GPL & "2" & later;
         when GPLv3      => return GPL & "3";
         when GPLv3x     => return GPL & "3" & later;
         when GPLv3RLE   => return RLE3;
         when GPLv3RLEx  => return RLE3 & later;
         when LGPL20     => return LGPL & "2.0";
         when LGPL20x    => return LGPL & "2.0" & later;
         when LGPL21     => return LGPL & "2.1";
         when LGPL21x    => return LGPL & "2.1" & later;
         when LGPL3      => return LGPL & "3.0";
         when LGPL3x     => return LGPL & "3.0" & later;
         when ISCL       => return "Internet Systems Consortium License";
         when MIT        => return "MIT license / X11 license";
         when PUBDOM     => return "Public Domain";
         when OPENSSL    => return "OpenSSL License";
      end case;
   end standard_license_names;

end Port_Specification.Makefile;
