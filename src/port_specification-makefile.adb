--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Utilities;
with Parameters;
with Ada.Characters.Latin_1;

package body Port_Specification.Makefile is

   package UTL renames Utilities;
   package PM  renames Parameters;
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
      procedure print_item             (position : string_crate.Cursor);
      procedure print_module           (position : string_crate.Cursor);
      procedure print_verbation        (position : string_crate.Cursor);
      procedure print_if_defined       (varname, value : String);
      procedure dump_list              (position : list_crate.Cursor);
      procedure dump_variant_index     (position : list_crate.Cursor);
      procedure dump_distfiles         (position : string_crate.Cursor);
      procedure dump_makesum           (position : string_crate.Cursor);
      procedure dump_ext_zip           (position : string_crate.Cursor);
      procedure dump_ext_7z            (position : string_crate.Cursor);
      procedure dump_ext_lha           (position : string_crate.Cursor);
      procedure dump_ext_deb           (position : string_crate.Cursor);
      procedure dump_line              (position : string_crate.Cursor);
      procedure dump_extract_head_tail (position : list_crate.Cursor);
      procedure dump_dirty_extract     (position : string_crate.Cursor);
      procedure dump_standard_target   (target : String);
      procedure dump_opsys_target      (target : String);
      procedure dump_option_target     (target : String);
      procedure dump_broken;
      procedure dump_catchall;
      procedure dump_has_configure     (value  : HT.Text);
      procedure dump_distname;
      procedure dump_license;
      procedure dump_subr;
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

      procedure print_if_defined (varname, value : String) is
      begin
         if value /= "" then
            send (varname & LAT.Equals_Sign & value);
         end if;
      end print_if_defined;

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
            when 11 =>
               crate.Iterate (Process => dump_ext_deb'Access);
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
         module_w_args : String := HT.USS (string_crate.Element (position));
         module        : String := HT.part_1 (module_w_args);
      begin
         --  TODO: convert to binary sort
         if module = "cpe" or else
           module = "makeinfo" or else
           module = "pkgconfig" or else
           module = "gprbuild" or else
           module = "gettext-tools" or else
           module = "bison" or else
           module = "zlib" or else
           module = "zstd" or else
           module = "jpeg" or else
           module = "tiff" or else
           module = "mesa" or else
           module = "readline" or else
           module = "execinfo" or else
           module = "sqlite" or else
           module = "ada" or else
           module = "c++" or else
           module = "gif" or else
           module = "cclibs" or else
           module = "compiler" or else
           module = "fortran"
         then
            null;
         else
            send (module_w_args & " ", True);
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
                 HT.leads (dlsite, "GITHUB_PRIVATE/") or else
                 HT.leads (dlsite, "GHPRIV/")
               then
                  send (NDX & generate_github_distfile (dlsite) & ":" & group);
                  return;
               elsif HT.leads (dlsite, "GITLAB/") then
                  send (NDX & generate_gitlab_distfile (dlsite) & ":" & group);
                  return;
               elsif HT.leads (dlsite, "CRATES/") then
                  send (NDX & generate_crates_distfile (dlsite) & ":" & group);
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
                 HT.leads (first_dlsite, "GITHUB_PRIVATE/") or else
                 HT.leads (first_dlsite, "GHPRIV/")
               then
                  send ("DISTNAME", generate_github_distname (first_dlsite));
               elsif HT.leads (first_dlsite, "GITLAB/") then
                  send ("DISTNAME", generate_gitlab_distname (first_dlsite));
               elsif HT.leads (first_dlsite, "CRATES/") then
                  send ("DISTNAME", generate_crates_distname (first_dlsite));
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
         send ("EXTRACT_HEAD_" & N & "=7z x -y -o${EXTRACT_WRKDIR_" & N & "} >/dev/null");
         send ("EXTRACT_TAIL_" & N & "=# empty");
      end dump_ext_7z;

      procedure dump_ext_lha (position : string_crate.Cursor)
      is
         N : String := HT.USS (string_crate.Element (position));
      begin
         send ("EXTRACT_HEAD_" & N & "=lha xfpw=${EXTRACT_WRKDIR_" & N & "}");
         send ("EXTRACT_TAIL_" & N & "=# empty");
      end dump_ext_lha;

      procedure dump_ext_deb (position : string_crate.Cursor)
      is
         N : String := HT.USS (string_crate.Element (position));
      begin
         send ("EXTRACT_HEAD_" & N & "=ar -x");
         send ("EXTRACT_TAIL_" & N & "=&& ${TAR} -xf data.tar.*");
      end dump_ext_deb;

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
         procedure dump_group (position : list_crate.Cursor);
         procedure dump_nv    (position : string_crate.Cursor);

         key_text : HT.Text;

         procedure dump_group (position : list_crate.Cursor)
         is
            rec : group_list renames list_crate.Element (position);
         begin
            key_text := rec.group;
            rec.list.Iterate (dump_nv'Access);
         end dump_group;

         procedure dump_nv (position : string_crate.Cursor)
         is
            text_value : HT.Text renames string_crate.Element (position);
            nvkey      : constant String := HT.USS (key_text);
         begin
            if nvkey = "CC" or else
              nvkey = "CXX" or else
              nvkey = "CPP"
            then
               send (nvkey & "=" & HT.USS (text_value));
            else
               send (nvkey & "+=" & HT.USS (text_value));
            end if;
         end dump_nv;
      begin
         specs.catch_all.Iterate (dump_group'Access);
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
            if varname = "MAKEFILE_LINE" then
               send (varval);
            else
               send (varname & "+=" & varval);
            end if;
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
         procedure dump_lic_terms     (position : string_crate.Cursor);
         procedure dump_lic_file      (position : string_crate.Cursor);
         procedure dump_lic_awk       (position : string_crate.Cursor);
         procedure dump_lic_source    (position : string_crate.Cursor);
         procedure dump_spkg_licenses (position : string_crate.Cursor);

         procedure dump_lic_terms (position : string_crate.Cursor)
         is
            value : String := HT.USS (string_crate.Element (position));
            spkg  : String := HT.part_1 (value, ":");
            path  : String := HT.part_2 (value, ":");
         begin
            send ("LICENSE_TERMS_" & spkg & " = " & path);
         end dump_lic_terms;

         procedure dump_lic_file (position : string_crate.Cursor)
         is
            value : String := HT.USS (string_crate.Element (position));
            lic   : String := HT.part_1 (value, ":");
            path  : String := HT.part_2 (value, ":");
         begin
            send ("LICENSE_FILE_" & lic & " = " & path);
         end dump_lic_file;

         procedure dump_lic_awk (position : string_crate.Cursor)
         is
            value : String := HT.USS (string_crate.Element (position));
            lic   : String := HT.part_1 (value, ":");
            delim : String (1 .. 1) := (others => LAT.Quotation);
            code  : String := HT.specific_field (HT.part_2 (value, ":"), 2, delim);
         begin
            send ("LICENSE_AWK_" & lic & " = " & code);
         end dump_lic_awk;

         procedure dump_lic_source (position : string_crate.Cursor)
         is
            value : String := HT.USS (string_crate.Element (position));
            lic   : String := HT.part_1 (value, ":");
            path  : String := HT.part_2 (value, ":");
         begin
            send ("LICENSE_SOURCE_" & lic & " = " & path);
         end dump_lic_source;

         procedure dump_spkg_licenses (position : string_crate.Cursor)
         is
            procedure search (name_pos : string_crate.Cursor);

            value : String  := HT.USS (string_crate.Element (position));
            lic   : String  := HT.part_1 (value, ":");
            spkg  : String  := HT.part_2 (value, ":");
            LNAME : String  := "LICENSE_NAME_" & lic & " = ";
            ltype : license_type := determine_license (lic);
            cname : HT.Text;

            procedure search (name_pos : string_crate.Cursor)
            is
               inner_value : String  := HT.USS (string_crate.Element (name_pos));
               inner_lic   : String  := HT.part_1 (inner_value, ":");
               inner_desc  : String  := HT.part_2 (inner_value, ":");
            begin
               if inner_lic = lic  then
                  cname := HT.SUS (HT.specific_field (inner_desc, 2, "" & LAT.Quotation));
               end if;
            end search;
         begin
            send ("LICENSE_" & spkg & " += " & lic);
            case ltype is
               when CUSTOM1 | CUSTOM2 | CUSTOM3 | CUSTOM4 =>
                  specs.lic_names.Iterate (search'Access);
                  send (LNAME & HT.USS (cname));
               when others =>
                  send (LNAME & standard_license_names (ltype));
            end case;
         end dump_spkg_licenses;
      begin
         if specs.licenses.Is_Empty and then
           specs.lic_terms.Is_Empty
         then
            return;
         else
            send ("LICENSE_SET=yes");
         end if;
         send ("LICENSE_SCHEME=" & HT.USS (specs.lic_scheme));
         specs.lic_terms.Iterate (dump_lic_terms'Access);
         specs.lic_files.Iterate (dump_lic_file'Access);
         specs.lic_awk.Iterate (dump_lic_awk'Access);
         specs.lic_source.Iterate (dump_lic_source'Access);
         specs.licenses.Iterate (dump_spkg_licenses'Access);
      end dump_license;

      procedure dump_subr
      is
         procedure dump_script (position : string_crate.Cursor);
         procedure dump_script (position : string_crate.Cursor)
         is
            value    : String := HT.USS (string_crate.Element (position));
            filename : String := HT.part_1 (value, ":");
            subpkg   : String := HT.part_2 (value, ":");
         begin
            send ("RC_SUBR_" & subpkg & " += " & filename);
         end dump_script;
      begin
         specs.subr_scripts.Iterate (dump_script'Access);
      end dump_subr;

      procedure print_verbation (position : string_crate.Cursor) is
      begin
         send (HT.USS (string_crate.Element (position)), False);
      end print_verbation;

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
      send ("PF_INDEX",         specs.patchfiles, 1);
      send ("DF_INDEX",         specs.df_index, 1);
      send ("SUBPACKAGES",      specs.subpackages, 8);
      send ("EXTRACT_ONLY",     specs.extract_only, 1);
      send ("DIRTY_EXTRACT",    specs.extract_dirty, 7);
      send ("ZIP-EXTRACT",      specs.extract_zip, 3);
      send ("7Z-EXTRACT",       specs.extract_7z, 4);
      send ("LHA-EXTRACT",      specs.extract_lha, 5);
      send ("DEB-EXTRACT",      specs.extract_deb, 11);
      send ("EXTRACT_HEAD",     specs.extract_head, 6);
      send ("EXTRACT_TAIL",     specs.extract_tail, 6);
      dump_broken;
      send ("USERS",            specs.users, 1);
      send ("GROUPS",           specs.groups, 1);
      send ("USES",             specs.uses, 10);
      dump_license;
      print_if_defined ("PREFIX", HT.USS (specs.prefix));
      dump_info;
      dump_catchall;
      send ("NO_CCACHE",        specs.skip_ccache, True);
      send ("PATCH_WRKSRC",     specs.patch_wrksrc);
      send ("PATCH_STRIP",      specs.patch_strip, 1);
      send ("PATCH_DIST_STRIP", specs.pfiles_strip, 1);

      dump_has_configure (specs.config_must);
      send ("GNU_CONFIGURE_PREFIX", specs.config_prefix);
      send ("CONFIGURE_OUTSOURCE",  specs.config_outsrc, True);
      send ("CONFIGURE_WRKSRC",     specs.config_wrksrc);
      send ("CONFIGURE_SCRIPT",     specs.config_script);
      send ("CONFIGURE_TARGET",     specs.config_target);
      send ("CONFIGURE_ARGS",       specs.config_args, 1);
      send ("CONFIGURE_ENV",        specs.config_env, 1);

      send ("NO_BUILD",         specs.skip_build, True);
      send ("BUILD_WRKSRC",     specs.build_wrksrc);
      send ("BUILD_TARGET",     specs.build_target, 1);
      send ("NO_INSTALL",       specs.skip_install, True);
      send ("INSTALL_REQ_TOOLCHAIN", specs.shift_install, True);
      send ("INSTALL_WRKSRC",   specs.install_wrksrc);
      send ("INSTALL_TARGET",   specs.install_tgt, 1);
      send ("SOVERSION",        specs.soversion);
      send ("PLIST_SUB",        specs.plist_sub, 1);
      send ("SUB_FILES",        specs.sub_files, 1);
      send ("SUB_LIST",         specs.sub_list, 1);
      send ("MANDIRS",          specs.mandirs, 1);
      send ("MAKEFILE",         specs.makefile);
      send ("MAKE_ENV",         specs.make_env, 1);
      send ("MAKE_ARGS",        specs.make_args, 1);
      send ("OPTIMIZER_LEVEL",  specs.optimizer_lvl, 2);
      send ("WITH_DEBUG",       specs.debugging_on, True);
      send ("CFLAGS",           specs.cflags, 1);
      send ("CXXFLAGS",         specs.cxxflags, 1);
      send ("CPPFLAGS",         specs.cppflags, 1);
      send ("LDFLAGS",          specs.ldflags, 1);
      send ("CMAKE_ARGS",       specs.cmake_args, 1);
      send ("QMAKE_ARGS",       specs.qmake_args, 1);
      dump_conditional_vars;
      send ("SINGLE_JOB",       specs.single_job, True);
      send ("DESTDIR_VIA_ENV",  specs.destdir_env, True);
      send ("DESTDIRNAME",      specs.destdirname);
      send ("TEST_TARGET",      specs.test_tgt, 1);
      send ("TEST_ARGS",        specs.test_args, 1);
      send ("TEST_ENV",         specs.test_env, 1);
      send ("GENERATED",        specs.generated, True);
      send ("PHP_EXTENSIONS",   specs.php_extensions, 1);

      send ("CARGO_SKIP_CONFIGURE", specs.cgo_skip_conf, True);
      send ("CARGO_SKIP_BUILD",     specs.cgo_skip_build, True);
      send ("CARGO_SKIP_INSTALL",   specs.cgo_skip_inst, True);
      send ("CARGO_CONFIG_ARGS",    specs.cgo_conf_args, 1);
      send ("CARGO_BUILD_ARGS",     specs.cgo_build_args, 1);
      send ("CARGO_INSTALL_ARGS",   specs.cgo_inst_args, 1);
      send ("CARGO_FEATURES",       specs.cgo_features, 1);
      send ("CARGO_CARGOLOCK",      specs.cgo_cargolock);
      send ("CARGO_CARGOTOML",      specs.cgo_cargotoml);
      send ("CARGO_CARGO_BIN",      specs.cgo_cargo_bin);
      send ("CARGO_TARGET_DIR",     specs.cgo_target_dir);
      send ("CARGO_VENDOR_DIR",     specs.cgo_vendor_dir);

      if specs.job_limit > 0 and then
         specs.job_limit < Natural (PM.configuration.jobs_limit)
      then
         send ("MAKE_JOBS_NUMBER_LIMIT", specs.job_limit, 0);
      end if;

      dump_subr;

      specs.mk_verbatim.Iterate (print_verbation'Access);

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
               when 7 => return "stage";
               when 8 => return "test";
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
         for phase in Positive range 1 .. 8 loop
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

      handle_github_relocations (specs, makefile_handle);

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
   --  generate_gitlab_distname
   --------------------------------------------------------------------------------------------
   function generate_gitlab_distname (download_site : String) return String
   is
      lab_args   : constant String  := HT.part_2 (download_site, "/");
      num_colons : constant Natural := HT.count_char (lab_args, LAT.Colon);
   begin
      if num_colons < 2 then
         --  NOT EXPECTED!!!  give garbage so maintainer notices and fixes it
         return lab_args;
      end if;
      --  So far gitlab doesn't seem to transform tags like github does, so keep the
      --  leading 'v' and '+' characters until this is proven wrong.
      declare
         proj : constant String := HT.specific_field (lab_args, 2, ":");
         vers : constant String := HT.specific_field (lab_args, 3, ":");
      begin
         return proj & LAT.Hyphen & vers;
      end;
   end generate_gitlab_distname;


   --------------------------------------------------------------------------------------------
   --  generate_crates_distname
   --------------------------------------------------------------------------------------------
   function generate_crates_distname (download_site : String) return String
   is
      url_args   : constant String  := HT.part_2 (download_site, "/");
      num_colons : constant Natural := HT.count_char (url_args, LAT.Colon);
   begin
      if num_colons < 1 then
         --  NOT EXPECTED!!!  give garbage so maintainer notices and fixes it
         return url_args;
      end if;
      declare
         proj : constant String := HT.specific_field (url_args, 1, ":");
         vers : constant String := HT.specific_field (url_args, 2, ":");
      begin
         return proj & LAT.Hyphen & vers;
      end;
   end generate_crates_distname;


   --------------------------------------------------------------------------------------------
   --  handle_github_relocations
   --------------------------------------------------------------------------------------------
   procedure handle_github_relocations (specs : Portspecs; makefile : TIO.File_Type)
   is
      procedure send (data : String);
      procedure filter_to_generated (position : string_crate.Cursor);
      procedure check_for_generated (position : string_crate.Cursor);
      procedure check_for_generated_shift (position : string_crate.Cursor);
      procedure check_for_generated_crate (position : string_crate.Cursor);

      target_set    : Boolean := False;
      shift_tgt_set : Boolean := False;
      crate_tgt_set : Boolean := False;
      write_to_file : Boolean := (TIO.Is_Open (makefile));
      genfile_list  : string_crate.Vector;

      procedure send (data : String) is
      begin
         if write_to_file then
            TIO.Put_Line (makefile, data);
         else
            TIO.Put_Line (data);
         end if;
      end send;

      procedure check_for_generated (position : string_crate.Cursor)
      is
         pload : String := HT.USS (string_crate.Element (position));
      begin
         declare
            group   : HT.Text := HT.SUS (HT.part_2 (pload, ":"));
            dlsite  : String  := HT.USS (specs.dl_sites.Element (group).list.First_Element);
            gh_args : String  := HT.part_2 (dlsite, "/");
            num_colons : constant Natural := HT.count_char (gh_args, LAT.Colon);
         begin
            if not HT.leads (dlsite, "GITHUB/") and then
              not HT.leads (dlsite, "GITHUB_PRIVATE/") and then
              not HT.leads (dlsite, "GHPRIV/") and then
              not HT.leads (dlsite, "GITLAB/")
            then
               return;
            end if;
            if num_colons < 3 then
               --  It's a github site, but there's no extraction wrksrc override
               return;
            end if;
            if HT.specific_field (gh_args, 4, ":") = "" then
               --  The extraction field is blank (usually seen with private github repositories
               --  So no extraction override in this case either
               return;
            end if;
            if not target_set then
               send (LAT.LF & "github-relocation:");
               target_set := True;
            end if;
            send (LAT.HT & "# relocate " & HT.specific_field (gh_args, 1, ":") &
                    "/" & HT.specific_field (gh_args, 2, ":") & " project");
            declare
               reldir     : constant String := HT.specific_field (gh_args, 4, ":");
               extractdir : constant String :=
                 HT.replace_all (S      => generate_github_distname (dlsite),
                                 reject => LAT.Colon,
                                 shiny  => LAT.Hyphen);
            begin
               send (LAT.HT & "@${RM} -r ${WRKSRC}/" & reldir & LAT.LF &
                     LAT.HT & "@${ECHO_MSG} " & LAT.Quotation & "==> Relocating " & extractdir &
                              " to WRKSRC/" & reldir & LAT.Quotation & LAT.LF &
                     LAT.HT & "@${MV} ${WRKDIR}/" & extractdir & " ${WRKSRC}/" & reldir & LAT.LF &
                     LAT.HT & "@${LN} -s ${WRKSRC:T}/" & reldir & " ${WRKDIR}/" & extractdir);
            end;
         end;
      end check_for_generated;

      procedure check_for_generated_shift (position : string_crate.Cursor)
      is
         pload : String := HT.USS (string_crate.Element (position));
      begin
         declare
            group   : HT.Text := HT.SUS (HT.part_2 (pload, ":"));
            dlsite  : String  := HT.USS (specs.dl_sites.Element (group).list.First_Element);
            gh_args : String  := HT.part_2 (dlsite, "/");
            num_colons : constant Natural := HT.count_char (gh_args, LAT.Colon);
         begin
            if not HT.leads (dlsite, "GITHUB_PRIVATE/") and then
              not HT.leads (dlsite, "GHPRIV/") and then
              not HT.leads (dlsite, "GITLAB/")
            then
               return;
            end if;
            if num_colons < 2 then
               return;
            end if;
            --  Gitlab and github private use full hashes in the tarball regardless of the
            --  reference used to generate it.  Thus it's cleanest if we can shift it's location
            --  to the normal $WRKSRC location after extraction.
            if not shift_tgt_set then
               send (LAT.LF & "shift-wrksrc:");
               shift_tgt_set := True;
            end if;

            declare
               account : constant String := HT.specific_field (gh_args, 1, ":");
               project : constant String := HT.specific_field (gh_args, 2, ":");
               taghash : constant String := HT.specific_field (gh_args, 3, ":");
               wrksrc  : constant String := "${WRKDIR}/" & project & "-" & taghash;
            begin
               send (LAT.HT & "# redefine " & account & "/" & project & " work directory");
               if HT.leads (dlsite, "GITLAB/") then
                  send (LAT.HT & "@${MV} ${WRKDIR}/" & project & "-* " & wrksrc);
               else -- GITHUB_PRIVATE, GHPRIV
                  send (LAT.HT & "@${MV} ${WRKDIR}/" & account & "-" & project & "-* " & wrksrc);
               end if;
            end;
         end;
      end check_for_generated_shift;

      procedure check_for_generated_crate (position : string_crate.Cursor)
      is
         pload : String := HT.USS (string_crate.Element (position));
      begin
         declare
            group      : HT.Text := HT.SUS (HT.part_2 (pload, ":"));
            dlsite     : String  := HT.USS (specs.dl_sites.Element (group).list.First_Element);
            url_args   : String  := HT.part_2 (dlsite, "/");
            crate      : constant String := HT.replace_char (url_args, ':', "-");
            num_colons : constant Natural := HT.count_char (url_args, LAT.Colon);
         begin
            if not HT.leads (dlsite, "CRATES/") then
               return;
            end if;
            if num_colons < 1 then
               return;
            end if;

            if not crate_tgt_set then
               crate_tgt_set := True;
               send (LAT.LF & "crate-relocation:");
               send (LAT.HT & "@${ECHO_MSG} " & LAT.Quotation &
                       "===>  Moving crates to ${CARGO_VENDOR_DIR}" & LAT.Quotation);
               send (LAT.HT & "@${MKDIR} ${CARGO_VENDOR_DIR}");
            end if;
            send (LAT.LF & LAT.HT & "# relocate " & crate & " crate");
            send (LAT.HT & "@${MV} ${WRKDIR}/" & crate & " ${CARGO_VENDOR_DIR}/" & crate);
            send (LAT.HT & "@${PRINTF} '{" &
                    LAT.Quotation & "package" & LAT.Quotation & ":" &
                    LAT.Quotation & "%s" & LAT.Quotation & "," &
                    LAT.Quotation & "files" & LAT.Quotation &
                    ":{}}' $$(${SHA256} -q ${DISTDIR}/${DIST_SUBDIR}/" &
                    crate & ".tar.gz) > ${CARGO_VENDOR_DIR}/" &
                    crate & "/.cargo-checksum.json");
         end;
      end check_for_generated_crate;

      procedure filter_to_generated (position : string_crate.Cursor)
      is
         pload : HT.Text renames string_crate.Element (position);
      begin
         if HT.leads (pload, "generated:") then
            genfile_list.Append (pload);
         end if;
      end filter_to_generated;
   begin
      specs.distfiles.Iterate (filter_to_generated'Access);
      genfile_list.Iterate (check_for_generated'Access);
      genfile_list.Iterate (check_for_generated_shift'Access);
      genfile_list.Iterate (check_for_generated_crate'Access);
   end handle_github_relocations;


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
      later : constant String := " or later";
      CCA   : constant String := "Creative Commons Attribution ";
   begin
      case license is
         when CUSTOM1 | CUSTOM2 | CUSTOM3 | CUSTOM4 => return "Don't use for custom licenses";
         when INVALID    => return "error, invalid license";

         when AFL        => return "Academic Free License 3.0";
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
         when BSDGROUP   => return "BSD, check license for number of clauses";
         when CC0_10     => return "Creative Commons Zero v1.0 Universal";
         when CC_30      => return CCA & "3.0";
         when CC_40      => return CCA & "4.0";
         when CC_NC_30   => return CCA & "Non-Commercial 3.0";
         when CC_NC_40   => return CCA & "Non-Commercial 4.0";
         when CC_NCND_30 => return CCA & "Non-Commercial No Derivatives 3.0";
         when CC_NCND_40 => return CCA & "Non-Commercial No Derivatives 4.0";
         when CC_NCSA_30 => return CCA & "Non-Commercial Share-Alike 3.0";
         when CC_NCSA_40 => return CCA & "Non-Commercial Share-Alike 4.0";
         when CC_ND_30   => return CCA & "No Derivatives 3.0";
         when CC_ND_40   => return CCA & "No Derivatives 4.0";
         when CC_SA_30   => return CCA & "Share-Alike 3.0";
         when CC_SA_40   => return CCA & "Share-Alike 4.0";
         when CDDL       => return "Common Development and Distribution License 1.0";
         when GFDL       => return "GNU Free Documentation License";
         when GMGPL      => return "GNAT Modified General Public License (v2)";
         when GMGPL3     => return "GNAT Modified General Public License (v3)";
         when GPLv1      => return GPL & "1";
         when GPLv1x     => return GPL & "1" & later;
         when GPLv2      => return GPL & "2";
         when GPLv2x     => return GPL & "2" & later;
         when GPLv3      => return GPL & "3";
         when GPLv3x     => return GPL & "3" & later;
         when GPLv3RLE   => return RLE3;
         when GPLv3RLEx  => return RLE3 & later;
         when HPND       => return "Historical Permission Notice and Disclaimer";
         when LGPL20     => return LGPL & "2.0";
         when LGPL20x    => return LGPL & "2.0" & later;
         when LGPL21     => return LGPL & "2.1";
         when LGPL21x    => return LGPL & "2.1" & later;
         when LGPL3      => return LGPL & "3.0";
         when LGPL3x     => return LGPL & "3.0" & later;
         when ISCL       => return "Internet Systems Consortium License";
         when MIT        => return "MIT license / X11 license";
         when MPL        => return "Mozilla Public License";
         when PUBDOM     => return "Public Domain";
         when OPENSSL    => return "OpenSSL License";
         when POSTGRESQL => return "PostgreSQL Licence";
         when PSFL       => return "Python Software Foundation License";
         when RUBY       => return "Ruby License";
         when ZLIB       => return "zlib License";
      end case;
   end standard_license_names;

end Port_Specification.Makefile;
