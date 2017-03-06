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
      arch_standard : supported_arch;
      osrelease     : String;
      osmajor       : String;
      osversion     : String;
      option_string : String;
      output_file   : String
     )
   is
      procedure send (data : String; use_put : Boolean := False);
      procedure send (varname, value : String);
      procedure send (varname : String; value : HT.Text);
      procedure send (varname : String; crate : string_crate.Vector; flavor : Positive := 1);
      procedure send (varname : String; crate : list_crate.Map; flavor : Positive := 1);
      procedure send (varname : String; value : Boolean; dummy : Boolean);
      procedure send (varname : String; value, default : Integer);
      procedure print_item (position : string_crate.Cursor);
      procedure dump_list (position : list_crate.Cursor);
      procedure dump_distfiles (position : string_crate.Cursor);
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

      procedure send (varname : String; crate : string_crate.Vector; flavor : Positive := 1) is
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
            when others =>
               null;
         end case;
      end send;

      procedure send (varname : String; crate : list_crate.Map; flavor : Positive := 1) is
      begin
         varname_prefix := HT.SUS (varname);
         case flavor is
            when 1 => crate.Iterate (Process => dump_list'Access);
            when 6 => crate.Iterate (Process => dump_extract_head_tail'Access);
            when others => null;
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

      procedure dump_line (position : string_crate.Cursor) is
      begin
         send (HT.USS (string_crate.Element (position)));
      end dump_line;

      procedure dump_distfiles (position : string_crate.Cursor)
      is
         index : Natural := string_crate.To_Index (position);
         NDX   : String  := HT.USS (varname_prefix)  & "_" & HT.int2str (index) & LAT.Equals_Sign;
      begin
         send (NDX & HT.USS (string_crate.Element (position)));
      end dump_distfiles;

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
         procedure precheck (position : list_crate.Cursor);
         procedure check    (position : list_crate.Cursor);

         num_reasons : Natural := 0;
         curnum      : Natural := 1;
         cpu_ia64    : constant String := UTL.cpu_arch (x86_64) & "_";
         cpu_ia32    : constant String := UTL.cpu_arch (i386) & "_";
         cpu_armv8   : constant String := UTL.cpu_arch (aarch64) & "_";
         separator   : constant String := ": ";
         varname     : constant String := "BROKEN=";

         procedure precheck (position : list_crate.Cursor)
         is
            procedure precheck_list (position : string_crate.Cursor);

            broken_Key : String := HT.USS (list_crate.Element (position).group);

            procedure precheck_list (position : string_crate.Cursor)
            is
               reason : String := HT.USS (string_crate.Element (position));
               used  : Boolean := False;
            begin
               if broken_Key = broken_all or else
                 broken_Key = UTL.cpu_arch (arch_standard)
               then
                  used := True;
               elsif broken_Key = UTL.lower_opsys (opsys) then
                  if HT.leads (reason, "REL_") then
                     used := (HT.partial_search (reason, 4, separator) = osmajor);
                  elsif HT.leads (reason, "GTE_") then
                     used := GTE (gen_opsys  => opsys,
                                  gen_major  => osmajor,
                                  spec_major => HT.partial_search (reason, 4, separator));
                  elsif HT.leads (reason, "LTE_") then
                     used := LTE (gen_opsys  => opsys,
                                  gen_major  => osmajor,
                                  spec_major => HT.partial_search (reason, 4, separator));
                  elsif HT.leads (reason, cpu_ia64) then
                     used := (HT.partial_search (reason, cpu_ia64'Length, separator) = osmajor);
                  elsif HT.leads (reason, cpu_ia32) then
                     used := (HT.partial_search (reason, cpu_ia32'Length, separator) = osmajor);
                  elsif HT.leads (reason, cpu_armv8) then
                     used := (HT.partial_search (reason, cpu_armv8'Length, separator) = osmajor);
                  else
                     used := True;
                  end if;
               end if;
               if used then
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
            use_prefix : Boolean := (num_reasons > 1);

            procedure check_list (position : string_crate.Cursor)
            is
               reason     : String  := HT.USS (string_crate.Element (position));
               used       : Boolean := False;
            begin
               if broken_Key = broken_all or else
                 broken_Key = UTL.cpu_arch (arch_standard)
               then
                  used := True;
               elsif broken_Key = UTL.lower_opsys (opsys) then
                  if HT.leads (reason, "REL_") then
                     used := (HT.partial_search (reason, 4, separator) = osmajor);
                  elsif HT.leads (reason, "GTE_") then
                     used := GTE (gen_opsys  => opsys,
                                  gen_major  => osmajor,
                                  spec_major => HT.partial_search (reason, 4, separator));
                  elsif HT.leads (reason, "LTE_") then
                     used := LTE (gen_opsys  => opsys,
                                  gen_major  => osmajor,
                                  spec_major => HT.partial_search (reason, 4, separator));
                  elsif HT.leads (reason, cpu_ia64) then
                     used := (HT.partial_search (reason, cpu_ia64'Length, separator) = osmajor);
                  elsif HT.leads (reason, cpu_ia32) then
                     used := (HT.partial_search (reason, cpu_ia32'Length, separator) = osmajor);
                  elsif HT.leads (reason, cpu_armv8) then
                     used := (HT.partial_search (reason, cpu_armv8'Length, separator) = osmajor);
                  else
                     used := True;
                  end if;
               end if;

               if used then
                  if use_prefix then
                     send ("[Reason " & HT.int2str (curnum) & "] ", True);
                  end if;
                  if curnum = num_reasons then
                     send (reason);
                  else
                     send (reason & " \");
                  end if;
                  curnum := curnum + 1;
               end if;
            end check_list;
         begin
            list_crate.Element (position).list.Iterate (Process => check_list'Access);
         end check;

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
      send ("DL_SITES",         specs.dl_sites);
      send ("DISTFILE",         specs.distfiles, 2);
      send ("DIST_SUBDIR",      specs.dist_subdir);
      send ("DISTNAME",         specs.distname);
      send ("DF_INDEX",         specs.df_index);
      send ("EXTRACT_ONLY",     specs.extract_only);
      send ("DIRTY_EXTRACT",    specs.extract_dirty, 7);
      send ("ZIP-EXTRACT",      specs.extract_zip, 3);
      send ("7Z-EXTRACT",       specs.extract_7z, 4);
      send ("LHA-EXTRACT",      specs.extract_lha, 5);
      send ("EXTRACT_HEAD",     specs.extract_head, 6);
      send ("EXTRACT_TAIL",     specs.extract_tail, 6);
      dump_broken;
      send ("NO_BUILD",         specs.skip_build, True);
      send ("NO_INSTALL",       specs.skip_install, True);
      send ("BUILD_WRKSRC",     specs.build_wrksrc);
      send ("BUILD_TARGET",     specs.build_target);
      send ("MAKEFILE",         specs.makefile);
      send ("MAKE_ENV",         specs.make_env);
      send ("MAKE_ARGS",        specs.make_args);
      send ("OPTIMIZER_LEVEL",  specs.optimizer_lvl, 2);
      send ("CFLAGS",           specs.cflags);
      send ("CXXFLAGS",         specs.cxxflags);
      send ("CPPFLAGS",         specs.cppflags);
      send ("LDFLAGS",          specs.ldflags);
      send ("SINGLE_JOB",       specs.single_job, True);
      send ("DESTDIR_VIA_ENV",  specs.destdir_env, True);
      send ("DESTDIRNAME",      specs.destdirname);

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

      --  TODO: This is not correct, placeholder.  rethink.
      send (LAT.LF & ".include " & LAT.Quotation & "/usr/raven/share/mk/raven.mk" & LAT.Quotation);

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
   function LTE (gen_opsys : supported_opsys; gen_major, spec_major : String) return Boolean
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
   function GTE (gen_opsys : supported_opsys; gen_major, spec_major : String) return Boolean
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


end Port_Specification.Makefile;
