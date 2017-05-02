--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Calendar;
with File_Operations;
with Port_Specification.Transform;
with Specification_Parser;
with PortScan.Log;
with Parameters;
with Signals;
with Unix;

package body PortScan.Scan is

   package PM  renames Parameters;
   package LOG renames PortScan.Log;
   package FOP renames File_Operations;
   package PAR renames Specification_Parser;
   package PST renames Port_Specification.Transform;
   package PSP renames Port_Specification;
   package LAT renames Ada.Characters.Latin_1;
   package DIR renames Ada.Directories;
   package EX  renames Ada.Exceptions;
   package CAL renames Ada.Calendar;

   --------------------------------------------------------------------------------------------
   --  scan_entire_ports_tree
   --------------------------------------------------------------------------------------------
   function scan_entire_ports_tree (sysrootver : sysroot_characteristics) return Boolean
   is
      good_scan    : Boolean;
      using_screen : constant Boolean := Unix.screen_attached;
      conspiracy   : constant String := HT.USS (PM.configuration.dir_conspiracy);
      unkindness   : constant String := HT.USS (PM.configuration.dir_unkindness);
   begin
      --  All scanning done on host system (no need to mount in a slave)
      if not prescanned then
         prescan_ports_tree (conspiracy, unkindness, sysrootver);
      end if;
      prescan_unkindness (unkindness);
      LOG.set_scan_start_time (CAL.Clock);
      parallel_deep_scan (conspiracy    => conspiracy,
                          unkindness    => unkindness,
                          sysrootver    => sysrootver,
                          success       => good_scan,
                          show_progress => using_screen);
      LOG.set_scan_complete (CAL.Clock);

      return good_scan;
   end scan_entire_ports_tree;


   --------------------------------------------------------------------------------------------
   --  prescan_ports_tree
   --------------------------------------------------------------------------------------------
   procedure prescan_ports_tree
     (conspiracy : String;
      unkindness : String;
      sysrootver : sysroot_characteristics)
   is
      conspindex_path : constant String := conspiracy & conspindex;
      max_lots        : constant scanners := get_max_lots;
      custom_avail    : constant Boolean := unkindness /= PM.no_unkindness;
   begin
      if not DIR.Exists (conspindex_path) then
         raise missing_index with conspindex;
      end if;

      declare
         fulldata  : String := FOP.get_file_contents (conspindex_path);
         markers   : HT.Line_Markers;
         linenum   : Natural := 0;
      begin
         HT.initialize_markers (fulldata, markers);
         loop
            exit when not HT.next_line_present (fulldata, markers);
            linenum := linenum + 1;
            declare
               line     : constant String := HT.extract_line (fulldata, markers);
               bucket   : constant String := HT.specific_field (line, 1);
               namebase : constant String := HT.specific_field (line, 2);
               numvar   : constant String := HT.specific_field (line, 3);
               bsheet   : constant String := "/bucket_" & bucket & "/" & namebase;
               linestr  : constant String := ", line " & HT.int2str (linenum);
               varcount : Integer;
            begin
               if bucket'Length /= 2 or else namebase'Length = 0 or else numvar'Length = 0 then
                  raise bad_index_data with conspindex & linestr;
               end if;
               begin
                  varcount := Integer'Value (numvar);
               exception
                  when others =>
                     raise bad_index_data
                       with conspindex & ", numvariant fields not an integer" & linestr;
               end;
               if not DIR.Exists (conspiracy & bsheet) then
                  raise bad_index_data
                    with conspindex & bsheet & " buildsheet does not exist" & linestr;
               end if;
               if custom_avail and then DIR.Exists (unkindness & bsheet) then
                  --  postpone custom port scan (done prescan_unkindness)
                  null;
               else
                  for varx in Integer range 1 .. varcount loop
                     declare
                        varxname : constant String := HT.specific_field (line, varx + 3);
                        portkey  : HT.Text := HT.SUS (namebase & ":" & varxname);
                        kc       : portkey_crate.Cursor;
                        success  : Boolean;
                     begin
                        if varxname = "" then
                           raise bad_index_data
                             with conspindex & ", less variants than counter" & linestr;
                        end if;
                        ports_keys.Insert (Key      => portkey,
                                           New_Item => lot_counter,
                                           Position => kc,
                                           Inserted => success);
                        last_port := lot_counter;
                        all_ports (lot_counter).sequence_id   := lot_counter;
                        all_ports (lot_counter).key_cursor    := kc;
                        all_ports (lot_counter).port_namebase := HT.SUS (namebase);
                        all_ports (lot_counter).port_variant  := HT.SUS (varxname);
                        all_ports (lot_counter).bucket        := bucket_code (bucket);
                        all_ports (lot_counter).unkind_custom := False;

                        make_queue (lot_number).Append (lot_counter);
                        lot_counter := lot_counter + 1;
                        if lot_number = max_lots then
                           lot_number := 1;
                        else
                           lot_number := lot_number + 1;
                        end if;
                     end;
                  end loop;
               end if;
            end;
         end loop;
      end;

      prescanned := True;
   end prescan_ports_tree;


   --------------------------------------------------------------------------------------------
   --  prescan_custom
   --------------------------------------------------------------------------------------------
   procedure prescan_custom
     (unkindness    : String;
      bucket        : bucket_code;
      namebase      : String;
      max_lots      : scanners)
   is
      --  Assume buildsheet exists (has already been validated)
      --  Assume it starts with directory separator
      successful : Boolean;
      customspec : PSP.Portspecs;
      arch_focus : supported_arch := x86_64;  -- unused, pick one
      buildsheet : constant String := "/bucket_" & bucket & "/" & namebase;
   begin
      PAR.parse_specification_file (dossier         => unkindness & buildsheet,
                                    specification   => customspec,
                                    opsys_focus     => platform_type,
                                    arch_focus      => arch_focus,
                                    success         => successful,
                                    stop_at_targets => True);
      if not successful then
         raise bsheet_parsing
           with unkindness & buildsheet & "-> " & PAR.get_parse_error;
      end if;
      declare
         varcount : Natural := customspec.get_number_of_variants;
         varlist  : String  := customspec.get_field_value (PSP.sp_variants);
      begin
         for varx in Integer range 1 .. varcount loop
            declare
               varxname : constant String := HT.specific_field (varlist, varx, ", ");
               portkey  : HT.Text := HT.SUS (namebase & ":" & varxname);
               kc       : portkey_crate.Cursor;
               success  : Boolean;
            begin
               ports_keys.Insert (Key      => portkey,
                                  New_Item => lot_counter,
                                  Position => kc,
                                  Inserted => success);
               last_port := lot_counter;
               all_ports (lot_counter).sequence_id   := lot_counter;
               all_ports (lot_counter).key_cursor    := kc;
               all_ports (lot_counter).port_namebase := HT.SUS (namebase);
               all_ports (lot_counter).port_variant  := HT.SUS (varxname);
               all_ports (lot_counter).bucket        := bucket;
               all_ports (lot_counter).unkind_custom := True;

               make_queue (lot_number).Append (lot_counter);
               lot_counter := lot_counter + 1;
               if lot_number = max_lots then
                  lot_number := 1;
               else
                  lot_number := lot_number + 1;
               end if;
            end;
         end loop;
      end;
   end prescan_custom;


   --------------------------------------------------------------------------------------------
   --  prescan_unkindness
   --------------------------------------------------------------------------------------------
   procedure prescan_unkindness (unkindness : String)
   is
      max_lots : constant scanners := get_max_lots;
      bucket   : bucket_code;
   begin
      if unkindness = PM.no_unkindness or else
        not DIR.Exists (unkindness)
      then
         return;
      end if;

      for highdigit in AF'Range loop
         for lowdigit in AF'Range loop
            bucket := tohex (highdigit) & tohex (lowdigit);
            declare
               bucket_dir   : constant String := unkindness & "/bucket_" & bucket;
               Inner_Search : DIR.Search_Type;
               Inner_Dirent : DIR.Directory_Entry_Type;
               use type DIR.File_Kind;
            begin
               if DIR.Exists (bucket_dir) and then
                 DIR.Kind (bucket_dir) = DIR.Directory
               then

                  DIR.Start_Search (Search    => Inner_Search,
                                    Directory => bucket_dir,
                                    Filter    => (DIR.Ordinary_File => True, others => False),
                                    Pattern   => "*");

                  while DIR.More_Entries (Inner_Search) loop
                     DIR.Get_Next_Entry (Inner_Search, Inner_Dirent);
                     declare
                        namebase : String := DIR.Simple_Name (Inner_Dirent);
                     begin
                        prescan_custom (unkindness => unkindness,
                                        bucket     => bucket,
                                        namebase   => namebase,
                                        max_lots   => max_lots);
                     end;
                  end loop;
                  DIR.End_Search (Inner_Search);
               end if;
            end;
         end loop;
      end loop;
   end prescan_unkindness;


   --------------------------------------------------------------------------------------------
   --  get_max_lots
   --------------------------------------------------------------------------------------------
   function get_max_lots return scanners
   is
      first_try : constant Positive := Positive (PM.configuration.number_cores) * 3;
   begin
      if first_try > Positive (scanners'Last) then
         return scanners'Last;
      else
         return scanners (first_try);
      end if;
   end get_max_lots;


   --------------------------------------------------------------------------------------------
   --  parallel_deep_scan
   --------------------------------------------------------------------------------------------
   procedure parallel_deep_scan
     (conspiracy    : String;
      unkindness    : String;
      sysrootver    : sysroot_characteristics;
      success       : out Boolean;
      show_progress : Boolean)
   is
      finished : array (scanners) of Boolean := (others => False);
      combined_wait : Boolean := True;
      aborted : Boolean := False;

      task type scan (lot : scanners);
      task body scan
      is
         procedure populate (cursor : subqueue.Cursor);
         procedure populate (cursor : subqueue.Cursor)
         is
            target_port : port_index := subqueue.Element (cursor);
         begin
            if not aborted then
               populate_port_data (conspiracy, unkindness, target_port, False, sysrootver);
               mq_progress (lot) := mq_progress (lot) + 1;
            end if;
         exception
            when issue : others =>
               TIO.Put_Line (LAT.LF & "culprit: " & get_port_variant (all_ports (target_port)));
               EX.Reraise_Occurrence (issue);
         end populate;
      begin
         make_queue (lot).Iterate (populate'Access);
         finished (lot) := True;
      exception
         when issue : populate_error =>
            aborted := True;
            TIO.Put_Line ("Scan aborted during port data population.");
            TIO.Put_Line (EX.Exception_Message (issue));
         when issue : others =>
            aborted := True;
            TIO.Put_Line ("Scan aborted for an unknown reason.");
            TIO.Put_Line (EX.Exception_Message (issue));
      end scan;

      scan_01 : scan (lot => 1);
      scan_02 : scan (lot => 2);
      scan_03 : scan (lot => 3);
      scan_04 : scan (lot => 4);
      scan_05 : scan (lot => 5);
      scan_06 : scan (lot => 6);
      scan_07 : scan (lot => 7);
      scan_08 : scan (lot => 8);
      scan_09 : scan (lot => 9);
      scan_10 : scan (lot => 10);
      scan_11 : scan (lot => 11);
      scan_12 : scan (lot => 12);
      scan_13 : scan (lot => 13);
      scan_14 : scan (lot => 14);
      scan_15 : scan (lot => 15);
      scan_16 : scan (lot => 16);
      scan_17 : scan (lot => 17);
      scan_18 : scan (lot => 18);
      scan_19 : scan (lot => 19);
      scan_20 : scan (lot => 20);
      scan_21 : scan (lot => 21);
      scan_22 : scan (lot => 22);
      scan_23 : scan (lot => 23);
      scan_24 : scan (lot => 24);
      scan_25 : scan (lot => 25);
      scan_26 : scan (lot => 26);
      scan_27 : scan (lot => 27);
      scan_28 : scan (lot => 28);
      scan_29 : scan (lot => 29);
      scan_30 : scan (lot => 30);
      scan_31 : scan (lot => 31);
      scan_32 : scan (lot => 32);

   begin
      TIO.Put_Line ("Scanning entire ports tree.");
      while combined_wait loop
         delay 1.0;
         if show_progress then
            TIO.Put (scan_progress);
         end if;
         combined_wait := False;
         for j in scanners'Range loop
            if not finished (j) then
               combined_wait := True;
               exit;
            end if;
         end loop;
         if Signals.graceful_shutdown_requested then
            aborted := True;
         end if;
      end loop;
      success := not aborted;
   end parallel_deep_scan;


   --------------------------------------------------------------------------------------------
   --  skeleton_compiler_data
   --------------------------------------------------------------------------------------------
   procedure skeleton_compiler_data
     (conspiracy    : String;
      unkindness    : String;
      target        : port_index;
      sysrootver    : sysroot_characteristics)
   is
      rec : port_record renames all_ports (target);
      function calc_dossier return String;

      thespec    : PSP.Portspecs;
      successful : Boolean;
      variant    : constant String := HT.USS (rec.port_variant);
      osrelease  : constant String := HT.USS (sysrootver.release);

      function calc_dossier return String
      is
         buildsheet : String := "/bucket_" & rec.bucket & "/" & HT.USS (rec.port_namebase);
      begin
         if rec.unkind_custom then
            return unkindness & buildsheet;
         else
            return conspiracy & buildsheet;
         end if;
      end calc_dossier;
   begin
      PAR.parse_specification_file (dossier         => calc_dossier,
                                    specification   => thespec,
                                    opsys_focus     => platform_type,
                                    arch_focus      => sysrootver.arch,
                                    success         => successful,
                                    stop_at_targets => True);
      if not successful then
         raise bsheet_parsing
           with calc_dossier & "-> " & PAR.get_parse_error;
      end if;

      PST.set_option_defaults
        (specs         => thespec,
         variant       => variant,
         opsys         => platform_type,
         arch_standard => sysrootver.arch,
         osrelease     => osrelease);

      --  TODO: implement option caching and determination (changes next line)
      PST.set_option_to_default_values (specs => thespec);

      PST.set_outstanding_ignore
        (specs         => thespec,
         variant       => variant,
         opsys         => platform_type,
         arch_standard => sysrootver.arch,
         osrelease     => osrelease,
         osmajor       => HT.USS (sysrootver.major));

      PST.apply_directives
        (specs         => thespec,
         variant       => variant,
         arch_standard => sysrootver.arch,
         osmajor       => HT.USS (sysrootver.major));

      rec.pkgversion    := HT.SUS (thespec.calculate_pkgversion);
      rec.ignore_reason := HT.SUS (thespec.aggregated_ignore_reason);
      rec.ignored       := not HT.IsBlank (rec.ignore_reason);
      rec.scanned       := False;

      for item in Positive range 1 .. thespec.get_subpackage_length (variant) loop
         declare
            newrec     : subpackage_record;
            subpackage : String := thespec.get_subpackage_item (variant, item);
         begin
            newrec.subpackage   := HT.SUS (subpackage);
            newrec.never_remote := True;
            newrec.pkg_present  := True;
            rec.subpackages.Append (newrec);
         end;
      end loop;

   end skeleton_compiler_data;



   --------------------------------------------------------------------------------------------
   --  populate_port_data
   --------------------------------------------------------------------------------------------
   procedure populate_port_data
     (conspiracy    : String;
      unkindness    : String;
      target        : port_index;
      always_build  : Boolean;
      sysrootver    : sysroot_characteristics)
   is
      rec : port_record renames all_ports (target);
      function calc_dossier return String;

      thespec    : PSP.Portspecs;
      successful : Boolean;
      variant    : constant String := HT.USS (rec.port_variant);
      osrelease  : constant String := HT.USS (sysrootver.release);
      prime_pkg  : HT.Text := HT.blank;

      function calc_dossier return String
      is
         buildsheet : String := "/bucket_" & rec.bucket & "/" & HT.USS (rec.port_namebase);
      begin
         if rec.unkind_custom then
            return unkindness & buildsheet;
         else
            return conspiracy & buildsheet;
         end if;
      end calc_dossier;
   begin
      PAR.parse_specification_file (dossier         => calc_dossier,
                                    specification   => thespec,
                                    opsys_focus     => platform_type,
                                    arch_focus      => sysrootver.arch,
                                    success         => successful,
                                    stop_at_targets => True);
      if not successful then
         raise bsheet_parsing
           with calc_dossier & "-> " & PAR.get_parse_error;
      end if;

      PST.set_option_defaults
        (specs         => thespec,
         variant       => variant,
         opsys         => platform_type,
         arch_standard => sysrootver.arch,
         osrelease     => osrelease);

      --  TODO: implement option caching and determination (changes next line)
      PST.set_option_to_default_values (specs => thespec);

      PST.set_outstanding_ignore
        (specs         => thespec,
         variant       => variant,
         opsys         => platform_type,
         arch_standard => sysrootver.arch,
         osrelease     => osrelease,
         osmajor       => HT.USS (sysrootver.major));

      PST.apply_directives
        (specs         => thespec,
         variant       => variant,
         arch_standard => sysrootver.arch,
         osmajor       => HT.USS (sysrootver.major));

      rec.pkgversion    := HT.SUS (thespec.calculate_pkgversion);
      rec.ignore_reason := HT.SUS (thespec.aggregated_ignore_reason);
      rec.ignored       := not HT.IsBlank (rec.ignore_reason);
      rec.scanned       := True;
      for item in Positive range 1 .. thespec.get_list_length (PSP.sp_build_deps) loop
         populate_set_depends (target, thespec.get_list_item (PSP.sp_build_deps, item), build);
      end loop;
      for item in Positive range 1 .. thespec.get_list_length (PSP.sp_buildrun_deps) loop
         populate_set_depends (target, thespec.get_list_item (PSP.sp_buildrun_deps, item),
                               buildrun);
      end loop;
      for item in Positive range 1 .. thespec.get_list_length (PSP.sp_run_deps) loop
         populate_set_depends (target, thespec.get_list_item (PSP.sp_run_deps, item), runtime);
      end loop;
      for item in Positive range 1 .. thespec.get_subpackage_length (variant) loop
         declare
            newrec     : subpackage_record;
            subpackage : String := thespec.get_subpackage_item (variant, item);
            is_primary : Boolean := False;
         begin
            newrec.subpackage   := HT.SUS (subpackage);
            newrec.never_remote := always_build;

            if subpackage /= spkg_complete and then
              subpackage /= spkg_examples and then
              subpackage /= spkg_docs and then
              HT.IsBlank (prime_pkg)
            then
               prime_pkg := newrec.subpackage;
               is_primary := True;
            end if;

            for subitem in Positive range 1 .. thespec.get_number_extra_run (subpackage) loop
               declare
                  dep : String := thespec.get_extra_runtime (subpackage, subitem);
               begin
                  populate_set_depends (target, dep, extra_runtime);
                  declare
                     --  These will pass because populate_set_depends didn't throw exception
                     portkey  : HT.Text := HT.SUS (convert_tuple_to_portkey (dep));
                     depindex : port_index := ports_keys.Element (portkey);
                     idrec    : subpackage_identifier;
                  begin
                     idrec.port := depindex;
                     idrec.subpackage := HT.SUS (extract_subpackage (dep));
                     newrec.spkg_run_deps.Append (idrec);
                  end;
               end;
            end loop;

            if is_primary then
               for si in Positive range 1 .. thespec.get_list_length (PSP.sp_buildrun_deps) loop
                  declare
                     dep      : String := thespec.get_list_item (PSP.sp_buildrun_deps, si);
                     portkey  : HT.Text := HT.SUS (convert_tuple_to_portkey (dep));
                     depindex : port_index := ports_keys.Element (portkey);
                     idrec    : subpackage_identifier;
                  begin
                     idrec.port := depindex;
                     idrec.subpackage := HT.SUS (extract_subpackage (dep));
                     newrec.spkg_run_deps.Append (idrec);
                  end;
               end loop;
               for si in Positive range 1 .. thespec.get_list_length (PSP.sp_run_deps) loop
                  declare
                     dep      : String := thespec.get_list_item (PSP.sp_run_deps, si);
                     portkey  : HT.Text := HT.SUS (convert_tuple_to_portkey (dep));
                     depindex : port_index := ports_keys.Element (portkey);
                     idrec    : subpackage_identifier;
                  begin
                     idrec.port := depindex;
                     idrec.subpackage := HT.SUS (extract_subpackage (dep));
                     newrec.spkg_run_deps.Append (idrec);
                  end;
               end loop;
            end if;

            if subpackage = spkg_complete then
               for si in Positive range 1 .. thespec.get_subpackage_length (variant) loop
                  declare
                     innersub : constant String := thespec.get_subpackage_item (variant, si);
                     idrec    : subpackage_identifier;
                  begin
                     if innersub /= spkg_complete then
                        idrec.port := target;
                        idrec.subpackage := HT.SUS (innersub);
                        newrec.spkg_run_deps.Append (idrec);
                     end if;
                  end;
               end loop;
            end if;

            rec.subpackages.Append (newrec);
         end;
      end loop;
      if variant = variant_standard then
         for item in Positive range 1 .. thespec.get_list_length (PSP.sp_opts_standard) loop
            declare
               optname : String := thespec.get_list_item (PSP.sp_opts_standard, item);
            begin
               if optname /= options_none then
                  populate_option (target, optname, thespec.option_current_setting (optname));
               end if;
            end;
         end loop;
      else
         for item in Positive range 1 .. thespec.get_list_length (PSP.sp_opts_avail) loop
            declare
               optname : String := thespec.get_list_item (PSP.sp_opts_avail, item);
            begin
               populate_option (target, optname, thespec.option_current_setting (optname));
            end;
         end loop;
      end if;
   end populate_port_data;


   --------------------------------------------------------------------------------------------
   --  populate_option
   --------------------------------------------------------------------------------------------
   procedure populate_option (target : port_index; option_name : String; setting : Boolean)
   is
      optname_text : HT.Text := HT.SUS (option_name);
   begin
      if not all_ports (target).options.Contains (optname_text) then
         all_ports (target).options.Insert (Key => optname_text, New_Item => setting);
      end if;
   end populate_option;


   --------------------------------------------------------------------------------------------
   --  populate_set_depends
   --------------------------------------------------------------------------------------------
   procedure populate_set_depends (target : port_index;
                                   tuple  : String;
                                   dtype  : dependency_type)
   is
      portkey  : HT.Text := HT.SUS (convert_tuple_to_portkey (tuple));
      depindex : port_index;
   begin
      if not ports_keys.Contains (portkey) then
         raise populate_error with "dependency on non-existent port " & tuple;
      end if;
      depindex := ports_keys.Element (portkey);
      if target = depindex then
         if dtype = extra_runtime then
            return;
         else
            raise populate_error with tuple & " can't depend on itself";
         end if;
      end if;
      if HT.USS (all_ports (depindex).port_namebase) = default_compiler then
         if dtype = extra_runtime then
            return;
         else
            raise populate_error with tuple & " belongs to the default compiler which is a " &
              "special case that can only be specified via EXRUN";
         end if;
      end if;
      if not all_ports (target).blocked_by.Contains (depindex) then
         all_ports (target).blocked_by.Insert (depindex, depindex);
      end if;
      if dtype in LR_set and then
        not all_ports (target).run_deps.Contains (depindex)
      then
         all_ports (target).run_deps.Insert (Key => depindex, New_Item => depindex);
      end if;

   end populate_set_depends;


   --------------------------------------------------------------------------------------------
   --  convert_tuple_to_portkey
   --------------------------------------------------------------------------------------------
   function convert_tuple_to_portkey (tuple : String) return String
   is
      --  tuple   format is <namebase>:<subpackage>:<variant>
      --  portkey format is <namebase>-<variant>
   begin
      if HT.count_char (tuple, LAT.Colon) /= 2 then
         raise populate_error with "tuple has invalid format: " & tuple;
      end if;
      declare
         namebase : String := HT.specific_field (tuple, 1, ":");
         variant  : String := HT.specific_field (tuple, 3, ":");
      begin
         return namebase & LAT.Colon & variant;
      end;
   end convert_tuple_to_portkey;


   --------------------------------------------------------------------------------------------
   --  extract_subpackage
   --------------------------------------------------------------------------------------------
   function extract_subpackage (tuple : String) return String
   is
      --  tuple format is <namebase>:<subpackage>:<variant>
   begin
      if HT.count_char (tuple, LAT.Colon) /= 2 then
         raise populate_error with "tuple has invalid format: " & tuple;
      end if;
      return  HT.tail (HT.head (tuple, ":"), ":");
   end extract_subpackage;


   --------------------------------------------------------------------------------------------
   --  scan_single_port
   --------------------------------------------------------------------------------------------
   function scan_single_port
     (namebase     : String;
      variant      : String;
      always_build : Boolean;
      sysrootver   : sysroot_characteristics;
      fatal        : out Boolean) return Boolean
   is
      procedure dig (cursor : block_crate.Cursor);

      conspiracy : constant String := HT.USS (PM.configuration.dir_conspiracy);
      unkindness : constant String := HT.USS (PM.configuration.dir_unkindness);

      two_partid : constant String := namebase & LAT.Colon & variant;
      portkey    : HT.Text := HT.SUS (two_partid);
      target     : port_index;
      aborted    : Boolean := False;
      indy500    : Boolean := False;

      procedure dig (cursor : block_crate.Cursor)
      is
         new_target : port_index := block_crate.Element (cursor);
      begin
         if not aborted then
            if all_ports (new_target).scan_locked then
               --  We've already seen this (circular dependency)
               raise circular_logic;
            end if;
            if not all_ports (new_target).scanned then
               populate_port_data (conspiracy, unkindness, new_target, False, sysrootver);
               all_ports (new_target).scan_locked := True;
               all_ports (new_target).blocked_by.Iterate (dig'Access);
               all_ports (new_target).scan_locked := False;
               if indy500 then
                  TIO.Put_Line ("... backtrace " & get_port_variant (all_ports (new_target)));
               end if;
            end if;
         end if;
      exception
         when issue : circular_logic =>
            aborted := True;
            indy500 := True;
            TIO.Put_Line (LAT.LF & two_partid & " scan aborted because a circular dependency on " &
                            get_port_variant (all_ports (new_target)) & " was detected.");
         when issue : populate_error =>
            aborted := True;
            TIO.Put_Line ("Scan aborted during port data population.");
            TIO.Put_Line (EX.Exception_Message (issue));
         when issue : others =>
            aborted := True;
            TIO.Put_Line ("Scan aborted for an unknown reason.");
            TIO.Put_Line (EX.Exception_Message (issue));
      end dig;
   begin
      fatal := False;
      if not prescanned then
         prescan_ports_tree (conspiracy, unkindness, sysrootver);
      end if;
      if ports_keys.Contains (portkey) then
         target := ports_keys.Element (portkey);
      else
         TIO.Put_Line (namebase & " specification not listed in Mk/Misc/conspiracy-variants, ");
         TIO.Put_Line ("nor found in the custom ports directory");
         return False;
      end if;
      begin
         if all_ports (target).scanned then
            --  This can happen when a dpendency is also on the build list.
            return True;
         else
            populate_port_data (conspiracy, unkindness, target, always_build, sysrootver);
         end if;
      exception
         when issue : others =>
            TIO.Put_Line ("Encountered issue with " & two_partid & " or its dependencies" &
                            LAT.LF & "  => " & EX.Exception_Message (issue));
            return False;
      end;
      all_ports (target).scan_locked := True;
      all_ports (target).blocked_by.Iterate (dig'Access);
      all_ports (target).scan_locked := False;
      if indy500 then
         TIO.Put_Line ("... backtrace " & two_partid);
         fatal := True;
      end if;
      return not aborted;
   end scan_single_port;


   --------------------------------------------------------------------------------------------
   --  set_build_priority
   --------------------------------------------------------------------------------------------
   procedure set_build_priority is
   begin
      iterate_reverse_deps;
      iterate_drill_down;
   end set_build_priority;


   --------------------------------------------------------------------------------------------
   --  iterate_reverse_deps
   --------------------------------------------------------------------------------------------
   procedure iterate_reverse_deps
   is
      procedure set_reverse (cursor : block_crate.Cursor);

      victim : port_index;

      procedure set_reverse (cursor : block_crate.Cursor)
      is
         blocker : port_index renames block_crate.Element (cursor);
      begin
         --  Using conditional insert here causes a finalization error when
         --  the program exists.  Reluctantly, do the condition check manually
         if not all_ports (blocker).blocks.Contains (victim) then
            all_ports (blocker).blocks.Insert (victim, victim);
         end if;
      end set_reverse;

   begin
      for port in port_index'First .. last_port loop
         if all_ports (port).scanned then
            victim := port;
            all_ports (port).blocked_by.Iterate (set_reverse'Access);
         end if;
      end loop;
   end iterate_reverse_deps;


   --------------------------------------------------------------------------------------------
   --  iterate_reverse_deps
   --------------------------------------------------------------------------------------------
   procedure iterate_drill_down is
   begin
      rank_queue.Clear;
      for port in port_index'First .. last_port loop
         if all_ports (port).scanned then
            drill_down (next_target => port, original_target => port);
            declare
               ndx : constant port_index   := port_index (all_ports (port).reverse_score);
               QR  : constant queue_record := (ap_index      => port,
                                               reverse_score => ndx);
            begin
               rank_queue.Insert (New_Item => QR);
            end;
         end if;
      end loop;
   end iterate_drill_down;


   --------------------------------------------------------------------------------------------
   --  drill_down
   --------------------------------------------------------------------------------------------
   procedure drill_down (next_target : port_index; original_target : port_index)
   is
      procedure stamp_and_drill (cursor : block_crate.Cursor);
      procedure slurp_scanned (cursor : block_crate.Cursor);

      rec : port_record renames all_ports (next_target);

      procedure slurp_scanned (cursor : block_crate.Cursor)
      is
         rev_id  : port_index := block_crate.Element (Position => cursor);
      begin
         if not all_ports (original_target).all_reverse.Contains (rev_id) then
            all_ports (original_target).all_reverse.Insert (rev_id, rev_id);
         end if;
      end slurp_scanned;

      procedure stamp_and_drill (cursor : block_crate.Cursor)
      is
         pmc : port_index := block_crate.Element (Position => cursor);
      begin
         if not all_ports (original_target).all_reverse.Contains (pmc) then
            all_ports (original_target).all_reverse.Insert (pmc, pmc);
         end if;
         if pmc = original_target then
            declare
               top_port  : constant String := get_port_variant (all_ports (original_target));
               this_port : constant String := get_port_variant (all_ports (next_target));
            begin
               raise circular_logic with top_port & " <=> " & this_port;
            end;
         end if;

         if not all_ports (pmc).rev_scanned then
            drill_down (next_target => pmc, original_target => pmc);
         end if;
         all_ports (pmc).all_reverse.Iterate (slurp_scanned'Access);
      end stamp_and_drill;

   begin
      if not rec.scanned then
         return;
      end if;
      if rec.rev_scanned then
         --  It is possible to get here if an earlier port scanned this port
         --  as a reverse dependencies
         return;
      end if;
      rec.blocks.Iterate (stamp_and_drill'Access);
      rec.reverse_score := port_index (rec.all_reverse.Length);
      rec.rev_scanned := True;
   end drill_down;


   --------------------------------------------------------------------------------------------
   --  scan_provided_list_of_ports
   --------------------------------------------------------------------------------------------
   function scan_provided_list_of_ports
     (always_build : Boolean;
      sysrootver   : sysroot_characteristics) return Boolean
   is
      procedure scan (plcursor : string_crate.Cursor);

      successful    : Boolean := True;
      just_stop_now : Boolean;
      compiler_key  : HT.Text := HT.SUS (default_compiler & ":" & variant_standard);

      procedure scan (plcursor : string_crate.Cursor)
      is
         origin   : constant String := HT.USS (string_crate.Element (plcursor));
         namebase : constant String := HT.part_1 (origin, ":");
         variant  : constant String := HT.part_2 (origin, ":");
      begin
         if not successful then
            return;
         end if;
         if Signals.graceful_shutdown_requested then
            successful := False;
            return;
         end if;
         if not scan_single_port (namebase     => namebase,
                                  variant      => variant,
                                  always_build => always_build,
                                  sysrootver   => sysrootver,
                                  fatal        => just_stop_now)
         then
            if just_stop_now then
               --  backtrace outputs, no need for more information.
               null;
            else
               TIO.Put_Line ("Scan of " & origin & " failed, bulk run cancelled");
            end if;
            successful := False;
         end if;
      end scan;
   begin
      portlist.Iterate (Process => scan'Access);
      if successful and then
        not portlist.Contains (compiler_key)
      then
         --  We always need current information on the default compiler
         begin
            skeleton_compiler_data (conspiracy => HT.USS (PM.configuration.dir_conspiracy),
                                    unkindness => HT.USS (PM.configuration.dir_unkindness),
                                    target     => ports_keys.Element (compiler_key),
                                    sysrootver => sysrootver);
         exception
            when others =>
               TIO.Put_Line ("Scan of the compiler port failed, fatal issue");
               successful := False;
         end;
      end if;
      return successful;
   end scan_provided_list_of_ports;


   --------------------------------------------------------------------------------------------
   --  generate_conspiracy_index
   --------------------------------------------------------------------------------------------
   function tohex (value : AF) return Character is
   begin
      case value is
         when 0 .. 9 => return Character'Val (Character'Pos ('0') + value);
         when others => return Character'Val (Character'Pos ('A') + value - 10);
      end case;
   end tohex;


   --------------------------------------------------------------------------------------------
   --  generate_conspiracy_index
   --------------------------------------------------------------------------------------------
   procedure generate_conspiracy_index (sysrootver : sysroot_characteristics)
   is
      procedure scan_port (position : string_crate.Cursor);

      conspiracy : constant String := HT.USS (PM.configuration.dir_conspiracy);
      conname    : constant String := "conspiracy_variants";
      finalcvar  : constant String := conspiracy & "/Mk/Misc/" & conname;
      summary    : constant String := conspiracy & "/Mk/Misc/summary.txt";
      indexfile  : TIO.File_Type;
      bucket     : bucket_code;
      total_ports    : Natural := 0;
      total_variants : Natural := 0;
      total_subpkgs  : Natural := 0;

      procedure scan_port (position : string_crate.Cursor)
      is
         namebase   : String := HT.USS (string_crate.Element (position));
         successful : Boolean;
         customspec : PSP.Portspecs;
         arch_focus : supported_arch := x86_64;  -- unused, pick one
         dossier    : constant String := conspiracy & "/bucket_" & bucket & "/" & namebase;
      begin
         PAR.parse_specification_file (dossier         => dossier,
                                       specification   => customspec,
                                       opsys_focus     => platform_type,
                                       arch_focus      => arch_focus,
                                       success         => successful,
                                       stop_at_targets => True);
         if not successful then
            raise bsheet_parsing with dossier & "-> " & PAR.get_parse_error;
         end if;

         declare
            varcnt  : Natural := customspec.get_number_of_variants;
            varlist : String  := customspec.get_field_value (PSP.sp_variants);
         begin
            total_ports := total_ports + 1;
            total_variants := total_variants + varcnt;
            TIO.Put (indexfile, bucket & " " & namebase & " " & HT.int2str (varcnt));
            for varx in Integer range 1 .. varcnt loop
               declare
                  variant : String  := HT.specific_field (varlist, varx, ", ");
                  spkgcnt : Natural := customspec.get_subpackage_length (variant);
               begin
                  total_subpkgs := total_subpkgs  + spkgcnt;
                  TIO.Put (indexfile, " " & variant);
               end;
            end loop;
            TIO.Put_Line (indexfile, "");
         end;
      end scan_port;
   begin
      LOG.set_scan_start_time (CAL.Clock);

      TIO.Create (File => indexfile,
                  Mode => TIO.Out_File,
                  Name => finalcvar);

      for highdigit in AF'Range loop
         for lowdigit in AF'Range loop
            bucket := tohex (highdigit) & tohex (lowdigit);
            declare
               bucket_dir   : constant String := conspiracy & "/bucket_" & bucket;
               Inner_Search : DIR.Search_Type;
               Inner_Dirent : DIR.Directory_Entry_Type;
               tempstore    : string_crate.Vector;
               use type DIR.File_Kind;
            begin
               if DIR.Exists (bucket_dir) and then
                 DIR.Kind (bucket_dir) = DIR.Directory
               then

                  DIR.Start_Search (Search    => Inner_Search,
                                    Directory => bucket_dir,
                                    Filter    => (DIR.Ordinary_File => True, others => False),
                                    Pattern   => "*");

                  while DIR.More_Entries (Inner_Search) loop
                     DIR.Get_Next_Entry (Search => Inner_Search, Directory_Entry => Inner_Dirent);
                     tempstore.Append (HT.SUS (DIR.Simple_Name (Inner_Dirent)));
                  end loop;
                  DIR.End_Search (Inner_Search);
                  sorter.Sort (tempstore);
                  tempstore.Iterate (scan_port'Access);
               end if;
            end;
         end loop;
      end loop;

      TIO.Close (indexfile);
      LOG.set_scan_complete (CAL.Clock);

      TIO.Put_Line ("Index successfully generated.");
      TIO.Put_Line ("       Total ports : " & HT.int2str (total_ports));
      TIO.Put_Line ("    Total variants : " & HT.int2str (total_variants));
      TIO.Put_Line ("    Total packages : " & HT.int2str (total_subpkgs));
      TIO.Put_Line ("  Linear scan time : " & LOG.scan_duration);

      TIO.Create (File => indexfile,
                  Mode => TIO.Out_File,
                  Name => summary);

      TIO.Put_Line (indexfile, "  Statistics derived from generation of conspiracy index");
      TIO.Put_Line (indexfile, "==========================================================");
      TIO.Put_Line (indexfile, "       Total ports : " & HT.int2str (total_ports));
      TIO.Put_Line (indexfile, "    Total variants : " & HT.int2str (total_variants));
      TIO.Put_Line (indexfile, "    Total packages : " & HT.int2str (total_subpkgs));
      TIO.Put_Line (indexfile, "  Linear scan time : " & LOG.scan_duration);

      TIO.Close (indexfile);

   exception
      when issue : others =>
         if TIO.Is_Open (indexfile) then
            TIO.Close (indexfile);
         end if;
         TIO.Put_Line ("Failure encountered: " & EX.Exception_Message (issue));
   end generate_conspiracy_index;


   --------------------------------------------------------------------------------------------
   --  version_difference
   --------------------------------------------------------------------------------------------
   function version_difference (id : port_id; kind : out verdiff) return String
   is
      procedure each_subpackage (position : subpackage_crate.Cursor);

      pkg8    : constant String := HT.USS (PM.configuration.sysroot_pkg8);
      dir_pkg : constant String := HT.USS (PM.configuration.dir_repository);
      version : constant String := HT.USS (all_ports (id).pkgversion);
      origin  : constant String := get_port_variant (id);
      upgrade : HT.Text;
      all_present : Boolean := True;

      procedure each_subpackage (position : subpackage_crate.Cursor)
      is
         rec : subpackage_record renames subpackage_crate.Element (position);
         subpackage   : String := HT.USS (rec.subpackage);
         current      : String := calculate_package_name (id, subpackage);
         base_pattern : String := HT.USS (all_ports (id).port_namebase) & "-" &
                                  HT.USS (all_ports (id).port_variant) & "-";
         pattern      : String := base_pattern & "*" & arc_ext;
         pkg_search   : DIR.Search_Type;
         dirent       : DIR.Directory_Entry_Type;
      begin

         if rec.pkg_present then
            return;
         else
            all_present := False;
         end if;
         if not HT.IsBlank (upgrade) then
            return;
         end if;

         DIR.Start_Search (Search    => pkg_search,
                           Directory => dir_pkg,
                           Filter    => (DIR.Ordinary_File => True, others => False),
                           Pattern   => pattern);
         while DIR.More_Entries (Search => pkg_search) loop
            DIR.Get_Next_Entry (Search => pkg_search, Directory_Entry => dirent);
            declare
               sname      : String := DIR.Simple_Name (dirent);
               verend     : Natural := sname'Length - arc_ext'Length;
               command    : String := pkg8 & " query -F "  & dir_pkg & "/" & sname & " %o";
               status     : Integer;
               testorigin : HT.Text := Unix.piped_command (command, status);
            begin
               if status = 0 and then HT.equivalent (testorigin, origin) then
                  upgrade := HT.SUS (" (" & sname (base_pattern'Length + 1 .. verend) &
                                       " => " & version & ")");
               end if;
            end;
         end loop;
         DIR.End_Search (pkg_search);
      end each_subpackage;


   begin
      all_ports (id).subpackages.Iterate (each_subpackage'Access);
      if all_present then
         kind := rebuild;
         return " (rebuild " & version & ")";
      end if;
      if not HT.IsBlank (upgrade) then
         kind := change;
         return HT.USS (upgrade);
      end if;
      kind := newbuild;
      return " (new " & version & ")";
   end version_difference;


   --------------------------------------------------------------------------------------------
   --  display_results_of_dry_run
   --------------------------------------------------------------------------------------------
   procedure display_results_of_dry_run
   is
      procedure print (cursor : ranking_crate.Cursor);
      listlog  : TIO.File_Type;
      filename : constant String   := "/tmp/ravenadm_status_results.txt";
      max_lots : constant scanners := get_max_lots;
      elapsed  : constant String   := LOG.scan_duration;
      goodlog  : Boolean;

      procedure print (cursor : ranking_crate.Cursor)
      is
         id     : port_id := ranking_crate.Element (cursor).ap_index;
         kind   : verdiff;
         diff   : constant String := version_difference (id, kind);
         origin : constant String := get_port_variant (id);
      begin
         case kind is
            when newbuild => TIO.Put_Line ("  N => " & origin);
            when rebuild  => TIO.Put_Line ("  R => " & origin);
            when change   => TIO.Put_Line ("  U => " & origin & diff);
         end case;
         if goodlog then
            TIO.Put_Line (listlog, origin & diff);
         end if;
      end print;
   begin
      begin
         TIO.Create (File => listlog, Mode => TIO.Out_File, Name => filename);
         goodlog := True;
      exception
         when others => goodlog := False;
      end;
      TIO.Put_Line ("These are the ports that would be built ([N]ew, " &
                   "[R]ebuild, [U]pgrade):");
      rank_queue.Iterate (print'Access);
      TIO.Put_Line ("Total packages that would be built:" &
                      rank_queue.Length'Img);
      if goodlog then
         TIO.Put_Line
           (listlog,
            LAT.LF &
              LAT.LF & "------------------------------" &
              LAT.LF & "--  Statistics" &
              LAT.LF & "------------------------------" &
              LAT.LF & " Ports scanned :" & last_port'Img &
              LAT.LF & "  Elapsed time : " & elapsed &
              LAT.LF & "   Parallelism :" & max_lots'Img & " scanners" &
              "          ncpu :" & Parameters.configuration.number_cores'Img);
         TIO.Close (listlog);
         TIO.Put_Line ("The complete build list can also be found at:"
                       & LAT.LF & filename);
      end if;
   end display_results_of_dry_run;


   --------------------------------------------------------------------------------------------
   --  gather_distfile_set
   --------------------------------------------------------------------------------------------
   function gather_distfile_set (sysrootver : sysroot_characteristics) return Boolean
   is
      good_scan    : Boolean;
      using_screen : constant Boolean := Unix.screen_attached;
      conspiracy   : constant String := HT.USS (PM.configuration.dir_conspiracy);
      unkindness   : constant String := HT.USS (PM.configuration.dir_unkindness);
   begin
      prescan_conspiracy_index_for_distfiles (conspiracy, unkindness, sysrootver);
      LOG.set_scan_start_time (CAL.Clock);
      parallel_distfile_scan (conspiracy    => conspiracy,
                              sysrootver    => sysrootver,
                              success       => good_scan,
                              show_progress => using_screen);
      LOG.set_scan_complete (CAL.Clock);
      return good_scan;
   end gather_distfile_set;


   --------------------------------------------------------------------------------------------
   --  prescan_conspiracy_index_for_distfiles
   --------------------------------------------------------------------------------------------
   procedure prescan_conspiracy_index_for_distfiles
     (conspiracy : String;
      unkindness : String;
      sysrootver : sysroot_characteristics)
   is
      conspindex_path : constant String := conspiracy & conspindex;
      max_lots        : constant scanners := get_max_lots;
      custom_avail    : constant Boolean := unkindness /= PM.no_unkindness;
   begin
      if not DIR.Exists (conspindex_path) then
         raise missing_index with conspindex;
      end if;

      declare
         fulldata  : String := FOP.get_file_contents (conspindex_path);
         markers   : HT.Line_Markers;
         linenum   : Natural := 0;
      begin
         HT.initialize_markers (fulldata, markers);
         loop
            exit when not HT.next_line_present (fulldata, markers);
            linenum := linenum + 1;
            declare
               line     : constant String := HT.extract_line (fulldata, markers);
               bucket   : constant String := HT.specific_field (line, 1);
               namebase : constant String := HT.specific_field (line, 2);
               bsheet   : constant String := "/bucket_" & bucket & "/" & namebase;
               linestr  : constant String := ", line " & HT.int2str (linenum);
            begin
               if bucket'Length /= 2 or else namebase'Length = 0 then
                  raise bad_index_data with conspindex & linestr;
               end if;
               if not DIR.Exists (conspiracy & bsheet) then
                  raise bad_index_data
                    with conspindex & bsheet & " buildsheet does not exist" & linestr;
               end if;
               if custom_avail and then DIR.Exists (unkindness & bsheet) then
                  --  postpone custom port scan (done prescan_unkindness)
                  null;
               else
                  declare
                     portkey  : HT.Text := HT.SUS (namebase);
                     kc       : portkey_crate.Cursor;
                     success  : Boolean;
                  begin
                     ports_keys.Insert (Key      => portkey,
                                        New_Item => lot_counter,
                                        Position => kc,
                                        Inserted => success);
                     last_port := lot_counter;
                     all_ports (lot_counter).sequence_id   := lot_counter;
                     all_ports (lot_counter).key_cursor    := kc;
                     all_ports (lot_counter).port_namebase := HT.SUS (namebase);
                     all_ports (lot_counter).bucket        := bucket_code (bucket);
                     all_ports (lot_counter).unkind_custom := False;
                     make_queue (lot_number).Append (lot_counter);
                     lot_counter := lot_counter + 1;
                     if lot_number = max_lots then
                        lot_number := 1;
                     else
                        lot_number := lot_number + 1;
                     end if;
                  end;
               end if;
            end;
         end loop;
      end;
   end prescan_conspiracy_index_for_distfiles;


   --------------------------------------------------------------------------------------------
   --  linear_scan_unkindness_for_distfiles
   --------------------------------------------------------------------------------------------
   procedure linear_scan_unkindness_for_distfiles (unkindness : String)
   is
      procedure insert_distfile (dist_subdir : String; distfile_group : String);

      dindex : port_index := 0;

      procedure insert_distfile (dist_subdir : String; distfile_group : String)
      is
         function determine_distname return String;
         use_subdir : Boolean := (dist_subdir /= "");

         function determine_distname return String
         is
            distfile : String := HT.part_1 (distfile_group, ":");
         begin
            if use_subdir then
               return dist_subdir & "/" & distfile;
            else
               return distfile;
            end if;
         end determine_distname;

         distfile_path : HT.Text := HT.SUS (determine_distname);
      begin
         if not distfile_set.Contains (distfile_path) then
            dindex := dindex + 1;
            distfile_set.Insert (distfile_path, dindex);
         end if;
      end insert_distfile;
   begin
      if unkindness = PM.no_unkindness or else
        not DIR.Exists (unkindness)
      then
         return;
      end if;

      for highdigit in AF'Range loop
         for lowdigit in AF'Range loop
            declare
               bucket       : bucket_code := tohex (highdigit) & tohex (lowdigit);
               bucket_dir   : constant String := unkindness & "/bucket_" & bucket;
               Inner_Search : DIR.Search_Type;
               Inner_Dirent : DIR.Directory_Entry_Type;
               use type DIR.File_Kind;
            begin
               if DIR.Exists (bucket_dir) and then
                 DIR.Kind (bucket_dir) = DIR.Directory
               then

                  DIR.Start_Search (Search    => Inner_Search,
                                    Directory => bucket_dir,
                                    Filter    => (DIR.Ordinary_File => True, others => False),
                                    Pattern   => "*");

                  while DIR.More_Entries (Inner_Search) loop
                     DIR.Get_Next_Entry (Inner_Search, Inner_Dirent);
                     declare
                        namebase : String := DIR.Simple_Name (Inner_Dirent);
                        successful : Boolean;
                        customspec : PSP.Portspecs;
                        arch_focus : supported_arch := x86_64;  -- unused, pick one
                        buildsheet : constant String := "/bucket_" & bucket & "/" & namebase;
                     begin
                        PAR.parse_specification_file (dossier         => unkindness & buildsheet,
                                                      specification   => customspec,
                                                      opsys_focus     => platform_type,
                                                      arch_focus      => arch_focus,
                                                      success         => successful,
                                                      stop_at_targets => True);
                        if not successful then
                           raise bsheet_parsing
                             with unkindness & buildsheet & "-> " & PAR.get_parse_error;
                        end if;
                        declare
                           dist_subdir : String := customspec.get_field_value (PSP.sp_distsubdir);
                           num_dfiles  : Natural := customspec.get_list_length (PSP.sp_distfiles);
                        begin
                           for df in 1 .. num_dfiles loop
                              insert_distfile (dist_subdir,
                                               customspec.get_list_item (PSP.sp_distfiles, df));
                           end loop;
                        end;
                     end;
                  end loop;
                  DIR.End_Search (Inner_Search);
               end if;
            end;
         end loop;
      end loop;
   end linear_scan_unkindness_for_distfiles;


   --------------------------------------------------------------------------------------------
   --  parallel_distfile_scan
   --------------------------------------------------------------------------------------------
   procedure parallel_distfile_scan
     (conspiracy    : String;
      sysrootver    : sysroot_characteristics;
      success       : out Boolean;
      show_progress : Boolean)
   is
      procedure combine (position : portkey_crate.Cursor);

      finished      : array (scanners) of Boolean := (others => False);
      task_storage  : array (scanners) of portkey_crate.Map;
      combined_wait : Boolean := True;
      aborted       : Boolean := False;
      dindex        : port_index := port_index (distfile_set.Length);

      procedure combine (position : portkey_crate.Cursor)
      is
         distfile : HT.Text := portkey_crate.Key (position);
      begin
         if not distfile_set.Contains (distfile) then
            dindex := dindex + 1;
            distfile_set.Insert (distfile, dindex);
         end if;
      end combine;

      task type scan (lot : scanners);
      task body scan
      is
         procedure populate (cursor : subqueue.Cursor);
         procedure populate (cursor : subqueue.Cursor)
         is
            procedure insert_distfile (dist_subdir : String; distfile : String);

            target_port : port_index := subqueue.Element (cursor);
            namebase    : String := HT.USS (all_ports (target_port).port_namebase);
            bucket      : bucket_code := all_ports (target_port).bucket;
            customspec  : PSP.Portspecs;
            successful  : Boolean;
            arch_focus  : supported_arch := x86_64;  -- unused, pick one
            buildsheet  : constant String := "/bucket_" & bucket & "/" & namebase;

            procedure insert_distfile (dist_subdir : String; distfile : String)
            is
               function determine_distname return String;
               use_subdir : Boolean := (dist_subdir /= "");

               function determine_distname return String is
               begin
                  if use_subdir then
                     return dist_subdir & "/" & distfile;
                  else
                     return distfile;
                  end if;
               end determine_distname;

               distfile_path : HT.Text := HT.SUS (determine_distname);
            begin
               if not task_storage (lot).Contains (distfile_path) then
                  task_storage (lot).Insert (distfile_path, 1);
               end if;
            end insert_distfile;
         begin
            if not aborted then
               PAR.parse_specification_file (dossier         => conspiracy & buildsheet,
                                             specification   => customspec,
                                             opsys_focus     => platform_type,
                                             arch_focus      => arch_focus,
                                             success         => successful,
                                             stop_at_targets => True);
               if not successful then
                  TIO.Put_Line (LAT.LF & "culprit: " & buildsheet & "-> " & PAR.get_parse_error);
                  aborted := True;
               end if;
               declare
                  dist_subdir : String := customspec.get_field_value (PSP.sp_distsubdir);
                  num_dfiles  : Natural := customspec.get_list_length (PSP.sp_distfiles);
               begin
                  for df in 1 .. num_dfiles loop
                     insert_distfile (dist_subdir,
                                      customspec.get_list_item (PSP.sp_distfiles, df));
                  end loop;
               end;
               mq_progress (lot) := mq_progress (lot) + 1;
            end if;
         end populate;
      begin
         make_queue (lot).Iterate (populate'Access);
         finished (lot) := True;
      end scan;

      scan_01 : scan (lot => 1);
      scan_02 : scan (lot => 2);
      scan_03 : scan (lot => 3);
      scan_04 : scan (lot => 4);
      scan_05 : scan (lot => 5);
      scan_06 : scan (lot => 6);
      scan_07 : scan (lot => 7);
      scan_08 : scan (lot => 8);
      scan_09 : scan (lot => 9);
      scan_10 : scan (lot => 10);
      scan_11 : scan (lot => 11);
      scan_12 : scan (lot => 12);
      scan_13 : scan (lot => 13);
      scan_14 : scan (lot => 14);
      scan_15 : scan (lot => 15);
      scan_16 : scan (lot => 16);
      scan_17 : scan (lot => 17);
      scan_18 : scan (lot => 18);
      scan_19 : scan (lot => 19);
      scan_20 : scan (lot => 20);
      scan_21 : scan (lot => 21);
      scan_22 : scan (lot => 22);
      scan_23 : scan (lot => 23);
      scan_24 : scan (lot => 24);
      scan_25 : scan (lot => 25);
      scan_26 : scan (lot => 26);
      scan_27 : scan (lot => 27);
      scan_28 : scan (lot => 28);
      scan_29 : scan (lot => 29);
      scan_30 : scan (lot => 30);
      scan_31 : scan (lot => 31);
      scan_32 : scan (lot => 32);

   begin
      TIO.Put_Line ("Scanning entire ports tree for distfiles.");
      while combined_wait loop
         delay 1.0;
         if show_progress then
            TIO.Put (scan_progress);
         end if;
         combined_wait := False;
         for j in scanners'Range loop
            if not finished (j) then
               combined_wait := True;
               exit;
            end if;
         end loop;
         if Signals.graceful_shutdown_requested then
            aborted := True;
         end if;
      end loop;

      success := not aborted;

      if success then
         --  Now that the scanners are complete, we can combine the results
         for j in scanners'Range loop
            task_storage (j).Iterate (combine'Access);
         end loop;
      end if;

   end parallel_distfile_scan;


   --------------------------------------------------------------------------------------------
   --  display_kmg
   --------------------------------------------------------------------------------------------
   function display_kmg (number : disktype) return String
   is
      type kmgtype is delta 0.01 digits 6;
      kilo : constant disktype := 1024;
      mega : constant disktype := kilo * kilo;
      giga : constant disktype := kilo * mega;
      XXX  : kmgtype;
   begin
      if number > giga then
         XXX := kmgtype (number / giga);
         return XXX'Img & " gigabytes";
      elsif number > mega then
         XXX := kmgtype (number / mega);
         return XXX'Img & " megabytes";
      else
         XXX := kmgtype (number / kilo);
         return XXX'Img & " kilobytes";
      end if;
   end display_kmg;


   --------------------------------------------------------------------------------------------
   --  purge_obsolete_distfiles
   --------------------------------------------------------------------------------------------
   procedure purge_obsolete_distfiles
   is
      procedure kill (position : portkey_crate.Cursor);
      procedure find_actual_tarballs (folder : String);

      distfiles_directory : String := Unix.true_path (HT.USS (PM.configuration.dir_distfiles));
      abort_purge  : Boolean := False;
      bytes_purged : disktype := 0;
      rmfiles      : portkey_crate.Map;

      procedure kill (position : portkey_crate.Cursor)
      is
         tarball : String := HT.USS (portkey_crate.Key (position));
         path    : String := distfiles_directory & "/" & tarball;
      begin
         TIO.Put_Line ("Deleting " & tarball);
         DIR.Delete_File (path);
      end kill;

      procedure find_actual_tarballs (folder : String)
      is
         procedure walkdir (item : DIR.Directory_Entry_Type);
         procedure print (item : DIR.Directory_Entry_Type);

         uniqid     : port_id := 0;
         leftindent : Natural := distfiles_directory'Length + 2;

         procedure print (item : DIR.Directory_Entry_Type)
         is
            FN    : constant String := DIR.Full_Name (item);
            tball : HT.Text := HT.SUS (FN (leftindent .. FN'Last));
         begin
            if not distfile_set.Contains (tball) then
               if not rmfiles.Contains (tball) then
                  uniqid := uniqid + 1;
                  rmfiles.Insert (Key => tball, New_Item => uniqid);
                  bytes_purged := bytes_purged + disktype (DIR.Size (FN));
               end if;
            end if;
         end print;

         procedure walkdir (item : DIR.Directory_Entry_Type) is
         begin
            if DIR.Simple_Name (item) /= "." and then
              DIR.Simple_Name (item) /= ".."
            then
               find_actual_tarballs (DIR.Full_Name (item));
            end if;
         exception
            when DIR.Name_Error =>
               abort_purge := True;
               TIO.Put_Line ("walkdir: " & folder & " directory does not exist");
         end walkdir;

      begin
         DIR.Search (folder, "*", (DIR.Ordinary_File => True, others => False), print'Access);
         DIR.Search (folder, "",  (DIR.Directory => True, others => False), walkdir'Access);
      exception
         when DIR.Name_Error =>
            abort_purge := True;
            TIO.Put_Line ("The " & folder & " directory does not exist");
         when DIR.Use_Error =>
            abort_purge := True;
            TIO.Put_Line ("Searching " & folder & " directory is not supported");
         when failed : others =>
            abort_purge := True;
            TIO.Put_Line ("purge_obsolete_distfiles: Unknown error - directory search");
            TIO.Put_Line (EX.Exception_Information (failed));
      end find_actual_tarballs;
   begin
      find_actual_tarballs (distfiles_directory);
      if abort_purge then
         TIO.Put_Line ("Distfile purge operation aborted.");
      else
         rmfiles.Iterate (kill'Access);
         TIO.Put_Line ("Recovered" & display_kmg (bytes_purged));
      end if;
   end purge_obsolete_distfiles;


end PortScan.Scan;
