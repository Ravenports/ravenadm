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
      conspindex      : constant String := "/Mk/Misc/" & "conspiracy_variants";
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
      Search   : DIR.Search_Type;
      Dir_Ent  : DIR.Directory_Entry_Type;
      max_lots : constant scanners := get_max_lots;
   begin
      if unkindness = PM.no_unkindness or else
        not DIR.Exists (unkindness)
      then
         return;
      end if;

      DIR.Start_Search (Search    => Search,
                        Directory => unkindness,
                        Filter    => (DIR.Directory => True, others => False),
                        Pattern   => "bucket_[0-9A-F][0-9A-F]");
      while DIR.More_Entries (Search) loop
         DIR.Get_Next_Entry (Search => Search, Directory_Entry => Dir_Ent);
         declare
            bucketdir    : constant String := DIR.Simple_Name (Dir_Ent);
            bucket       : bucket_code := bucketdir (bucketdir'Last - 1 .. bucketdir'Last);
            Inner_Search : DIR.Search_Type;
            Inner_Dirent : DIR.Directory_Entry_Type;
         begin
            DIR.Start_Search (Search    => Search,
                              Directory => unkindness & "/" & bucketdir,
                              Filter    => (DIR.Ordinary_File => True, others => False),
                              Pattern   => "*");
            while DIR.More_Entries (Inner_Search) loop
               DIR.Get_Next_Entry (Search => Inner_Search, Directory_Entry => Inner_Dirent);
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
         end;
      end loop;
      DIR.End_Search (Search);
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

      successful : Boolean := True;
      just_stop_now : Boolean;

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
               successful := False;
            else
               TIO.Put_Line ("Scan of " & origin & " failed, it will not be considered.");
            end if;
         end if;
      end scan;
   begin
      portlist.Iterate (Process => scan'Access);
      return successful;
   end scan_provided_list_of_ports;

end PortScan.Scan;
