--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Directories;
with Ada.Calendar;
with File_Operations;
with Port_Specification;
with Specification_Parser;
with PortScan.Log;
with Parameters;
with Unix;

package body PortScan.Scan is

   package PM  renames Parameters;
   package LOG renames PortScan.Log;
   package FOP renames File_Operations;
   package PAR renames Specification_Parser;
   package PSP renames Port_Specification;
   package DIR renames Ada.Directories;
   package CAL renames Ada.Calendar;

   --------------------------------------------------------------------------------------------
   --  scan_entire_ports_tree
   --------------------------------------------------------------------------------------------
   function scan_entire_ports_tree (conspiracy, unkindness : String) return Boolean
   is
      good_scan    : Boolean;
      using_screen : constant Boolean := Unix.screen_attached;
   begin
      --  tree must be already mounted in the scan slave.
      --  However, prescan works on the real ports tree, not the mount.
      if not prescanned then
         prescan_ports_tree (conspiracy, unkindness);
      end if;
      prescan_unkindness (unkindness);
      LOG.set_scan_start_time (CAL.Clock);
      parallel_deep_scan (success => good_scan, show_progress => using_screen);
      LOG.set_scan_complete (CAL.Clock);

      return good_scan;
   end scan_entire_ports_tree;


   --------------------------------------------------------------------------------------------
   --  prescan_ports_tree
   --------------------------------------------------------------------------------------------
   procedure prescan_ports_tree (conspiracy, unkindness : String)
   is
      conspindex      : constant String := "conspiracy_index";
      conspindex_path : constant String := conspiracy & "/Mk/" & conspindex;
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
   procedure prescan_custom (unkindness, buildsheet : String; max_lots : scanners)
   is
      --  Assume buildsheet exists (has already been validated)
      --  Assume it starts with directory separator
      successful : Boolean;
      customspec : PSP.Portspecs;
      arch_focus : supported_arch := x86_64;  -- unused, pick one
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
         namebase : String  := customspec.get_namebase;
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
   --  walk_the_bucket
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
                  bsheet : constant String := "/" & bucketdir & DIR.Simple_Name (Inner_Dirent);
               begin
                  prescan_custom (unkindness, bsheet, max_lots);
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

end PortScan.Scan;
