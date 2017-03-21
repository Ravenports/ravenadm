--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Unix;
with Ada.Directories;
with Ada.Characters.Latin_1;
with PortScan.Log;
with PortScan.Buildcycle;

package body PortScan.Operations is

   package DIR renames Ada.Directories;
   package LAT renames Ada.Characters.Latin_1;
   package LOG renames PortScan.Log;
   package CYC renames PortScan.Buildcycle;

   --------------------------------------------------------------------------------------------
   --  initialize_hooks
   --------------------------------------------------------------------------------------------
   procedure initialize_hooks is
   begin
      for hook in hook_type'Range loop
         declare
            script : constant String := HT.USS (hook_location (hook));
         begin
            active_hook (hook) := DIR.Exists (script) and then file_is_executable (script);
         end;
      end loop;
   end initialize_hooks;


   --------------------------------------------------------------------------------------------
   --  run_hook
   --------------------------------------------------------------------------------------------
   procedure run_hook (hook : hook_type; envvar_list : String)
   is
      function nvpair (name : String; value : HT.Text) return String;
      function nvpair (name : String; value : HT.Text) return String is
      begin
         return
           name & LAT.Equals_Sign & LAT.Quotation & HT.USS (value) & LAT.Quotation & LAT.Space;
      end nvpair;
      common_env : constant String :=
        nvpair ("PROFILE", PM.configuration.profile) &
        nvpair ("DIR_PACKAGES", PM.configuration.dir_packages) &
        nvpair ("DIR_LOCALBASE", PM.configuration.dir_localbase) &
        nvpair ("DIR_CONSPIRACY", PM.configuration.dir_conspiracy) &
        nvpair ("DIR_CUSTOM_PORTS", PM.configuration.dir_unkindness) &
        nvpair ("DIR_DISTFILES", PM.configuration.dir_distfiles) &
        nvpair ("DIR_LOGS", PM.configuration.dir_logs) &
        nvpair ("DIR_BUILDBASE", PM.configuration.dir_buildbase);
      --  The follow command works on every platform
      command : constant String := "/usr/bin/env -i " & common_env &
        envvar_list & " " & HT.USS (hook_location (hook));
   begin
      if not active_hook (hook) then
         return;
      end if;
      if Unix.external_command (command) then
         null;
      end if;
   end run_hook;


   --------------------------------------------------------------------------------------------
   --  run_start_hook
   --------------------------------------------------------------------------------------------
   procedure run_start_hook is
   begin
      run_hook (run_start, "PORTS_QUEUED=" & HT.int2str (queue_length) & " ");
   end run_start_hook;


   --------------------------------------------------------------------------------------------
   --  run_hook_after_build
   --------------------------------------------------------------------------------------------
   procedure run_hook_after_build (built : Boolean; id : port_id) is
   begin
      if built then
         run_package_hook (pkg_success, id);
      else
         run_package_hook (pkg_failure, id);
      end if;
   end run_hook_after_build;


   --------------------------------------------------------------------------------------------
   --  run_hook_after_build
   --------------------------------------------------------------------------------------------
   procedure run_package_hook (hook : hook_type; id : port_id)
   is
      tail : String := " ORIGIN=" & get_port_variant (id);
   begin
      case hook is
         when pkg_success => run_hook (hook, "RESULT=success" & tail);
         when pkg_failure => run_hook (hook, "RESULT=failure" & tail);
         when pkg_ignored => run_hook (hook, "RESULT=ignored" & tail);
         when pkg_skipped => run_hook (hook, "RESULT=skipped" & tail);
         when others => null;
      end case;
   end run_package_hook;


   --------------------------------------------------------------------------------------------
   --  file_is_executable
   --------------------------------------------------------------------------------------------
   function file_is_executable (filename : String) return Boolean
   is
      function spit_command return String;
      function spit_command return String is
      begin
         case platform_type is
         when dragonfly | freebsd | netbsd | openbsd | macos | linux =>
            return "/usr/bin/file -b -L -e ascii -e encoding -e tar -e compress " &
                LAT.Quotation & filename & LAT.Quotation;
         when sunos =>
            return "/usr/bin/file " & LAT.Quotation & filename & LAT.Quotation;
         end case;
      end spit_command;

      status : Integer;
      cmdout : String := HT.USS (Unix.piped_command (spit_command, status));
   begin
      if status = 0 then
         return HT.contains (cmdout, "executable");
      else
         return False;
      end if;
   end file_is_executable;


   --------------------------------------------------------------------------------------------
   --  delete_existing_web_history_files
   --------------------------------------------------------------------------------------------
   procedure delete_existing_web_history_files
   is
      search    : DIR.Search_Type;
      dirent    : DIR.Directory_Entry_Type;
      pattern   : constant String := "*_history.json";
      filter    : constant DIR.Filter_Type := (DIR.Ordinary_File => True, others => False);
      reportdir : constant String := HT.USS (PM.configuration.dir_logs);
   begin
      if not DIR.Exists (reportdir) then
         return;
      end if;
      DIR.Start_Search (Search    => search,
                       Directory => reportdir,
                       Pattern   => pattern,
                       Filter    => filter);
      while DIR.More_Entries (search) loop
         DIR.Get_Next_Entry (search, dirent);
         DIR.Delete_File (reportdir & "/" & DIR.Simple_Name (dirent));
      end loop;
   exception
      when DIR.Name_Error => null;
   end delete_existing_web_history_files;


   --------------------------------------------------------------------------------------------
   --  delete_existing_packages_of_ports_list
   --------------------------------------------------------------------------------------------
   procedure delete_existing_packages_of_ports_list
   is
      procedure force_delete (plcursor : portkey_crate.Cursor);
      procedure force_delete (plcursor : portkey_crate.Cursor)
      is
         procedure delete_subpackage (position : string_crate.Cursor);

         origin : HT.Text := portkey_crate.Key (plcursor);
         pndx   : constant port_index := ports_keys.Element (origin);
         repo   : constant String := HT.USS (PM.configuration.dir_packages) & "/All/";

         procedure delete_subpackage (position : string_crate.Cursor)
         is
            tball : constant String := repo & HT.USS (all_ports (pndx).port_namebase) &
              LAT.Hyphen & HT.USS (string_crate.Element (position)) & LAT.Hyphen &
              HT.USS (all_ports (pndx).port_variant) & LAT.Hyphen &
              HT.USS (all_ports (pndx).pkgversion) & ".txz";
         begin
            if DIR.Exists (tball) then
               DIR.Delete_File (tball);
            end if;
         end delete_subpackage;

      begin
         all_ports (pndx).subpackages.Iterate (delete_subpackage'Access);
      end force_delete;

   begin
      portlist.Iterate (Process => force_delete'Access);
   end delete_existing_packages_of_ports_list;


   --------------------------------------------------------------------------------------------
   --  next_ignored_port
   --------------------------------------------------------------------------------------------
   function next_ignored_port return port_id
   is
      list_len : constant Integer := Integer (rank_queue.Length);
      cursor   : ranking_crate.Cursor;
      QR       : queue_record;
      result   : port_id := port_match_failed;
   begin
      if list_len = 0 then
         return result;
      end if;
      cursor := rank_queue.First;
      for k in 1 .. list_len loop
         QR := ranking_crate.Element (Position => cursor);
         if all_ports (QR.ap_index).ignored then
            result := QR.ap_index;
            DPY.insert_history (CYC.assemble_history_record (1, QR.ap_index, DPY.action_ignored));
            run_package_hook (pkg_ignored, QR.ap_index);
            exit;
         end if;
         cursor := ranking_crate.Next (Position => cursor);
      end loop;
      return result;
   end next_ignored_port;


   --------------------------------------------------------------------------------------------
   --  assimulate_substring
   --------------------------------------------------------------------------------------------
   procedure assimulate_substring (history : in out progress_history; substring : String)
   is
      first : constant Positive := history.last_index + 1;
      last  : constant Positive := history.last_index + substring'Length;
   begin
      --  silently fail (this shouldn't be practically possible)
      if last < kfile_content'Last then
         history.content (first .. last) := substring;
      end if;
      history.last_index := last;
   end assimulate_substring;


   --------------------------------------------------------------------------------------------
   --  nv #1
   --------------------------------------------------------------------------------------------
   function nv (name, value : String) return String is
   begin
      return
        LAT.Quotation & name & LAT.Quotation & LAT.Colon &
        LAT.Quotation & value & LAT.Quotation;
   end nv;


   --------------------------------------------------------------------------------------------
   --  nv #2
   --------------------------------------------------------------------------------------------
   function nv (name : String; value : Integer) return String is
   begin
      return LAT.Quotation & name & LAT.Quotation & LAT.Colon & HT.int2str (value);
   end nv;


   --------------------------------------------------------------------------------------------
   --  handle_first_history_entry
   --------------------------------------------------------------------------------------------
   procedure handle_first_history_entry is
   begin
      if history.segment_count = 1 then
         assimulate_substring (history, "[" & LAT.LF & " {" & LAT.LF);
      else
         assimulate_substring (history, " ,{" & LAT.LF);
      end if;
   end handle_first_history_entry;


   --------------------------------------------------------------------------------------------
   --  write_history_json
   --------------------------------------------------------------------------------------------
   procedure write_history_json
   is
      jsonfile : TIO.File_Type;
      filename : constant String := HT.USS (PM.configuration.dir_logs) &
                 "/" & HT.zeropad (history.segment, 2) & "_history.json";
   begin
      if history.segment_count = 0 then
         return;
      end if;
      if history.last_written = history.last_index then
         return;
      end if;
      TIO.Create (File => jsonfile,
                  Mode => TIO.Out_File,
                  Name => filename);
      TIO.Put (jsonfile, history.content (1 .. history.last_index));
      TIO.Put (jsonfile, "]");
      TIO.Close (jsonfile);
      history.last_written := history.last_index;
   exception
      when others =>
         if TIO.Is_Open (jsonfile) then
            TIO.Close (jsonfile);
         end if;
   end write_history_json;


   --------------------------------------------------------------------------------------------
   --  check_history_segment_capacity
   --------------------------------------------------------------------------------------------
   procedure check_history_segment_capacity is
   begin
      if history.segment_count = 1 then
         history.segment := history.segment + 1;
         return;
      end if;
      if history.segment_count < kfile_units_limit then
         return;
      end if;
      write_history_json;

      history.last_index    := 0;
      history.last_written  := 0;
      history.segment_count := 0;
   end check_history_segment_capacity;


   --------------------------------------------------------------------------------------------
   --  record_history_ignored
   --------------------------------------------------------------------------------------------
   procedure record_history_ignored
     (elapsed   : String;
      origin    : String;
      reason    : String;
      skips     : Natural)
   is
      cleantxt : constant String := HT.strip_control (reason);
      info : constant String :=
        HT.replace_char
          (HT.replace_char (cleantxt, LAT.Quotation, "&nbsp;"), LAT.Reverse_Solidus, "&#92;")
          & ":|:" & HT.int2str (skips);
   begin
      history.log_entry := history.log_entry + 1;
      history.segment_count := history.segment_count + 1;
      handle_first_history_entry;

      assimulate_substring (history, "   " & nv ("entry", history.log_entry) & LAT.LF);
      assimulate_substring (history, "  ," & nv ("elapsed", elapsed) & LAT.LF);
      assimulate_substring (history, "  ," & nv ("ID", "--") & LAT.LF);
      assimulate_substring (history, "  ," & nv ("result", "ignored") & LAT.LF);
      assimulate_substring (history, "  ," & nv ("origin", origin) & LAT.LF);
      assimulate_substring (history, "  ," & nv ("info", info) & LAT.LF);
      assimulate_substring (history, "  ," & nv ("duration", "--:--:--") & LAT.LF);
      assimulate_substring (history, " }" & LAT.LF);

      check_history_segment_capacity;
   end record_history_ignored;


   --------------------------------------------------------------------------------------------
   --  record_history_skipped
   --------------------------------------------------------------------------------------------
   procedure record_history_skipped
     (elapsed   : String;
      origin    : String;
      reason    : String)
   is
   begin
      history.log_entry := history.log_entry + 1;
      history.segment_count := history.segment_count + 1;
      handle_first_history_entry;

      assimulate_substring (history, "   " & nv ("entry", history.log_entry) & LAT.LF);
      assimulate_substring (history, "  ," & nv ("elapsed", elapsed) & LAT.LF);
      assimulate_substring (history, "  ," & nv ("ID", "--") & LAT.LF);
      assimulate_substring (history, "  ," & nv ("result", "skipped") & LAT.LF);
      assimulate_substring (history, "  ," & nv ("origin", origin) & LAT.LF);
      assimulate_substring (history, "  ," & nv ("info", reason) & LAT.LF);
      assimulate_substring (history, "  ," & nv ("duration", "--:--:--") & LAT.LF);
      assimulate_substring (history, " }" & LAT.LF);

      check_history_segment_capacity;
   end record_history_skipped;


   --------------------------------------------------------------------------------------------
   --  skip_verified
   --------------------------------------------------------------------------------------------
   function skip_verified (id : port_id) return Boolean is
   begin
      if id = port_match_failed then
         return False;
      end if;
      return not all_ports (id).unlist_failed;
   end skip_verified;


   --------------------------------------------------------------------------------------------
   --  delete_rank
   --------------------------------------------------------------------------------------------
   procedure delete_rank (id : port_id)
   is
      rank_cursor : ranking_crate.Cursor := rank_arrow (id);
      use type ranking_crate.Cursor;
   begin
      if rank_cursor /= ranking_crate.No_Element then
         rank_queue.Delete (Position => rank_cursor);
      end if;
   end delete_rank;


   --------------------------------------------------------------------------------------------
   --  still_ranked
   --------------------------------------------------------------------------------------------
   function still_ranked (id : port_id) return Boolean
   is
      rank_cursor : ranking_crate.Cursor := rank_arrow (id);
      use type ranking_crate.Cursor;
   begin
      return rank_cursor /= ranking_crate.No_Element;
   end still_ranked;


   --------------------------------------------------------------------------------------------
   --  unlist_port
   --------------------------------------------------------------------------------------------
   procedure unlist_port (id : port_id) is
   begin
      if id = port_match_failed then
         return;
      end if;
      if still_ranked (id) then
         delete_rank (id);
      else
         --  don't raise exception.  Since we don't prune all_reverse as
         --  we go, there's no guarantee the reverse dependency hasn't already
         --  been removed (e.g. when it is a common reverse dep)
         all_ports (id).unlist_failed := True;
      end if;
   end unlist_port;


   --------------------------------------------------------------------------------------------
   --  rank_arrow
   --------------------------------------------------------------------------------------------
   function rank_arrow (id : port_id) return ranking_crate.Cursor
   is
      rscore : constant port_index := all_ports (id).reverse_score;
      seek_target : constant queue_record := (ap_index      => id,
                                              reverse_score => rscore);
   begin
      return rank_queue.Find (seek_target);
   end rank_arrow;


   --------------------------------------------------------------------------------------------
   --  skip_next_reverse_dependency
   --------------------------------------------------------------------------------------------
   function skip_next_reverse_dependency (pinnacle : port_id) return port_id
   is
      rev_cursor : block_crate.Cursor;
      next_dep : port_index;
   begin
      if all_ports (pinnacle).all_reverse.Is_Empty then
         return port_match_failed;
      end if;
      rev_cursor := all_ports (pinnacle).all_reverse.First;
      next_dep := block_crate.Element (rev_cursor);
      unlist_port (id => next_dep);
      all_ports (pinnacle).all_reverse.Delete (rev_cursor);

      return next_dep;
   end skip_next_reverse_dependency;


   --------------------------------------------------------------------------------------------
   --  cascade_failed_build
   --------------------------------------------------------------------------------------------
   procedure cascade_failed_build (id : port_id; numskipped : out Natural)
   is
      purged  : PortScan.port_id;
      culprit : constant String := get_port_variant (id);
   begin
      numskipped := 0;
      loop
         purged := skip_next_reverse_dependency (id);
         exit when purged = port_match_failed;
         if skip_verified (purged) then
            numskipped := numskipped + 1;
            LOG.scribe (PortScan.total, "           Skipped: " & get_port_variant (purged), False);
            LOG.scribe (PortScan.skipped, get_port_variant (purged) & " by " & culprit, False);
            DPY.insert_history (CYC.assemble_history_record (1, purged, DPY.action_skipped));
            record_history_skipped (elapsed => LOG.elapsed_now,
                                    origin  => get_port_variant (purged),
                                    reason  => culprit);
            run_package_hook (pkg_skipped, purged);
         end if;
      end loop;
      unlist_port (id);
   end cascade_failed_build;


   --------------------------------------------------------------------------------------------
   --  integrity_intact
   --------------------------------------------------------------------------------------------
   function integrity_intact return Boolean
   is
      procedure check_dep (cursor : block_crate.Cursor);
      procedure check_rank (cursor : ranking_crate.Cursor);

      intact : Boolean := True;
      procedure check_dep (cursor : block_crate.Cursor)
      is
         did : constant port_index := block_crate.Element (cursor);
      begin
         if not still_ranked (did) then
            intact := False;
         end if;
      end check_dep;

      procedure check_rank (cursor : ranking_crate.Cursor)
      is
         QR : constant queue_record := ranking_crate.Element (cursor);
      begin
         if intact then
            all_ports (QR.ap_index).blocked_by.Iterate (check_dep'Access);
         end if;
      end check_rank;
   begin
      rank_queue.Iterate (check_rank'Access);
      return intact;
   end integrity_intact;


   --------------------------------------------------------------------------------------------
   --  limited_cached_options_check
   --------------------------------------------------------------------------------------------
   function limited_cached_options_check return Boolean
   is
      --  TODO: Implement options
   begin
      return True;
   end limited_cached_options_check;


   --------------------------------------------------------------------------------------------
   --  located_external_repository
   --------------------------------------------------------------------------------------------
   function located_external_repository return Boolean
   is
      command : constant String := host_pkg8 & " -vv";
      found   : Boolean := False;
      inspect : Boolean := False;
      status  : Integer;
   begin
      declare
         dump    : String := HT.USS (Unix.piped_command (command, status));
         markers : HT.Line_Markers;
         linenum : Natural := 0;
      begin
         if status /= 0 then
            return False;
         end if;
         HT.initialize_markers (dump, markers);
         loop
            exit when not HT.next_line_present (dump, markers);
            declare
               line : constant String := HT.extract_line (dump, markers);
               len  : constant Natural := line'Length;
            begin
               if inspect then
                  if len > 7 and then
                    line (1 .. 2) = "  " and then
                    line (len - 3 .. len) = ": { " and then
                    line (3 .. len - 4) /= "ravenadm"
                  then
                     found := True;
                     external_repository := HT.SUS (line (3 .. len - 4));
                     exit;
                  end if;
               else
                  if line = "Repositories:" then
                     inspect := True;
                  end if;
               end if;
            end;
         end loop;
      end;
      return found;
   end located_external_repository;


   --------------------------------------------------------------------------------------------
   --  top_external_repository
   --------------------------------------------------------------------------------------------
   function top_external_repository return String is
   begin
      return HT.USS (external_repository);
   end top_external_repository;


end PortScan.Operations;
