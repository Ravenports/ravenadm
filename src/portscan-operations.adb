--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Unix;
with Signals;
with Replicant;
with Ada.Exceptions;
with Ada.Directories;
with Ada.Characters.Latin_1;
with Ada.Numerics.Discrete_Random;
with PortScan.Log;
with PortScan.Buildcycle;
with Specification_Parser;
with Port_Specification.Makefile;
with Port_Specification.Transform;
with INI_File_Manager;
with Options_Dialog;

package body PortScan.Operations is

   package EX  renames Ada.Exceptions;
   package DIR renames Ada.Directories;
   package LAT renames Ada.Characters.Latin_1;
   package LOG renames PortScan.Log;
   package CYC renames PortScan.Buildcycle;
   package REP renames Replicant;
   package PAR renames Specification_Parser;
   package PSM renames Port_Specification.Makefile;
   package PST renames Port_Specification.Transform;
   package IFM renames INI_File_Manager;
   package DLG renames Options_Dialog;

   --------------------------------------------------------------------------------------------
   --  parallel_bulk_run
   --------------------------------------------------------------------------------------------
   procedure parallel_bulk_run (num_builders : builders; sysrootver : sysroot_characteristics)
   is
      subtype cycle_count   is Natural range 1 .. 9;
      subtype refresh_count is Natural range 1 .. 4;
      subtype www_count     is Natural range 1 .. 3;
      subtype alert_count   is Natural range 1 .. 200;

      procedure text_display (builder : builders; info : String);
      procedure common_display (flavor : count_type; info : String);
      procedure slave_display (flavor : count_type; builder : builders; info : String);
      function slave_name (slave : builders) return String;

      instructions   : dim_instruction   := (others => port_match_failed);
      builder_states : dim_builder_state := (others => idle);
      cntcycle       : cycle_count       := cycle_count'First;
      cntrefresh     : refresh_count     := refresh_count'First;
      cntalert       : alert_count       := alert_count'First;
      cntwww         : www_count         := www_count'First;
      run_complete   : Boolean           := False;
      available      : Positive          := Integer (num_builders);
      target         : port_id;
      all_idle       : Boolean;
      cntskip        : Natural;
      sumdata        : DPY.summary_rec;

      procedure text_display (builder : builders; info : String) is
      begin
         TIO.Put_Line
           (LOG.elapsed_now & " => [" & HT.zeropad (Integer (builder), 2) & "] " & info);
      end text_display;

      procedure slave_display (flavor : count_type; builder : builders; info : String)
      is
         slavid : constant String := HT.zeropad (Integer (builder), 2);
      begin
         LOG.scribe
           (flavor, LOG.elapsed_now & " [" & slavid & "] => " & info, False);
      end slave_display;

      procedure common_display (flavor : count_type; info : String) is
      begin
         LOG.scribe (flavor, LOG.elapsed_now & " " & info, False);
      end common_display;

      function slave_name (slave : builders) return String is
      begin
         return get_port_variant (instructions (slave));
      end slave_name;

      task type build (builder : builders);
      task body build
      is
         type Rand_Draw is range 1 .. 20;
         package Rand20 is new Ada.Numerics.Discrete_Random (Rand_Draw);
         seed : Rand20.Generator;
         build_result : Boolean;
         need_procfs  : Boolean;
      begin
         if builder <= num_builders then
            if not curses_support then
               text_display (builder, "Builder launched");
            end if;
            loop
               exit when builder_states (builder) = shutdown;
               if builder_states (builder) = tasked then
                  builder_states (builder) := busy;
                  need_procfs := all_ports (instructions (builder)).use_procfs;
                  REP.launch_slave (builder, need_procfs);
                  build_result := build_subpackages (builder, instructions (builder), sysrootver);
                  REP.destroy_slave (builder, need_procfs);
                  if build_result then
                     builder_states (builder) := done_success;
                  else
                     builder_states (builder) := done_failure;
                  end if;
               else
                  --  idle or done-(failure|success), just wait a bit
                  delay 0.1;
               end if;
            end loop;
            if not curses_support then
               text_display (builder, "         Shutting down");
            end if;
         end if;
      end build;

      builder_01 : build (builder => 1);
      builder_02 : build (builder => 2);
      builder_03 : build (builder => 3);
      builder_04 : build (builder => 4);
      builder_05 : build (builder => 5);
      builder_06 : build (builder => 6);
      builder_07 : build (builder => 7);
      builder_08 : build (builder => 8);
      builder_09 : build (builder => 9);
      builder_10 : build (builder => 10);
      builder_11 : build (builder => 11);
      builder_12 : build (builder => 12);
      builder_13 : build (builder => 13);
      builder_14 : build (builder => 14);
      builder_15 : build (builder => 15);
      builder_16 : build (builder => 16);
      builder_17 : build (builder => 17);
      builder_18 : build (builder => 18);
      builder_19 : build (builder => 19);
      builder_20 : build (builder => 20);
      builder_21 : build (builder => 21);
      builder_22 : build (builder => 22);
      builder_23 : build (builder => 23);
      builder_24 : build (builder => 24);
      builder_25 : build (builder => 25);
      builder_26 : build (builder => 26);
      builder_27 : build (builder => 27);
      builder_28 : build (builder => 28);
      builder_29 : build (builder => 29);
      builder_30 : build (builder => 30);
      builder_31 : build (builder => 31);
      builder_32 : build (builder => 32);
      builder_33 : build (builder => 33);
      builder_34 : build (builder => 34);
      builder_35 : build (builder => 35);
      builder_36 : build (builder => 36);
      builder_37 : build (builder => 37);
      builder_38 : build (builder => 38);
      builder_39 : build (builder => 39);
      builder_40 : build (builder => 40);
      builder_41 : build (builder => 41);
      builder_42 : build (builder => 42);
      builder_43 : build (builder => 43);
      builder_44 : build (builder => 44);
      builder_45 : build (builder => 45);
      builder_46 : build (builder => 46);
      builder_47 : build (builder => 47);
      builder_48 : build (builder => 48);
      builder_49 : build (builder => 49);
      builder_50 : build (builder => 50);
      builder_51 : build (builder => 51);
      builder_52 : build (builder => 52);
      builder_53 : build (builder => 53);
      builder_54 : build (builder => 54);
      builder_55 : build (builder => 55);
      builder_56 : build (builder => 56);
      builder_57 : build (builder => 57);
      builder_58 : build (builder => 58);
      builder_59 : build (builder => 59);
      builder_60 : build (builder => 60);
      builder_61 : build (builder => 61);
      builder_62 : build (builder => 62);
      builder_63 : build (builder => 63);
      builder_64 : build (builder => 64);

   begin
      loop
         all_idle := True;
         for slave in 1 .. num_builders loop
            begin
               case builder_states (slave) is
               when busy | tasked =>
                  all_idle := False;
               when shutdown =>
                  null;
               when idle =>
                  if run_complete then
                     builder_states (slave) := shutdown;
                  else
                     target := top_buildable_port;
                     if target = port_match_failed then
                        if Signals.graceful_shutdown_requested or else
                          nothing_left (num_builders)
                        then
                           run_complete := True;
                           builder_states (slave) := shutdown;
                           if curses_support then
                              DPY.insert_history
                                (CYC.assemble_history_record (slave, 0, DPY.action_shutdown));
                           end if;
                        else
                           if shutdown_recommended (available) then
                              builder_states (slave) := shutdown;
                              if curses_support then
                                 DPY.insert_history
                                   (CYC.assemble_history_record (slave, 0, DPY.action_shutdown));
                              end if;
                              available := available - 1;
                           end if;
                        end if;
                     else
                        lock_package (target);
                        instructions (slave) := target;
                        builder_states (slave) := tasked;
                        slave_display (total, slave, slave_name (slave));
                        if not curses_support then
                           text_display (slave, "         Kickoff " & slave_name (slave));
                        end if;
                     end if;
                  end if;
               when done_success | done_failure =>
                  all_idle := False;
                  if builder_states (slave) = done_success then
                     if curses_support then
                        DPY.insert_history
                          (CYC.assemble_history_record
                             (slave, instructions (slave), DPY.action_success));
                     else
                        text_display
                          (slave, CYC.elapsed_build (slave) & " Success " & slave_name (slave));
                     end if;
                     record_history_built (elapsed   => LOG.elapsed_now,
                                           slave_id  => slave,
                                           origin    => slave_name (slave),
                                           duration  => CYC.elapsed_build (slave));
                     run_package_hook (pkg_success, instructions (slave));
                     cascade_successful_build (instructions (slave));
                     LOG.increment_build_counter (success);
                     common_display (success, slave_name (slave));
                     common_display (total, slave_name (slave) & " success");
                  else
                     common_display (total, slave_name (slave) & " FAILED!");
                     cascade_failed_build (instructions (slave), cntskip);
                     LOG.increment_build_counter (skipped, cntskip);
                     LOG.increment_build_counter (failure);
                     common_display (total, slave_name (slave) & " failure skips:" & cntskip'Img);
                     common_display
                       (failure, slave_name (slave) & " (skipped" & cntskip'Img & ")");
                     if curses_support then
                        DPY.insert_history
                          (CYC.assemble_history_record
                             (slave, instructions (slave), DPY.action_failure));
                     else
                        text_display
                          (slave, CYC.elapsed_build (slave) & " Failure " & slave_name (slave));
                     end if;
                     record_history_failed
                       (elapsed   => LOG.elapsed_now,
                        slave_id  => slave,
                        origin    => slave_name (slave),
                        duration  => CYC.elapsed_build (slave),
                        die_phase => CYC.last_build_phase (slave),
                        skips     => cntskip);
                     run_package_hook (pkg_failure, instructions (slave));
                  end if;
                  instructions (slave) := port_match_failed;
                  if run_complete then
                     builder_states (slave) := shutdown;
                     if curses_support then
                        DPY.insert_history
                          (CYC.assemble_history_record (slave, 0, DPY.action_shutdown));
                     end if;
                  else
                     builder_states (slave) := idle;
                  end if;
               end case;
            exception
               when earthquake : others =>
                    LOG.scribe (total, LOG.elapsed_now & " UNHANDLED EXCEPTION: " &
                       EX.Exception_Information (earthquake), False);
            end;
         end loop;
         exit when run_complete and all_idle;
         if cntcycle = cycle_count'Last then
            cntcycle := cycle_count'First;
            LOG.flush_log (success);
            LOG.flush_log (failure);
            LOG.flush_log (skipped);
            LOG.flush_log (total);
            if curses_support then
               if cntrefresh = refresh_count'Last then
                  cntrefresh := refresh_count'First;
                  DPY.set_full_redraw_next_update;
               else
                  cntrefresh := cntrefresh + 1;
               end if;
               sumdata.Initially := LOG.port_counter_value (total);
               sumdata.Built     := LOG.port_counter_value (success);
               sumdata.Failed    := LOG.port_counter_value (failure);
               sumdata.Ignored   := LOG.port_counter_value (ignored);
               sumdata.Skipped   := LOG.port_counter_value (skipped);
               sumdata.elapsed   := LOG.elapsed_now;
               sumdata.swap      := get_swap_status;
               sumdata.load      := CYC.load_core (True);
               sumdata.pkg_hour  := LOG.hourly_build_rate;
               sumdata.impulse   := LOG.impulse_rate;
               DPY.summarize (sumdata);

               for b in builders'First .. num_builders loop
                     if builder_states (b) = shutdown then
                        DPY.update_builder (CYC.builder_status (b, True, False));
                     elsif builder_states (b) = idle then
                        DPY.update_builder (CYC.builder_status (b, False, True));
                     else
                        CYC.set_log_lines (b);
                        DPY.update_builder (CYC.builder_status (b));
                     end if;
               end loop;
               DPY.refresh_builder_window;
               DPY.refresh_history_window;
            else
               --  text mode support, periodic status reports
               if cntalert = alert_count'Last then
                  cntalert := alert_count'First;
                  TIO.Put_Line (LOG.elapsed_now & " =>    " &
                                  "  Left:" & LOG.ports_remaining_to_build'Img &
                                  "  Succ:" & LOG.port_counter_value (success)'Img &
                                  "  Fail:" & LOG.port_counter_value (failure)'Img &
                                  "  Skip:" & LOG.port_counter_value (skipped)'Img &
                                  "   Ign:" & LOG.port_counter_value (ignored)'Img);
               else
                  cntalert := cntalert + 1;
               end if;

               --  Update log lines every 4 seconds for the watchdog
               if cntrefresh = refresh_count'Last then
                  cntrefresh := refresh_count'First;
                  for b in builders'First .. num_builders loop
                     if builder_states (b) /= shutdown and then
                       builder_states (b) /= idle
                     then
                        CYC.set_log_lines (b);
                     end if;
                  end loop;
               else
                  cntrefresh := cntrefresh + 1;
               end if;
            end if;

            --  Generate latest history file every 3 seconds.
            --  With a poll period of 6 seconds, we need twice that frequency to avoid aliasing
            --  Note that in text mode, the logs are updated every 4 seconds, so in this mode
            --  the log lines will often be identical for a cycle.
            if cntwww = www_count'Last then
               cntwww := www_count'First;
               write_history_json;
               write_summary_json (active            => True,
                                   states            => builder_states,
                                   num_builders      => num_builders,
                                   num_history_files => history.segment);
            else
               cntwww := cntwww + 1;
            end if;
         else
            cntcycle := cntcycle + 1;
         end if;
         delay 0.10;
      end loop;
      if PM.configuration.avec_ncurses and then curses_support
      then
         DPY.terminate_monitor;
      end if;
      write_history_json;
      write_summary_json (active            => False,
                          states            => builder_states,
                          num_builders      => num_builders,
                          num_history_files => history.segment);
      run_hook (run_end,
                  "PORTS_BUILT=" & HT.int2str (LOG.port_counter_value (success)) &
                  " PORTS_FAILED=" & HT.int2str (LOG.port_counter_value (failure)) &
                  " PORTS_IGNORED=" & HT.int2str (LOG.port_counter_value (ignored)) &
                  " PORTS_SKIPPED=" & HT.int2str (LOG.port_counter_value (skipped)));
   end parallel_bulk_run;


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
   --  run_package_hook
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
      status  : Integer;
      sysroot : constant String := HT.USS (PM.configuration.dir_sysroot);
      command : constant String := sysroot & "/usr/bin/file -m " & sysroot &
                                   "/usr/share/file/magic.mgc -b " & sysroot & "/bin/sh";
      cmdout  : String := HT.USS (Unix.piped_command (command, status));
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
      procedure force_delete (plcursor : string_crate.Cursor);

      compkey  : HT.Text := HT.SUS (default_compiler & LAT.Colon & variant_standard);
      compiler : constant port_index := ports_keys.Element (compkey);

      procedure force_delete (plcursor : string_crate.Cursor)
      is
         procedure delete_subpackage (position : subpackage_crate.Cursor);

         origin : HT.Text := string_crate.Element (plcursor);
         pndx   : constant port_index := ports_keys.Element (origin);
         repo   : constant String := HT.USS (PM.configuration.dir_repository) & "/";

         procedure delete_subpackage (position : subpackage_crate.Cursor)
         is
            rec        : subpackage_record renames subpackage_crate.Element (position);
            subpackage : constant String := HT.USS (rec.subpackage);
            tball      : constant String := repo &
                         PortScan.calculate_package_name (pndx, subpackage) & arc_ext;
         begin
            --  Never delete the port compiler's packages
            if pndx /= compiler then
               if DIR.Exists (tball) then
                  DIR.Delete_File (tball);
               end if;
            end if;
         end delete_subpackage;

      begin
         all_ports (pndx).subpackages.Iterate (delete_subpackage'Access);
      end force_delete;

   begin
      portlist.Iterate (Process => force_delete'Access);
   end delete_existing_packages_of_ports_list;


   --------------------------------------------------------------------------------------------
   --  list_subpackages_of_queued_ports
   --------------------------------------------------------------------------------------------
   procedure list_subpackages_of_queued_ports
   is
      procedure list (plcursor : string_crate.Cursor);
      procedure list (plcursor : string_crate.Cursor)
      is
         procedure name (position : subpackage_crate.Cursor);
         origin : HT.Text renames string_crate.Element (plcursor);
         pndx   : constant port_index := ports_keys.Element (origin);
         procedure name (position : subpackage_crate.Cursor)
         is
            rec        : subpackage_record renames subpackage_crate.Element (position);
            subpackage : constant String := HT.USS (rec.subpackage);
         begin
            TIO.Put (" " & subpackage);
         end name;
      begin
         TIO.Put (HT.USS (origin) & " subpackages:");
         all_ports (pndx).subpackages.Iterate (name'Access);
         TIO.Put_Line ("");
      end list;
   begin
      portlist.Iterate (list'Access);
   end list_subpackages_of_queued_ports;


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
            DPY.insert_history
              (CYC.assemble_history_record (1, QR.ap_index, DPY.action_ignored));
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
   --  record_history_built
   --------------------------------------------------------------------------------------------
   procedure record_history_built
     (elapsed   : String;
      slave_id  : builders;
      origin    : String;
      duration  : String)
   is
      ID : constant String := HT.zeropad (Integer (slave_id), 2);
   begin
      history.log_entry := history.log_entry + 1;
      history.segment_count := history.segment_count + 1;
      handle_first_history_entry;

      assimulate_substring (history, "   " & nv ("entry", history.log_entry) & LAT.LF);
      assimulate_substring (history, "  ," & nv ("elapsed", elapsed) & LAT.LF);
      assimulate_substring (history, "  ," & nv ("ID", ID) & LAT.LF);
      assimulate_substring (history, "  ," & nv ("result", "built") & LAT.LF);
      assimulate_substring (history, "  ," & nv ("origin", origin) & LAT.LF);
      assimulate_substring (history, "  ," & nv ("info", "") & LAT.LF);
      assimulate_substring (history, "  ," & nv ("duration", duration) & LAT.LF);
      assimulate_substring (history, " }" & LAT.LF);

      check_history_segment_capacity;
   end record_history_built;


   --------------------------------------------------------------------------------------------
   --  record_history_built
   --------------------------------------------------------------------------------------------
   procedure record_history_failed
     (elapsed   : String;
      slave_id  : builders;
      origin    : String;
      duration  : String;
      die_phase : String;
      skips     : Natural)
   is
      info : constant String := die_phase & ":" & HT.int2str (skips);
      ID   : constant String := HT.zeropad (Integer (slave_id), 2);
   begin
      history.log_entry := history.log_entry + 1;
      history.segment_count := history.segment_count + 1;
      handle_first_history_entry;

      assimulate_substring (history, "   " & nv ("entry", history.log_entry) & LAT.LF);
      assimulate_substring (history, "  ," & nv ("elapsed", elapsed) & LAT.LF);
      assimulate_substring (history, "  ," & nv ("ID", ID)  & LAT.LF);
      assimulate_substring (history, "  ," & nv ("result", "failed") & LAT.LF);
      assimulate_substring (history, "  ," & nv ("origin", origin) & LAT.LF);
      assimulate_substring (history, "  ," & nv ("info", info) & LAT.LF);
      assimulate_substring (history, "  ," & nv ("duration", duration) & LAT.LF);
      assimulate_substring (history, " }" & LAT.LF);

      check_history_segment_capacity;
   end record_history_failed;


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
   --  unlist_first_port
   --------------------------------------------------------------------------------------------
   function unlist_first_port return port_id
   is
      origin : HT.Text := string_crate.Element (portlist.First);
      id     : port_id;
   begin
      if ports_keys.Contains (origin) then
         id := ports_keys.Element (origin);
      else
         return port_match_failed;
      end if;

      if id = port_match_failed then
         return port_match_failed;
      end if;
      delete_rank (id);
      return id;
   end unlist_first_port;


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
   --  cascade_successful_build
   --------------------------------------------------------------------------------------------
   procedure cascade_successful_build (id : port_id)
   is
      procedure cycle (cursor : block_crate.Cursor);
      procedure cycle (cursor : block_crate.Cursor)
      is
         target : port_index renames block_crate.Element (cursor);
      begin
         if all_ports (target).blocked_by.Contains (id) then
            all_ports (target).blocked_by.Delete (id);
         else
            raise seek_failure
              with get_port_variant (target) & " was expected to be blocked by " &
              get_port_variant (id);
         end if;
      end cycle;
   begin
      all_ports (id).blocks.Iterate (cycle'Access);
      delete_rank (id);
   end cascade_successful_build;


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


   --------------------------------------------------------------------------------------------
   --  isolate_arch_from_file_type
   --------------------------------------------------------------------------------------------
   function isolate_arch_from_file_type (fileinfo : String) return filearch
   is
      --  DF: ELF 64-bit LSB executable, x86-64
      --  FB: ELF 64-bit LSB executable, x86-64
      --  FB: ELF 32-bit LSB executable, Intel 80386
      --  NB: ELF 64-bit LSB executable, x86-64
      --   L: ELF 64-bit LSB executable, x86-64
      --  NATIVE Solaris (we use our own file)
      --  /usr/bin/sh:    ELF 64-bit LSB executable AMD64 Version 1

      fragment : constant String := HT.trim (HT.specific_field (fileinfo, 2, ","));
      answer   : filearch := (others => ' ');
   begin
      if fragment'Length > filearch'Length then
         answer := fragment (fragment'First .. fragment'First + filearch'Length - 1);
      else
         answer (answer'First .. answer'First + fragment'Length - 1) := fragment;
      end if;
      return answer;
   end isolate_arch_from_file_type;


   --------------------------------------------------------------------------------------------
   --  establish_package_architecture
   --------------------------------------------------------------------------------------------
   procedure establish_package_architecture
   is
      function newsuffix   (arch : filearch) return String;
      function suffix      (arch : filearch) return String;
      function get_major   (fileinfo : String; OS : String) return String;
      function even        (fileinfo : String) return String;
      function get_version (fileinfo : String; OS : String) return String;
      procedure craft_common_endings (release : String);

      sysroot : constant String := HT.USS (PM.configuration.dir_sysroot);
      command : constant String := sysroot & "/usr/bin/file -m " & sysroot &
                                   "/usr/share/file/magic.mgc -b " & sysroot & "/bin/sh";
      status  : Integer;
      arch    : filearch;
      UN      : HT.Text;

      function suffix (arch : filearch) return String is
      begin
         if arch (arch'First .. arch'First + 5) = "x86-64" or else
           arch (arch'First .. arch'First + 4) = "AMD64"
         then
            return "x86:64";
         elsif arch = "Intel 80386" then
            return "x86:32";
         elsif arch = "ARM aarch64" then
            return "aarch64:64";
         elsif arch = "ARM, EABI5 " then
            return "armv6:32:el:eabi:softfp";
         else
            return "unknown:" & arch;
         end if;
      end suffix;

      function newsuffix (arch : filearch) return String is
      begin
         if arch (arch'First .. arch'First + 5) = "x86-64" or else
           arch (arch'First .. arch'First + 4) = "AMD64"
         then
            return "amd64";
         elsif arch = "Intel 80386" then
            return "i386";
         elsif arch = "ARM aarch64" then
            return "arm64";
         elsif arch = "ARM, EABI5 " then
            return "armv6";
         else
            return "unknown:" & arch;
         end if;
      end newsuffix;

      function even (fileinfo : String) return String
      is
         --  DF  4.5-DEVELOPMENT: ... DragonFly 4.0.501
         --  DF 4.10-RELEASE    : ... DragonFly 4.0.1000
         --  DF 4.11-DEVELOPMENT: ... DragonFly 4.0.1102
         --
         --  Alternative future format (file version 2.0)
         --  DFV 400702: ... DragonFly 4.7.2
         --  DFV 401117: ..  DragonFly 4.11.17
         rest  : constant String := HT.part_2 (fileinfo, "DragonFly ");
         major : constant String := HT.specific_field (rest, 1, ".");
         part2 : constant String := HT.specific_field (rest, 2, ".");
         part3 : constant String := HT.part_1 (HT.specific_field (rest, 3, "."), ",");
         minor : String (1 .. 2) := "00";
         point : Character;
      begin
         if part2 = "0" then
            --  version format in October 2016
            declare
               mvers : String (1 .. 4) := "0000";
               lenp3 : constant Natural := part3'Length;
            begin
               mvers (mvers'Last - lenp3 + 1 .. mvers'Last) := part3;
               minor := mvers (1 .. 2);
            end;
         else
            --  Alternative future format (file version 2.0)
            declare
               lenp2 : constant Natural := part2'Length;
            begin
               minor (minor'Last - lenp2 + 1 .. minor'Last) := part2;
            end;
         end if;

         point := minor (2);
         case point is
            when '1' => minor (2) := '2';
            when '3' => minor (2) := '4';
            when '5' => minor (2) := '6';
            when '7' => minor (2) := '8';
            when '9' => minor (2) := '0';
                        minor (1) := Character'Val (Character'Pos (minor (1)) + 1);
            when others => null;
         end case;
         if minor (1) = '0' then
            return major & "." & minor (2);
         else
            return major & "." & minor (1 .. 2);
         end if;

      end even;

      function get_major (fileinfo : String; OS : String) return String
      is
         --  FreeBSD 10.2, stripped
         --  FreeBSD 11.0 (1100093), stripped
         --  NetBSD 7.0.1, not stripped
         rest  : constant String := HT.part_2 (fileinfo, OS);
         major : constant String := HT.part_1 (rest, ".");
      begin
         return major;
      end get_major;

      function get_version (fileinfo : String; OS : String) return String
      is
         --  GNU/Linux 2.6.32, BuildID[sha1]=03d7a9de009544a1fe82313544a3c36e249858cc, stripped
         rest  : constant String := HT.part_2 (fileinfo, OS);
      begin
         return HT.part_1 (rest, ",");
      end get_version;

      procedure craft_common_endings (release : String) is
      begin
         HT.SU.Append (abi_formats.calculated_abi, release & ":");
         HT.SU.Append (abi_formats.calculated_alt_abi, release & ":");
         abi_formats.calc_abi_noarch     := abi_formats.calculated_abi;
         abi_formats.calc_alt_abi_noarch := abi_formats.calculated_alt_abi;
         HT.SU.Append (abi_formats.calculated_abi, newsuffix (arch));
         HT.SU.Append (abi_formats.calculated_alt_abi, suffix (arch));
         HT.SU.Append (abi_formats.calc_abi_noarch, "*");
         HT.SU.Append (abi_formats.calc_alt_abi_noarch, "*");
      end craft_common_endings;

   begin
      UN   := Unix.piped_command (command, status);
      arch := isolate_arch_from_file_type (HT.USS (UN));
      case platform_type is
         when dragonfly =>
            declare
               dfly    : constant String := "dragonfly:";
               release : constant String := even (HT.USS (UN));
            begin
               abi_formats.calculated_abi := HT.SUS (dfly);
               HT.SU.Append (abi_formats.calculated_abi, release & ":");
               abi_formats.calc_abi_noarch := abi_formats.calculated_abi;
               HT.SU.Append (abi_formats.calculated_abi, suffix (arch));
               HT.SU.Append (abi_formats.calc_abi_noarch, "*");
               abi_formats.calculated_alt_abi  := abi_formats.calculated_abi;
               abi_formats.calc_alt_abi_noarch := abi_formats.calc_abi_noarch;
            end;
         when freebsd =>
            declare
               fbsd1   : constant String := "FreeBSD:";
               fbsd2   : constant String := "freebsd:";
               release : constant String := get_major (HT.USS (UN), "FreeBSD ");
            begin
               abi_formats.calculated_abi     := HT.SUS (fbsd1);
               abi_formats.calculated_alt_abi := HT.SUS (fbsd2);
               craft_common_endings (release);
            end;
         when netbsd =>
            declare
               net1    : constant String := "NetBSD:";
               net2    : constant String := "netbsd:";
               release : constant String := get_major (HT.USS (UN), "NetBSD ");
            begin
               abi_formats.calculated_abi     := HT.SUS (net1);
               abi_formats.calculated_alt_abi := HT.SUS (net2);
               craft_common_endings (release);
            end;
         when linux   =>
            declare
               gnu1    : constant String := "Linux:";
               gnu2    : constant String := "linux:";
               release : constant String := get_version (HT.USS (UN), "GNU/Linux ");
            begin
               abi_formats.calculated_abi     := HT.SUS (gnu1);
               abi_formats.calculated_alt_abi := HT.SUS (gnu2);
               craft_common_endings (release);
            end;
         when sunos   => null;  --  TBD (check ABI first)
         when macos   => null;
         when openbsd => null;
      end case;
   end establish_package_architecture;


   --------------------------------------------------------------------------------------------
   --  limited_sanity_check
   --------------------------------------------------------------------------------------------
   procedure limited_sanity_check
     (repository       : String;
      dry_run          : Boolean;
      rebuild_compiler : Boolean;
      suppress_remote  : Boolean)
   is
      procedure prune_packages (cursor : ranking_crate.Cursor);
      procedure check_package (cursor : ranking_crate.Cursor);
      procedure determine_fully_built (cursor : subpackage_queue.Cursor);
      procedure prune_queue (cursor : subqueue.Cursor);
      procedure print (cursor : subpackage_queue.Cursor);
      procedure fetch (cursor : subpackage_queue.Cursor);
      procedure check (cursor : subpackage_queue.Cursor);
      procedure set_delete  (Element : in out subpackage_record);
      procedure kill_remote (Element : in out subpackage_record);

      compkey       : HT.Text := HT.SUS (default_compiler & LAT.Colon & variant_standard);
      compiler      : constant port_index := ports_keys.Element (compkey);
      already_built : subpackage_queue.Vector;
      fetch_list    : subpackage_queue.Vector;
      prune_list    : subqueue.Vector;
      fetch_fail    : Boolean := False;
      clean_pass    : Boolean := False;
      listlog       : TIO.File_Type;
      goodlog       : Boolean;
      using_screen  : constant Boolean := Unix.screen_attached;
      filename      : constant String := "/tmp/synth_prefetch_list.txt";
      package_list  : HT.Text := HT.blank;

      procedure set_delete (Element : in out subpackage_record) is
      begin
         Element.deletion_due := True;
      end set_delete;

      procedure kill_remote (Element : in out subpackage_record) is
      begin
         Element.remote_pkg := False;
      end kill_remote;

      procedure check_package (cursor : ranking_crate.Cursor)
      is
         procedure check_subpackage (position : subpackage_crate.Cursor);

         target : port_id := ranking_crate.Element (cursor).ap_index;

         procedure check_subpackage (position : subpackage_crate.Cursor)
         is
            rec        : subpackage_record renames subpackage_crate.Element (position);
            subpackage : constant String  := HT.USS (rec.subpackage);
            pkgname    : constant String  := calculate_package_name (target, subpackage);
            available  : constant Boolean :=
                         (rec.remote_pkg or else rec.pkg_present) and then not rec.deletion_due;
            newrec     : subpackage_identifier := (target, rec.subpackage);
         begin
            if not available then
               return;
            end if;

            if passed_dependency_check (subpackage   => subpackage,
                                        query_result => rec.pkg_dep_query,
                                        id           => target)
            then
               if not rebuild_compiler or else
                 target /= compiler
               then
                  already_built.Append (New_Item => newrec);
                  if rec.remote_pkg then
                     fetch_list.Append (New_Item => newrec);
                  end if;
               end if;
            else
               if rec.remote_pkg then
                  --  silently fail, remote packages are a bonus anyway
                  all_ports (target).subpackages.Update_Element (Position => position,
                                                                 Process  => kill_remote'Access);
               else
                  TIO.Put_Line (pkgname & " failed dependency check.");
                  all_ports (target).subpackages.Update_Element (Position => position,
                                                                 Process  => set_delete'Access);
               end if;
               clean_pass := False;
            end if;
         end check_subpackage;
      begin
         all_ports (target).subpackages.Iterate (check_subpackage'Access);
      end check_package;

      procedure prune_queue (cursor : subqueue.Cursor)
      is
         id : constant port_index := subqueue.Element (cursor);
      begin
         cascade_successful_build (id);
      end prune_queue;

      procedure prune_packages (cursor : ranking_crate.Cursor)
      is
         procedure check_subpackage (position : subpackage_crate.Cursor);

         target : port_id := ranking_crate.Element (cursor).ap_index;

         procedure check_subpackage (position : subpackage_crate.Cursor)
         is
            rec        : subpackage_record renames subpackage_crate.Element (position);
            delete_it  : Boolean := rec.deletion_due;
         begin
            if delete_it then
               declare
                  subpackage : constant String  := HT.USS (rec.subpackage);
                  pkgname    : constant String  := calculate_package_name (target, subpackage);
                  fullpath   : constant String := repository & "/" & pkgname & arc_ext;
               begin
                  DIR.Delete_File (fullpath);
               exception
                  when others => null;
               end;
            end if;
         end check_subpackage;

      begin
         all_ports (target).subpackages.Iterate (check_subpackage'Access);
      end prune_packages;

      procedure print (cursor : subpackage_queue.Cursor)
      is
         id      : constant port_index := subpackage_queue.Element (cursor).id;
         subpkg  : constant String := HT.USS (subpackage_queue.Element (cursor).subpackage);
         pkgfile : constant String := calculate_package_name (id, subpkg) & arc_ext;
      begin
         TIO.Put_Line ("  => " & pkgfile);
         if goodlog then
            TIO.Put_Line (listlog, pkgfile);
         end if;
      end print;

      procedure fetch (cursor : subpackage_queue.Cursor)
      is
         id      : constant port_index := subpackage_queue.Element (cursor).id;
         subpkg  : constant String := HT.USS (subpackage_queue.Element (cursor).subpackage);
         pkgbase : constant String := " " & calculate_package_name (id, subpkg);
      begin
         HT.SU.Append (package_list, pkgbase);
      end fetch;

      procedure check (cursor : subpackage_queue.Cursor)
      is
         id      : constant port_index := subpackage_queue.Element (cursor).id;
         subpkg  : constant String := HT.USS (subpackage_queue.Element (cursor).subpackage);
         pkgfile : constant String := calculate_package_name (id, subpkg) & arc_ext;
         loc     : constant String := HT.USS (PM.configuration.dir_repository) & "/" & pkgfile;
      begin
         if not DIR.Exists (loc) then
            TIO.Put_Line ("Download failed: " & pkgfile);
            fetch_fail := True;
         end if;
      end check;

      procedure determine_fully_built (cursor : subpackage_queue.Cursor)
      is
         procedure check_subpackage (cursor : subpackage_crate.Cursor);

         glass_full : Boolean := True;
         target : port_id := subpackage_queue.Element (cursor).id;

         procedure check_subpackage (cursor : subpackage_crate.Cursor)
         is
            procedure check_already_built (position : subpackage_queue.Cursor);

            rec : subpackage_record renames subpackage_crate.Element (cursor);
            found : Boolean := False;

            procedure check_already_built (position : subpackage_queue.Cursor)
            is
               builtrec : subpackage_identifier renames subpackage_queue.Element (position);
            begin
               if not found then
                  if HT.equivalent (builtrec.subpackage, rec.subpackage) then
                     found := True;
                     if rec.deletion_due or else
                       not (rec.pkg_present or else rec.remote_pkg)
                     then
                        glass_full := False;
                     end if;
                  end if;
               end if;
            end check_already_built;
         begin
            if glass_full then
               already_built.Iterate (check_already_built'Access);
            end if;
         end check_subpackage;
      begin
         all_ports (target).subpackages.Iterate (check_subpackage'Access);
         if glass_full then
            if not prune_list.Contains (target) then
              prune_list.Append (target);
            end if;
         end if;
      end determine_fully_built;

   begin
      if Unix.env_variable_defined ("WHYFAIL") then
         activate_debugging_code;
      end if;
      establish_package_architecture;
      original_queue_len := rank_queue.Length;
      for m in scanners'Range loop
         mq_progress (m) := 0;
      end loop;
      LOG.start_obsolete_package_logging;
      parallel_package_scan (repository, False, using_screen);

      if Signals.graceful_shutdown_requested then
         LOG.stop_obsolete_package_logging;
         return;
      end if;

      while not clean_pass loop
         clean_pass := True;
         already_built.Clear;
         rank_queue.Iterate (check_package'Access);
      end loop;
      if not suppress_remote and then PM.configuration.defer_prebuilt then
         --  The defer_prebuilt options has been elected, so check all the
         --  missing and to-be-pruned ports for suitable prebuilt packages
         --  So we need to an incremental scan (skip valid, present packages)
         for m in scanners'Range loop
            mq_progress (m) := 0;
         end loop;
         parallel_package_scan (repository, True, using_screen);

         if Signals.graceful_shutdown_requested then
            LOG.stop_obsolete_package_logging;
            return;
         end if;

         clean_pass := False;
         while not clean_pass loop
            clean_pass := True;
            already_built.Clear;
            fetch_list.Clear;
            rank_queue.Iterate (check_package'Access);
         end loop;
      end if;
      LOG.stop_obsolete_package_logging;
      if Signals.graceful_shutdown_requested then
         return;
      end if;
      if dry_run then
         if not fetch_list.Is_Empty then
            begin
               TIO.Create (File => listlog, Mode => TIO.Out_File, Name => filename);
               goodlog := True;
            exception
               when others => goodlog := False;
            end;
            TIO.Put_Line ("These are the packages that would be fetched:");
            fetch_list.Iterate (print'Access);
            TIO.Put_Line ("Total packages that would be fetched:" & fetch_list.Length'Img);
            if goodlog then
               TIO.Close (listlog);
               TIO.Put_Line ("The complete build list can also be found at:"
                             & LAT.LF & filename);
            end if;
         else
            if PM.configuration.defer_prebuilt then
               TIO.Put_Line ("No packages qualify for prefetching from " &
                               "official package repository.");
            end if;
         end if;
      else
         rank_queue.Iterate (prune_packages'Access);
         fetch_list.Iterate (fetch'Access);
         if not HT.equivalent (package_list, HT.blank) then
            declare
               cmd : constant String := host_pkg8 & " fetch -r " &
                 HT.USS (external_repository) & " -U -y --output " &
                 HT.USS (PM.configuration.dir_packages) & HT.USS (package_list);
            begin
               if Unix.external_command (cmd) then
                  null;
               end if;
            end;
            fetch_list.Iterate (check'Access);
         end if;
      end if;
      if fetch_fail then
         TIO.Put_Line ("At least one package failed to fetch, aborting build!");
         rank_queue.Clear;
      else
         --  All subpackages must be "already_built" before we can prune.
         --  we have iterate through the rank_queue, then subiterate through subpackages.
         --  If all subpackages are present, add port to prune queue.
         already_built.Iterate (determine_fully_built'Access);
         prune_list.Iterate (prune_queue'Access);
      end if;
   end limited_sanity_check;


   --------------------------------------------------------------------------------------------
   --  result_of_dependency_query
   --------------------------------------------------------------------------------------------
   function result_of_dependency_query
     (repository : String;
      id         : port_id;
      subpackage : String) return HT.Text
   is
      rec : port_record renames all_ports (id);

      pkg_base : constant String := PortScan.calculate_package_name (id, subpackage);
      fullpath : constant String := repository & "/" & pkg_base & arc_ext;
      pkg8     : constant String := HT.USS (PM.configuration.sysroot_pkg8);
      command  : constant String := pkg8 & " query -F "  & fullpath & " %dn-%dv@%do";
      remocmd  : constant String := pkg8 & " rquery -r " & HT.USS (external_repository) &
                                    " -U %dn-%dv@%do " & pkg_base;
      status   : Integer;
      comres   : HT.Text;
   begin
      if repository = "" then
         comres := Unix.piped_command (remocmd, status);
      else
         comres := Unix.piped_command (command, status);
      end if;
      if status = 0 then
         return comres;
      else
         return HT.blank;
      end if;
   end result_of_dependency_query;


   --------------------------------------------------------------------------------------------
   --  activate_debugging_code
   --------------------------------------------------------------------------------------------
   procedure activate_debugging_code is
   begin
      debug_opt_check := True;
      debug_dep_check := True;
   end activate_debugging_code;


   --------------------------------------------------------------------------------------------
   --  passed_dependency_check
   --------------------------------------------------------------------------------------------
   function passed_dependency_check
     (subpackage   : String;
      query_result : HT.Text;
      id           : port_id) return Boolean
   is
      procedure get_rundeps (position : subpackage_crate.Cursor);
      procedure log_run_deps (position : subpackage_crate.Cursor);

      content  : String  := HT.USS (query_result);
      headport : constant String := HT.USS (all_ports (id).port_namebase) & LAT.Colon &
                                    subpackage & LAT.Colon & HT.USS (all_ports (id).port_variant);
      counter  : Natural := 0;
      req_deps : Natural := 0;
      markers  : HT.Line_Markers;
      pkgfound : Boolean := False;

      procedure get_rundeps (position : subpackage_crate.Cursor)
      is
         rec : subpackage_record renames subpackage_crate.Element (position);
      begin
         if not pkgfound then
            if HT.equivalent (rec.subpackage, subpackage) then
               req_deps := Natural (rec.spkg_run_deps.Length);
               pkgfound := True;
            end if;
         end if;
      end get_rundeps;

      procedure log_run_deps (position : subpackage_crate.Cursor)
      is
         procedure logme (logpos : spkg_id_crate.Cursor);
         rec : subpackage_record renames subpackage_crate.Element (position);
         procedure logme (logpos : spkg_id_crate.Cursor) is
            rec2 : PortScan.subpackage_identifier renames spkg_id_crate.Element (logpos);
            message : constant String := get_port_variant (rec2.port) & " (" &
              HT.USS (rec2.subpackage) & ")";
         begin
            LOG.obsolete_notice (message, debug_dep_check);
         end logme;
      begin
         if HT.equivalent (rec.subpackage, subpackage) then
            LOG.obsolete_notice ("Port requirements:", debug_dep_check);
            rec.spkg_run_deps.Iterate (logme'Access);
         end if;
      end log_run_deps;
   begin
      all_ports (id).subpackages.Iterate (get_rundeps'Access);
      HT.initialize_markers (content, markers);
      loop
         exit when not HT.next_line_present (content, markers);
         declare
            line   : constant String := HT.extract_line (content, markers);
            deppkg : constant String := HT.part_1 (line, "@");
            origin : constant String := HT.part_2 (line, "@");
         begin
            exit when line = "";
            declare
               procedure set_available (position : subpackage_crate.Cursor);

               subpackage : String := subpackage_from_pkgname (deppkg);
               target_id  : port_index := ports_keys.Element (HT.SUS (origin));
               target_pkg : String := calculate_package_name (target_id, subpackage);
               available  : Boolean;

               procedure set_available (position : subpackage_crate.Cursor)
               is
                  rec : subpackage_record renames subpackage_crate.Element (position);
               begin
                  if not pkgfound and then
                    HT.equivalent (rec.subpackage, subpackage)
                  then
                     available := (rec.remote_pkg or else rec.pkg_present) and then
                       not rec.deletion_due;
                     pkgfound := True;
                  end if;
               end set_available;
            begin
               if valid_port_id (target_id) then
                  pkgfound := False;
                  all_ports (target_id).subpackages.Iterate (set_available'Access);
               else
                  --  package seems to have a dependency that has been removed from the conspiracy
                  LOG.obsolete_notice
                    (message         => origin & " has been removed from Ravenports",
                     write_to_screen => debug_dep_check);
                  return False;
               end if;

               counter := counter + 1;
               if counter > req_deps then
                  --  package has more dependencies than we are looking for
                  LOG.obsolete_notice
                    (write_to_screen => debug_dep_check,
                     message         => headport & " package has more dependencies than the " &
                       "port requires (" & HT.int2str (req_deps) & ")" & LAT.LF &
                       "Query: " & LAT.LF & HT.USS (query_result) &
                       "Tripped on: " & line);
                  all_ports (id).subpackages.Iterate (log_run_deps'Access);
                  return False;
               end if;

               if deppkg /= target_pkg then
                  --  The version that the package requires differs from the
                  --  version that Ravenports will now produce
                  LOG.obsolete_notice
                    (write_to_screen => debug_dep_check,
                     message         =>  "Current " & headport & " package depends on " &
                       deppkg & ", but this is a different version than requirement of " &
                       target_pkg & " (from " & origin & ")");
                  return False;
               end if;

               if not available then
                  --  Even if all the versions are matching, we still need
                  --  the package to be in repository.
                  LOG.obsolete_notice
                    (write_to_screen => debug_dep_check,
                     message         => headport & " package depends on " & target_pkg &
                       " which doesn't exist or has been scheduled for deletion");
                  return False;
               end if;
            end;
         end;
      end loop;

      if counter < req_deps then
         --  The ports tree requires more dependencies than the existing package does
         LOG.obsolete_notice
           (write_to_screen => debug_dep_check,
            message         => headport & " package has less dependencies than the port " &
              "requires (" & HT.int2str (req_deps) & ")" & LAT.LF &
              "Query: " & LAT.LF & HT.USS (query_result));
         all_ports (id).subpackages.Iterate (log_run_deps'Access);
         return False;
      end if;

      --  If we get this far, the package dependencies match what the
      --  port tree requires exactly.  This package passed sanity check.
      return True;

   exception
      when issue : others =>
         LOG.obsolete_notice
           (write_to_screen => debug_dep_check,
            message         => content & "Dependency check exception" & LAT.LF &
              EX.Exception_Message (issue));
         return False;
   end passed_dependency_check;


   --------------------------------------------------------------------------------------------
   --  package_scan_progress
   --------------------------------------------------------------------------------------------
   function package_scan_progress return String
   is
      type percent is delta 0.01 digits 5;
      complete : port_index := 0;
      pc : percent;
      total : constant Float := Float (pkgscan_total);
   begin
      for k in scanners'Range loop
         complete := complete + pkgscan_progress (k);
      end loop;
      pc := percent (100.0 * Float (complete) / total);
      return " progress:" & pc'Img & "%              " & LAT.CR;
   end package_scan_progress;


   --------------------------------------------------------------------------------------------
   --  passed_abi_check
   --------------------------------------------------------------------------------------------
   function passed_abi_check
     (repository       : String;
      id               : port_id;
      subpackage       : String;
      skip_exist_check : Boolean := False) return Boolean
   is
      rec : port_record renames all_ports (id);

      pkg_base : constant String := PortScan.calculate_package_name (id, subpackage);
      fullpath : constant String := repository & "/" & pkg_base & arc_ext;
      pkg8     : constant String := HT.USS (PM.configuration.sysroot_pkg8);
      command  : constant String := pkg8 & " query -F "  & fullpath & " %q";
      remocmd  : constant String := pkg8 & " rquery -r " & HT.USS (external_repository) &
                                    " -U %q " & pkg_base;
      status : Integer;
      comres : HT.Text;
   begin
      if not skip_exist_check and then not DIR.Exists (Name => fullpath)
      then
         return False;
      end if;

      if repository = "" then
         comres := Unix.piped_command (remocmd, status);
      else
         comres := Unix.piped_command (command, status);
      end if;
      if status /= 0 then
         return False;
      end if;
      declare
         topline : String := HT.first_line (HT.USS (comres));
      begin
         if HT.equivalent (abi_formats.calculated_abi, topline) or else
           HT.equivalent (abi_formats.calculated_alt_abi, topline) or else
           HT.equivalent (abi_formats.calc_abi_noarch, topline) or else
           HT.equivalent (abi_formats.calc_alt_abi_noarch, topline)
         then
            return True;
         end if;
      end;
      return False;
   end passed_abi_check;


   --------------------------------------------------------------------------------------------
   --  passed_option_check
   --------------------------------------------------------------------------------------------
   function passed_option_check
     (repository       : String;
      id               : port_id;
      subpackage       : String;
      skip_exist_check : Boolean := False) return Boolean
   is
      rec : port_record renames all_ports (id);

      pkg_base : constant String := PortScan.calculate_package_name (id, subpackage);
      fullpath : constant String := repository & "/" & pkg_base & arc_ext;
      pkg8     : constant String := HT.USS (PM.configuration.sysroot_pkg8);
      command  : constant String := pkg8 & " query -F "  & fullpath & " %Ok:%Ov";
      remocmd  : constant String := pkg8 & " rquery -r " & HT.USS (external_repository) &
                                    " -U %Ok:%Ov " & pkg_base;
      status   : Integer;
      comres   : HT.Text;
      counter  : Natural := 0;
      required : constant Natural := Natural (all_ports (id).options.Length);
   begin
      if id = port_match_failed or else
        not all_ports (id).scanned or else
        (not skip_exist_check and then not DIR.Exists (fullpath))
      then
         return False;
      end if;

      if repository = "" then
         comres := Unix.piped_command (remocmd, status);
      else
         comres := Unix.piped_command (command, status);
      end if;
      if status /= 0 then
         return False;
      end if;

      declare
         command_result : constant String := HT.USS (comres);
         markers  : HT.Line_Markers;
      begin
         HT.initialize_markers (command_result, markers);
         loop
            exit when not HT.next_line_present (command_result, markers);
            declare
               line     : constant String := HT.extract_line (command_result, markers);
               namekey  : constant String := HT.part_1 (line, ":");
               knob     : constant String := HT.part_2 (line, ":");
               nametext : HT.Text := HT.SUS (namekey);
               knobval  : Boolean;
            begin
               exit when line = "";
               if HT.count_char (line, LAT.Colon) /= 1 then
                  raise unknown_format with line;
               end if;
               if knob = "on" then
                  knobval := True;
               elsif knob = "off" then
                  knobval := False;
               else
                  raise unknown_format with "knob=" & knob & "(" & line & ")";
               end if;

               counter := counter + 1;
               if counter > required then
                  --  package has more options than we are looking for
                  LOG.obsolete_notice
                    (write_to_screen => debug_opt_check,
                     message => "options " & namekey & LAT.LF & pkg_base &
                       " has more options than required (" & HT.int2str (required) & ")");
                  return False;
               end if;

               if all_ports (id).options.Contains (nametext) then
                  if knobval /= all_ports (id).options.Element (nametext) then
                     --  port option value doesn't match package option value
                     if knobval then
                        LOG.obsolete_notice
                          (write_to_screen => debug_opt_check,
                           message => pkg_base & " " & namekey &
                             " is ON but specifcation says it must be OFF");
                     else
                        LOG.obsolete_notice
                          (write_to_screen => debug_opt_check,
                           message => pkg_base & " " & namekey &
                             " is OFF but specifcation says it must be ON");
                     end if;
                     return False;
                  end if;
               else
                  --  Name of package option not found in port options
                  LOG.obsolete_notice
                    (write_to_screen => debug_opt_check,
                     message => pkg_base & " option " & namekey &
                       " is no longer present in the specification");
                  return False;
               end if;
            end;
         end loop;

         if counter < required then
            --  The ports tree has more options than the existing package
            LOG.obsolete_notice
              (write_to_screen => debug_opt_check,
               message => pkg_base & " has less options than required (" &
                 HT.int2str (required) & ")");
            return False;
         end if;

         --  If we get this far, the package options must match port options
         return True;
      end;
   exception
      when issue : others =>
         LOG.obsolete_notice
           (write_to_screen => debug_opt_check,
            message => "option check exception" & LAT.LF & EX.Exception_Message (issue));
         return False;
   end passed_option_check;


   --------------------------------------------------------------------------------------------
   --  initial_package_scan
   --------------------------------------------------------------------------------------------
   procedure initial_package_scan (repository : String; id : port_id; subpackage : String)
   is
      procedure set_position (position : subpackage_crate.Cursor);
      procedure set_delete   (Element : in out subpackage_record);
      procedure set_present  (Element : in out subpackage_record);
      procedure set_query    (Element : in out subpackage_record);

      subpackage_position : subpackage_crate.Cursor := subpackage_crate.No_Element;
      query_result        : HT.Text;

      procedure set_position (position : subpackage_crate.Cursor)
      is
         rec : subpackage_record renames subpackage_crate.Element (position);
      begin
         if HT.USS (rec.subpackage) = subpackage then
            subpackage_position := position;
         end if;
      end set_position;
      procedure set_delete (Element : in out subpackage_record) is
      begin
         Element.deletion_due := True;
      end set_delete;
      procedure set_present (Element : in out subpackage_record) is
      begin
         Element.pkg_present := True;
      end set_present;
      procedure set_query (Element : in out subpackage_record) is
      begin
         Element.pkg_dep_query := query_result;
      end set_query;

      use type subpackage_crate.Cursor;
   begin
      if id = port_match_failed or else
        not all_ports (id).scanned
      then
           return;
      end if;

      all_ports (id).subpackages.Iterate (set_position'Access);
      if subpackage_position = subpackage_crate.No_Element then
         return;
      end if;

      declare
         pkgname  : constant String := calculate_package_name (id, subpackage);
         fullpath : constant String := repository & "/" & pkgname & arc_ext;
         msg_opt  : constant String := pkgname & " failed option check.";
         msg_abi  : constant String := pkgname & " failed architecture (ABI) check.";
      begin
         if DIR.Exists (fullpath) then
            all_ports (id).subpackages.Update_Element (subpackage_position, set_present'Access);
         else
            return;
         end if;
         if not passed_option_check (repository, id, subpackage, True) then
            LOG.obsolete_notice (msg_opt, True);
            all_ports (id).subpackages.Update_Element (subpackage_position, set_delete'Access);
            return;
         end if;
         if not passed_abi_check (repository, id, subpackage, True) then
            LOG.obsolete_notice (msg_abi, True);
            all_ports (id).subpackages.Update_Element (subpackage_position, set_delete'Access);
            return;
         end if;
      end;
      query_result := result_of_dependency_query (repository, id, subpackage);
      all_ports (id).subpackages.Update_Element (subpackage_position, set_query'Access);
   end initial_package_scan;


   --------------------------------------------------------------------------------------------
   --  remote_package_scan
   --------------------------------------------------------------------------------------------
   procedure remote_package_scan (id : port_id; subpackage : String)
   is
      procedure set_position   (position : subpackage_crate.Cursor);
      procedure set_remote_on  (Element : in out subpackage_record);
      procedure set_remote_off (Element : in out subpackage_record);
      procedure set_query      (Element : in out subpackage_record);

      subpackage_position : subpackage_crate.Cursor := subpackage_crate.No_Element;
      query_result        : HT.Text;

      procedure set_position (position : subpackage_crate.Cursor)
      is
         rec : subpackage_record renames subpackage_crate.Element (position);
      begin
         if HT.USS (rec.subpackage) = subpackage then
            subpackage_position := position;
         end if;
      end set_position;
      procedure set_remote_on (Element : in out subpackage_record) is
      begin
         Element.remote_pkg := True;
      end set_remote_on;
      procedure set_remote_off (Element : in out subpackage_record) is
      begin
         Element.remote_pkg := False;
      end set_remote_off;
      procedure set_query (Element : in out subpackage_record) is
      begin
         Element.pkg_dep_query := query_result;
      end set_query;

      use type subpackage_crate.Cursor;
   begin
      all_ports (id).subpackages.Iterate (set_position'Access);
      if subpackage_position = subpackage_crate.No_Element then
         return;
      end if;

      if passed_abi_check (repository       => "",
                           id               => id,
                           subpackage       => subpackage,
                           skip_exist_check => True)
      then
         all_ports (id).subpackages.Update_Element (subpackage_position, set_remote_on'Access);
      else
         return;
      end if;
      if not passed_option_check (repository       => "",
                                  id               => id,
                                  subpackage       => subpackage,
                                  skip_exist_check => True)
      then
         all_ports (id).subpackages.Update_Element (subpackage_position, set_remote_off'Access);
         return;
      end if;
      query_result := result_of_dependency_query ("", id, subpackage);
      all_ports (id).subpackages.Update_Element (subpackage_position, set_query'Access);
   end remote_package_scan;


   --------------------------------------------------------------------------------------------
   --  parallel_package_scan
   --------------------------------------------------------------------------------------------
   procedure parallel_package_scan
     (repository    : String;
      remote_scan   : Boolean;
      show_progress : Boolean)
   is
      task type scan (lot : scanners);
      finished : array (scanners) of Boolean := (others => False);
      combined_wait : Boolean := True;
      label_shown   : Boolean := False;
      aborted       : Boolean := False;

      task body scan
      is
         procedure populate (cursor : subqueue.Cursor);
         procedure populate (cursor : subqueue.Cursor)
         is
            procedure check_subpackage (position : subpackage_crate.Cursor);

            target_port : port_index := subqueue.Element (cursor);
            important   : constant Boolean := all_ports (target_port).scanned;

            procedure check_subpackage (position : subpackage_crate.Cursor)
            is
               rec        : subpackage_record renames subpackage_crate.Element (position);
               subpackage : String := HT.USS (rec.subpackage);
            begin
               if not aborted and then important then
                  if remote_scan and then
                    not rec.never_remote
                  then
                     if not rec.pkg_present or else
                       rec.deletion_due
                     then
                        remote_package_scan (target_port, subpackage);
                     end if;
                  else
                     initial_package_scan (repository, target_port, subpackage);
                  end if;
               end if;
            end check_subpackage;

         begin
            all_ports (target_port).subpackages.Iterate (check_subpackage'Access);
            mq_progress (lot) := mq_progress (lot) + 1;
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
      while combined_wait loop
         delay 1.0;
         combined_wait := False;
         for j in scanners'Range loop
            if not finished (j) then
               combined_wait := True;
               exit;
            end if;
         end loop;
         if combined_wait then
            if not label_shown then
               label_shown := True;
               TIO.Put_Line ("Scanning existing packages.");
            end if;
            if show_progress then
               TIO.Put (scan_progress);
            end if;
            if Signals.graceful_shutdown_requested then
               aborted := True;
            end if;
         end if;
      end loop;
   end parallel_package_scan;


   --------------------------------------------------------------------------------------------
   --  initialize_web_report
   --------------------------------------------------------------------------------------------
   procedure initialize_web_report (num_builders : builders) is
      idle_slaves  : constant dim_builder_state := (others => idle);
      reportdir    : constant String := HT.USS (PM.configuration.dir_logs);
      sharedir     : constant String := host_localbase & "/share/ravenadm";
   begin
      DIR.Create_Path (reportdir);
      DIR.Copy_File (sharedir & "/raven.png",     reportdir & "/raven.png");
      DIR.Copy_File (sharedir & "/favicon.png",   reportdir & "/favicon.png");
      DIR.Copy_File (sharedir & "/progress.js",   reportdir & "/progress.js");
      DIR.Copy_File (sharedir & "/progress.css",  reportdir & "/progress.css");
      DIR.Copy_File (sharedir & "/progress.html", reportdir & "/index.html");
      write_summary_json (active            => True,
                          states            => idle_slaves,
                          num_builders      => num_builders,
                          num_history_files => 0);
   end initialize_web_report;


   --------------------------------------------------------------------------------------------
   --  write_summary_json
   --------------------------------------------------------------------------------------------
   procedure write_summary_json
     (active            : Boolean;
      states            : dim_builder_state;
      num_builders      : builders;
      num_history_files : Natural)
   is
      function TF (value : Boolean) return Natural;

      jsonfile : TIO.File_Type;
      filename : constant String := HT.USS (PM.configuration.dir_logs) & "/summary.json";
      leftover : constant Integer := LOG.ports_remaining_to_build;
      slave    : DPY.builder_rec;

      function TF (value : Boolean) return Natural is
      begin
         if value then
            return 1;
         else
            return 0;
         end if;
      end TF;
   begin
      TIO.Create (File => jsonfile, Mode => TIO.Out_File, Name => filename);
      TIO.Put (jsonfile, "{" & LAT.LF &
           "  " & nv ("profile", HT.USS (PM.configuration.profile)) & LAT.LF);
      TIO.Put
        (jsonfile,
           " ," & nv ("kickoff", LOG.www_timestamp_start_time) & LAT.LF &
           " ," & nv ("kfiles", num_history_files) & LAT.LF &
           " ," & nv ("active", TF (active)) & LAT.LF &
           " ," & LAT.Quotation & "stats" & LAT.Quotation & LAT.Colon & "{" & LAT.LF);
      TIO.Put
        (jsonfile,
           "   " & nv ("queued",   LOG.port_counter_value (total))   & LAT.LF &
           "  ," & nv ("built",    LOG.port_counter_value (success)) & LAT.LF &
           "  ," & nv ("failed",   LOG.port_counter_value (failure)) & LAT.LF &
           "  ," & nv ("ignored",  LOG.port_counter_value (ignored)) & LAT.LF &
           "  ," & nv ("skipped",  LOG.port_counter_value (skipped)) & LAT.LF &
           "  ," & nv ("remains",  leftover)              & LAT.LF &
           "  ," & nv ("elapsed",  LOG.elapsed_now)       & LAT.LF &
           "  ," & nv ("pkghour",  LOG.hourly_build_rate) & LAT.LF &
           "  ," & nv ("impulse",  LOG.impulse_rate)      & LAT.LF &
           "  ," & nv ("swapinfo", DPY.fmtpc (get_swap_status, True)) & LAT.LF &
           "  ," & nv ("load",     DPY.fmtpc (CYC.load_core (True), False)) & LAT.LF &
           " }" & LAT.LF &
           " ," & LAT.Quotation & "builders" & LAT.Quotation & LAT.Colon & "[" & LAT.LF);

      for b in builders'First .. num_builders loop
         if states (b) = shutdown then
            slave := CYC.builder_status (b, True, False);
         elsif states (b) = idle then
            slave := CYC.builder_status (b, False, True);
         else
            slave := CYC.builder_status (b);
         end if;
         if b = builders'First then
            TIO.Put (jsonfile, "  {" & LAT.LF);
         else
            TIO.Put (jsonfile, "  ,{" & LAT.LF);
         end if;

         TIO.Put
           (jsonfile,
              "    " & nv ("ID",      slave.slavid)  & LAT.LF &
              "   ," & nv ("elapsed", HT.trim (slave.Elapsed)) & LAT.LF &
              "   ," & nv ("phase",   HT.trim (slave.phase))   & LAT.LF &
              "   ," & nv ("origin",  HT.trim (slave.origin))  & LAT.LF &
              "   ," & nv ("lines",   HT.trim (slave.LLines))  & LAT.LF &
              "  }" & LAT.LF);
      end loop;
      TIO.Put (jsonfile, " ]" & LAT.LF & "}" & LAT.LF);
      TIO.Close (jsonfile);
   exception
      when others =>
         if TIO.Is_Open (jsonfile) then
            TIO.Close (jsonfile);
         end if;
   end write_summary_json;


   --------------------------------------------------------------------------------------------
   --  swapinfo_command
   --------------------------------------------------------------------------------------------
   function swapinfo_command return String is
   begin
      case platform_type is
         when dragonfly | freebsd =>
            return "/usr/sbin/swapinfo -k";
         when netbsd | openbsd =>
            return "/sbin/swapctl -lk";
         when linux =>
            return "/sbin/swapon --bytes --show=NAME,SIZE,USED,PRIO";
         when sunos =>
            return "/usr/sbin/swap -l";
         when macos =>
            return "/usr/bin/vm_stat";
      end case;
   end swapinfo_command;


   --------------------------------------------------------------------------------------------
   --  get_swap_status
   --------------------------------------------------------------------------------------------
   function get_swap_status return Float
   is
      type memtype is mod 2**64;
      command : String := swapinfo_command;
      status  : Integer;
      comres  : HT.Text;

      blocks_total : memtype := 0;
      blocks_used  : memtype := 0;
   begin
      if platform_type = macos then
         return 0.0;
      end if;
      comres := Unix.piped_command (command, status);
      if status /= 0 then
         return 200.0;  --  [ERROR] Signal to set swap display to "N/A"
      end if;
      --  Throw first line away, e.g "Device 1K-blocks Used  Avail ..."
      --  Distinguishes platforms though:
      --     Net/Free/Dragon start with "Device"
      --     Linux starts with "NAME"
      --     Solaris starts with "swapfile"
      --  MacOS has no limit, it will keep generating swapfiles as needed, so return 0.0
      declare
         command_result : String := HT.USS (comres);
         markers        : HT.Line_Markers;
         line_present   : Boolean;
         oneline_total  : memtype;
         oneline_other  : memtype;
      begin
         HT.initialize_markers (command_result, markers);
         --  Throw first line away (valid for all platforms
         line_present := HT.next_line_present (command_result, markers);
         if line_present then
            declare
               line : String := HT.extract_line (command_result, markers);
            begin
               null;
            end;
         else
            return 200.0;  --  [ERROR] Signal to set swap display to "N/A"
         end if;
         case platform_type is
            when freebsd | dragonfly | netbsd | openbsd | linux | sunos =>
               --  Normally 1 swap line, but there is no limit
               loop
                  exit when not HT.next_line_present (command_result, markers);
                  declare
                     line : constant String :=
                       HT.strip_excessive_spaces (HT.extract_line (command_result, markers));
                  begin
                     case platform_type is
                        when freebsd | dragonfly | netbsd | openbsd =>
                           blocks_total := blocks_total +
                                           memtype'Value (HT.specific_field (line, 4));
                           blocks_used  := blocks_used +
                                           memtype'Value (HT.specific_field (line, 3));
                        when sunos =>
                           oneline_total := memtype'Value (HT.specific_field (line, 4));
                           oneline_other := memtype'Value (HT.specific_field (line, 5));
                           blocks_total  := blocks_total + oneline_total;
                           blocks_used   := blocks_used + (oneline_total - oneline_other);
                        when linux =>
                           blocks_total := blocks_total +
                                           memtype'Value (HT.specific_field (line, 2));
                           blocks_used  := blocks_used +
                                           memtype'Value (HT.specific_field (line, 3));
                        when macos => null;
                     end case;
                  exception
                     when Constraint_Error =>
                        return  200.0;  --  [ERROR] Signal to set swap display to "N/A"
                  end;
               end loop;
            when macos =>
               null;
         end case;
      end;
      if blocks_total = 0 then
         return 200.0;  --  Signal to set swap display to "N/A"
      else
         return 100.0 * Float (blocks_used) / Float (blocks_total);
      end if;
   end get_swap_status;


   --------------------------------------------------------------------------------------------
   --  initialize_display
   --------------------------------------------------------------------------------------------
   procedure initialize_display (num_builders : builders) is
   begin
      if PM.configuration.avec_ncurses then
         curses_support := DPY.launch_monitor (num_builders);
      end if;
   end initialize_display;


   --------------------------------------------------------------------------------------------
   --  nothing_left
   --------------------------------------------------------------------------------------------
   function nothing_left (num_builders : builders) return Boolean
   is
      list_len : constant Integer := Integer (rank_queue.Length);
   begin
      return list_len = 0;
   end nothing_left;


   --------------------------------------------------------------------------------------------
   --  shutdown_recommended
   --------------------------------------------------------------------------------------------
   function shutdown_recommended (active_builders : Positive) return Boolean
   is
      list_len : constant Natural  := Integer (rank_queue.Length);
      list_max : constant Positive := 2 * active_builders;
      num_wait : Natural := 0;
      cursor   : ranking_crate.Cursor;
      QR       : queue_record;
   begin
      if list_len = 0 or else list_len >= list_max then
         return False;
      end if;
      cursor := rank_queue.First;
      for k in 1 .. list_len loop
         QR := ranking_crate.Element (Position => cursor);
         if not all_ports (QR.ap_index).work_locked then
            num_wait := num_wait + 1;
            if num_wait >= active_builders then
               return False;
            end if;
         end if;
         cursor := ranking_crate.Next (Position => cursor);
      end loop;
      return True;
   end shutdown_recommended;


   --------------------------------------------------------------------------------------------
   --  lock_package
   --------------------------------------------------------------------------------------------
   procedure lock_package (id : port_id) is
   begin
      if id /= port_match_failed then
         all_ports (id).work_locked := True;
      end if;
   end lock_package;


   --------------------------------------------------------------------------------------------
   --  top_buildable_port
   --------------------------------------------------------------------------------------------
   function top_buildable_port return port_id
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
         if not all_ports (QR.ap_index).work_locked and then
           all_ports (QR.ap_index).blocked_by.Is_Empty
         then
            result := QR.ap_index;
            exit;
         end if;
         cursor := ranking_crate.Next (Position => cursor);
      end loop;
      if Signals.graceful_shutdown_requested then
         return port_match_failed;
      end if;
      return result;
   end top_buildable_port;


   --------------------------------------------------------------------------------------------
   --  parse_and_transform_buildsheet
   --------------------------------------------------------------------------------------------
   procedure parse_and_transform_buildsheet
     (specification : in out Port_Specification.Portspecs;
      successful    : out Boolean;
      buildsheet    : String;
      variant       : String;
      portloc       : String;
      excl_targets  : Boolean;
      avoid_dialog  : Boolean;
      sysrootver    : sysroot_characteristics)
   is
      function read_option_file return Boolean;
      function launch_and_read (optfile, cookie : String) return Boolean;

      makefile : String := portloc & "/Makefile";
      dir_opt  : constant String := HT.USS (PM.configuration.dir_options);

      function read_option_file return Boolean
      is
         result   : Boolean := True;
         required : Natural := specification.get_list_length (Port_Specification.sp_opts_standard);
      begin
         IFM.scan_file (directory => dir_opt, filename => specification.get_namebase);
         declare
            list : constant String := IFM.show_value (section => "parameters",
                                                      name    => "available");
            num_opt : Natural := HT.count_char (list, LAT.Comma) + 1;
         begin
            if num_opt /= required then
               result := False;
            end if;
            for item in 1 .. num_opt loop
               declare
                  setting  : Boolean;
                  good     : Boolean := True;
                  nv_name  : String := HT.specific_field (list, item, ",");
                  nv_value : String := IFM.show_value ("options", nv_name);
               begin
                  if nv_value = "true" then
                     setting := True;
                  elsif nv_value = "false" then
                     setting := False;
                  else
                     good   := False;
                     result := False;
                  end if;
                  if good then
                     if specification.option_exists (nv_name) then
                        PST.define_option_setting (specification, nv_name, setting);
                     else
                        result := False;
                     end if;
                  end if;
               end;
            end loop;
         end;
         return result;
      exception
         when others =>
            return False;
      end read_option_file;

      function launch_and_read (optfile, cookie : String) return Boolean is
      begin
         PST.set_option_to_default_values (specification);
         if not avoid_dialog then
            if not DLG.launch_dialog (specification) then
               return False;
            end if;

            if DIR.Exists (cookie) then
               --  We keep the already-set standard option settings
               return True;
            end if;

            if not DIR.Exists (optfile) then
               TIO.Put_Line ("Saved option file and cookie missing after dialog executed.  bug?");
               return False;
            end if;

            if not read_option_file then
               TIO.Put_Line ("Saved option file invalid after dialog executed.  bug?");
               return False;
            end if;
         end if;
         return True;
      end launch_and_read;
   begin
      PAR.parse_specification_file (dossier         => buildsheet,
                                    specification   => specification,
                                    opsys_focus     => platform_type,
                                    arch_focus      => sysrootver.arch,
                                    success         => successful,
                                    stop_at_targets => excl_targets,
                                    extraction_dir  => portloc);
      if not successful then
         TIO.Put_Line ("Failed to parse " & buildsheet);
         TIO.Put_Line (PAR.get_parse_error);
         return;
      end if;

      PST.set_option_defaults
        (specs         => specification,
         variant       => variant,
         opsys         => platform_type,
         arch_standard => sysrootver.arch,
         osrelease     => HT.USS (sysrootver.release));

      --  If no available options, skip (remember, if variants there are ALWAYS options
      --  otherwise
      --    if batch mode, ignore cookies.  if no option file, use default values.
      --    if not batch mode:
      --       If option file, use it.
      --       if no option file: if cookie exists, used default values, otherwise show dialog

      declare
         optfile : constant String := dir_opt & "/" & specification.get_namebase;
         cookie  : constant String := dir_opt & "/defconf_cookies/" & specification.get_namebase;
      begin
         if variant = variant_standard then
            if specification.standard_options_present then
               --  This port has at least one user-definable option
               if PM.configuration.batch_mode then
                  --  In batch mode, option settings are optional.  Use default values if not set
                  if DIR.Exists (optfile) then
                     if not read_option_file then
                        TIO.Put_Line ("BATCH MODE ERROR: Invalid option configuration of " &
                                        specification.get_namebase & ":standard port");
                        TIO.Put_Line ("Run ravenadm set-options " & specification.get_namebase &
                                        " to rectify the issue");
                        TIO.Put_Line ("Alternatively, set configuration option '[Q] Assume " &
                                        "default options' to False");
                        successful := False;
                        return;
                     end if;
                  else
                     PST.set_option_to_default_values (specification);
                  end if;
               else
                  if DIR.Exists (optfile) then
                     if not read_option_file then
                        if not launch_and_read (optfile, cookie) then
                           successful := False;
                           return;
                        end if;
                     end if;
                  else
                     if DIR.Exists (cookie) then
                        PST.set_option_to_default_values (specification);
                     else
                        if not launch_and_read (optfile, cookie) then
                           successful := False;
                           return;
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         else
            --  All defined options are dedicated to variant definition (nothing to configure)
            PST.set_option_to_default_values (specification);
         end if;
      end;

      PST.apply_directives
        (specs         => specification,
         variant       => variant,
         arch_standard => sysrootver.arch,
         osmajor       => HT.USS (sysrootver.major));

      PST.set_outstanding_ignore
        (specs         => specification,
         variant       => variant,
         opsys         => platform_type,
         arch_standard => sysrootver.arch,
         osrelease     => HT.USS (sysrootver.release),
         osmajor       => HT.USS (sysrootver.major));

      if portloc /= "" then
         PST.shift_extra_patches
           (specs         => specification,
            extract_dir   => portloc);

         PSM.generator
           (specs         => specification,
            variant       => variant,
            opsys         => platform_type,
            arch          => sysrootver.arch,
            output_file   => makefile);
      end if;
   end parse_and_transform_buildsheet;


   --------------------------------------------------------------------------------------------
   --  build_subpackages
   --------------------------------------------------------------------------------------------
   function build_subpackages
     (builder     : builders;
      sequence_id : port_id;
      sysrootver  : sysroot_characteristics;
      interactive : Boolean := False;
      enterafter  : String := "") return Boolean
   is
      function get_buildsheet return String;

      namebase : String := HT.USS (all_ports (sequence_id).port_namebase);
      variant  : String := HT.USS (all_ports (sequence_id).port_variant);
      bucket   : String := all_ports (sequence_id).bucket;
      portloc  : String := HT.USS (PM.configuration.dir_buildbase) &
                           "/" & REP.slave_name (builder) & "/port";

      function get_buildsheet return String is
      begin
         if all_ports (sequence_id).unkind_custom then
            return HT.USS (PM.configuration.dir_unkindness) & "/bucket_" & bucket & "/" & namebase;
         else
            return HT.USS (PM.configuration.dir_conspiracy) & "/bucket_" & bucket & "/" & namebase;
         end if;
      end get_buildsheet;

      buildsheet    : constant String := get_buildsheet;
      specification : Port_Specification.Portspecs;
      successful    : Boolean;
   begin
      parse_and_transform_buildsheet (specification => specification,
                                      successful    => successful,
                                      buildsheet    => buildsheet,
                                      variant       => variant,
                                      portloc       => portloc,
                                      excl_targets  => False,
                                      avoid_dialog  => True,
                                      sysrootver    => sysrootver);
      if not successful then
         return False;
      end if;

      return CYC.build_package (id            => builder,
                                specification => specification,
                                sequence_id   => sequence_id,
                                interactive   => interactive,
                                interphase    => enterafter);
   end build_subpackages;


   --------------------------------------------------------------------------------------------
   --  eliminate_obsolete_packages
   --------------------------------------------------------------------------------------------
   procedure eliminate_obsolete_packages
   is
      procedure search (position : subpackage_crate.Cursor);
      procedure kill (position : string_crate.Cursor);

      id : port_index;
      counter : Natural := 0;
      repo : constant String := HT.USS (PM.configuration.dir_repository) & "/";

      procedure search (position : subpackage_crate.Cursor)
      is
         rec : subpackage_record renames subpackage_crate.Element (position);
         subpackage   : constant String := HT.USS (rec.subpackage);
         package_name : HT.Text := HT.SUS (calculate_package_name (id, subpackage) & arc_ext);
      begin
         if package_list.Contains (package_name) then
            package_list.Delete (package_list.Find_Index (package_name));
         end if;
      end search;

      procedure kill (position : string_crate.Cursor)
      is
         package_name : constant String := HT.USS (string_crate.Element (position));
      begin
         DIR.Delete_File (repo & package_name);
         counter := counter + 1;
      exception
         when others =>
            TIO.Put (LAT.LF & "Failed to remove " & package_name);
      end kill;
   begin
      for index in port_index'First .. last_port loop
         id := index;
         all_ports (index).subpackages.Iterate (search'Access);
      end loop;
      TIO.Put ("Removing obsolete packages ... ");
      package_list.Iterate (kill'Access);
      TIO.Put_Line ("done!  (packaged deleted: " & HT.int2str (counter) & ")");
   end eliminate_obsolete_packages;

end PortScan.Operations;
