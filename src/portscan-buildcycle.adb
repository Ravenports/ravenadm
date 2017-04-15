--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Unix;
with Replicant;
with Parameters;
with PortScan.Log;
with PortScan.Tests;
with PortScan.Packager;
with File_Operations;
with Ada.Directories;
with Ada.Characters.Latin_1;

package body PortScan.Buildcycle is

   package LAT renames Ada.Characters.Latin_1;
   package DIR renames Ada.Directories;
   package FOP renames File_Operations;
   package LOG renames PortScan.Log;
   package PKG renames PortScan.Packager;
   package TST renames PortScan.Tests;
   package PM  renames Parameters;
   package REP renames Replicant;

   --------------------------------------------------------------------------------------------
   --  build_package
   --------------------------------------------------------------------------------------------
   function build_package (id            : builders;
                           sequence_id   : port_id;
                           specification : PSP.Portspecs;
                           interactive   : Boolean := False;
                           interphase    : String  := "") return Boolean
   is
      R : Boolean;
      break_phase  : constant phases := valid_test_phase (interphase);
      run_selftest : constant Boolean := Unix.env_variable_defined (selftest);
      pkgversion   : constant String := HT.USS (all_ports (trackers (id).seq_id).pkgversion);
      port_prefix  : constant String := get_port_prefix (id);
   begin
      trackers (id).seq_id := sequence_id;
      trackers (id).loglines := 0;
      trackers (id).check_strip := not specification.debugging_is_on;
      trackers (id).rpath_fatal := specification.rpath_check_errors_are_fatal;
      if not LOG.initialize_log (log_handle => trackers (id).log_handle,
                                 head_time  => trackers (id).head_time,
                                 seq_id     => trackers (id).seq_id,
                                 slave_root => get_root (id),
                                 UNAME      => HT.USS (uname_mrv),
                                 BENV       => get_environment (id),
                                 COPTS      => specification.options_summary,
                                 PTVAR      => get_port_variables (id))
      then
         LOG.finalize_log (trackers (id).log_handle,
                           trackers (id).head_time,
                           trackers (id).tail_time);
         return False;
      end if;
      for phase in phases'Range loop
         phase_trackers (id) := phase;
         case phase is
            when blr_depends =>
               R := exec_phase_depends (specification => specification,
                                        phase_name    => phase2str (phase),
                                        id            => id);

            when  fetch =>
               REP.hook_toolchain (id);
               R := exec_phase_generic (id, phase);

            when  checksum | extract | patch =>
               R := exec_phase_generic (id, phase);

            when configure =>
               if testing then
                  mark_file_system (id, "preconfig");
               end if;
               R := exec_phase_generic (id, phase);

            when build =>
               R := exec_phase_build (id);

            when stage =>
               if testing then
                  mark_file_system (id, "prestage");
               end if;
               R := exec_phase_generic (id, phase);

            when test =>
               if testing and run_selftest then
                  R := exec_phase_generic (id, phase);
               end if;
               REP.unhook_toolchain (id);

            when pkg_package =>
               R := PKG.exec_phase_package (specification => specification,
                                            log_handle    => trackers (id).log_handle,
                                            log_name      => LOG.log_name (trackers (id).seq_id),
                                            phase_name    => phase2str (phase),
                                            seq_id        => trackers (id).seq_id,
                                            port_prefix   => port_prefix,
                                            rootdir       => get_root (id));

            when install =>
               if testing then
                  R := exec_phase_install (id, pkgversion);
               end if;

            when check_plist =>
               if testing then
                  R := TST.exec_check_plist (specification => specification,
                                             log_handle    => trackers (id).log_handle,
                                             phase_name    => phase2str (phase),
                                             seq_id        => trackers (id).seq_id,
                                             port_prefix   => port_prefix,
                                             rootdir       => get_root (id));
               end if;

            when deinstall =>
               if testing then
                  R := exec_phase_deinstall (id, pkgversion);
               end if;
         end case;
         exit when R = False;
         exit when interactive and then phase = break_phase;
      end loop;
      LOG.finalize_log (trackers (id).log_handle,
                        trackers (id).head_time,
                        trackers (id).tail_time);
      if interactive then
         interact_with_builder (id);
      end if;
      return R;
   end build_package;


   --------------------------------------------------------------------------------------------
   --  last_build_phase
   --------------------------------------------------------------------------------------------
   function last_build_phase (id : builders) return String is
   begin
      return phase2str (phase => phase_trackers (id));
   end last_build_phase;


   --------------------------------------------------------------------------------------------
   --  max_time_without_output
   --------------------------------------------------------------------------------------------
   function max_time_without_output (phase : phases) return execution_limit
   is
      base : Integer;
   begin
      case phase is
         when blr_depends      => base := 15;  --  octave forge extraction is driver
         when fetch | checksum => return 480;  --  8 hours
         when extract          => base := 20;
         when patch            => base := 3;
         when configure        => base := 15;
         when build            => base := 25;   --  for gcc linking, tex
         when stage            => base := 20;   --  desire 15 but too many rogue builders-in-stage
         when test             => base := 25;
         when check_plist      => base := 10;   --  For packages with thousands of files
         when pkg_package      => base := 80;
         when install          => base := 10;
         when deinstall        => base := 10;
      end case;
      declare
         multiplier_x10 : constant Positive := timeout_multiplier_x10;
      begin
         return execution_limit (base * multiplier_x10 / 10);
      end;
   end max_time_without_output;


   --------------------------------------------------------------------------------------------
   --  phase2str
   --------------------------------------------------------------------------------------------
   function phase2str (phase : phases) return String
   is
      --  Locked into 12-character length limit.  Any longer requires modification to
      --  display package and build_status function.
   begin
      case phase is
         when blr_depends     => return "dependencies";
         when fetch           => return "fetch";
         when checksum        => return "checksum";
         when extract         => return "extract";
         when patch           => return "patch";
         when configure       => return "configure";
         when build           => return "build";
         when stage           => return "stage";
         when test            => return "test";
         when pkg_package     => return "package";
         when install         => return "install";
         when deinstall       => return "deinstall";
         when check_plist     => return "check-plist";
      end case;
   end phase2str;


   --------------------------------------------------------------------------------------------
   --  valid_test_phase #1
   --------------------------------------------------------------------------------------------
   function valid_test_phase (afterphase : String) return phases is
   begin
      if afterphase = "extract" then
         return extract;
      elsif afterphase = "patch" then
         return patch;
      elsif afterphase = "configure" then
         return configure;
      elsif afterphase = "build" then
         return build;
      elsif afterphase = "stage" then
         return stage;
      elsif afterphase = "package" then
         return pkg_package;
      elsif afterphase = "install" then
         return install;
      elsif afterphase = "deinstall" then
         return deinstall;
      else
         return phases'First;
      end if;
   end valid_test_phase;


   --------------------------------------------------------------------------------------------
   --  valid_test_phase #2
   --------------------------------------------------------------------------------------------
   function valid_test_phase (afterphase : String) return Boolean is
   begin
      return
        afterphase = "extract"   or else
        afterphase = "patch"     or else
        afterphase = "configure" or else
        afterphase = "build"     or else
        afterphase = "stage"     or else
        afterphase = "install"   or else
        afterphase = "deinstall";
   end valid_test_phase;


   --------------------------------------------------------------------------------------------
   --  exec_phase_generic
   --------------------------------------------------------------------------------------------
   function exec_phase_generic (id : builders; phase : phases) return Boolean
   is
      time_limit : execution_limit := max_time_without_output (phase);
   begin
      return exec_phase (id => id, phase => phase, time_limit => time_limit);
   end exec_phase_generic;


   --------------------------------------------------------------------------------------------
   --  exec_phase_build
   --------------------------------------------------------------------------------------------
   function exec_phase_build (id : builders) return Boolean
   is
      time_limit : execution_limit := max_time_without_output (build);
      passed : Boolean;
   begin
      passed := exec_phase (id          => id,
                            phase       => build,
                            time_limit  => time_limit,
                            skip_header => False,
                            skip_footer => True);
      if testing and then passed then
         passed := detect_leftovers_and_MIA (id, "preconfig", "between port configure and build");
      end if;
      LOG.log_phase_end (trackers (id).log_handle);
      return passed;
   end exec_phase_build;


   --------------------------------------------------------------------------------------------
   --  exec_phase_depends
   --------------------------------------------------------------------------------------------
   function  exec_phase_depends
     (specification : PSP.Portspecs;
      phase_name    : String;
      id            : builders) return Boolean
   is
      time_limit : execution_limit := max_time_without_output (install);
      block      : constant String := specification.combined_dependency_origins;
      PKG_ADD    : constant String := "/usr/bin/pkg-static add ";
      root       : constant String := get_root (id);
      markers    : HT.Line_Markers;
      still_good : Boolean := True;
      timed_out  : Boolean;
   begin
      LOG.log_phase_begin (trackers (id).log_handle, phase_name);

       HT.initialize_markers (block, markers);
      loop
         exit when not still_good;
         exit when not HT.next_line_present (block, markers);
         declare
            line       : constant String  := HT.extract_line (block, markers);
            portkey    : constant String  := convert_depend_origin_to_portkey (line);
            ptid       : constant port_id := ports_keys (HT.SUS (portkey));
            pkgname    : constant String  := HT.replace_all (line, LAT.Colon, LAT.Hyphen);
            pkgversion : constant String  := HT.USS (all_ports (ptid).pkgversion);
            pkgfile    : constant String  := pkgname & LAT.Hyphen & pkgversion & arc_ext;
            fullpath   : constant String  := HT.USS (PM.configuration.dir_repository) & "/" &
                                             pkgfile;
            command    : constant String  := chroot & root & environment_override (id) &
                                             PKG_ADD &  "/packages/All/" & pkgfile;
         begin
            if DIR.Exists (fullpath) then
               TIO.Put_Line (trackers (id).log_handle, "===>  Installing " & pkgname & " package");
               TIO.Close (trackers (id).log_handle);
               still_good := generic_execute (id, command, timed_out, time_limit);
               TIO.Open (File => trackers (id).log_handle,
                         Mode => TIO.Append_File,
                         Name => LOG.log_name (trackers (id).seq_id));
               if timed_out then
                  TIO.Put_Line (trackers (id).log_handle, watchdog_message (time_limit));
               end if;
            else
               still_good := False;
               TIO.Put_Line (trackers (id).log_handle, "Dependency package not found: " & pkgfile);
            end if;
         end;
      end loop;

      LOG.log_phase_end (trackers (id).log_handle);
      return still_good;
   end exec_phase_depends;


   --------------------------------------------------------------------------------------------
   --  exec_phase_install
   --------------------------------------------------------------------------------------------
   function exec_phase_install (id : builders; pkgversion : String) return Boolean
   is
      procedure install_it (position : subpackage_crate.Cursor);

      time_limit : execution_limit := max_time_without_output (install);
      root       : constant String := get_root (id);
      namebase   : constant String := HT.USS (all_ports (trackers (id).seq_id).port_namebase);
      PKG_ADD    : constant String := "/usr/bin/pkg-static add ";
      still_good : Boolean := True;
      timed_out  : Boolean;

      procedure install_it (position : subpackage_crate.Cursor)
      is
         rec        : subpackage_record renames subpackage_crate.Element (position);
         subpackage : constant String :=  HT.USS (rec.subpackage);
         pkgname    : String := calculate_package_name (trackers (id).seq_id, subpackage);
         PKG_FILE   : constant String := "/packages/All/" & pkgname & arc_ext;
         command    : constant String := chroot & root & environment_override (id) &
                      PKG_ADD & PKG_FILE;
      begin
         if still_good then
            TIO.Put_Line (trackers (id).log_handle, "===>  Installing " & pkgname & " package");
            TIO.Close (trackers (id).log_handle);
            still_good := generic_execute (id, command, timed_out, time_limit);
            TIO.Open (File => trackers (id).log_handle,
                      Mode => TIO.Append_File,
                      Name => LOG.log_name (trackers (id).seq_id));
            if timed_out then
               TIO.Put_Line (trackers (id).log_handle, watchdog_message (time_limit));
            end if;
         end if;
      end install_it;
   begin
      LOG.log_phase_begin (trackers (id).log_handle, phase2str (install));
      all_ports (trackers (id).seq_id).subpackages.Iterate (install_it'Access);
      LOG.log_phase_end (trackers (id).log_handle);
      return still_good;
   end exec_phase_install;


   --------------------------------------------------------------------------------------------
   --  exec_phase
   --------------------------------------------------------------------------------------------
   function exec_phase (id            : builders;
                        phase         : phases;
                        time_limit    : execution_limit;
                        phaseenv      : String := "";
                        depends_phase : Boolean := False;
                        skip_header   : Boolean := False;
                        skip_footer   : Boolean := False)
                        return Boolean
   is
      root       : constant String := get_root (id);
      pid        : port_id := trackers (id).seq_id;
      result     : Boolean;
      timed_out  : Boolean;
   begin
      --  Nasty, we have to switch open and close the log file for each
      --  phase because we have to switch between File_Type and File
      --  Descriptors.  I can't find a safe way to get the File Descriptor
      --  out of the File type.

      if not skip_header then
         LOG.log_phase_begin (trackers (id).log_handle, phase2str (phase));
      end if;
      TIO.Close (trackers (id).log_handle);

      declare
         command : constant String := chroot & root & environment_override (id) &
           phaseenv & chroot_make_program & " -C /port " & phase2str (phase);
      begin
         result := generic_execute (id, command, timed_out, time_limit);
      end;

      --  Reopen the log.  I guess we can leave off the exception check
      --  since it's been passing before

      TIO.Open (File => trackers (id).log_handle,
                Mode => TIO.Append_File,
                Name => LOG.log_name (trackers (id).seq_id));
      if timed_out then
         TIO.Put_Line (trackers (id).log_handle, watchdog_message (time_limit));
      end if;
      if not skip_footer then
         LOG.log_phase_end (trackers (id).log_handle);
      end if;

      return result;
   end exec_phase;


   --------------------------------------------------------------------------------------------
   --  get_port_variables
   --------------------------------------------------------------------------------------------
   function get_port_variables (id : builders) return String
   is
      root    : constant String := get_root (id);
      command : constant String := chroot & root & environment_override (id) &
        chroot_make_program & " -C /port -VCONFIGURE_ENV -VCONFIGURE_ARGS" &
                              " -VMAKE_ENV -VMAKE_ARGS -VPLIST_SUB -VSUB_LIST";
   begin
      return generic_system_command (command);
   exception
      when others => return discerr;
   end get_port_variables;


   --------------------------------------------------------------------------------------------
   --  generic_system_command
   --------------------------------------------------------------------------------------------
   function generic_system_command (command : String) return String
   is
      content : HT.Text;
      status  : Integer;
   begin
      content := Unix.piped_command (command, status);
      if status /= 0 then
         declare
            message : String := command & " (return code =" & status'Img & ")";
            projlen : Natural := message'Length + 5;
         begin
            if projlen > 200 then
               raise cycle_cmd_error
                 with "cmd: ..." & message (message'Last - 191 .. message'Last);
            else
               raise cycle_cmd_error with "cmd: " & message;
            end if;
         end;
      end if;
      return HT.USS (content);
   end generic_system_command;


   --------------------------------------------------------------------------------------------
   --  set_uname_mrv
   --------------------------------------------------------------------------------------------
   procedure set_uname_mrv
   is
      command : constant String := HT.USS (PM.configuration.dir_sysroot) & "/usr/bin/uname -mrv";
   begin
      uname_mrv := HT.SUS (generic_system_command (command));
   end set_uname_mrv;


   --------------------------------------------------------------------------------------------
   --  get_root
   --------------------------------------------------------------------------------------------
   function get_root (id : builders) return String
   is
      suffix : String := "/SL" & HT.zeropad (Integer (id), 2);
   begin
      return HT.USS (PM.configuration.dir_buildbase) & suffix;
   end get_root;


   --------------------------------------------------------------------------------------------
   --  get_environment
   --------------------------------------------------------------------------------------------
   function get_environment (id : builders) return String
   is
      root    : constant String := get_root (id);
      command : constant String := chroot & root & environment_override (id);
   begin
      return generic_system_command (command);
   exception
      when others =>
         return discerr;
   end get_environment;


   --------------------------------------------------------------------------------------------
   --  environment_override
   --------------------------------------------------------------------------------------------
   function environment_override (id : builders; enable_tty : Boolean := False) return String
   is
      function set_terminal (enable_tty : Boolean) return String;
      function toolchain_path return String;

      localbase : constant String := HT.USS (PM.configuration.dir_localbase);

      function set_terminal (enable_tty : Boolean) return String is
      begin
         if enable_tty then
            return "TERM=cons25 ";
         end if;
         return "TERM=dumb ";
      end set_terminal;

      function toolchain_path return String is
      begin
         if phases'Pos (phase_trackers (id)) < phases'Pos (stage)
           or else phase_trackers (id) = test
         then
            return localbase & "/toolchain/gcc6/bin:";
         else
            return "";
         end if;
      end toolchain_path;

      PATH : constant String := "PATH=/bin:/usr/bin:"
        & toolchain_path
        & localbase & "/toolchain/bin:"
        & localbase & "/sbin:"
        & localbase & "/bin ";

      TERM : constant String := set_terminal (enable_tty);
      USER : constant String := "USER=root ";
      HOME : constant String := "HOME=/root ";
      LANG : constant String := "LANG=C ";
      PKG8 : constant String := "PKG_DBDIR=/var/db/pkg8 " &
                                "PKG_CACHEDIR=/var/cache/pkg8 ";
      CENV : constant String := HT.USS (customenv);
   begin
      return " /usr/bin/env -i " & CENV & LANG & TERM & USER & HOME & PKG8 & PATH;
   end environment_override;


   --------------------------------------------------------------------------------------------
   --  obtain_custom_environment
   --------------------------------------------------------------------------------------------
   procedure obtain_custom_environment
   is
      target_name : constant String := PM.raven_confdir & "/" &
                                       HT.USS (PM.configuration.profile) & "-environment";
   begin
      customenv := HT.blank;
      if not DIR.Exists (target_name) then
         return;
      end if;

      declare
         contents : String := FOP.get_file_contents (target_name);
         markers  : HT.Line_Markers;
      begin
         HT.initialize_markers (contents, markers);
         loop
            exit when not HT.next_line_present (contents, markers);
            declare
               line : constant String := HT.extract_line (contents, markers);
            begin
               if HT.contains (line, "=") then
                  HT.SU.Append (customenv, HT.trim (line) & " ");
               end if;
            end;
         end loop;
      end;
   end obtain_custom_environment;


   --------------------------------------------------------------------------------------------
   --  initialize
   --------------------------------------------------------------------------------------------
   procedure initialize (test_mode : Boolean) is
   begin
      set_uname_mrv;
      testing   := test_mode;
      declare
         logdir : constant String := HT.USS (PM.configuration.dir_logs);
      begin
         if not DIR.Exists (logdir) then
            DIR.Create_Path (New_Directory => logdir);
         end if;
      exception
         when error : others =>
            raise scan_log_error
              with "failed to create " & logdir;
      end;
      obtain_custom_environment;
   end initialize;


   --------------------------------------------------------------------------------------------
   --  initialize
   --------------------------------------------------------------------------------------------
   function exec_phase_deinstall (id : builders; pkgversion : String) return Boolean
   is
      procedure deinstall_it (position : subpackage_crate.Cursor);

      time_limit : execution_limit := max_time_without_output (deinstall);
      root       : constant String := get_root (id);
      namebase   : constant String := HT.USS (all_ports (trackers (id).seq_id).port_namebase);
      PKG_DELETE : constant String := "/usr/bin/pkg-static delete -f -y ";
      still_good : Boolean := True;
      dyn_good   : Boolean;
      timed_out  : Boolean;

      procedure deinstall_it (position : subpackage_crate.Cursor)
      is
         rec        : subpackage_record renames subpackage_crate.Element (position);
         subpackage : constant String := HT.USS (rec.subpackage);
         pkgname    : String := calculate_package_name (trackers (id).seq_id, subpackage);
         command    : constant String := chroot & root & environment_override (id) &
                      PKG_DELETE & pkgname;
      begin
         if still_good then
            TIO.Put_Line (trackers (id).log_handle, "===>  Deinstalling " & pkgname & " package");
            TIO.Close (trackers (id).log_handle);
            still_good := generic_execute (id, command, timed_out, time_limit);
            TIO.Open (File => trackers (id).log_handle,
                      Mode => TIO.Append_File,
                      Name => LOG.log_name (trackers (id).seq_id));
            if timed_out then
               TIO.Put_Line (trackers (id).log_handle, watchdog_message (time_limit));
            end if;
         end if;
      end deinstall_it;
   begin
      LOG.log_phase_begin (trackers (id).log_handle, phase2str (deinstall));
      dyn_good := log_linked_libraries (id, pkgversion);
      all_ports (trackers (id).seq_id).subpackages.Iterate (deinstall_it'Access);
      if still_good then
         still_good := detect_leftovers_and_MIA
           (id, "prestage", "between staging and package deinstallation");
      end if;
      LOG.log_phase_end (trackers (id).log_handle);
      return still_good and then dyn_good;
   end exec_phase_deinstall;


   --------------------------------------------------------------------------------------------
   --  stack_linked_libraries
   --------------------------------------------------------------------------------------------
   procedure stack_linked_libraries (id : builders; base, filename : String)
   is
      objdump : String := "/usr/bin/objdump-sysroot";
      command : String := chroot & base & environment_override (id) & objdump & " -p " & filename;
   begin
      declare
         comres  : String :=  generic_system_command (command);
         markers : HT.Line_Markers;
         pathstr : HT.Text := HT.blank;
         initial : String := "  NEEDED";
         runpath : String := "  RUNPATH";
         rpath   : String := "  RPATH";
      begin
         HT.initialize_markers (comres, markers);
         if HT.next_line_with_content_present (comres, runpath, markers) then
            declare
               line : constant String := HT.extract_line (comres, markers);
            begin
               pathstr := HT.SUS (HT.trim (HT.part_2 (line, runpath)));
            end;
         else
            HT.initialize_markers (comres, markers);
            if HT.next_line_with_content_present (comres, rpath, markers) then
               declare
                  line : constant String := HT.extract_line (comres, markers);
               begin
                  pathstr := HT.SUS (HT.trim (HT.part_2 (line, rpath)));
               end;
            end if;
         end if;
         HT.initialize_markers (comres, markers);
         loop
            exit when not HT.next_line_with_content_present (comres, initial, markers);
            declare
               line      : constant String := HT.extract_line (comres, markers);
               shlib     : constant String := " " & HT.trim (HT.part_2 (line, initial));
               shpayload : HT.Text := HT.SUS (HT.USS (pathstr) & shlib);
               line_text : HT.Text := HT.SUS (line);
            begin
               if not trackers (id).dynlink.Contains (line_text) then
                  trackers (id).dynlink.Append (line_text);
               end if;
               if not trackers (id).runpaths.Contains (shpayload) then
                  trackers (id).runpaths.Append (shpayload);
               end if;
            end;
         end loop;
      end;
   exception
         --  the command result was not zero, so it was an expected format
         --  or static file.  Just skip it.  (Should never happen)
      when bad_result : others => null;
   end stack_linked_libraries;


   --------------------------------------------------------------------------------------------
   --  log_linked_libraries
   --------------------------------------------------------------------------------------------
   function log_linked_libraries (id : builders; pkgversion : String) return Boolean
   is
      procedure log_dump (position : string_crate.Cursor);
      procedure check_package (position : subpackage_crate.Cursor);

      root     : constant String := get_root (id);
      namebase : constant String := HT.USS (all_ports (trackers (id).seq_id).port_namebase);
      result   : Boolean := True;

      procedure log_dump (position : string_crate.Cursor)
      is
        info : String := "   " &  HT.USS (string_crate.Element (position));
      begin
         TIO.Put_Line (trackers (id).log_handle, info);
      end log_dump;

      procedure check_package (position : subpackage_crate.Cursor)
      is
         rec        : subpackage_record renames subpackage_crate.Element (position);
         subpackage : constant String := HT.USS (rec.subpackage);
         pkgname    : String := calculate_package_name (trackers (id).seq_id, subpackage);
         command    : constant String := chroot & root & environment_override (id) &
                      "/usr/bin/pkg-static query %Fp " & pkgname;
         comres     : String :=  generic_system_command (command);
         markers    : HT.Line_Markers;
      begin
         trackers (id).dynlink.Clear;
         trackers (id).runpaths.Clear;
         trackers (id).checkpaths.Clear;
         trackers (id).goodpaths.Clear;
         HT.initialize_markers (comres, markers);
         loop
            exit when not HT.next_line_present (comres, markers);
            declare
               filename   : constant String := HT.extract_line (comres, markers);
               unstripped : Boolean;
            begin
               if dynamically_linked (base        => root,
                                      filename    => filename,
                                      strip_check => trackers (id).check_strip,
                                      unstripped  => unstripped)
               then
                  stack_linked_libraries (id, root, filename);
                  if not passed_runpath_check (id) then
                     result := False;
                  end if;
               end if;
               if unstripped then
                  TIO.Put_Line
                    (trackers (id).log_handle,
                     "### WARNING ###  " & filename & " is not stripped.  " &
                       "See Ravenporter's guide.");
               end if;
            end;
         end loop;
         if not trackers (id).dynlink.Is_Empty then
            TIO.Put_Line (trackers (id).log_handle, "===> " & pkgname & " subpackage:");
            trackers (id).dynlink.Iterate (log_dump'Access);
         end if;
      exception
         when others => null;
      end check_package;
   begin
      TIO.Put_Line (trackers (id).log_handle, "=> Checking shared library dependencies");
      all_ports (trackers (id).seq_id).subpackages.Iterate (check_package'Access);
      return result;
   end log_linked_libraries;


   --------------------------------------------------------------------------------------------
   --  dynamically_linked
   --------------------------------------------------------------------------------------------
   function dynamically_linked
     (base        : String;
      filename    : String;
      strip_check : Boolean;
      unstripped  : out Boolean) return Boolean
   is
      command : String :=
        chroot & base & " /usr/bin/file -b -L -e ascii -e encoding -e tar -e compress " &
        "-m /usr/share/file/magic.mgc " & LAT.Quotation & filename & LAT.Quotation;
      dynlinked  : Boolean;
      statlinked : Boolean;
   begin
      unstripped := False;
      declare
         comres  : constant String := generic_system_command (command);
      begin
         dynlinked  := HT.contains (comres,  "dynamically linked");
         if dynlinked then
            statlinked := False;
         else
            statlinked := HT.contains (comres,  "statically linked");
         end if;

         if strip_check then
            if dynlinked or else statlinked then
               if HT.contains (comres, ", not stripped") then
                  unstripped := True;
               end if;
            end if;
         end if;
         return dynlinked;
      end;
   exception
      when others =>
         return False;
   end dynamically_linked;


   --------------------------------------------------------------------------------------------
   --  passed_runpath_check
   --------------------------------------------------------------------------------------------
   function passed_runpath_check (id : builders) return Boolean
   is
      procedure scan (position : string_crate.Cursor);
      function errmsg_prefix return String;

      result : Boolean := True;
      root   : constant String := get_root (id);
      fail_result : Boolean := not trackers (id).rpath_fatal;

      function errmsg_prefix return String is
      begin
         if trackers (id).rpath_fatal then
            return "### FATAL ERROR ###  ";
         else
            return "### WARNING ###  ";
         end if;
      end errmsg_prefix;

      procedure scan (position : string_crate.Cursor)
      is
         line      : String := HT.USS (string_crate.Element (position));
         paths     : String := HT.part_1 (line, " ");
         library   : String := HT.part_2 (line, " ");
         lib_text  : HT.Text := HT.SUS (library);
         numfields : Natural := HT.count_char (paths, LAT.Colon) + 1;
         errmsg    : String := errmsg_prefix & library &
                     " is not in located in /usr/lib or within the RPATH/RUNPATH";
         systemlib : String := "/usr/lib/" & library;
         syslibtxt : HT.Text := HT.SUS (systemlib);
         attempted : Boolean := False;
      begin
         --  Check /usr/lib first
         if trackers (id).goodpaths.Contains (syslibtxt) then
            return;
         end if;
         if not trackers (id).checkpaths.Contains (syslibtxt) then
            if DIR.Exists (root & systemlib) then
               trackers (id).goodpaths.Append (syslibtxt);
               return;
            end if;
            trackers (id).checkpaths.Append (syslibtxt);
            attempted := True;
         end if;

         if paths = "" then
            TIO.Put_Line (trackers (id).log_handle, errmsg);
            result := fail_result;
            return;
         end if;

         for n in 1 .. numfields loop
            declare
               testpath     : String := HT.specific_field (paths, n, ":");
               test_library : String := testpath & "/" & library;
               test_lib_txt : HT.Text := HT.SUS (test_library);
            begin
               if trackers (id).goodpaths.Contains (test_lib_txt) then
                  return;
               end if;
               if not trackers (id).checkpaths.Contains (test_lib_txt) then
                  if DIR.Exists (root & test_library) then
                     trackers (id).goodpaths.Append (test_lib_txt);
                     return;
                  end if;
                  trackers (id).checkpaths.Append (test_lib_txt);
                  attempted := True;
               end if;
            end;
         end loop;

         if attempted then
            TIO.Put_Line (trackers (id).log_handle, errmsg);
            result := fail_result;
         end if;
      end scan;
   begin
      trackers (id).runpaths.Iterate (scan'Access);
      return result;
   end passed_runpath_check;


   --------------------------------------------------------------------------------------------
   --  timeout_multiplier_x10
   --------------------------------------------------------------------------------------------
   function timeout_multiplier_x10 return Positive
   is
      average5 : constant Float := load_core (instant_load => False);
      avefloat : constant Float := average5 / Float (PM.configuration.number_cores);
   begin
      if avefloat <= 1.0 then
         return 10;
      else
         return Integer (avefloat * 10.0);
      end if;
   exception
      when others => return 10;
   end timeout_multiplier_x10;


   --------------------------------------------------------------------------------------------
   --  load_core
   --------------------------------------------------------------------------------------------
   function load_core (instant_load : Boolean) return Float
   is
      function probe_load return String;

      ----------------- 123456789-123456789-123456789-
      --  DFLY/FreeBSD: vm.loadavg: { 0.00 0.00 0.00 }
      --  NetBSD:       vm.loadavg: 0.00 0.00 0.00
      --  Darwin:       vm.loadavg: { 1.21 1.07 1.15 }
      --  Linux:        0.00 0.01 0.05 3/382 15409
      --  Solaris:      [~42 chars]load average: 0.01, 0.01, 0.01

      zero : constant Float := 0.0;
      lo   : Integer;

      function probe_load return String
      is
         bsd  : constant String := "/usr/bin/env LANG=C /sbin/sysctl vm.loadavg";
         lin  : constant String := "/usr/bin/cat /proc/loadavg";
         sol  : constant String := "/usr/bin/uptime";
      begin
         case platform_type is
            when dragonfly | freebsd | macos =>
               lo := 14;
               return generic_system_command (bsd);
            when netbsd | openbsd =>
               lo := 12;
               return generic_system_command (bsd);
            when linux =>
               lo := 0;
               return generic_system_command (lin);
         when sunos =>
            return generic_system_command (sol);
         end case;
      end probe_load;

      comres : constant String := probe_load;
   begin
      case platform_type is
         when dragonfly | freebsd | netbsd | openbsd | linux | macos =>
            declare
               stripped : constant String := comres (comres'First + lo .. comres'Last);
            begin
               if instant_load then
                  declare
                     instant : String := HT.part_1 (stripped, " ");
                  begin
                     return Float'Value (instant);
                  end;
               else
                  declare
                     min5 : String := HT.part_1 (HT.part_2 (stripped, " "), " ");
                  begin
                      return Float'Value (min5);
                  end;
               end if;
            end;
         when sunos =>
            declare
               stripped : constant String := HT.part_2 (comres, "load average: ");
            begin
               if instant_load then
                  declare
                     instant  : constant String := HT.part_1 (stripped, ", ");
                  begin
                     return Float'Value (instant);
                  end;
               else
                  declare
                     min5 : String := HT.part_1 (HT.part_2 (stripped, ", "), ", ");
                  begin
                      return Float'Value (min5);
                  end;
               end if;
            end;
      end case;
   exception
      when others => return zero;
   end load_core;


   --------------------------------------------------------------------------------------------
   --  builder_status
   --------------------------------------------------------------------------------------------
   function builder_status (id       : builders;
                            shutdown : Boolean := False;
                            idle     : Boolean := False)
                            return Display.builder_rec
   is
      phasestr : constant String := phase2str (phase_trackers (id));
      result   : Display.builder_rec;
      orilimit : constant Positive := Display.fld_origin'Length;
      orishort : constant Natural  := orilimit - 1;
   begin
      --  123456789 123456789 123456789 123456789 1234
      --   SL  elapsed   phase              lines  origin
      --   01  00:00:00  extract-depends  9999999  www/joe

      result.id       := id;
      result.slavid   := HT.zeropad (Natural (id), 2);
      result.LLines   := (others => ' ');
      result.phase    := (others => ' ');
      result.origin   := (others => ' ');
      result.shutdown := False;
      result.idle     := False;

      if shutdown then
         --  Overrides "idle" if both Shutdown and Idle are True
         result.Elapsed  := "Shutdown";
         result.shutdown := True;
         return result;
      end if;
      if idle then
         result.Elapsed := "Idle    ";
         result.idle    := True;
         return result;
      end if;

      declare
         catport  : constant String := get_port_variant (all_ports (trackers (id).seq_id));
         numlines : constant String := format_loglines (trackers (id).loglines);
         linehead : constant Natural := 8 - numlines'Length;
      begin
         result.Elapsed := LOG.elapsed_HH_MM_SS (start => trackers (id).head_time,
                                                 stop  => CAL.Clock);
         result.LLines (linehead .. 7) := numlines;
         result.phase  (1 .. phasestr'Length) := phasestr;

         if catport'Length > orilimit then
            result.origin (1 .. orishort) := catport (catport'First .. catport'First + orishort);
            result.origin (orilimit) := LAT.Asterisk;
         else
            result.origin (1 .. catport'Length) := catport;
         end if;
      end;
      return result;
   end builder_status;


   --------------------------------------------------------------------------------------------
   --  format_loglines
   --------------------------------------------------------------------------------------------
   function format_loglines (numlines : Natural) return String
   is
   begin
      if numlines < 10000000 then      --  10 million
         return HT.int2str (numlines);
      end if;
      declare
         kilo    : constant Natural := numlines / 1000;
         kilotxt : constant String  := HT.int2str (kilo);
      begin
         if numlines < 100000000 then      --  100 million
            return kilotxt (1 .. 2) & "." & kilotxt (3 .. 5) & 'M';
         elsif numlines < 1000000000 then  --  1 billion
            return kilotxt (1 .. 3) & "." & kilotxt (3 .. 4) & 'M';
         else
            return kilotxt (1 .. 4) & "." & kilotxt (3 .. 3) & 'M';
         end if;
      end;
   end format_loglines;


   --------------------------------------------------------------------------------------------
   --  mark_file_system
   --------------------------------------------------------------------------------------------
   procedure mark_file_system (id : builders; action : String)
   is
      function attributes (action : String) return String;

      root     : constant String := get_root (id);
      mtfile   : constant String := "/etc/mtree." & action & ".exclude";
      resfile  : TIO.File_Type;

      function attributes (action : String) return String
      is
         core : constant String := "uid,gid,mode,sha1digest";
      begin
         if action = "preconfig" then
            return core & ",time";
         else
            return core;
         end if;
      end attributes;

      command  : constant String := chroot & root & environment_override (id) &
                 " /usr/bin/mtree -X " & mtfile & " -cn -k " & attributes (action) & " -p /";
      filename : constant String := root & "/tmp/mtree." & action;

   begin
      TIO.Create (File => resfile, Mode => TIO.Out_File, Name => filename);
      TIO.Put (resfile, generic_system_command (command));
      TIO.Close (resfile);
   exception
      when others =>
         if TIO.Is_Open (resfile) then
            TIO.Close (resfile);
         end if;
   end mark_file_system;


   --------------------------------------------------------------------------------------------
   --  interact_with_builder
   --------------------------------------------------------------------------------------------
   procedure interact_with_builder (id : builders)
   is
      root    : constant String := get_root (id);
      command : String := chroot & root & environment_override (id, True) & "/bin/sh";
      result  : Boolean;
   begin
      TIO.Put_Line ("Entering interactive test mode at the builder root directory.");
      TIO.Put_Line ("Type 'exit' when done exploring.");
      result := Unix.external_command (command);
   end interact_with_builder;


   --------------------------------------------------------------------------------------------
   --  detect_leftovers_and_MIA
   --------------------------------------------------------------------------------------------
   function detect_leftovers_and_MIA (id : builders;
                                      action : String;
                                      description : String) return Boolean
   is
      package crate is new CON.Vectors (Index_Type   => Positive,
                                       Element_Type => HT.Text,
                                       "="          => HT.SU."=");
      package sorter is new crate.Generic_Sorting ("<" => HT.SU."<");
      function  ignore_modifications return Boolean;
      procedure print (cursor : crate.Cursor);
      procedure close_active_modifications;

      root      : constant String := get_root (id);
      mtfile    : constant String := "/etc/mtree." & action & ".exclude";
      filename  : constant String := root & "/tmp/mtree." & action;
      command   : constant String := chroot & root & environment_override (id) &
                  "/usr/bin/mtree -X " & mtfile & " -f " & filename & " -p /";
      lbasewrk  : constant String := HT.USS (PM.configuration.dir_localbase);
      lbase     : constant String := lbasewrk (lbasewrk'First + 1 .. lbasewrk'Last);
      lblen     : constant Natural := lbase'Length;
      status    : Integer;
      skiprest  : Boolean;
      passed    : Boolean := True;
      activemod : Boolean := False;
      modport   : HT.Text := HT.blank;
      reasons   : HT.Text := HT.blank;
      leftover  : crate.Vector;
      missing   : crate.Vector;
      changed   : crate.Vector;
      markers   : HT.Line_Markers;

      --  we can't use generic_system_command because exit code /= 0 normally
      comres    : String := HT.USS (Unix.piped_command (command, status));

      function ignore_modifications return Boolean
      is
         --  Some modifications need to be ignored
         --  A) */ls-R
         --     #ls-R files from texmf are often regenerated
         --  B) share/xml/catalog.ports
         --     # xmlcatmgr is constantly updating catalog.ports, ignore
         --  C) share/octave/octave_packages
         --     # Octave packages database, blank lines can be inserted
         --     # between pre-install and post-deinstall
         --  D) info/dir | */info/dir
         --  E) lib/gio/modules/giomodule.cache
         --     # gio modules cache could be modified for any gio modules
         --  F) etc/gconf/gconf.xml.defaults/%gconf-tree*.xml
         --     # gconftool-2 --makefile-uninstall-rule is unpredictable
         --  G) %%PEARDIR%%/.depdb | %%PEARDIR%%/.filemap
         --     # The is pear database cache
         --  H) "." with timestamp modification
         --     # this happens when ./tmp or ./var is used, which is legal
         filename : constant String := HT.USS (modport);
         fnlen    : constant Natural := filename'Last;
      begin
         if filename = lbase & "/share/xml/catalog.ports" or else
           filename = lbase & "/share/octave/octave_packages" or else
           filename = lbase & "/share/info/dir" or else
           filename = lbase & "/lib/gio/modules/giomodule.cache" or else
           filename = lbase & "/share/pear/.depdb" or else
           filename = lbase & "/share/pear/.filemap"
         then
            return True;
         end if;
         if filename = "." and then HT.equivalent (reasons, "modification") then
            return True;
         end if;
         if fnlen > lblen + 7 and then
           filename (1 .. lblen + 1) = lbase & "/"
         then
            if filename (fnlen - 4 .. fnlen) = "/ls-R" or else
              filename (fnlen - 14 .. fnlen) = "/share/info/dir"
            then
               return True;
            end if;
         end if;
         if fnlen > 47 + lblen and then
           filename (1 .. 30 + lblen) = lbase & "/etc/gconf/gconf.xml.defaults/" and then
           filename (fnlen - 3 .. fnlen) = ".xml"
         then
            if HT.contains (filename, "/%gconf-tree") then
               return True;
            end if;
         end if;
         return False;
      end ignore_modifications;

      procedure close_active_modifications is
      begin
         if activemod and then not ignore_modifications then
            HT.SU.Append (modport, " [ ");
            HT.SU.Append (modport, reasons);
            HT.SU.Append (modport, " ]");
            if not changed.Contains (modport) then
               changed.Append (modport);
            end if;
         end if;
         activemod := False;
         reasons := HT.blank;
         modport := HT.blank;
      end close_active_modifications;

      procedure print (cursor : crate.Cursor)
      is
         dossier : constant String := HT.USS (crate.Element (cursor));
      begin
         TIO.Put_Line (trackers (id).log_handle, LAT.HT & dossier);
      end print;

   begin
       HT.initialize_markers (comres, markers);
      loop
         skiprest := False;
         exit when not HT.next_line_present (comres, markers);
         declare
            line    : constant String := HT.extract_line (comres, markers);
            linelen : constant Natural := line'Length;
         begin
            if not skiprest and then linelen > 6 then
               declare
                  caboose  : constant String := line (line'Last - 5 .. line'Last);
                  filename : HT.Text := HT.SUS (line (line'First .. line'Last - 6));
               begin
                  if caboose = " extra" then
                     close_active_modifications;
                     if not leftover.Contains (filename) then
                        leftover.Append (filename);
                     end if;
                     skiprest := True;
                  end if;
               end;
            end if;
            if not skiprest and then linelen > 7 then
               declare
                  canopy   : constant String := line (line'First .. line'First + 6);
                  filename : HT.Text := HT.SUS (line (line'First + 7 .. line'Last));
               begin
                  if canopy = "extra: " then
                     close_active_modifications;
                     if not leftover.Contains (filename) then
                        leftover.Append (filename);
                     end if;
                     skiprest := True;
                  end if;
               end;
            end if;
            if not skiprest and then linelen > 10 then
               declare
                  caboose  : constant String := line (line'Last - 7 .. line'Last);
                  filename : HT.Text := HT.SUS (line (line'First + 2 .. line'Last - 8));
               begin
                  if caboose = " missing" then
                     close_active_modifications;
                     if not missing.Contains (filename) then
                        missing.Append (filename);
                     end if;
                     skiprest := True;
                  end if;
               end;
            end if;
            if not skiprest then
               declare
                  blank8 : constant String := "        ";
               begin
                  if linelen > 5 and then line (line'First) = LAT.HT then
                     --  reason, but only valid if modification is active
                     if activemod then
                        if not HT.IsBlank (reasons) then
                           HT.SU.Append (reasons, " | ");
                        end if;
                        HT.SU.Append
                          (reasons, HT.part_1 (line (line'First + 1 .. line'Last), " "));
                     end if;
                     skiprest := True;
                  end if;
                  if not skiprest and then line (line'Last) = LAT.Colon then
                     close_active_modifications;
                     activemod := True;
                     modport := HT.SUS (line (line'First .. line'Last - 1));
                     skiprest := True;
                  end if;
                  if not skiprest and then
                    line (line'Last - 7 .. line'Last) = " changed"
                  then
                     close_active_modifications;
                     activemod := True;
                     modport := HT.SUS (line (line'First .. line'Last - 8));
                     skiprest := True;
                  end if;
               end;
            end if;
         end;
      end loop;
      close_active_modifications;
      sorter.Sort (Container => changed);
      sorter.Sort (Container => missing);
      sorter.Sort (Container => leftover);

      TIO.Put_Line (trackers (id).log_handle,
                    LAT.LF & "=> Checking for system changes " & description);
      if not leftover.Is_Empty then
         passed := False;
         TIO.Put_Line (trackers (id).log_handle, LAT.LF & "   Left over files/directories:");
         leftover.Iterate (Process => print'Access);
      end if;
      if not missing.Is_Empty then
         passed := False;
         TIO.Put_Line (trackers (id).log_handle, LAT.LF & "   Missing files/directories:");
         missing.Iterate (Process => print'Access);
      end if;
      if not changed.Is_Empty then
         passed := False;
         TIO.Put_Line (trackers (id).log_handle, LAT.LF & "   Modified files/directories:");
         changed.Iterate (Process => print'Access);
      end if;
      if passed then
         TIO.Put_Line (trackers (id).log_handle, "Everything is fine.");
      end if;
      return passed;
   end detect_leftovers_and_MIA;


   --------------------------------------------------------------------------------------------
   --  generic_execute
   --------------------------------------------------------------------------------------------
   function generic_execute (id : builders; command : String;
                             dogbite : out Boolean;
                             time_limit : execution_limit) return Boolean
   is
      subtype time_cycle is execution_limit range 1 .. time_limit;
      subtype one_minute is Positive range 1 .. 230;  --  lose 10 in rounding
      type dim_watchdog is array (time_cycle) of Natural;
      use type Unix.process_exit;
      watchdog    : dim_watchdog;
      squirrel    : time_cycle := time_cycle'First;
      cycle_done  : Boolean := False;
      pid         : Unix.pid_t;
      status      : Unix.process_exit;
      lock_lines  : Natural;
      quartersec  : one_minute := one_minute'First;
      hangmonitor : constant Boolean := True;
      truecommand : constant String := ravenexec & " " &
                             LOG.log_name (trackers (id).seq_id) & " " & command;
   begin
      dogbite := False;
      watchdog (squirrel) := trackers (id).loglines;

      pid := Unix.launch_process (truecommand);
      if Unix.fork_failed (pid) then
         return False;
      end if;
      loop
         delay 0.25;
         if quartersec = one_minute'Last then
            quartersec := one_minute'First;
            --  increment squirrel
            if squirrel = time_cycle'Last then
               squirrel := time_cycle'First;
               cycle_done := True;
            else
               squirrel := squirrel + 1;
            end if;
            if hangmonitor then
               lock_lines := trackers (id).loglines;
               if cycle_done then
                  if watchdog (squirrel) = lock_lines then
                     --  Log hasn't advanced in a full cycle so bail out
                     dogbite := True;
                     Unix.kill_process_tree (process_group => pid);
                     delay 5.0;  --  Give some time for error to write to log
                     return False;
                  end if;
               end if;
               watchdog (squirrel) := lock_lines;
            end if;
         else
            quartersec := quartersec + 1;
         end if;
         status := Unix.process_status (pid);
         if status = Unix.exited_normally then
            return True;
         end if;
         if status = Unix.exited_with_error then
            return False;
         end if;
      end loop;
   end generic_execute;


   --------------------------------------------------------------------------------------------
   --  watchdog_message
   --------------------------------------------------------------------------------------------
   function watchdog_message (minutes : execution_limit) return String is
   begin
      return "###  Watchdog killed runaway process!  (no activity for" &
                               minutes'Img & " minutes)  ###";
   end watchdog_message;


   --------------------------------------------------------------------------------------------
   --  assemble_history_record
   --------------------------------------------------------------------------------------------
   function assemble_history_record (slave  : builders;
                                     pid    : port_id;
                                     action : Display.history_action) return Display.history_rec
   is
      HR      : Display.history_rec;
      HOLast  : constant Natural := Display.history_origin'Last;
      catport : String := get_port_variant (pid);
      hyphens : constant Display.history_elapsed := "--:--:--";
   begin
      HR.id := slave;
      HR.slavid      := HT.zeropad (Integer (slave), 2);
      HR.established := True;
      HR.action      := action;
      HR.origin      := (others => ' ');
      HR.run_elapsed := LOG.elapsed_now;
      if action = Display.action_shutdown then
         HR.pkg_elapsed := hyphens;
      else
         if action = Display.action_skipped or else
           action = Display.action_ignored
         then
            HR.pkg_elapsed := hyphens;
         else
            HR.pkg_elapsed := LOG.elapsed_HH_MM_SS (start => trackers (slave).head_time,
                                                    stop  => trackers (slave).tail_time);
         end if;
         if catport'Last > HOLast then
            HR.origin (1 .. HOLast - 1) := catport (1 .. HOLast - 1);
            HR.origin (HOLast) := LAT.Asterisk;
         else
            HR.origin (1 .. catport'Last) := catport;
         end if;
      end if;
      return HR;
   end assemble_history_record;


   --------------------------------------------------------------------------------------------
   --  set_log_lines
   --------------------------------------------------------------------------------------------
   procedure set_log_lines (id : builders)
   is
      log_path : constant String := LOG.log_name (trackers (id).seq_id);
      command  : constant String := HT.USS (PM.configuration.dir_sysroot) &
                 "/usr/bin/wc -l " & log_path;
   begin
      declare
         numtext : constant String :=
           HT.part_1 (S => HT.trim (generic_system_command (command)), separator => " ");
      begin
         trackers (id).loglines := Natural'Value (numtext);
      end;
   exception
      when others => null;  -- just skip this cycle
   end set_log_lines;


   --------------------------------------------------------------------------------------------
   --  elapsed_build
   --------------------------------------------------------------------------------------------
   function elapsed_build (id : builders) return String is
   begin
      return LOG.elapsed_HH_MM_SS (start => trackers (id).head_time,
                                   stop  => trackers (id).tail_time);
   end elapsed_build;


   --------------------------------------------------------------------------------------------
   --  run_makesum
   --------------------------------------------------------------------------------------------
   procedure run_makesum (id : builders)
   is
      root     : constant String := get_root (id);
      command  : constant String := chroot & root & environment_override (id) &
                 chroot_make_program & " -C /port makesum";
      distinfo : constant String := root & "/port/distinfo";
      result   : constant String := generic_system_command (command);
   begin
      TIO.Put_Line (result);
      if DIR.Exists (distinfo) then
         TIO.Put_Line ("Copying " & distinfo & " to current directory");
         DIR.Copy_File (distinfo, "distinfo");
      end if;
   end run_makesum;


   --------------------------------------------------------------------------------------------
   --  get_port_prefix
   --------------------------------------------------------------------------------------------
   function get_port_prefix (id : builders) return String
   is
      root     : constant String := get_root (id);
      command  : constant String := chroot & root & environment_override (id) &
                 chroot_make_program & " -C /port -V PREFIX";
      result   : constant String := generic_system_command (command);
   begin
      return HT.first_line (result);
   end get_port_prefix;


end PortScan.Buildcycle;
