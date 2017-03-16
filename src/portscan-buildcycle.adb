--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Unix;
with Parameters;
with PortScan.Log;
with File_Operations;
with Ada.Directories;
with Ada.Characters.Latin_1;

package body PortScan.Buildcycle is

   package LAT renames Ada.Characters.Latin_1;
   package DIR renames Ada.Directories;
   package FOP renames File_Operations;
   package LOG renames PortScan.Log;
   package PM  renames Parameters;

   --------------------------------------------------------------------------------------------
   --  build_package
   --------------------------------------------------------------------------------------------
   function build_package (id          : builders;
                           sequence_id : port_id;
                           interactive : Boolean := False;
                           interphase  : String  := "") return Boolean
   is
      R : Boolean;
      break_phase  : constant phases := valid_test_phase (interphase);
      run_selftest : constant Boolean := Unix.env_variable_defined (selftest);
   begin
      trackers (id).seq_id := sequence_id;
      trackers (id).loglines := 0;
      if uselog then
         if not LOG.initialize_log (log_handle => trackers (id).log_handle,
                                    head_time  => trackers (id).head_time,
                                    seq_id     => trackers (id).seq_id,
                                    slave_root => get_root (id),
                                    UNAME      => HT.USS (uname_mrv),
                                    BENV       => get_environment (id),
                                    COPTS      => get_options_configuration (id),
                                    PTVAR      => get_port_variables (id))
         then
            LOG.finalize_log (trackers (id).log_handle,
                              trackers (id).head_time,
                              trackers (id).tail_time);
            return False;
         end if;
      end if;
      for phase in phases'Range loop
         phase_trackers (id) := phase;
         case phase is
            when blr_depends =>
               R := exec_phase_depends (id, phase);

            when  fetch | checksum | extract | patch | pkg_package =>
               R := exec_phase_generic (id, phase);

            when configure =>
               if testing then
                  mark_file_system (id, "preconfig");
               end if;
               R := exec_phase_generic (id, phase);

            when build =>
               R := exec_phase_build (id);

            when test =>
               if testing and run_selftest then
                  R := exec_phase_generic (id, phase);
               end if;

            when stage =>
               if testing then
                  mark_file_system (id, "prestage");
               end if;
               R := exec_phase_generic (id, phase);

            when install_mtree | install | check_plist =>
               if testing then
                  R := exec_phase_generic (id, phase);
               end if;

            when deinstall =>
               if testing then
                  R := exec_phase_deinstall (id);
               end if;
         end case;
         exit when R = False;
         exit when interactive and then phase = break_phase;
      end loop;
      if uselog then
         LOG.finalize_log (trackers (id).log_handle,
                           trackers (id).head_time,
                           trackers (id).tail_time);
      end if;
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
         when install_mtree    => base := 3;
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
   function phase2str (phase : phases) return String is
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
         when install_mtree   => return "install-mtree";
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
         return check_plist;
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
      if uselog then
         LOG.log_phase_end (trackers (id).log_handle);
      end if;
      return passed;
   end exec_phase_build;


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
         raise cycle_cmd_error with "cmd: " & command &
           " (return code =" & status'Img & ")";
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
      command : constant String := chroot & root & environment_override;
   begin
      return generic_system_command (command);
   exception
      when others =>
         return discerr;
   end get_environment;


   --------------------------------------------------------------------------------------------
   --  environment_override
   --------------------------------------------------------------------------------------------
   function environment_override (enable_tty : Boolean := False) return String
   is
      function set_terminal (enable_tty : Boolean) return String;
      function set_terminal (enable_tty : Boolean) return String is
      begin
         if enable_tty then
            return "TERM=cons25 ";
         end if;
         return "TERM=dumb ";
      end set_terminal;

      localbase : constant String := HT.USS (PM.configuration.dir_localbase);

      PATH : constant String := "PATH=/bin:/usr/bin:"
        & localbase & "/toolchain/gcc6/bin:"
        & localbase & "/sbin:"
        & localbase & "/bin ";

      TERM : constant String := set_terminal (enable_tty);
      USER : constant String := "USER=root ";
      HOME : constant String := "HOME=/root ";
      LANG : constant String := "LANG=C ";
      PKG8 : constant String := "PKG_DBDIR=/var/db/pkg8 " &
                                "PKG_CACHEDIR=/var/cache/pkg8 ";
      CENV : constant String := HT.USS (customenv);
      JENV : constant String := HT.USS (slave_env);
   begin
      return " /usr/bin/env -i " & USER & HOME & LANG & PKG8 & TERM & PATH & JENV & CENV;
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
   procedure initialize (test_mode : Boolean; jail_env : String) is
   begin
      set_uname_mrv;
      testing   := test_mode;
      slave_env := HT.SUS (jail_env);
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
   function exec_phase_deinstall (id : builders) return Boolean
   is
      time_limit : execution_limit := max_time_without_output (deinstall);
      result     : Boolean;
   begin
      --  This is only run during "testing" so assume that.
      if uselog then
         LOG.log_phase_begin (trackers (id).log_handle, phase2str (deinstall));
         log_linked_libraries (id);
      end if;
      result := False;
--        result := exec_phase (id          => id,
--                              phase       => deinstall,
--                              time_limit  => time_limit,
--                              skip_header => True,
--                              skip_footer => True);
      if not result then
         if uselog then
            LOG.log_phase_end (trackers (id).log_handle);
         end if;
         return False;
      end if;
      if uselog then
         result := detect_leftovers_and_MIA
           (id, "prestage", "between staging and package deinstallation");
         LOG.log_phase_end (trackers (id).log_handle);
      end if;
      return result;
   end exec_phase_deinstall;


   --------------------------------------------------------------------------------------------
   --  stack_linked_libraries
   --------------------------------------------------------------------------------------------
   procedure stack_linked_libraries (id : builders; base, filename : String)
   is
      command : String := chroot & base & " " & HT.USS (PM.configuration.dir_localbase) &
                          "/toolchain/bin/objdump -p " & filename;
      comres  : String :=  generic_system_command (command);
      markers : HT.Line_Markers;
   begin
      HT.initialize_markers (comres, markers);
      loop
         exit when not HT.next_line_present (comres, markers);
         declare
            line      : constant String := HT.extract_line (comres, markers);
            line_text : HT.Text := HT.SUS (line);
         begin
            if not HT.IsBlank (line) and then
              HT.contains (line, "NEEDED") and then
              not trackers (id).dynlink.Contains (line_text)
            then
               trackers (id).dynlink.Append (line_text);
            end if;
         end;
      end loop;
   exception
         --  the command result was not zero, so it was an expected format
         --  or static file.  Just skip it.  (Should never happen)
      when bad_result : others => null;
   end stack_linked_libraries;


   --------------------------------------------------------------------------------------------
   --  log_linked_libraries
   --------------------------------------------------------------------------------------------
   procedure log_linked_libraries (id : builders)
   is
      procedure log_dump (position : string_crate.Cursor);
      procedure check_package (position : string_crate.Cursor);

      root : constant String := get_root (id);

      procedure log_dump (position : string_crate.Cursor)
      is
        info : String := "   " &  HT.USS (string_crate.Element (position));
      begin
         TIO.Put_Line (trackers (id).log_handle, info);
      end log_dump;

      procedure check_package (position : string_crate.Cursor)
      is
         pkgfile : String := HT.USS (string_crate.Element (position));
         pkgname : constant String := pkgfile (pkgfile'First .. pkgfile'Last - 4);
         command : constant String := chroot & root & environment_override &
           HT.USS (PM.configuration.dir_localbase) & "/sbin/pkg-static query %Fp " & pkgname;
         comres  : String :=  generic_system_command (command);
         markers : HT.Line_Markers;
      begin
         trackers (id).dynlink.Clear;
         HT.initialize_markers (comres, markers);
         loop
            exit when not HT.next_line_present (comres, markers);
            declare
               line : constant String := HT.extract_line (comres, markers);
            begin
               if dynamically_linked (root, line) then
                  stack_linked_libraries (id, root, line);
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
   end log_linked_libraries;


   --------------------------------------------------------------------------------------------
   --  dynamically_linked
   --------------------------------------------------------------------------------------------
   function  dynamically_linked (base, filename : String) return Boolean
   is
      command : String := chroot & base & " /usr/bin/file -b -L -e ascii -e encoding -e tar " &
                "-e compress " & LAT.Quotation & filename & LAT.Quotation;
      comres  : constant String := generic_system_command (command);
   begin
      return HT.contains (comres,  "dynamically linked");
   exception
      when others => return False;
   end dynamically_linked;

end PortScan.Buildcycle;
