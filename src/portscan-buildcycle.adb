--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Unix.Ravexec;
with Replicant;
with Parameters;
with PortScan.Log;
with PortScan.Tests;
with PortScan.Packager;
with File_Operations;
with Ada.Directories;
with Ada.Characters.Latin_1;
with Ada.Exceptions;

package body PortScan.Buildcycle is

   package EX  renames Ada.Exceptions;
   package LAT renames Ada.Characters.Latin_1;
   package DIR renames Ada.Directories;
   package FOP renames File_Operations;
   package LOG renames PortScan.Log;
   package PKG renames PortScan.Packager;
   package TST renames PortScan.Tests;
   package PM  renames Parameters;
   package REP renames Replicant;
   package RAX renames Unix.Ravexec;

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
      pkgversion   : constant String := HT.USS (all_ports (sequence_id).pkgversion);
      normsslv     : constant String := PM.ssl_selection (PM.configuration);
      sslv         : constant String := specification.get_ssl_variant (normsslv);
      environ      : constant String := environment_override (True, sslv);
      env_nochain  : constant String := environment_override (False, sslv);
      port_prefix  : constant String := get_port_prefix (id, environ);
      variant      : constant String := HT.USS (all_ports (sequence_id).port_variant);

   begin
      trackers (id).seq_id := sequence_id;
      trackers (id).loglines := 0;
      trackers (id).check_strip := not specification.debugging_is_on;
      trackers (id).rpath_fatal := specification.rpath_check_errors_are_fatal;
      trackers (id).disable_dog := specification.watchdog_disabled;
      trackers (id).genesis.Clear;
      trackers (id).preconfig.Clear;
      if not LOG.initialize_log (log_fd     => trackers (id).log_fd,
                                 head_time  => trackers (id).head_time,
                                 seq_id     => trackers (id).seq_id,
                                 slave_root => get_root (id),
                                 UNAME      => HT.USS (uname_mrv),
                                 BENV       => get_environment (id, environ),
                                 COPTS      => specification.options_summary (variant),
                                 PTVAR      => get_port_variables (id, environ),
                                 block_dog  => trackers (id).disable_dog)
      then
         LOG.finalize_log (trackers (id).log_fd,
                           trackers (id).head_time,
                           trackers (id).tail_time);
         return False;
      end if;
      if testing then
         Hierarchy.take_snapshot (trackers (id).genesis, get_root (id), Positive (id));
      end if;
      begin
         for phase in phases'Range loop
            phase_trackers (id) := phase;
            case phase is
            when blr_depends =>
               R := exec_phase_depends (specification => specification,
                                        phase_name    => phase2str (phase),
                                        id            => id,
                                        environ       => environ);

            when  fetch =>
               REP.hook_toolchain (id);
               R := exec_phase_generic (id, phase, environ);

            when  extract | patch =>
               R := exec_phase_generic (id, phase, environ);

            when configure =>
               if testing then
                  Hierarchy.take_snapshot (trackers (id).preconfig, get_root (id), Positive (id));
               end if;
               R := exec_phase_generic (id, phase, environ);

            when build =>
               R := exec_phase_build (id, environ);

            when stage =>
               R := exec_phase_generic (id, phase, env_nochain);

            when test =>
               if testing and run_selftest then
                  R := exec_phase_generic (id, phase, environ);
               end if;
               if testing then
                  R := exec_preconfig_check (id);
               end if;
               REP.unhook_toolchain (id);

            when pkg_package =>
               R := PKG.exec_phase_package (specification => specification,
                                            log_fd        => trackers (id).log_fd,
                                            builder_id    => id,
                                            phase_name    => phase2str (phase),
                                            seq_id        => trackers (id).seq_id,
                                            port_prefix   => port_prefix,
                                            rootdir       => get_root (id),
                                            environ       => env_nochain);

            when install =>
               if R and then testing then
                  R := deinstall_all_packages (id, env_nochain);
               end if;
               if testing then
                  R := exec_phase_install (id, pkgversion, env_nochain);
               end if;

            when check_plist =>
               if testing then
                  R := TST.exec_check_plist (specification => specification,
                                             log_fd        => trackers (id).log_fd,
                                             phase_name    => phase2str (phase),
                                             seq_id        => trackers (id).seq_id,
                                             port_prefix   => port_prefix,
                                             rootdir       => get_root (id));
               end if;

            when deinstall =>
               if testing then
                  R := exec_phase_deinstall (id, pkgversion, env_nochain);
               end if;
            end case;
            exit when R = False;
            exit when interactive and then phase = break_phase;
         end loop;
      exception
         when crash : others =>
            R := False;
            RAX.writeln (trackers (id).log_fd, "!!!! CRASH !!!! " &
                           EX.Exception_Information (crash));
            RAX.dump_stack (trackers (id).log_fd);
      end;
      LOG.finalize_log (trackers (id).log_fd,
                        trackers (id).head_time,
                        trackers (id).tail_time);
      if interactive then
         interact_with_builder (id, sslv);
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
         when fetch            => return 60;   --  1 hour, for more disable watchdog via buildsheet
         when extract          => base := 20;
         when patch            => base := 3;
         when configure        => base := 15;
         when build            => base := 40;   --  for gcc linking, tex, *llvm linking*
         when stage            => base := 15;   --  compiling impossible; toolchain removed
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
      if afterphase = "fetch" then
         return fetch;
      elsif afterphase = "extract" then
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
        afterphase = "fetch"     or else
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
   function exec_phase_generic
     (id            : builders;
      phase         : phases;
      environ       : String) return Boolean
   is
      time_limit : execution_limit := max_time_without_output (phase);
   begin
      return exec_phase (id         => id,
                         phase      => phase,
                         time_limit => time_limit,
                         environ    => environ);
   end exec_phase_generic;


   --------------------------------------------------------------------------------------------
   --  exec_phase_build
   --------------------------------------------------------------------------------------------
   function exec_phase_build
     (id            : builders;
      environ       : String) return Boolean
   is
      time_limit : execution_limit := max_time_without_output (build);
      passed : Boolean;
   begin
      passed := exec_phase (id          => id,
                            phase       => build,
                            time_limit  => time_limit,
                            skip_header => False,
                            skip_footer => True,
                            environ     => environ);
      LOG.log_phase_end (trackers (id).log_fd);
      return passed;
   end exec_phase_build;


   --------------------------------------------------------------------------------------------
   --  exec_phase_depends
   --------------------------------------------------------------------------------------------
   function  exec_phase_depends
     (specification : PSP.Portspecs;
      phase_name    : String;
      id            : builders;
      environ       : String) return Boolean
   is
      procedure copy_to_local_repo (Position : string_crate.Cursor);
      procedure check_run_depends (Position : string_crate.Cursor);
      procedure clone_to_queue (Position : string_crate.Cursor);
      procedure mark_seen (Position : string_crate.Cursor);
      procedure generate_local_repo;
      procedure install_catalog;
      procedure prefetch_all_packages;
      procedure install_dependency_pyramid;
      procedure special_case_ravensys_toolchain;

      root       : constant String := get_root (id);
      rvn_repos  : constant String := "/usr/bin/rvn -R /etc/repos ";
      time_limit : constant execution_limit := max_time_without_output (install);
      still_good : Boolean := True;
      depend_set : string_crate.Vector;
      ondeck     : string_crate.Vector;
      queue      : string_crate.Vector;
      seen       : string_crate.Vector;
      exact_list : HT.Text := HT.SU.Null_Unbounded_String;

      procedure clone_to_queue (Position : string_crate.Cursor)
      is
         colon_nsv : HT.Text renames string_crate.Element (Position);
      begin
         queue.Append (colon_nsv);
      end clone_to_queue;

      procedure mark_seen (Position : string_crate.Cursor)
      is
         --  Only run for top-level dependencies, so piggy-back to build the install list.
         colon_nsv : HT.Text renames string_crate.Element (Position);
         pkgname   : constant String := HT.replace_all (HT.USS (colon_nsv), LAT.Colon, LAT.Tilde);
      begin
         seen.Append (colon_nsv);
         HT.SU.Append (exact_list, " " & pkgname);
      end mark_seen;

      procedure copy_to_local_repo (Position : string_crate.Cursor)
      is
         colon_nsv  : constant String  := HT.USS (string_crate.Element (Position));
         portkey    : constant String  := convert_colon_nsv_to_portkey (colon_nsv);
         ptid       : constant port_id := ports_keys (HT.SUS (portkey));
         pkgname    : constant String  := HT.replace_all (colon_nsv, LAT.Colon, LAT.Tilde);
         pkgversion : constant String  := HT.USS (all_ports (ptid).pkgversion);
         pkgfile    : constant String  := pkgname & LAT.Tilde & pkgversion & arc_ext;
         systempath : constant String  := HT.USS (PM.configuration.dir_repository) & "/" & pkgfile;
         slavepath  : constant String  := root & "/repo/files/" & pkgfile;
      begin
         if DIR.Exists (systempath) then
            DIR.Copy_File (Source_Name => systempath, Target_Name => slavepath);
         else
            still_good := False;
            RAX.writeln (trackers (id).log_fd, "Dependency package not found: " & pkgfile);
         end if;
      end copy_to_local_repo;

      procedure check_run_depends (Position : string_crate.Cursor)
      is
         procedure search_subpkg (subpos : subpackage_crate.Cursor);

         colon_nsv  : constant String  := HT.USS (string_crate.Element (Position));
         portkey    : constant String  := convert_colon_nsv_to_portkey (colon_nsv);
         ptid       : constant port_id := ports_keys (HT.SUS (portkey));
         xx_subpkg  : constant String  := HT.specific_field (colon_nsv, 2, ":");

         procedure search_subpkg (subpos : subpackage_crate.Cursor)
         is
            procedure quantum (rpos : spkg_id_crate.Cursor);

            myrec : subpackage_record renames subpackage_crate.Element (subpos);

            procedure quantum (rpos : spkg_id_crate.Cursor)
            is
               qrec : subpackage_identifier renames spkg_id_crate.Element (rpos);
               rdid : port_index := qrec.port;
            begin
               declare
                  rdn : constant String := HT.USS (all_ports (rdid).port_namebase);
                  rds : constant String := HT.USS (qrec.subpackage);
                  rdv : constant String := HT.USS (all_ports (rdid).port_variant);
                  rdnsv : constant HT.Text := HT.SUS (rdn & ":" & rds & ":" & rdv);
               begin
                  if not seen.Contains (rdnsv) then
                     seen.Append (rdnsv);
                     ondeck.Append (rdnsv);
                     depend_set.Append (rdnsv);
                  end if;
               end;
            end quantum;
         begin
            if HT.equivalent (myrec.subpackage, xx_subpkg) then
               myrec.spkg_run_deps.Iterate (quantum'Access);
            end if;
         end search_subpkg;
      begin
         all_ports (ptid).subpackages.Iterate (search_subpkg'Access);
      end check_run_depends;

      procedure dump_catalog (dump_type : String)
      is
         cat_ucl : constant String := root & "/var/cache/rvn/remote/catalog.ucl";
      begin
         if DIR.Exists (cat_ucl) then
            RAX.writeln (trackers (id).log_fd, "Diagnostic catalog dump (" & dump_type & "):");
            declare
               handle : TIO.File_Type;
            begin
               TIO.Open (File => handle,
                         Mode => TIO.In_File,
                         Name => cat_ucl);
               while not TIO.End_Of_File (handle) loop
                  RAX.writeln (trackers (id).log_fd, TIO.Get_Line (handle));
               end loop;
               TIO.Close (handle);
            exception
               when others =>
                  if TIO.Is_Open (handle) then
                     TIO.Close (handle);
                  end if;
            end;
         end if;
      end dump_catalog;

      procedure generate_local_repo
      is
         cmd        : constant String := "/usr/bin/rvn genrepo --quiet /repo";
         arguments  : constant String := root & environ & cmd;
         max_time   : constant execution_limit := 4;
         timed_out  : Boolean;
      begin
         still_good := generic_execute (id, PM.chroot_program, arguments, timed_out, max_time);
         if timed_out then
            RAX.writeln (trackers (id).log_fd, "FATAL: Generate repository command timed out.");
         end if;
      end generate_local_repo;

      procedure install_catalog
      is
         cmd        : constant String := rvn_repos & "catalog --force";
         arguments  : constant String := root & environ & cmd;
         max_time   : constant execution_limit := 3;
         timed_out  : Boolean;
      begin
         still_good := generic_execute (id, PM.chroot_program, arguments, timed_out, max_time);
         if timed_out then
            RAX.writeln (trackers (id).log_fd, "FATAL: Install catalog command timed out.");
         end if;
      end install_catalog;

      procedure prefetch_all_packages
      is
         cmd        : constant String := rvn_repos & "fetch --all --no-repo-update --quiet";
         arguments  : constant String := root & environ & cmd;
         max_time   : constant execution_limit := 5;
         timed_out  : Boolean;
      begin
         still_good := generic_execute (id, PM.chroot_program, arguments, timed_out, max_time);
         if timed_out then
            RAX.writeln (trackers (id).log_fd, "FATAL: Prefetch package command timed out.");
         end if;
         if not still_good then
            dump_catalog ("prefetch packages");
         end if;
      end prefetch_all_packages;

      procedure install_dependency_pyramid
      is
         cmd        : constant String := rvn_repos & "install --no-repo-update --exact-match --yes";
         arguments  : constant String := root & environ & cmd & HT.USS (exact_list);
         timed_out  : Boolean;
      begin
         still_good := generic_execute (id, PM.chroot_program, arguments, timed_out, time_limit);
         if timed_out then
            RAX.writeln (trackers (id).log_fd, "FATAL: Dependency install command timed out.");
         end if;
         if not still_good then
            dump_catalog ("dependency pyramid");
         end if;
      end install_dependency_pyramid;

      procedure special_case_ravensys_toolchain
      is
         function ccnsv (subpackage : String) return HT.Text;

         --  The ravensys-toolchain needs the latest version of ravensys-gcc (possibly newer
         --  that those in the toolchain) but it's not possible to add ravensys-gcc as a
         --  dependency.  The files need to be manually copied into /repo/files for this
         --  one particular port.
         seq  : constant port_id := trackers (id).seq_id;
         name : constant String := HT.USS (all_ports (seq).port_namebase);

         function ccnsv (subpackage : String) return HT.Text
         is
            nsv : constant String := "ravensys-gcc:" & subpackage & ":" & variant_standard;
         begin
            return HT.SUS (nsv);
         end ccnsv;
      begin
         if name /= "ravensys-toolchain" then
            return;
         end if;
         queue.Clear;
         queue.Append (ccnsv ("ada_run"));
         queue.Append (ccnsv ("compilers"));
         queue.Append (ccnsv ("set"));
         queue.Append (ccnsv ("cxx_run"));
         queue.Append (ccnsv ("fortran_run"));
         queue.Append (ccnsv ("infopages"));
         queue.Append (ccnsv ("libs"));
         queue.Iterate (copy_to_local_repo'Access);
      end special_case_ravensys_toolchain;
   begin
      LOG.log_phase_begin (trackers (id).log_fd, phase_name);
      specification.combined_dependency_nsv (include_run => True,
                                             limit_to_run => False,
                                             dependency_set => depend_set);
      if depend_set.Is_Empty then
         RAX.writeln (trackers (id).log_fd, "This package has no dependency requirements.");
         LOG.log_phase_end (trackers (id).log_fd);
         return still_good;
      end if;

      --  Recursively get child-dependencies.
      seen.Clear;
      queue.Clear;
      depend_set.Iterate (clone_to_queue'Access);
      depend_set.Iterate (mark_seen'Access);
      loop
         ondeck.Clear;
         queue.Iterate (check_run_depends'Access);
         exit when ondeck.Is_Empty;
         queue.Clear;
         ondeck.Iterate (clone_to_queue'Access);
      end loop;

      depend_set.Iterate (copy_to_local_repo'Access);
      special_case_ravensys_toolchain;

      if still_good then
         generate_local_repo;
      end if;
      if still_good then
         install_catalog;
      end if;
      if still_good then
         prefetch_all_packages;
      end if;
      if still_good then
         install_dependency_pyramid;
      end if;

      LOG.log_phase_end (trackers (id).log_fd);
      return still_good;
   end exec_phase_depends;


   --------------------------------------------------------------------------------------------
   --  deinstall_all_packages
   --------------------------------------------------------------------------------------------
   function  deinstall_all_packages
     (id         : builders;
      environ    : String) return Boolean
   is
      time_limit : execution_limit := max_time_without_output (test);
      root       : constant String := get_root (id);
      phase_name : constant String := "install / test / deinstall all packages";
      CMD_RM_ALL : constant String := "/usr/bin/rvn remove --all --yes --skip-verify";
      arguments  : constant String := root & environ & CMD_RM_ALL;
      still_good : Boolean := True;
      timed_out  : Boolean;
   begin
      LOG.log_phase_begin (trackers (id).log_fd, phase_name);
      RAX.writeln (trackers (id).log_fd, "===>  Removing all packages");
      still_good := generic_execute (id, PM.chroot_program, arguments, timed_out, time_limit);
      LOG.log_phase_end (trackers (id).log_fd);
      return still_good;
   end deinstall_all_packages;


   --------------------------------------------------------------------------------------------
   --  exec_phase_install
   --------------------------------------------------------------------------------------------
   function  exec_phase_install
     (id            : builders;
      pkgversion    : String;
      environ       : String) return Boolean
   is
      procedure generate_local_repo;
      procedure install_catalog;
      procedure prefetch_all_packages;
      procedure install_built_package;
      procedure build_list (position : subpackage_crate.Cursor);

      time_limit : execution_limit := max_time_without_output (install);
      root       : constant String := get_root (id);
      namebase   : constant String := HT.USS (all_ports (trackers (id).seq_id).port_namebase);
      variant    : constant String := HT.USS (all_ports (trackers (id).seq_id).port_variant);
      rvn_repos  : constant String := "/usr/bin/rvn -R /etc/repos ";
      INST_PRIME : constant String := "install --no-repo-update --exact-match --yes";
      exact_list : HT.Text := HT.SU.Null_Unbounded_String;
      still_good : Boolean := True;

      procedure generate_local_repo
      is
         cmd        : constant String := "/usr/bin/rvn genrepo --quiet /repo";
         arguments  : constant String := root & environ & cmd;
         max_time   : constant execution_limit := 4;
         timed_out  : Boolean;
      begin
         still_good := generic_execute (id, PM.chroot_program, arguments, timed_out, max_time);
         if timed_out then
            RAX.writeln (trackers (id).log_fd, "FATAL: Generate repository command timed out.");
         end if;
      end generate_local_repo;

      procedure install_catalog
      is
         cmd        : constant String := rvn_repos & "catalog --force";
         arguments  : constant String := root & environ & cmd;
         max_time   : constant execution_limit := 3;
         timed_out  : Boolean;
      begin
         still_good := generic_execute (id, PM.chroot_program, arguments, timed_out, max_time);
         if timed_out then
            RAX.writeln (trackers (id).log_fd, "FATAL: Install catalog command timed out.");
         end if;
      end install_catalog;

      procedure prefetch_all_packages
      is
         cmd        : constant String := rvn_repos & "fetch --all --no-repo-update --quiet";
         arguments  : constant String := root & environ & cmd;
         max_time   : constant execution_limit := 5;
         timed_out  : Boolean;
      begin
         still_good := generic_execute (id, PM.chroot_program, arguments, timed_out, max_time);
         if timed_out then
            RAX.writeln (trackers (id).log_fd, "FATAL: Prefetch package command timed out.");
         end if;
      end prefetch_all_packages;

      procedure build_list (position : subpackage_crate.Cursor)
      is
         rec        : subpackage_record renames subpackage_crate.Element (position);
         subpackage : constant String :=  HT.USS (rec.subpackage);
         pkgname    : String := calculate_nsv (trackers (id).seq_id, subpackage);
      begin
         HT.SU.Append (exact_list, " " & pkgname);
      end build_list;

      procedure install_built_package
      is
         cmd        : constant String := rvn_repos & "install --no-repo-update --exact-match --yes";
         arguments  : constant String := root & environ & cmd & HT.USS (exact_list);
         timed_out  : Boolean;
      begin
         RAX.writeln (trackers (id).log_fd,
                       "===>  Install the " & variant & " variant of the " & namebase & " package");
         still_good := generic_execute (id, PM.chroot_program, arguments, timed_out, time_limit);
         if timed_out then
            RAX.writeln (trackers (id).log_fd, watchdog_message (time_limit));
         end if;
      end install_built_package;
   begin
      LOG.log_phase_begin (trackers (id).log_fd, phase2str (install));
      generate_local_repo;
      if still_good then
         install_catalog;
      end if;
      if still_good then
         prefetch_all_packages;
      end if;
      if still_good then
         all_ports (trackers (id).seq_id).subpackages.Iterate (build_list'Access);
         install_built_package;
      end if;
      LOG.log_phase_end (trackers (id).log_fd);
      return still_good;
   end exec_phase_install;


   --------------------------------------------------------------------------------------------
   --  exec_phase
   --------------------------------------------------------------------------------------------
   function exec_phase (id            : builders;
                        phase         : phases;
                        time_limit    : execution_limit;
                        environ       : String;
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
         LOG.log_phase_begin (trackers (id).log_fd, phase2str (phase));
      end if;

      declare
         arguments : constant String := root & environ & phaseenv & chroot_make_program &
           " -C /port " & phase2str (phase);
      begin
         result := generic_execute (id, PM.chroot_program, arguments, timed_out, time_limit);
      end;

      if timed_out then
         RAX.writeln (trackers (id).log_fd, watchdog_message (time_limit));
      end if;
      if not skip_footer then
         LOG.log_phase_end (trackers (id).log_fd);
      end if;

      return result;
   end exec_phase;


   --------------------------------------------------------------------------------------------
   --  get_port_variables
   --------------------------------------------------------------------------------------------
   function get_port_variables (id : builders; environ : String) return String
   is
      root    : constant String := get_root (id);
      command : constant String := PM.chroot_program & " " & root & environ & chroot_make_program &
        " -C /port -VCONFIGURE_ENV -VCONFIGURE_ARGS" &
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
            REP.append_abnormal_log ("COMMAND: " & command);
            REP.append_abnormal_log (" OUTPUT: " & HT.USS (content));
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
   function get_environment (id : builders; environ : String) return String
   is
      root    : constant String := get_root (id);
      command : constant String := PM.chroot_program & " " & root & environ;
   begin
      return generic_system_command (command);
   exception
      when others =>
         return discerr;
   end get_environment;


   --------------------------------------------------------------------------------------------
   --  environment_override
   --------------------------------------------------------------------------------------------
   function  environment_override (toolchain   : Boolean;
                                   ssl_variant : String;
                                   enable_tty  : Boolean := False) return String
   is
      function set_terminal (enable_tty : Boolean) return String;
      function toolchain_path return String;
      function dyld_fallback return String;

      localbase : constant String := HT.USS (PM.configuration.dir_localbase);

      function set_terminal (enable_tty : Boolean) return String is
      begin
         if enable_tty then
            return "TERM=xterm ";
         end if;
         return "TERM=dumb ";
      end set_terminal;

      function toolchain_path return String
      is
         defcomp : String := localbase & "/toolchain/" & ports_compiler & "/bin:";
      begin
         if toolchain then
            return defcomp;
         end if;
         return "";
      end toolchain_path;

      function dyld_fallback return String is
      begin
         case platform_type is
            when macos => return "DYLD_FALLBACK_LIBRARY_PATH=" &
                                 localbase & "/toolchain-fallback/" & ports_compiler & "/lib ";
            when others => return "";
         end case;
      end dyld_fallback;

      PATH : constant String := "PATH=/bin:/usr/bin:"
        & toolchain_path
        & localbase & "/toolchain/bin:"
        & localbase & "/sbin:"
        & localbase & "/bin ";

      TERM : constant String := set_terminal (enable_tty);
      USER : constant String := "USER=root ";
      HOME : constant String := "HOME=/root ";
      LANG : constant String := "LANG=C ";
      SHLL : constant String := "SHELL=/bin/sh ";
      RAVN : constant String := "RAVENADM=building ";
      SSLV : constant String := "SSL_VARIANT=" & ssl_variant & " ";
      RVN8 : constant String := "RVN_DBDIR=/var/db/rvn " &
                                "RVN_CACHEDIR=/var/cache/rvn " &
                                "KEYWORDS_DIR=/xports/Mk/Keywords " &
                                "SKIP_DEV_SUBPKG=false " &
                                "SKIP_INFO_SUBPKG=false ";
      CXML : constant String := "XML_CATALOG_FILES=" & localbase & "/share/xml/catalog ";
      SGML : constant String := "SGML_CATALOG_FILES=" & localbase & "/share/sgml/docbook/catalog ";
      CENV : constant String := HT.USS (customenv);
      DYLD : constant String := dyld_fallback;
   begin
      return " /usr/bin/env -i " &
        CENV & LANG & TERM & SHLL & USER & HOME & SGML & CXML & RAVN & SSLV & RVN8 & DYLD & PATH;
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
   --  exec_preconfig_check
   --------------------------------------------------------------------------------------------
   function exec_preconfig_check  (id : builders) return Boolean
   is
      root        : constant String := get_root (id);
      phase_name  : constant String := "Post-stage integrity check";
      description : constant String := "between configuration and staging";
      still_good  : Boolean;
   begin
      LOG.log_phase_begin (trackers (id).log_fd, phase_name);
      still_good := Hierarchy.detect_leftovers_and_MIA (log_fd      => trackers (id).log_fd,
                                                        DC          => trackers (id).preconfig,
                                                        rootdir     => root,
                                                        description => description,
                                                        fatal       => False,
                                                        builder     => Positive (id));
      LOG.log_phase_end (trackers (id).log_fd);
      return still_good;
   end exec_preconfig_check;


   --------------------------------------------------------------------------------------------
   --  exec_phase_deinstall
   --------------------------------------------------------------------------------------------
   function exec_phase_deinstall
     (id            : builders;
      pkgversion    : String;
      environ       : String) return Boolean
   is
      procedure concatenate (position : subpackage_crate.Cursor);

      time_limit : execution_limit := max_time_without_output (deinstall);
      root       : constant String := get_root (id);
      namebase   : constant String := HT.USS (all_ports (trackers (id).seq_id).port_namebase);
      variant    : constant String := HT.USS (all_ports (trackers (id).seq_id).port_variant);
      CMD_RM_ALL : constant String := "/usr/bin/rvn remove --all --yes --skip-verify --force";
      still_good : Boolean := True;
      dyn_good   : Boolean;
      timed_out  : Boolean;
      rem_list   : HT.Text;

      procedure concatenate (position : subpackage_crate.Cursor)
      is
         rec        : subpackage_record renames subpackage_crate.Element (position);
         subpackage : constant String := HT.USS (rec.subpackage);
         nsv        : constant String := calculate_nsv (trackers (id).seq_id, subpackage);
      begin
         HT.SU.Append (rem_list, " " & nsv);
      end concatenate;

   begin
      LOG.log_phase_begin (trackers (id).log_fd, phase2str (deinstall));
      dyn_good := log_linked_libraries (id, pkgversion, environ);
      --  all_ports (trackers (id).seq_id).subpackages.Iterate (concatenate'Access);

      RAX.writeln (trackers (id).log_fd, "===>  Deinstalling " & namebase & ":" & variant);

      declare
         arguments : constant String := root & environ & CMD_RM_ALL;
      begin
         still_good := generic_execute (id, PM.chroot_program, arguments, timed_out, time_limit);
      end;

      if timed_out then
         RAX.writeln (trackers (id).log_fd, watchdog_message (time_limit));
      end if;

      if still_good then
         still_good := Hierarchy.detect_leftovers_and_MIA
           (log_fd      => trackers (id).log_fd,
            DC          => trackers (id).genesis,
            rootdir     => root,
            description => "between clean builder and package deinstallation",
            fatal       => True,
            builder     => Positive (id));
      end if;
      LOG.log_phase_end (trackers (id).log_fd);
      return still_good and then dyn_good;
   end exec_phase_deinstall;


   --------------------------------------------------------------------------------------------
   --  stack_linked_libraries
   --------------------------------------------------------------------------------------------
   procedure stack_linked_libraries
     (id            : builders;
      base          : String;
      filename      : String;
      environ       : String)
   is
      command : String := PM.chroot_program & " " & base & environ &
                          "/usr/bin/objdump-sysroot -p " & filename;
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
               pathstr := HT.SUS (FOP.convert_ORIGIN_in_runpath
                                  (filename => filename,
                                   runpath  => HT.trim (HT.part_2 (line, runpath))));
            end;
         else
            HT.initialize_markers (comres, markers);
            if HT.next_line_with_content_present (comres, rpath, markers) then
               declare
                  line : constant String := HT.extract_line (comres, markers);
               begin
                  pathstr := HT.SUS (FOP.convert_ORIGIN_in_runpath
                                     (filename => filename,
                                      runpath  => HT.trim (HT.part_2 (line, rpath))));
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
   function log_linked_libraries
     (id            : builders;
      pkgversion    : String;
      environ       : String) return Boolean
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
         RAX.writeln (trackers (id).log_fd, info);
      end log_dump;

      procedure check_package (position : subpackage_crate.Cursor)
      is
         rec        : subpackage_record renames subpackage_crate.Element (position);
         subpackage : constant String := HT.USS (rec.subpackage);
         pkg_nsv    : constant String := calculate_nsv (trackers (id).seq_id, subpackage);
         pkgname    : constant String := calculate_package_name (trackers (id).seq_id, subpackage);
         command    : constant String := PM.chroot_program & " " & root & environ &
                      "/usr/bin/rvn query --exact-match '{xfile:path}' " & pkg_nsv;
         comres     : String :=  generic_system_command (command);
         markers    : HT.Line_Markers;
      begin
         trackers (id).dynlink.Clear;
         trackers (id).runpaths.Clear;
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
                  stack_linked_libraries (id, root, filename, environ);
                  if not passed_runpath_check (id) then
                     result := False;
                  end if;
               end if;
               if unstripped then
                  RAX.writeln
                    (trackers (id).log_fd,
                     "### WARNING ###  " & filename & " is not stripped.  " &
                       "See Ravenporter's guide.");
               end if;
            end;
         end loop;
         if not trackers (id).dynlink.Is_Empty then
            RAX.writeln (trackers (id).log_fd, "===> " & pkgname & " subpackage:");
            RAX.writeln (trackers (id).log_fd, "");
            trackers (id).dynlink.Iterate (log_dump'Access);
            RAX.writeln (trackers (id).log_fd, "");
         end if;
      exception
         when others => null;
      end check_package;
   begin
      --  dynlink, runpaths, nonexistent, seen_libs already empty at this point
      RAX.writeln (trackers (id).log_fd, "=> Checking shared library dependencies");
      all_ports (trackers (id).seq_id).subpackages.Iterate (check_package'Access);

      trackers (id).dynlink.Clear;
      trackers (id).runpaths.Clear;
      trackers (id).nonexistent.Clear;
      trackers (id).seen_libs.Clear;
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
      command : String := PM.chroot_program & " " &
        base & " /usr/bin/file -b -L -e ascii -e encoding -e tar -e compress " &
        "-h -m /usr/share/file/magic.mgc " & HT.shell_quoted (filename);
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
         procedure squawk;
         function get_system_lib_level_1 return String;
         function get_system_lib_level_2 return String;

         line        : String := HT.USS (string_crate.Element (position));
         paths       : constant String := HT.part_1 (line, " ");
         library     : constant String := HT.part_2 (line, " ");
         numfields   : constant Natural := HT.count_char (paths, LAT.Colon) + 1;
         attempted   : Boolean := False;

         function get_system_lib_level_1 return String is
         begin
            if platform_type = linux then
               return "/lib/x86_64-linux-gnu";
            else
               return "/lib";
            end if;
         end get_system_lib_level_1;

         function get_system_lib_level_2 return String is
         begin
            if platform_type = linux then
               return "/usr/lib/x86_64-linux-gnu";
            else
               return "/usr/lib";
            end if;
         end get_system_lib_level_2;

         systemdir_1 : constant String := get_system_lib_level_1;
         systemdir_2 : constant String := get_system_lib_level_2;
         systemlib_1 : constant String := systemdir_1 & "/" & library;
         systemlib_2 : constant String := systemdir_2 & "/" & library;
         syslib_1txt : constant HT.Text := HT.SUS (systemlib_1);
         syslib_2txt : constant HT.Text := HT.SUS (systemlib_2);
         library_txt : constant HT.Text := HT.SUS (library);
         abs_path    : constant Boolean := library (library'First) = '/';

         procedure squawk is
         begin
            RAX.writeln (trackers (id).log_fd,
                          errmsg_prefix & library & " is not in located in " & systemdir_1 &
                            ", " & systemdir_2 & " or within the RPATH/RUNPATH (" & paths & ")");
         end squawk;

      begin
         --  Check system library paths first
         if trackers (id).seen_libs.Contains (syslib_1txt) or else
           trackers (id).seen_libs.Contains (syslib_2txt)
         then
            return;
         end if;

         --  Check for shared libraries with absolute paths (rare)
         --  Issue 86 (handle library with absolute path, e.g. /raven/lib/lua/5.1/lpeg.so
         if abs_path then

            if trackers (id).seen_libs.Contains (library_txt) then
               return;
            end if;

            if not trackers (id).nonexistent.Contains (library_txt) then
               if Unix.target_exists (root & library) then
                  trackers (id).seen_libs.Append (HT.SUS (library));
                  return;
               end if;
               trackers (id).nonexistent.Append (library_txt);
               attempted := True;
            end if;
         else

            if not trackers (id).nonexistent.Contains (syslib_1txt) then
               if Unix.target_exists (root & systemlib_1) then
                  trackers (id).seen_libs.Append (syslib_1txt);
                  return;
               end if;
               trackers (id).nonexistent.Append (syslib_1txt);
               attempted := True;
            end if;

            if not trackers (id).nonexistent.Contains (syslib_2txt) then
               if Unix.target_exists (root & systemlib_2) then
                  trackers (id).seen_libs.Append (syslib_2txt);
                  return;
               end if;
               trackers (id).nonexistent.Append (syslib_2txt);
               attempted := True;
            end if;

         end if;

         if HT.IsBlank (paths) then
            squawk;
            result := fail_result;
            return;
         end if;

         for n in 1 .. numfields loop
            declare
               testpath     : String := HT.specific_field (paths, n, ":");
               test_library : String := testpath & "/" & library;
               test_lib_txt : HT.Text := HT.SUS (test_library);
            begin
               if trackers (id).seen_libs.Contains (test_lib_txt) then
                  return;
               end if;
               if not trackers (id).nonexistent.Contains (test_lib_txt) then
                  if Unix.target_exists (root & test_library) then
                     trackers (id).seen_libs.Append (test_lib_txt);
                     return;
                  end if;
                  trackers (id).nonexistent.Append (test_lib_txt);
                  attempted := True;
               end if;
            end;
         end loop;

         if attempted then
            squawk;
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
         mac  : constant String := "/usr/bin/env LANG=C /usr/sbin/sysctl vm.loadavg";
         lin  : constant String := "/bin/cat /proc/loadavg";
         sol  : constant String := "/usr/bin/uptime";
      begin
         case platform_type is
            when dragonfly | freebsd | midnightbsd =>
               lo := 14;
               return generic_system_command (bsd);
            when macos =>
               lo := 14;
               return generic_system_command (mac);
            when netbsd | openbsd =>
               lo := 12;
               return generic_system_command (bsd);
            when linux =>
               lo := 0;
               return generic_system_command (lin);
            when sunos =>
               return generic_system_command (sol);
         end case;
      exception
         when others =>
            case platform_type is
               when dragonfly | freebsd |
                    macos | midnightbsd         => return "vm.loadavg: { 0.00 0.00 0.00 }";
               when netbsd | openbsd            => return "vm.loadavg: 0.00 0.00 0.00";
               when linux                       => return "0.00 0.00 0.00";
               when sunos                       => return "load average: 0.00, 0.00, 0.00";
            end case;
      end probe_load;

      comres : constant String := probe_load;
   begin
      case platform_type is
         when dragonfly | freebsd | netbsd | openbsd | linux | macos | midnightbsd =>
            declare
               stripped : constant String := comres (comres'First + lo .. comres'Last);
            begin
               if instant_load then
                  return Float'Value (HT.specific_field (stripped, 1, " "));
               else
                  return Float'Value (HT.specific_field (stripped, 2, " "));
               end if;
            end;
         when sunos =>
            declare
               stripped : constant String := HT.part_2 (comres, "load average: ");
            begin
               if instant_load then
                  return Float'Value (HT.specific_field (stripped, 1, ", "));
               else
                  return Float'Value (HT.specific_field (stripped, 2, ", "));
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
         FST      : constant Natural := catport'First;
      begin
         result.Elapsed := LOG.elapsed_HH_MM_SS (start => trackers (id).head_time,
                                                 stop  => CAL.Clock);
         result.LLines (linehead .. 7) := numlines;
         result.phase  (1 .. phasestr'Length) := phasestr;

         if catport'Length > orilimit then
            result.origin (1 .. orilimit - 1) := catport (FST .. FST + orilimit - 2);
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
   --  interact_with_builder
   --------------------------------------------------------------------------------------------
   procedure interact_with_builder (id : builders; ssl_variant : String)
   is
      function shell return String;

      root   : constant String := get_root (id);
      result : Boolean;

      function shell return String is
      begin
         case platform_type is
            when linux | sunos =>
               return "/bin/bash";
            when others =>
               return "/bin/sh";
         end case;
      end shell;

      command : String := PM.chroot_program & " " & root &
                          environment_override (True, ssl_variant, True) & shell;
   begin
      TIO.Put_Line ("Entering interactive test mode at the builder root directory.");
      TIO.Put_Line ("Type 'exit' when done exploring.");
      result := Unix.external_command (command);
   end interact_with_builder;


   --------------------------------------------------------------------------------------------
   --  generic_execute
   --------------------------------------------------------------------------------------------
   function generic_execute
     (id            : builders;
      command       : String;
      arguments     : String;
      dogbite       : out Boolean;
      time_limit    : execution_limit) return Boolean
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
      hangmonitor : constant Boolean := True and then not trackers (id).disable_dog;
   begin
      dogbite := False;
      watchdog (squirrel) := trackers (id).loglines;

      pid := RAX.launch_separate_process (id, trackers (id).log_fd, command, arguments);
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
                     RAX.kill_process_tree (id, trackers (id).log_fd);
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
   begin
      case platform_type is
         when macos =>
            --  essentially disable this code without removing it.
            --  It was thought lines_in_log() was causing freezing on FreeBSD but it wasn't.
            declare
               command  : constant String := HT.USS (PM.configuration.dir_sysroot) &
                 "/usr/bin/wc -l " & LOG.log_name (trackers (id).seq_id);
            begin
               declare
                  lc : constant String := generic_system_command (command);
               begin
                  trackers (id).loglines := Natural'Value (HT.part_1 (lc, " "));
               end;
            exception
               when others => null;  -- just skip this cycle
            end;
         when others =>
            trackers (id).loglines := FOP.lines_in_log (LOG.log_name (trackers (id).seq_id));
      end case;
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
   procedure run_makesum (id : builders; ssl_variant : String)
   is
      root     : constant String := get_root (id);
      distinfo : constant String := root & "/port/distinfo";
      locfile  : constant String := "distinfo";
      environ  : constant String := environment_override (False, ssl_variant);
      command  : constant String := PM.chroot_program & " " & root & environ &
                                    chroot_make_program & " -C /port makesum";
      content : HT.Text;
      status  : Integer;

      use type DIR.File_Size, DIR.File_Kind;
   begin
      content := Unix.piped_command (command, status);
      if status = 0 then
         if DIR.Exists (distinfo) then
            if not HT.IsBlank (content) then
               TIO.Put_Line (HT.USS (content));
            end if;
            if DIR.Size (distinfo) = DIR.File_Size (0) then
               --  The generated distinfo file is empty
               --  Not only do we not copy it over, let's erase the port's distinfo
               --  file if it exists
               if DIR.Exists (locfile) and then
                 DIR.Kind (locfile) = DIR.Ordinary_File
               then
                  DIR.Delete_File (locfile);
               end if;
            else
               TIO.Put_Line ("Copying " & distinfo & " to current directory");
               DIR.Copy_File (distinfo, locfile);
            end if;
         else
            TIO.Put_Line ("####### failure, distinfo not found #######");
         end if;
      else
         TIO.Put_Line ("####### MAKESUM COMMAND FAILED #######");
         TIO.Put_Line ("hint 1: Check SITES array contents are valid and accessible");
         TIO.Put_Line ("hint 2: Check that all distfile names are correct.");
         TIO.Put_Line ("======= FAILED COMMAND OUTPUT =======");
         TIO.Put_Line (HT.USS (content));
      end if;
   end run_makesum;


   --------------------------------------------------------------------------------------------
   --  run_patch_regen
   --------------------------------------------------------------------------------------------
   procedure run_patch_regen (id : builders; sourceloc : String; ssl_variant : String)
   is
      function get_wrksrc return String;
      function get_strip_component return String;
      procedure copy_files (subdir : String; pattern : String);

      root     : constant String := get_root (id);
      environ  : constant String := environment_override (False, ssl_variant);
      premake  : constant String := PM.chroot_program & " " & root & environ &
                                    chroot_make_program & " -C /port ";
      cextract : constant String := premake & "extract";
      cpatch   : constant String := premake & "repatch";
      patch_wc : constant String := "patch-*";

      function get_wrksrc return String
      is
         command  : constant String := premake & " -V WRKSRC";
         result   : constant String := generic_system_command (command);
      begin
         return HT.first_line (result);
      end get_wrksrc;

      function get_strip_component return String
      is
         command  : constant String := premake & " -V PATCH_STRIP:S/-p//";
         result   : constant String := generic_system_command (command);
      begin
         declare
            raw     : constant String := HT.first_line (result);
            snumber : Integer;
         begin
            snumber := Integer'Value (raw);
            return HT.int2str (snumber);
         exception
            when others =>
               TIO.Put_Line ("Failed to convert '" & raw & "' to an integer; going with '0'");
               return "0";
         end;
      end get_strip_component;

      procedure copy_files (subdir : String; pattern : String)
      is
         shinydir : constant String := root & "/tmp/shiny";
      begin
         if sourceloc = "" then
            FOP.replace_directory_contents (shinydir, subdir, pattern);
         else
            FOP.replace_directory_contents (shinydir, sourceloc & "/" & subdir, pattern);
         end if;
      end copy_files;

      cregen : constant String := PM.chroot_program & " " & root &
        " /bin/sh /xports/Mk/Scripts/repatch.sh " & get_wrksrc & " " & get_strip_component;
   begin
      if DIR.Exists (root & "/port/patches") or else
        DIR.Exists (root & "/port/opsys")
      then
         if Unix.external_command (cextract) then
            if Unix.external_command (cpatch) then
               if Unix.external_command (cregen) then
                  --  copy contents of /tmp/shiny to sourceloc/patches and sourceloc/files
                  copy_files ("patches", patch_wc);
                  copy_files ("dragonfly", patch_wc);
                  copy_files ("freebsd", patch_wc);
                  copy_files ("netbsd", patch_wc);
                  copy_files ("darwin", patch_wc);
                  copy_files ("linux", patch_wc);
                  copy_files ("sunos", patch_wc);
                  copy_files ("midnightbsd", patch_wc);
                  copy_files ("files", "extra-patch-*");
               else
                  TIO.Put_Line ("patch regen: failed to regenerate patches");
               end if;
            else
               TIO.Put_Line ("patch regen: failed to apply patches");
            end if;
         else
            TIO.Put_Line ("patch regen: failed to extract distfile");
         end if;
      else
         TIO.Put_Line ("This port has no patches to regenerated.");
      end if;
   end run_patch_regen;


   --------------------------------------------------------------------------------------------
   --  get_port_prefix
   --------------------------------------------------------------------------------------------
   function get_port_prefix (id : builders; environ : String) return String
   is
      root     : constant String := get_root (id);
      command  : constant String := PM.chroot_program & " " & root & environ &
                                    chroot_make_program & " -C /port -V PREFIX";
      result   : constant String := generic_system_command (command);
   begin
      return HT.first_line (result);
   end get_port_prefix;


end PortScan.Buildcycle;
