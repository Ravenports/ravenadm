--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Exceptions;
with Ada.Directories;
with Ada.Containers.Vectors;
with Ada.Characters.Latin_1;
with File_Operations;
with Parameters;
with Signals;
with Unix;

package body Replicant is

   package EX  renames Ada.Exceptions;
   package PM  renames Parameters;
   package CON renames Ada.Containers;
   package DIR renames Ada.Directories;
   package LAT renames Ada.Characters.Latin_1;
   package FOP renames File_Operations;

   --------------------------------------------------------------------------------------------
   --  initialize
   --------------------------------------------------------------------------------------------
   procedure initialize (testmode  : Boolean)
   is
      raven_sysroot : constant String := HT.USS (PM.configuration.dir_sysroot);
      mm     : constant String := get_master_mount;
      sretc  : constant String := raven_sysroot & "/usr/share";
      maspas : constant String := "/master.passwd";
      passwd : constant String := "/passwd";
      spwd   : constant String := "/spwd.db";
      pwd    : constant String := "/pwd.db";
      rcconf : constant String := "/rc.conf";
      hints  : constant String := "/ld-elf.so.hints";
      nhints : constant String := "/ld.so.hints";
      group  : constant String := "/group";
      ldcnf1 : constant String := "/x86_64-linux-gnu.conf";
      ldcnf2 : constant String := "/ld.so.conf";
      ldcnf3 : constant String := "/ld.so.cache";
   begin
      developer_mode := testmode;
      ravenbase      := PM.configuration.dir_localbase;

      start_abnormal_logging;

      DIR.Create_Path (mm);
      case platform_type is
         when dragonfly |
              freebsd   |
              macos     |
              netbsd    |
              openbsd   |
              midnightbsd =>
            DIR.Copy_File (sretc & passwd, mm & passwd);
            DIR.Copy_File (sretc & maspas, mm & maspas);
            DIR.Copy_File (sretc & group,  mm & group);
         when linux     |
              sunos     =>
            DIR.Copy_File (sretc & passwd, mm & passwd);
            DIR.Copy_File (sretc & group,  mm & group);
      end case;
      case platform_type is
         when dragonfly |
              freebsd   |
              netbsd    |
              openbsd   |
              midnightbsd =>
            DIR.Copy_File (sretc & spwd, mm & spwd);
            DIR.Copy_File (sretc & pwd,  mm & pwd);
         when linux     |
              macos     |
              sunos     => null;  -- pwd.db not used
      end case;
      case platform_type is
         when dragonfly |
              freebsd   |
              netbsd    |
              openbsd   |
              midnightbsd =>
            DIR.Copy_File (sretc & rcconf, mm & rcconf);
         when linux     |
              macos     |
              sunos     => null;  --  rc.conf not used
      end case;
      case platform_type is
         when dragonfly =>
            DIR.Copy_File (sretc & hints, mm & hints);
         when freebsd   =>
            DIR.Copy_File (sretc & hints, mm & hints);
         when midnightbsd =>
            DIR.Copy_File (sretc & hints, mm & hints);
         when openbsd   =>
            DIR.Copy_File (sretc & nhints, mm & nhints);
         when linux     =>
            DIR.Copy_File (sretc & ldcnf1, mm & ldcnf1);
            DIR.Copy_File (sretc & ldcnf2, mm & ldcnf2);
            if DIR.Exists (sretc & ldcnf3) then
               DIR.Copy_File (sretc & ldcnf3, mm & ldcnf3);
            end if;
         when netbsd    |
              macos     |
              sunos     => null;
      end case;
      create_mtree_exc_genesis (mm);
      create_mtree_exc_preconfig (mm);

   end initialize;


   --------------------------------------------------------------------------------------------
   --  finalize
   --------------------------------------------------------------------------------------------
   procedure finalize
   is
      mm : constant String := get_master_mount;
   begin
      if DIR.Exists (mm) then
         annihilate_directory_tree (mm);
      end if;
      stop_abnormal_logging;
   end finalize;


   --------------------------------------------------------------------------------------------
   --  get_master_mount
   --------------------------------------------------------------------------------------------
   function get_master_mount return String is
   begin
      return HT.USS (PM.configuration.dir_buildbase) & "/" & reference_base;
   end get_master_mount;


   --------------------------------------------------------------------------------------------
   --  get_slave_mount
   --------------------------------------------------------------------------------------------
   function get_slave_mount  (id : builders) return String is
   begin
      return HT.USS (PM.configuration.dir_buildbase) & "/" & slave_name (id);
   end get_slave_mount;


   --------------------------------------------------------------------------------------------
   --  start_abnormal_logging
   --------------------------------------------------------------------------------------------
   procedure start_abnormal_logging
   is
      logpath : constant String := HT.USS (PM.configuration.dir_logs)
        & "/logs/" & abnormal_cmd_logname;
   begin
      if DIR.Exists (logpath) then
         DIR.Delete_File (logpath);
      end if;
      TIO.Create (File => abnormal_log,
                  Mode => TIO.Out_File,
                  Name => logpath);
      abn_log_ready := True;
   exception
      when others => abn_log_ready := False;
   end start_abnormal_logging;


   --------------------------------------------------------------------------------------------
   --  stop_abnormal_logging
   --------------------------------------------------------------------------------------------
   procedure stop_abnormal_logging is
   begin
      if abn_log_ready then
         TIO.Close (abnormal_log);
      end if;
   end stop_abnormal_logging;


   --------------------------------------------------------------------------------------------
   --  annihilate_directory_tree
   --------------------------------------------------------------------------------------------
   procedure annihilate_directory_tree (tree : String)
   is
      command : constant String := "/bin/rm -rf " & tree;
      retry   : Boolean := False;
   begin
      silent_exec (command);
   exception
      when others =>
         --  Only can occur when tmpfs is avoided
         if DIR.Exists (tree & "/home") then
            folder_access (tree & "/home", unlock);
            retry := True;
         end if;
         if DIR.Exists (tree & "/root") then
            folder_access (tree & "/root", unlock);
            retry := True;
         end if;
         if retry then
            silent_exec (command);
         else
            raise scenario_unexpected with "annihilate_directory_tree " & tree & " failed";
         end if;
   end annihilate_directory_tree;


   --------------------------------------------------------------------------------------------
   --  annihilate_directory_tree_contents
   --------------------------------------------------------------------------------------------
   procedure annihilate_directory_tree_contents (tree : String)
   is
      command : constant String := "/usr/bin/find -s " & tree &
                                   " -depth 1 -maxdepth 1 -exec /bin/rm -rf {} \;";
   begin
      silent_exec (command);
   end annihilate_directory_tree_contents;

   --------------------------------------------------------------------------------------------
   --  execute
   --------------------------------------------------------------------------------------------
   procedure execute (command : String)
   is
      Exit_Status : Integer;
      output : HT.Text := Unix.piped_command (command, Exit_Status);
   begin
      if abn_log_ready and then not HT.IsBlank (output) then
         TIO.Put_Line (abnormal_log, HT.USS (output));
      end if;
      if Exit_Status /= 0 then
         raise scenario_unexpected with
           command & " => failed with code" & Exit_Status'Img;
      end if;
   end execute;


   --------------------------------------------------------------------------------------------
   --  silent_exec
   --------------------------------------------------------------------------------------------
   procedure silent_exec (command : String)
   is
      cmd_output : HT.Text;
      success : Boolean := Unix.piped_mute_command (command, cmd_output);
   begin
      if not success then
         if abn_log_ready and then not HT.IsBlank (cmd_output) then
            TIO.Put_Line (abnormal_log, "piped_mute_command failure:");
            TIO.Put_Line (abnormal_log, HT.USS (cmd_output));
         end if;
         raise scenario_unexpected with
           command & " => failed (exit code not 0)";
      end if;
   end silent_exec;


   --------------------------------------------------------------------------------------------
   --  append_abnormal_log
   --------------------------------------------------------------------------------------------
   procedure append_abnormal_log (line : String) is
   begin
      TIO.Put_Line (abnormal_log, line);
   end append_abnormal_log;


   --------------------------------------------------------------------------------------------
   --  internal_system_command
   --------------------------------------------------------------------------------------------
   function internal_system_command (command : String) return HT.Text
   is
      content : HT.Text;
      status  : Integer;
   begin
      content := Unix.piped_command (command, status);
      if status /= 0 then
         raise scenario_unexpected with "cmd: " & command &
           " (return code =" & status'Img & ")";
      end if;
      return content;
   end internal_system_command;


   --------------------------------------------------------------------------------------------
   --  specific_mount_exists
   --------------------------------------------------------------------------------------------
   function specific_mount_exists (mount_point : String) return Boolean
   is
      comres  : constant String := HT.USS (internal_system_command (df_command));
      markers : HT.Line_Markers;
   begin
      HT.initialize_markers (comres, markers);
      loop
         exit when not HT.next_line_present (comres, markers);
         declare
            line : constant String := HT.extract_line (comres, markers);
         begin
            if HT.contains (line, mount_point) then
               return True;
            end if;
         end;
      end loop;
      return False;
   exception
      when others =>
         return True;
   end specific_mount_exists;


   --------------------------------------------------------------------------------------------
   --  ravenadm_mounts_exist
   --------------------------------------------------------------------------------------------
   function ravenadm_mounts_exist return Boolean
   is
      buildbase : constant String := HT.USS (PM.configuration.dir_buildbase);
      comres    : constant String := HT.USS (internal_system_command (df_command));
      markers   : HT.Line_Markers;
   begin
      HT.initialize_markers (comres, markers);
      loop
         exit when not HT.next_line_present (comres, markers);
         declare
            line : constant String := HT.extract_line (comres, markers);
         begin
            if HT.contains (line, buildbase) then
               return True;
            end if;
         end;
      end loop;
      return False;
   exception
      when others =>
         return True;
   end ravenadm_mounts_exist;


   --------------------------------------------------------------------------------------------
   --  clear_existing_mounts
   --------------------------------------------------------------------------------------------
   function clear_existing_mounts return Boolean
   is
      package crate is new CON.Vectors (Index_Type   => Positive,
                                        Element_Type => HT.Text,
                                        "="          => HT.SU."=");
      procedure annihilate (cursor : crate.Cursor);

      buildbase : constant String := HT.USS (PM.configuration.dir_buildbase);
      comres    : constant String := HT.USS (internal_system_command (df_command));
      markers   : HT.Line_Markers;
      mpoints   : crate.Vector;

      procedure annihilate (cursor : crate.Cursor)
      is
         mountpoint : constant String := HT.USS (crate.Element (cursor));
      begin
         unmount (mountpoint);
         if DIR.Exists (mountpoint) then
            DIR.Delete_Directory (mountpoint);
         end if;
      exception
         when others => null;
      end annihilate;
   begin
      HT.initialize_markers (comres, markers);
      loop
         exit when not HT.next_line_present (comres, markers);
         declare
            line   : constant String := HT.extract_line (comres, markers);
            mindex : Natural;
         begin
            mindex := HT.start_index (line, buildbase);
            if mindex > 0 then
               mpoints.Append (HT.SUS (line (mindex .. line'Last)));
            end if;
         end;
      end loop;

      mpoints.Reverse_Iterate (Process => annihilate'Access);

      if ravenadm_mounts_exist then
         return False;
      end if;

      --  No need to remove empty dirs, the upcoming run will do that.
      return True;
   end clear_existing_mounts;


   --------------------------------------------------------------------------------------------
   --  disk_workareas_exist
   --------------------------------------------------------------------------------------------
   function disk_workareas_exist return Boolean
   is
      Search    : DIR.Search_Type;
      buildbase : constant String := HT.USS (PM.configuration.dir_buildbase);
      result    : Boolean := False;
   begin
      if not DIR.Exists (buildbase) then
         return False;
      end if;
      if DIR.Exists (buildbase & "/Base") then
         return True;
      end if;
      --  SLXX may be present if tmpfs is avoided
      DIR.Start_Search (Search    => Search,
                        Directory => buildbase,
                        Filter    => (DIR.Directory => True, others => False),
                        Pattern   => "SL*");

      result := DIR.More_Entries (Search => Search);
      DIR.End_Search (Search);
      return result;
   end disk_workareas_exist;


   --------------------------------------------------------------------------------------------
   --  clear_existing_workareas
   --------------------------------------------------------------------------------------------
   function clear_existing_workareas return Boolean
   is
      Search    : DIR.Search_Type;
      Dir_Ent   : DIR.Directory_Entry_Type;
      buildbase : constant String := HT.USS (PM.configuration.dir_buildbase);
      base      : constant String := buildbase & "/Base";
   begin
      if DIR.Exists (base) then
         annihilate_directory_tree (base);
      end if;
      --  SLXX may be present if tmpfs is avoided
      DIR.Start_Search (Search    => Search,
                        Directory => buildbase,
                        Filter    => (DIR.Directory => True, others => False),
                        Pattern   => "SL*");
      while DIR.More_Entries (Search => Search) loop
         DIR.Get_Next_Entry (Search => Search, Directory_Entry => Dir_Ent);
         declare
            target : constant String := buildbase & "/" & DIR.Simple_Name (Dir_Ent);
         begin
            annihilate_directory_tree (target);
         end;
      end loop;
      DIR.End_Search (Search);
      return True;
   exception
      when others => return False;
   end clear_existing_workareas;


   --------------------------------------------------------------------------------------------
   --  create_mtree_exc_preinst
   --------------------------------------------------------------------------------------------
   procedure create_mtree_exc_genesis (path_to_mm : String)
   is
      mtreefile : TIO.File_Type;
      filename  : constant String := path_to_mm & "/mtree.genesis.exclude";
   begin
      TIO.Create (File => mtreefile, Mode => TIO.Out_File, Name => filename);
      write_common_mtree_exclude_base (mtreefile);
      write_genesis_section (mtreefile);
      TIO.Close (mtreefile);
   end create_mtree_exc_genesis;


   --------------------------------------------------------------------------------------------
   --  create_mtree_exc_preconfig
   --------------------------------------------------------------------------------------------
   procedure create_mtree_exc_preconfig (path_to_mm : String)
   is
      mtreefile : TIO.File_Type;
      filename  : constant String := path_to_mm & "/mtree.preconfig.exclude";
   begin
      TIO.Create (File => mtreefile, Mode => TIO.Out_File, Name => filename);
      write_common_mtree_exclude_base (mtreefile);
      TIO.Close (mtreefile);
   end create_mtree_exc_preconfig;


   --------------------------------------------------------------------------------------------
   --  write_common_mtree_exclude_base
   --------------------------------------------------------------------------------------------
   procedure write_common_mtree_exclude_base (mtreefile : TIO.File_Type)
   is
      function write_usr return String;
      function opsys_specific return String;

      function write_usr return String is
      begin
         if HT.equivalent (ravenbase, bsd_localbase) then
            return "./usr/bin" & LAT.LF
              & "./usr/include" & LAT.LF
              & "./usr/lib" & LAT.LF
              & "./usr/lib32" & LAT.LF
              & "./usr/share" & LAT.LF;
         else
            return "./usr" & LAT.LF;
         end if;
      end write_usr;

      function opsys_specific return String is
      begin
         case platform_type is
            when freebsd | dragonfly | netbsd | openbsd | midnightbsd =>
               return "./libexec" & LAT.LF;
            when linux =>
               return "./lib" & LAT.LF & "./lib64" & LAT.LF;
            when sunos =>
               return "./lib" & LAT.LF & "./devices" & LAT.LF;
            when macos =>
               return "./System" & LAT.LF;
         end case;
      end opsys_specific;
   begin
      TIO.Put_Line
        (mtreefile,
           "./bin" & LAT.LF
         & "./ccache" & LAT.LF
         & "./construction" & LAT.LF
         & "./dev" & LAT.LF
         & "./distfiles" & LAT.LF
         & "./home" & LAT.LF
         & "./port" & LAT.LF
         & "./proc" & LAT.LF
         & "./repo" & LAT.LF
         & "./root" & LAT.LF
         & "./tmp" & LAT.LF
         & write_usr
         & opsys_specific
         & "./xports"
        );
   end write_common_mtree_exclude_base;


   --------------------------------------------------------------------------------------------
   --  write_preinstall_section
   --------------------------------------------------------------------------------------------
   procedure write_genesis_section (mtreefile : TIO.File_Type)
   is
      RB : String := LAT.Full_Stop & HT.USS (ravenbase);
   begin
      TIO.Put_Line
        (mtreefile,
           "./etc/group" & LAT.LF
         & "./etc/make.conf" & LAT.LF
         & "./etc/master.passwd" & LAT.LF
         & "./etc/mtree.*" & LAT.LF
         & "./etc/passwd" & LAT.LF
         & "./etc/pwd.db" & LAT.LF
         & "./etc/resolv.conf*" & LAT.LF
         & "./etc/shells" & LAT.LF
         & "./etc/spwd.db" & LAT.LF
         & "./etc/ld.so.conf.d/x86_64-linux-gnu.conf" & LAT.LF
         & "./var/cache" & LAT.LF
         & "./var/db" & LAT.LF
         & "./var/log" & LAT.LF
         & "./var/mail" & LAT.LF
         & "./var/run" & LAT.LF
         & "./var/spool" & LAT.LF
         & "./var/tmp"
        );
   end write_genesis_section;


   --------------------------------------------------------------------------------------------
   --  df_command
   --------------------------------------------------------------------------------------------
   function df_command return String is
   begin
      case platform_type is
         when freebsd   |
              macos     |
              midnightbsd => return "/bin/df -h";
         when dragonfly |
              netbsd    |
              openbsd   => return "/bin/df -h -t null,tmpfs,devfs,procfs";
         when sunos     => return "/usr/sbin/df -h";
         when linux     => return "/bin/df -h -a";
      end case;
   end df_command;


   --------------------------------------------------------------------------------------------
   --  unmount
   --------------------------------------------------------------------------------------------
   procedure unmount (device_or_node : String; retry_times : Natural := 0)
   is
      bsd_command : constant String := "/sbin/umount -f " & device_or_node;
      sol_command : constant String := "/usr/sbin/umount -f " & device_or_node;
      lin_command : constant String := "/bin/umount -f " & device_or_node;
      counter     : Natural := 0;
      success     : Boolean := False;
   begin
      --  failure to unmount causes stderr squawks which messes up curses display
      --  Just log it and ignore for now (Add robustness later)
      loop
         begin
            exit when counter > retry_times;
            case platform_type is
               when dragonfly |
                    freebsd   |
                    macos     |
                    netbsd    |
                    openbsd   |
                    midnightbsd => execute (bsd_command);
               when linux     => execute (lin_command);
               when sunos     => execute (sol_command);
            end case;
            success := True;
            exit;
         exception
            when others =>
               counter := counter + 1;
               delay 10.0;
         end;
      end loop;
      if not success then
         raise failed_unmount with device_or_node;
      end if;
   end unmount;


   --------------------------------------------------------------------------------------------
   --  mount_nullfs
   --------------------------------------------------------------------------------------------
   procedure mount_nullfs (target, mount_point : String; mode : mount_mode := readonly)
   is
      cmd_freebsd   : constant String := "/sbin/mount_nullfs";
      cmd_dragonfly : constant String := "/sbin/mount_null";
      cmd_solaris   : constant String := "/sbin/mount -F lofs";
      cmd_linux     : constant String := "/bin/mount --bind";
      cmd_macos     : constant String := "/sbin/mount -t nfs";
      cmd_openbsd   : constant String := "/sbin/mount_nfs";
      command       : HT.Text;
   begin
      if not DIR.Exists (mount_point) then
         raise scenario_unexpected with
           "mount point " & mount_point & " does not exist";
      end if;
      if not DIR.Exists (target) then
         raise scenario_unexpected with
           "mount target " & target & " does not exist";
      end if;

      case platform_type is
         when freebsd   |
              midnightbsd => command := HT.SUS (cmd_freebsd);
         when dragonfly |
              netbsd    => command := HT.SUS (cmd_dragonfly);
         when sunos     => command := HT.SUS (cmd_solaris);
         when linux     => command := HT.SUS (cmd_linux);
         when macos     => command := HT.SUS (cmd_macos);
         when openbsd   => command := HT.SUS (cmd_openbsd);
      end case;
      case mode is
         when readonly  => HT.SU.Append (command, " -o ro");
         when readwrite => null;
      end case;
      case platform_type is
         when macos | openbsd =>
            execute (HT.USS (command) & " 127.0.0.1:" & target & " " & mount_point);
         when others =>
            execute (HT.USS (command) & " " & target & " " & mount_point);
      end case;
   end mount_nullfs;


   --------------------------------------------------------------------------------------------
   --  mount_tmpfs
   --------------------------------------------------------------------------------------------
   procedure mount_tmpfs (mount_point : String; max_size_M : Natural := 0)
   is
      cmd_freebsd   : constant String := "/sbin/mount -t tmpfs";
      cmd_dragonfly : constant String := "/sbin/mount_tmpfs";
      cmd_solaris   : constant String := "/sbin/mount -F tmpfs";
      cmd_linux     : constant String := "/bin/mount -t tmpfs";
      command       : HT.Text;
   begin
      case platform_type is
         when freebsd   |
              netbsd    |
              midnightbsd => command := HT.SUS (cmd_freebsd);
         when dragonfly => command := HT.SUS (cmd_dragonfly);
         when sunos     => command := HT.SUS (cmd_solaris);
         when linux     => command := HT.SUS (cmd_linux);
         when macos     |     --  Not available at all
              openbsd   =>    --  Was available, disabled on OpenBSD 6.0 (no maintenance)
            raise scenario_unexpected with
              "tmpfs not supported on " & platform_type'Img;
      end case;
      if max_size_M > 0 then
         case platform_type is
            when netbsd =>
               HT.SU.Append (command, " -o -s" & HT.trim (max_size_M'Img) & "M");
            when macos     => null;
            when openbsd   => null;
            when others =>
               HT.SU.Append (command, " -o size=" & HT.trim (max_size_M'Img) & "M");
         end case;
      end if;
      case platform_type is
         when sunos     => HT.SU.Append (command, " swap " & mount_point);
         when freebsd   |
              dragonfly |
              netbsd    |
              linux     |
              midnightbsd => HT.SU.Append (command, " tmpfs " & mount_point);
         when macos     => null;
         when openbsd   => null;
      end case;
      execute (HT.USS (command));
   end mount_tmpfs;


   --------------------------------------------------------------------------------------------
   --  mount_devices
   --------------------------------------------------------------------------------------------
   procedure mount_devices (path_to_dev : String)
   is
      bsd_command : constant String := "/sbin/mount -t devfs devfs " & path_to_dev;
      lin_command : constant String := "/bin/mount --bind /dev " & path_to_dev;
   begin
      case platform_type is
         when dragonfly |
              freebsd   |
              macos     |
              midnightbsd => execute (bsd_command);
         when linux     => execute (lin_command);
         when netbsd    |
              openbsd   |
              sunos     => mount_nullfs (target => "/dev", mount_point => path_to_dev);
      end case;
   end mount_devices;


   --------------------------------------------------------------------------------------------
   --  unmount_devices
   --------------------------------------------------------------------------------------------
   procedure unmount_devices (path_to_dev : String) is
   begin
      unmount (path_to_dev);
   end unmount_devices;


   --------------------------------------------------------------------------------------------
   --  mount_procfs
   --------------------------------------------------------------------------------------------
   procedure mount_procfs (path_to_proc : String)
   is
      bsd_command : constant String := "/sbin/mount -t procfs proc " & path_to_proc;
      net_command : constant String := "/sbin/mount_procfs /proc " & path_to_proc;
      lin_command : constant String := "/bin/mount --bind /proc " & path_to_proc;
   begin
      case platform_type is
         when dragonfly |
              freebsd   |
              midnightbsd => execute (bsd_command);
         when netbsd    |
              openbsd   => execute (net_command);
         when linux     => execute (lin_command);
         when sunos     => mount_nullfs (target => "/proc", mount_point => path_to_proc);
         when macos =>
            raise scenario_unexpected with
              "procfs not supported on " & platform_type'Img;
      end case;
   end mount_procfs;


   --------------------------------------------------------------------------------------------
   --  unmount_procfs
   --------------------------------------------------------------------------------------------
   procedure unmount_procfs (path_to_proc : String) is
   begin
      unmount (path_to_proc);
   end unmount_procfs;


   --------------------------------------------------------------------------------------------
   --  mount_hardlink
   --------------------------------------------------------------------------------------------
   procedure mount_hardlink (target, mount_point, sysroot : String)
   is
      find_program  : constant String := sysroot & "/usr/bin/find ";
      copy_program  : constant String := sysroot & "/bin/cp -RpPl ";
      chmod_program : constant String := sysroot & "/bin/chmod ";
   begin
      if DIR.Exists (mount_point) then
         DIR.Delete_Directory (mount_point);
      end if;
      execute (copy_program & target & " " & mount_point);
      execute (find_program & mount_point & " -type d -exec " & chmod_program & "555 {} +");
   end mount_hardlink;


   --------------------------------------------------------------------------------------------
   --  mount_fullcopy
   --------------------------------------------------------------------------------------------
   procedure mount_fullcopy (target, mount_point, sysroot : String)
   is
      find_program  : constant String := sysroot & "/usr/bin/find ";
      copy_program  : constant String := sysroot & "/bin/cp -RpP ";
      chmod_program : constant String := sysroot & "/bin/chmod ";
   begin
      if DIR.Exists (mount_point) then
         DIR.Delete_Directory (mount_point);
      end if;
      execute (copy_program & target & " " & mount_point);
      execute (find_program & mount_point & " -type d -exec " & chmod_program & "555 {} +");
   end mount_fullcopy;


   --------------------------------------------------------------------------------------------
   --  location
   --------------------------------------------------------------------------------------------
   function location (mount_base : String; point : folder) return String is
   begin
      case point is
         when bin         => return mount_base & root_bin;
         when usr         => return mount_base & root_usr;
         when dev         => return mount_base & root_dev;
         when etc         => return mount_base & root_etc;
         when etc_default => return mount_base & root_etc_default;
         when etc_rcd     => return mount_base & root_etc_rcd;
         when etc_ldsocnf => return mount_base & root_etc_ldsocnf;
         when tmp         => return mount_base & root_tmp;
         when var         => return mount_base & root_var;
         when home        => return mount_base & root_home;
         when proc        => return mount_base & root_proc;
         when root        => return mount_base & root_root;
         when xports      => return mount_base & root_xports;
         when port        => return mount_base & root_port;
         when lib         => return mount_base & root_lib;
         when lib64       => return mount_base & root_lib64;
         when libexec     => return mount_base & root_libexec;
         when packages    => return mount_base & root_packages;
         when distfiles   => return mount_base & root_distfiles;
         when wrkdirs     => return mount_base & root_wrkdirs;
         when ccache      => return mount_base & root_ccache;
         when devices     => return mount_base & root_devices;
         when repofiles   => return mount_base & root_repofiles;
         when frameworks  => return mount_base & root_frameworks;
         when localbase   => return mount_base & HT.USS (PM.configuration.dir_localbase);
         when toolchain   => return mount_base & HT.USS (PM.configuration.dir_localbase) &
                                    toolchain_dir;
      end case;
   end location;


   --------------------------------------------------------------------------------------------
   --  mount_target
   --------------------------------------------------------------------------------------------
   function mount_target (point : folder) return String is
   begin
      case point is
         when xports    => return HT.USS (PM.configuration.dir_conspiracy);
         when toolchain => return HT.USS (PM.configuration.dir_toolchain);
         when distfiles => return HT.USS (PM.configuration.dir_distfiles);
         when ccache    => return HT.USS (PM.configuration.dir_ccache);
         when others    => return "ERROR";
      end case;
   end mount_target;


   --------------------------------------------------------------------------------------------
   --  forge_directory
   --------------------------------------------------------------------------------------------
   procedure forge_directory (target : String) is
   begin
      DIR.Create_Path (New_Directory => target);
   exception
      when failed : others =>
         TIO.Put_Line (EX.Exception_Information (failed));
         raise scenario_unexpected with
           "failed to create " & target & " directory";
   end forge_directory;


   --------------------------------------------------------------------------------------------
   --  folder_access
   --------------------------------------------------------------------------------------------
   procedure folder_access (path : String; operation : folder_operation)
   is
      --  chattr does not work on tmpfs partitions
      --  It appears immutable locking can't be supported on Linux
      --  Don't use chflags schg on *BSD as securitylevel > 0 (BSD) will block it
      cmd_fallback  : constant String := "/bin/chmod";
      fback_lock    : constant String := " 555 ";
      fback_unlock  : constant String := " 755 ";

      command       : HT.Text  := HT.SUS (cmd_fallback);
   begin
      if not DIR.Exists (path) then
         return;
      end if;

      case operation is
         when lock   => HT.SU.Append (command, fback_lock & path);
         when unlock => HT.SU.Append (command, fback_unlock & path);
      end case;

      execute (HT.USS (command));
   end folder_access;


   --------------------------------------------------------------------------------------------
   --  folder_access
   --------------------------------------------------------------------------------------------
   procedure set_folder_mode (path : String; operation : folder_operation)
   is
      cmd      : constant String := "/bin/chmod";
      oplock   : constant String := " 555 ";
      opunlock : constant String := " 755 ";
      command  : HT.Text;
   begin
      case operation is
         when lock   => command := HT.SUS (cmd & oplock & path);
         when unlock => command := HT.SUS (cmd & opunlock & path);
      end case;
      execute (HT.USS (command));
   end set_folder_mode;


   --------------------------------------------------------------------------------------------
   --  get_workzone_path
   --------------------------------------------------------------------------------------------
   function get_workzone_path return String is
   begin
      return get_slave_mount (workzone_id);
   end get_workzone_path;


   --------------------------------------------------------------------------------------------
   --  launch_workzone
   --------------------------------------------------------------------------------------------
   procedure launch_workzone
   is
      zone_base : constant String := get_workzone_path;
   begin
      forge_directory (zone_base);
      if not PM.configuration.avoid_tmpfs then
         --  Limit slave to 32Mb
         mount_tmpfs (zone_base, 32);
      end if;
   end launch_workzone;


   --------------------------------------------------------------------------------------------
   --  destroy_workzone
   --------------------------------------------------------------------------------------------
   procedure destroy_workzone
   is
      zone_base : constant String := get_workzone_path;
   begin
      if not PM.configuration.avoid_tmpfs then
         unmount (zone_base, 50);
      end if;
      annihilate_directory_tree (zone_base);
   end destroy_workzone;


   --------------------------------------------------------------------------------------------
   --  clear_workzone_directory
   --------------------------------------------------------------------------------------------
   procedure clear_workzone_directory (subpath : String)
   is
      zone_base : constant String := get_workzone_path;
   begin
      annihilate_directory_tree (zone_base & "/" & subpath);
   end clear_workzone_directory;


   --------------------------------------------------------------------------------------------
   --  launch_slave
   --------------------------------------------------------------------------------------------
   procedure launch_slave  (id : builders; need_procfs : Boolean := False)
    is
      slave_base  : constant String := get_slave_mount (id);
      slave_local : constant String := slave_base & "_localbase";
      dir_system  : constant String := HT.USS (PM.configuration.dir_sysroot);
      lbase       : constant String := HT.USS (PM.configuration.dir_localbase);
      etc_path    : constant String := location (slave_base, etc);
   begin
      forge_directory (slave_base);

      if PM.configuration.avoid_tmpfs then
         if lbase = bsd_localbase then
            --  /usr is write only, so to build on /usr/local, we need a dedicated mount
            --  restriction isn't necessary on mac or openbsd which copies via hardlink
            case platform_type is
               when macos | openbsd =>
                  set_folder_mode (slave_base & lbase, unlock);
               when others =>
                  forge_directory (location (slave_local, toolchain));
            end case;
         end if;
      else
         --  Limit slave to 24Gb, covers localbase + construction mainly
         mount_tmpfs (slave_base, 24 * 1024);
         if lbase = bsd_localbase then
            mount_tmpfs (slave_base & bsd_localbase, 12 * 1024);
         end if;
      end if;

      for mnt in safefolders'Range loop
         forge_directory (location (slave_base, mnt));
      end loop;

      --  Save a null mount on all platforms (all keeps /xports to the bare minimum)
      declare
         mk_directory : constant String := mount_target (xports) & "/Mk";
         slave_mk     : constant String := location (slave_base, xports) & "/Mk";
      begin
         if PM.configuration.avoid_tmpfs then
            mount_hardlink (mk_directory, slave_mk, dir_system);
         else
            mount_fullcopy (mk_directory, slave_mk, dir_system);
         end if;
         process_keyword_files (slave_mk, lbase);
      end;

      case platform_type is
         when macos | openbsd =>
            mount_hardlink (location (dir_system, bin), location (slave_base, bin), dir_system);
            mount_hardlink (location (dir_system, usr), location (slave_base, usr), dir_system);
            mount_hardlink (mount_target (toolchain),
                            location (slave_base, toolchain) & "-off",
                            dir_system);
            preplace_libgcc_s (location (slave_base, toolchain) & "-fallback");
         when others =>
            mount_nullfs (location (dir_system, bin), location (slave_base, bin));
            mount_nullfs (location (dir_system, usr), location (slave_base, usr));
      end case;
      case platform_type is
         when freebsd | dragonfly | netbsd | openbsd | midnightbsd =>
            --  should be limited to rtld executable
            if PM.configuration.avoid_tmpfs then
               mount_hardlink (target      => location (dir_system, libexec),
                               mount_point => location (slave_base, libexec),
                               sysroot     => dir_system);
            else
               --  saves a null mount (at the cost of memory)
               mount_fullcopy (target      => location (dir_system, libexec),
                               mount_point => location (slave_base, libexec),
                               sysroot     => dir_system);
            end if;
         when linux =>
            mount_nullfs (location (dir_system, lib),   location (slave_base, lib));
            mount_nullfs (location (dir_system, lib64), location (slave_base, lib64));
         when sunos =>
            forge_directory (location (slave_base, devices));
            mount_nullfs (location (dir_system, lib),   location (slave_base, lib));
            mount_nullfs (root_devices,                 location (slave_base, devices));
         when macos =>
            forge_directory (location (slave_base, frameworks));
            mount_nullfs (location (dir_system, frameworks), location (slave_base, frameworks));
      end case;

      folder_access (location (slave_base, home), lock);
      folder_access (location (slave_base, root), lock);

      mount_nullfs (mount_target (distfiles), location (slave_base, distfiles), mode => readwrite);

      if need_procfs or else
        platform_type = linux or else
        platform_type = sunos
      then
         mount_procfs (path_to_proc => location (slave_base, proc));
      end if;

      if DIR.Exists (mount_target (ccache)) then
         mount_nullfs (mount_target (ccache), location (slave_base, ccache), readwrite);
      end if;

      mount_devices (location (slave_base, dev));

      populate_var_folder      (location (slave_base, var));
      copy_rc_default          (etc_path);
      copy_resolv_conf         (etc_path);
      install_libmap_conf      (etc_path);
      copy_ldconfig_hints      (slave_base & "/var/run");
      copy_unkindness_IDs      (slave_base & "/construction");
      fix_macos_resolv         (slave_base & "/var/run");
      create_make_conf         (etc_path);
      install_passwd_and_group (etc_path);
      create_etc_services      (etc_path);
      create_etc_shells        (etc_path);
      create_sun_files         (etc_path);
      create_etc_localtime     (etc_path);
      create_repo_conf         (etc_path);
      install_linux_ldsoconf   (location (slave_base, etc_ldsocnf));

   exception
      when hiccup : others =>
         TIO.Put_Line (abnormal_log,
                       "LAUNCH SLAVE" & id'Img & " FAILED: " & EX.Exception_Information (hiccup));
         Signals.initiate_shutdown;
   end launch_slave;


   --------------------------------------------------------------------------------------------
   --  destroy_slave
   --------------------------------------------------------------------------------------------
   procedure destroy_slave (id : builders; need_procfs : Boolean := False)
   is
      slave_base  : constant String := get_slave_mount (id);
      slave_local : constant String := slave_base & "_localbase";
      dir_system  : constant String := HT.USS (PM.configuration.dir_sysroot);
      lbase       : constant String := HT.USS (PM.configuration.dir_localbase);
      retry1min   : constant Natural := 6;
      counter     : Natural := 0;
   begin

      unmount_devices (location (slave_base, dev));

      if DIR.Exists (mount_target (ccache)) then
         unmount (location (slave_base, ccache), retry1min);
      end if;

      if need_procfs or else
        platform_type = linux or else
        platform_type = sunos
      then
         unmount_procfs (location (slave_base, proc));
      end if;

      unmount (location (slave_base, distfiles), retry1min);

      if DIR.Exists (slave_base & toolchain_tag) then
         unhook_toolchain (id);
      end if;

      case platform_type is
         when macos | openbsd => null;
         when others =>
            unmount (location (slave_base, bin));
            unmount (location (slave_base, usr));
      end case;
      case platform_type is
         when freebsd | dragonfly | netbsd | openbsd | midnightbsd =>
            null;  --  libexec is copied now
         when linux =>
            unmount (location (slave_base, lib));
            unmount (location (slave_base, lib64));
         when sunos =>
            unmount (location (slave_base, lib));
            unmount (location (slave_base, devices));
         when macos =>
            unmount (location (slave_base, frameworks));
      end case;

      if PM.configuration.avoid_tmpfs then
         if lbase = bsd_localbase then
            case platform_type is
               when macos | openbsd => null;
               when others =>
                  unmount (slave_base & lbase, retry1min);
            end case;
         end if;
         annihilate_directory_tree (slave_local);
      else
         if lbase = bsd_localbase then
            unmount (slave_base & lbase, retry1min);
         end if;
         unmount (slave_base, retry1min * 5);
      end if;
      annihilate_directory_tree (slave_base);

   exception
      when hiccup : others =>
         TIO.Put_Line (abnormal_log,
                       "DESTROY SLAVE" & id'Img & " FAILED: " & EX.Exception_Information (hiccup));
         Signals.initiate_shutdown;
   end destroy_slave;


   --------------------------------------------------------------------------------------------
   --  hook_toolchain
   --------------------------------------------------------------------------------------------
   procedure hook_toolchain (id : builders)
   is
      use type DIR.File_Kind;
      slave_base : constant String := get_slave_mount (id);
      tc_path    : constant String := location (slave_base, toolchain);
      lbase      : constant String := HT.USS (PM.configuration.dir_localbase);
      forged     : TIO.File_Type;
   begin
      --  When hook_toolchain is called, there very well may be installed packages that
      --  brought in gcc libs and installed them at /raven/toolchain.
      --  For null-mount systems, the toolchain is mounted over /raven/toolchain, but it's
      --  unmounted before package deinstallation.
      --  For NFS-mount systems, the toolchain is hardlink-copied at toolchain-off.  The
      --  toolchain and toolchain-off are renamed toolchain-packages and toolchain respectively.
      --  This is reversed during unhooking.

      if PM.configuration.avoid_tmpfs then
         if lbase = bsd_localbase then
            case platform_type is
               when macos | openbsd =>
                  forge_directory (location (slave_base, toolchain));
               when others =>
                  mount_nullfs (slave_base & "_localbase", slave_base & lbase, readwrite);
            end case;
         else
            forge_directory (location (slave_base, toolchain));
         end if;
      else
         forge_directory (location (slave_base, toolchain));
      end if;

      case platform_type is
         when macos | openbsd =>
            DIR.Rename (Old_Name => tc_path, New_Name => tc_path & "-packaged");
            DIR.Rename (Old_Name => tc_path & "-off", New_Name => tc_path);
         when others =>
            mount_nullfs (mount_target (toolchain), tc_path);
      end case;

      TIO.Create (File => forged,
                  Mode => TIO.Out_File,
                  Name => slave_base & toolchain_tag);
      TIO.Close (forged);

   end hook_toolchain;


   --------------------------------------------------------------------------------------------
   --  unhook_toolchain
   --------------------------------------------------------------------------------------------
   procedure unhook_toolchain (id : builders)
   is
      use type DIR.File_Kind;
      slave_base : constant String := get_slave_mount (id);
      tc_path    : constant String := location (slave_base, toolchain);
   begin
      case platform_type is
         when macos | openbsd =>
            DIR.Rename (Old_Name => tc_path, New_Name => tc_path & "-off");
            DIR.Rename (Old_Name => tc_path & "-packaged", New_Name => tc_path);
         when others =>
            unmount (tc_path);
      end case;
      DIR.Delete_File (slave_base & toolchain_tag);
      begin
         DIR.Delete_Directory (location (slave_base, toolchain));
      exception
         when others =>
            null;  -- File silently.  The only impact is leftover file check failure
      end;
   end unhook_toolchain;


   --------------------------------------------------------------------------------------------
   --  slave_name
   --------------------------------------------------------------------------------------------
   function slave_name (id : builders) return String
   is
      id_image     : constant String := HT.int2str (Integer (id));
      suffix       : String := "SL00";
   begin
      if id < 10 then
         suffix (4) := id_image (id_image'First);
      else
         suffix (3 .. 4) := id_image (id_image'First .. id_image'First + 1);
      end if;
      return suffix;
   end slave_name;


   --------------------------------------------------------------------------------------------
   --  populate_var_folder
   --------------------------------------------------------------------------------------------
   procedure populate_var_folder (path : String) is
   begin
      forge_directory (path & "/cache");
      forge_directory (path & "/cron");
      forge_directory (path & "/db");
      forge_directory (path & "/empty");
      forge_directory (path & "/games");
      forge_directory (path & "/log");
      forge_directory (path & "/mail");
      forge_directory (path & "/msgs");
      forge_directory (path & "/preserve");
      forge_directory (path & "/run/sem");
      forge_directory (path & "/spool");
      forge_directory (path & "/tmp");
   end populate_var_folder;


   --------------------------------------------------------------------------------------------
   --  copy_ldconfig_hints
   --------------------------------------------------------------------------------------------
   procedure copy_ldconfig_hints (path_to_varrun : String)
   is
      mm     : constant String := get_master_mount;
      hints  : constant String := "/ld-elf.so.hints";
      nhints : constant String := "/ld.so.hints";
   begin
      case platform_type is
         when dragonfly | freebsd | midnightbsd =>
            DIR.Copy_File (mm & hints, path_to_varrun & hints);
         when openbsd =>
            DIR.Copy_File (mm & nhints, path_to_varrun & nhints);
         when macos | linux | sunos | netbsd => null;
      end case;
   end copy_ldconfig_hints;


   --------------------------------------------------------------------------------------------
   --  fix_macos_resolv
   --------------------------------------------------------------------------------------------
   procedure fix_macos_resolv (path_to_varrun : String)
   is

      DNSR      : constant String := "/mDNSResponder";
      path_orig : constant String := "/var/run" & DNSR;
      path_dest : constant String := path_to_varrun & DNSR;
      errprefix : constant String := "Hardlink failure: ";
   begin
      case platform_type is
         when macos =>
            if DIR.Exists (path_orig) then
               if not Unix.create_hardlink (path_orig, path_dest) then
                  TIO.Put_Line (errprefix & "link " & path_dest & " to " & path_orig);
               end if;
            else
               raise scenario_unexpected
                 with errprefix & path_orig & " is not present on system";
            end if;
         when others => null;
      end case;
   end fix_macos_resolv;


   --------------------------------------------------------------------------------------------
   --  preplace_libgcc_s
   --------------------------------------------------------------------------------------------
   procedure preplace_libgcc_s (path_to_toolchain : String)
   is
      mpath : constant String := "/" & default_compiler & "/lib";
      dylib : constant String := mpath & "/libgcc_s.1.dylib";
      TC : constant String := mount_target (toolchain);
   begin
      if DIR.Exists (TC & dylib) then
         forge_directory (path_to_toolchain & mpath);
         DIR.Copy_File (Source_Name => TC & dylib,
                        Target_Name => path_to_toolchain & dylib);
      else
         raise scenario_unexpected
           with TC & dylib & " is not present on system";
      end if;
   end preplace_libgcc_s;


   --------------------------------------------------------------------------------------------
   --  populate_var_folder
   --------------------------------------------------------------------------------------------
   procedure copy_rc_default (path_to_etc : String)
   is
      mm     : constant String := get_master_mount;
      rcconf : constant String := "/rc.conf";
   begin
      if DIR.Exists (mm & rcconf) then
         DIR.Copy_File (Source_Name => mm & rcconf,
                        Target_Name => path_to_etc & "/defaults" & rcconf);
      end if;
   end copy_rc_default;


   --------------------------------------------------------------------------------------------
   --  copy_resolv_conf
   --------------------------------------------------------------------------------------------
   procedure copy_resolv_conf (path_to_etc : String)
   is
      procedure install (filename : String);

      resolv    : constant String := "/resolv.conf";
      netconfig : constant String := "/netconfig";
      nssconf   : constant String := "/nsswitch.conf";
      nssfiles  : constant String := "/nsswitch.files";
      nssdns    : constant String := "/nsswitch.dns";

      procedure install (filename : String) is
      begin
         if DIR.Exists ("/etc" & filename) then
            DIR.Copy_File (Source_Name => "/etc" & filename,
                           Target_Name => path_to_etc & filename);
         end if;
      end install;
   begin
      install (resolv);
      install (netconfig);
      install (nssconf);
      install (nssfiles);
      install (nssdns);
   end copy_resolv_conf;


   --------------------------------------------------------------------------------------------
   --  install_libmap_conf
   --------------------------------------------------------------------------------------------
   procedure install_libmap_conf (path_to_etc : String)
   is
      profilelc : constant String := PM.raven_confdir & "/" &
        HT.USS (PM.configuration.profile) & "-libmap.conf";
   begin
      if DIR.Exists (profilelc) then
         DIR.Copy_File (Source_Name => profilelc,
                        Target_Name => path_to_etc & "/libmap.conf");
      end if;
   end install_libmap_conf;


   --------------------------------------------------------------------------------------------
   --  copy_unkindness_IDs
   --------------------------------------------------------------------------------------------
   procedure copy_unkindness_IDs (path_to_construction : String)
   is
      procedure install (filename, new_name : String);

      dir_unkindness : constant String := HT.USS (PM.configuration.dir_unkindness);

      procedure install (filename, new_name : String) is
      begin
         if DIR.Exists (filename) then
            DIR.Copy_File (Source_Name => filename,
                           Target_Name => path_to_construction & new_name);
         end if;
      end install;
   begin
      if dir_unkindness /= PM.no_unkindness then
         install (dir_unkindness & "/custom_UID", "/.UID.custom");
         install (dir_unkindness & "/custom_GID", "/.GID.custom");
      end if;
   end copy_unkindness_IDs;


   --------------------------------------------------------------------------------------------
   --  install_passwd
   --------------------------------------------------------------------------------------------
   procedure install_passwd_and_group (path_to_etc : String)
   is
      procedure install (filename : String);

      mm     : constant String := get_master_mount;
      maspwd : constant String := "/master.passwd";
      passwd : constant String := "/passwd";
      spwd   : constant String := "/spwd.db";
      pwd    : constant String := "/pwd.db";
      group  : constant String := "/group";
      mtree1 : constant String := "/mtree.preconfig.exclude";
      mtree2 : constant String := "/mtree.genesis.exclude";
      ldcnf2 : constant String := "/ld.so.conf";

      procedure install (filename : String) is
      begin
         if DIR.Exists (mm & filename) then
            DIR.Copy_File (Source_Name => mm & filename,
                           Target_Name => path_to_etc & filename);
         end if;
      end install;
   begin
      install (passwd);
      install (maspwd);
      install (spwd);
      install (pwd);
      install (group);
      install (mtree1);
      install (mtree2);
      install (ldcnf2);
   end install_passwd_and_group;


   --------------------------------------------------------------------------------------------
   --  install_linux_ldsoconf
   --------------------------------------------------------------------------------------------
   procedure install_linux_ldsoconf (path_to_etc_ldsocnf : String)
   is
      procedure install (filename : String);
      procedure install_cache;

      mm     : constant String := get_master_mount;
      ldconf : constant String := "/x86_64-linux-gnu.conf";
      cache  : constant String := "/ld.so.cache";

      procedure install (filename : String) is
      begin
         if DIR.Exists (mm & filename) then
            DIR.Copy_File (Source_Name => mm & filename,
                           Target_Name => path_to_etc_ldsocnf & filename);
         end if;
      end install;

      procedure install_cache is
      begin
         if DIR.Exists (mm & cache) then
            DIR.Copy_File (Source_Name => mm & cache,
                           Target_Name => path_to_etc_ldsocnf & "/.." & cache);
         end if;
      end install_cache;
   begin
      install (ldconf);
      install_cache;
   end install_linux_ldsoconf;


   --------------------------------------------------------------------------------------------
   --  create_etc_services
   --------------------------------------------------------------------------------------------
   procedure create_etc_services (path_to_etc : String)
   is
      svcfile : TIO.File_Type;
   begin
      TIO.Create (File => svcfile,
                  Mode => TIO.Out_File,
                  Name => path_to_etc & "/services");
      TIO.Put_Line (svcfile,
                      "ftp    21/tcp" & LAT.LF &
                      "ftp    21/udp" & LAT.LF &
                      "ssh    22/tcp" & LAT.LF &
                      "ssh    22/udp" & LAT.LF &
                      "http   80/tcp" & LAT.LF &
                      "http   80/udp" & LAT.LF &
                      "https 443/tcp" & LAT.LF &
                      "https 443/udp" & LAT.LF);
      TIO.Close (svcfile);
   end create_etc_services;


   --------------------------------------------------------------------------------------------
   --  create_etc_shells
   --------------------------------------------------------------------------------------------
   procedure create_etc_shells (path_to_etc : String)
   is
      shells : TIO.File_Type;
   begin
      TIO.Create (File => shells,
                  Mode => TIO.Out_File,
                  Name => path_to_etc & "/shells");
      TIO.Put_Line (shells, "/bin/sh");
      TIO.Put_Line (shells, "/bin/csh");
      if platform_type = linux then
         TIO.Put_Line (shells, "/bin/bash");
      end if;
      TIO.Close (shells);
   end create_etc_shells;


   --------------------------------------------------------------------------------------------
   --  create_repo_conf
   --------------------------------------------------------------------------------------------
   procedure create_repo_conf (path_to_etc : String)
   is
      conf : TIO.File_Type;
      reposdir : constant String := path_to_etc & "/repos";
   begin
      DIR.Create_Path (reposdir);
      TIO.Create (conf, TIO.Out_File, reposdir & "/repo.conf");
      TIO.Put_Line (conf, "local: {");
      TIO.Put_Line (conf, "   url: file:///repo");
      TIO.Put_Line (conf, "   enabled: true");
      TIO.Put_Line (conf, "}");
      TIO.Close (conf);
   end create_repo_conf;


   --------------------------------------------------------------------------------------------
   --  create_etc_localtime
   --------------------------------------------------------------------------------------------
   procedure create_etc_localtime (path_to_etc : String)
   is
      result : Boolean;
   begin
      case platform_type is
         when netbsd =>
            result := Unix.create_symlink (actual_file    => "/usr/share/zoneinfo/UTC",
                                           link_to_create => path_to_etc & "/localtime");
         when others => null;
      end case;
   end create_etc_localtime;


   --------------------------------------------------------------------------------------------
   --  create_make_conf
   --------------------------------------------------------------------------------------------
   procedure create_make_conf (path_to_etc : String)
   is
      procedure override_defaults (label : String; value : HT.Text);

      makeconf  : TIO.File_Type;
      profilemc : constant String := PM.raven_confdir & "/" &
                  HT.USS (PM.configuration.profile) & "-make.conf";
      profile   : constant String := HT.USS (PM.configuration.profile);
      mjnum     : constant Integer := Integer (PM.configuration.jobs_limit);

      procedure override_defaults (label : String; value : HT.Text) is
      begin
         if not (HT.equivalent (value, ports_default)) then
            TIO.Put_Line (makeconf, "DEFAULT_VERSIONS+=" & label & "=" & HT.USS (value));
         end if;
      end override_defaults;
   begin

      TIO.Create (File => makeconf,
                  Mode => TIO.Out_File,
                  Name => path_to_etc & "/make.conf");

      TIO.Put_Line
        (makeconf,
           "RAVENPROFILE=" & profile & LAT.LF &
           "RAVENBASE=" & HT.USS (PM.configuration.dir_localbase) & LAT.LF &
           "WRKDIRPREFIX=/construction" & LAT.LF &
           "DISTDIR=/distfiles" & LAT.LF &
           "NUMBER_CPUS=" & HT.int2str (Integer (PM.configuration.number_cores)) & LAT.LF &
           "MAKE_JOBS_NUMBER_LIMIT=" & HT.int2str (mjnum));

      if developer_mode then
         TIO.Put_Line (makeconf, "DEVELOPER=1");
         TIO.Put_Line (makeconf, "PATCH_DEBUG=yes");
      end if;
      if DIR.Exists (HT.USS (PM.configuration.dir_ccache)) then
         TIO.Put_Line (makeconf, "BUILD_WITH_CCACHE=yes");
         TIO.Put_Line (makeconf, "CCACHE_DIR=/ccache");
      end if;
      override_defaults ("lua",      PM.configuration.def_lua);
      override_defaults ("mysql",    PM.configuration.def_mysql_group);
      override_defaults ("perl5",    PM.configuration.def_perl);
      override_defaults ("php",      PM.configuration.def_php);
      override_defaults ("pgsql",    PM.configuration.def_postgresql);
      override_defaults ("python3",  PM.configuration.def_python3);
      override_defaults ("ruby",     PM.configuration.def_ruby);
      override_defaults ("ssl",      PM.configuration.def_ssl);
      override_defaults ("tcl",      PM.configuration.def_tcl_tk);
      concatenate_makeconf (makeconf, profilemc);
      TIO.Close (makeconf);

   end create_make_conf;


   --------------------------------------------------------------------------------------------
   --  concatenate_makeconf
   --------------------------------------------------------------------------------------------
   procedure concatenate_makeconf (makeconf_handle : TIO.File_Type; target_name : String)
   is
      fragment : TIO.File_Type;
   begin
      if DIR.Exists (target_name) then
         TIO.Open (File => fragment, Mode => TIO.In_File, Name => target_name);
         while not TIO.End_Of_File (fragment) loop
            declare
               Line : String := TIO.Get_Line (fragment);
            begin
               TIO.Put_Line (makeconf_handle, Line);
            end;
         end loop;
         TIO.Close (fragment);
      end if;
   exception
      when others => null;
   end concatenate_makeconf;


   --------------------------------------------------------------------------------------------
   --  create_sun_files
   --------------------------------------------------------------------------------------------
   procedure create_sun_files (path_to_etc : String)
   is
      sun_file : TIO.File_Type;
      security : constant String := path_to_etc & "/security";
      skel     : constant String := path_to_etc & "/skel";
   begin
      if platform_type /= sunos then
         return;
      end if;
      --  version found in Solaris 10u8
      --  #
      --  # Copyright 2008 Sun Microsystems, Inc.  All rights reserved.
      --  # Use is subject to license terms.
      --  #
      --  #ident  "@(#)crypt.conf 1.2     08/05/14 SMI"
      --  #
      --  # The algorithm name __unix__ is reserved.
      --
      --  1       crypt_bsdmd5.so.1
      --  2a      crypt_bsdbf.so.1
      --  md5     crypt_sunmd5.so.1
      --  5       crypt_sha256.so.1
      --  6       crypt_sha512.so.1

      DIR.Create_Path (security);
      TIO.Create (sun_file, TIO.Out_File, security & "/crypt.conf");
      TIO.Put_Line (sun_file, "1   crypt_bsdmd5.so.1");
      TIO.Put_Line (sun_file, "2a  crypt_bsdbf.so.1");
      TIO.Put_Line (sun_file, "md5 crypt_sunmd5.so.1");
      TIO.Put_Line (sun_file, "5   crypt_sha256.so.1");
      TIO.Put_Line (sun_file, "6   crypt_sha512.so.1");
      TIO.Close (sun_file);

      --  Dummy /etc/user_attr to allow useradd to succeed
      TIO.Create (sun_file, TIO.Out_File, path_to_etc & "/user_attr");
      TIO.Put_Line (sun_file, "adm::::profiles=Log Management");
      TIO.Put_Line (sun_file, "lp::::profiles=Printer Management");
      TIO.Close (sun_file);

      --  Create skel files
      DIR.Create_Path (skel);
      TIO.Create (sun_file, TIO.Out_File, skel & "/local.cshrc");
      TIO.Put_Line (sun_file, "umask 022");
      TIO.Put_Line (sun_file, "set path=(/bin /usr/bin /usr/ucb /etc .)");
      TIO.Put_Line (sun_file, "if ( $?prompt ) then");
      TIO.Put_Line (sun_file, "   set history=32");
      TIO.Put_Line (sun_file, "endif");
      TIO.Close (sun_file);

      --  skel/local.login
      TIO.Create (sun_file, TIO.Out_File, skel & "/local.login");
      TIO.Put_Line (sun_file, "stty -istrip");
      TIO.Close (sun_file);

      --  skel/local.profile
      TIO.Create (sun_file, TIO.Out_File, skel & "/local.profile");
      TIO.Put_Line (sun_file, "stty istrip");
      TIO.Put_Line (sun_file, "PATH=/usr/bin:/usr/ucb:/etc:.");
      TIO.Put_Line (sun_file, "export PATH");
      TIO.Close (sun_file);

      --  etc/datemsk (extremely pared done)
      TIO.Create (sun_file, TIO.Out_File, path_to_etc & "/datemsk");
      TIO.Put_Line (sun_file, "%m/%d/%y %H:%M");
      TIO.Put_Line (sun_file, "%m%d%H%M%y");
      TIO.Close (sun_file);

      --  etc/shadow (couple of entries)
      TIO.Create (sun_file, TIO.Out_File, path_to_etc & "/shadow");
      TIO.Put_Line (sun_file, "root:kF/MO3YejnKKE:6445::::::");
      TIO.Put_Line (sun_file, "adm:NP:6445::::::");
      TIO.Put_Line (sun_file, "lp:NP:6445::::::");
      TIO.Put_Line (sun_file, "nobody:*LK*:6445::::::");
      TIO.Put_Line (sun_file, "nobody4:*LK*:6445::::::");
      TIO.Close (sun_file);

      --  etc/project
      TIO.Create (sun_file, TIO.Out_File, path_to_etc & "/project");
      TIO.Put_Line (sun_file, "system:0::::");
      TIO.Put_Line (sun_file, "user.root:1::::");
      TIO.Put_Line (sun_file, "noproject:2::::");
      TIO.Put_Line (sun_file, "default:3::::");
      TIO.Put_Line (sun_file, "group.staff:10::::");
      TIO.Close (sun_file);

   end create_sun_files;


   --------------------------------------------------------------------------------------------
   --  process keyword files
   --------------------------------------------------------------------------------------------
   procedure process_keyword_files (slave_mk : String; localbase : String)
   is
      procedure process_file (keyfile : String);

      Search  : DIR.Search_Type;
      Dir_Ent : DIR.Directory_Entry_Type;
      keydir  : constant String := slave_mk & "/Keywords";

      procedure process_file (keyfile : String)
      is
         contents : constant String := FOP.get_file_contents (keyfile);
         lpattern : constant String := "%LOCALBASE%";
         modified : Boolean := False;
         finaltxt : HT.Text := HT.SUS (contents);
      begin
         loop
            exit when not HT.contains (finaltxt, lpattern);
            modified := True;
            finaltxt := HT.replace_substring (finaltxt, lpattern, localbase);
         end loop;
         if modified then
            DIR.Delete_File (keyfile);
            FOP.dump_contents_to_file (HT.USS (finaltxt), keyfile);
         end if;
      end process_file;
   begin
      DIR.Start_Search (Search    => Search,
                        Directory => keydir,
                        Filter    => (DIR.Ordinary_File => True, others => False),
                        Pattern   => "*.ucl");
      while DIR.More_Entries (Search => Search) loop
         DIR.Get_Next_Entry (Search => Search, Directory_Entry => Dir_Ent);
         declare
            kfile : constant String := keydir & "/" & DIR.Simple_Name (Dir_Ent);
         begin
            process_file (kfile);
         end;
      end loop;
      DIR.End_Search (Search);
   end process_keyword_files;


end Replicant;
