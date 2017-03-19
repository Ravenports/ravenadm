--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Exceptions;
with Ada.Directories;
with Ada.Containers.Vectors;
with Ada.Characters.Latin_1;
with Parameters;
with Unix;

package body Replicant is

   package EX  renames Ada.Exceptions;
   package PM  renames Parameters;
   package CON renames Ada.Containers;
   package DIR renames Ada.Directories;
   package LAT renames Ada.Characters.Latin_1;

   --------------------------------------------------------------------------------------------
   --  initialize
   --------------------------------------------------------------------------------------------
   procedure initialize (testmode  : Boolean)
   is
      mm     : constant String := get_master_mount;
      sretc  : constant String := raven_sysroot & "/usr/share";
      maspas : constant String := "/master.passwd";
      passwd : constant String := "/passwd";
      spwd   : constant String := "/spwd.db";
      pwd    : constant String := "/pwd.db";
      rcconf : constant String := "/rc.conf";
      hints  : constant String := "/ld-elf.so.hints";
      nhints : constant String := "/ld.so.hints";
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
              openbsd   =>
            DIR.Copy_File (sretc & passwd, mm & passwd);
            DIR.Copy_File (sretc & maspas, mm & maspas);
         when linux     |
              sunos     => null;  -- passwd not used
      end case;
      case platform_type is
         when dragonfly |
              freebsd   |
              netbsd    |
              openbsd   =>
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
              openbsd   =>
            DIR.Copy_File (sretc & rcconf, mm & rcconf);
         when linux     |
              macos     |
              sunos     => null;  --  rc.conf not used
      end case;
      case platform_type is
         when dragonfly |
              freebsd   =>
            DIR.Copy_File (sretc & hints, mm & hints);
         when netbsd    |
              openbsd   =>
            DIR.Copy_File (sretc & nhints, mm & nhints);
         when linux     |
              macos     |
              sunos     => null;
      end case;
      create_mtree_exc_preinst (mm);
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
        & "/" & abnormal_cmd_logname;
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
   procedure create_mtree_exc_preinst (path_to_mm : String)
   is
      mtreefile : TIO.File_Type;
      filename  : constant String := path_to_mm & "/mtree.prestage.exclude";
   begin
      TIO.Create (File => mtreefile, Mode => TIO.Out_File, Name => filename);
      write_common_mtree_exclude_base (mtreefile);
      write_preinstall_section (mtreefile);
      TIO.Close (mtreefile);
   end create_mtree_exc_preinst;


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
   procedure write_common_mtree_exclude_base (mtreefile : TIO.File_Type) is
   begin
      TIO.Put_Line
        (mtreefile,
           "./bin" & LAT.LF
         & "./ccache" & LAT.LF
         & "./construction" & LAT.LF
         & "./dev" & LAT.LF
         & "./distfiles" & LAT.LF
         & "./libexec" & LAT.LF
         & "./home" & LAT.LF
         & "./packages" & LAT.LF
         & "./proc" & LAT.LF
         & "./root" & LAT.LF
         & "./tmp" & LAT.LF
         & "./usr/bin" & LAT.LF
         & "./usr/include" & LAT.LF
         & "./usr/lib" & LAT.LF
         & "./usr/lib32" & LAT.LF
         & "./usr/share" & LAT.LF
         & "./var/db/rvnfontconfig" & LAT.LF
         & "./var/run" & LAT.LF
         & "./var/tmp" & LAT.LF
         & "./xports"
        );
   end write_common_mtree_exclude_base;


   --------------------------------------------------------------------------------------------
   --  write_preinstall_section
   --------------------------------------------------------------------------------------------
   procedure write_preinstall_section (mtreefile : TIO.File_Type)
   is
      RB : String := LAT.Full_Stop & HT.USS (ravenbase);
   begin
      TIO.Put_Line
        (mtreefile,
           "./etc/group" & LAT.LF
         & "./etc/make.conf" & LAT.LF
         & "./etc/make.conf.bak" & LAT.LF
         & "./etc/make.nxb.conf" & LAT.LF
         & "./etc/master.passwd" & LAT.LF
         & "./etc/passwd" & LAT.LF
         & "./etc/pwd.db" & LAT.LF
         & "./etc/shells" & LAT.LF
         & "./etc/spwd.db" & LAT.LF
         & "./var/db" & LAT.LF
         & "./var/log" & LAT.LF
         & "./var/mail" & LAT.LF
         & "./var/spool" & LAT.LF
         & "./var/tmp" & LAT.LF
         & RB & "/etc/gconf/gconf.xml.defaults/%gconf-tree*.xml" & LAT.LF
         & RB & "/lib/gio/modules/giomodule.cache" & LAT.LF
         & RB & "/share/info/dir" & LAT.LF
         & RB & "/share/info" & LAT.LF
         & RB & "/share/*/info/dir" & LAT.LF
         & RB & "/share/*/info" & LAT.LF
         & RB & "/*/ls-R" & LAT.LF
         & RB & "/share/octave/octave_packages" & LAT.LF
         & RB & "/share/xml/catalog.ports"
        );
   end write_preinstall_section;


   --------------------------------------------------------------------------------------------
   --  df_command
   --------------------------------------------------------------------------------------------
   function df_command return String is
   begin
      case platform_type is
         when dragonfly |
              freebsd   |
              macos     |
              netbsd    |
              openbsd   => return "/bin/df -h -t null,tmpfs,devfs";
         when sunos     => return "/usr/sbin/df -h";
         when linux     => return "/usr/bin/df -h";
      end case;
   end df_command;


   --------------------------------------------------------------------------------------------
   --  unmount
   --------------------------------------------------------------------------------------------
   procedure unmount (device_or_node : String)
   is
      bsd_command : constant String := "/sbin/umount " & device_or_node;
      sol_command : constant String := "/usr/sbin/umount " & device_or_node;
      lin_command : constant String := "/usr/bin/umount " & device_or_node;
   begin
      --  failure to unmount causes stderr squawks which messes up curses display
      --  Just log it and ignore for now (Add robustness later)
      case platform_type is
         when dragonfly |
              freebsd   |
              macos     |
              netbsd    |
              openbsd   => execute (bsd_command);
         when linux     => execute (lin_command);
         when sunos     => execute (sol_command);
      end case;

   exception
      when others => null;  -- silently fail
   end unmount;


   --------------------------------------------------------------------------------------------
   --  mount_nullfs
   --------------------------------------------------------------------------------------------
   procedure mount_nullfs (target, mount_point : String; mode : mount_mode := readonly)
   is
      cmd_freebsd   : constant String := "/sbin/mount_nullfs";
      cmd_dragonfly : constant String := "/sbin/mount_null";
      cmd_solaris   : constant String := "/usr/sbin/mount -F lofs";
      cmd_linux     : constant String := "/usr/bin/mount --bind";
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
         when freebsd   => command := HT.SUS (cmd_freebsd);
         when dragonfly |
              netbsd    => command := HT.SUS (cmd_dragonfly);
         when sunos     => command := HT.SUS (cmd_solaris);
         when linux     => command := HT.SUS (cmd_linux);
         when openbsd | macos =>
            raise scenario_unexpected with
              "Null mounting not supported on " & platform_type'Img;
      end case;
      case mode is
         when readonly  => HT.SU.Append (command, " -o ro");
         when readwrite => null;
      end case;
      execute (HT.USS (command) & " " & target & " " & mount_point);
   end mount_nullfs;


   --------------------------------------------------------------------------------------------
   --  mount_tmpfs
   --------------------------------------------------------------------------------------------
   procedure mount_tmpfs (mount_point : String; max_size_M : Natural := 0)
   is
      cmd_freebsd   : constant String := "/sbin/mount -t tmpfs";
      cmd_dragonfly : constant String := "/sbin/mount_tmpfs";
      cmd_solaris   : constant String := "/sbin/mount -F tmpfs";
      command       : HT.Text;
   begin
      case platform_type is
         when freebsd   |
              netbsd    |
              openbsd   |
              linux     => command := HT.SUS (cmd_freebsd);
         when dragonfly => command := HT.SUS (cmd_dragonfly);
         when sunos     => command := HT.SUS (cmd_solaris);
         when macos     =>
            raise scenario_unexpected with
              "Null mounting not supported on " & platform_type'Img;
      end case;
      if max_size_M > 0 then
         HT.SU.Append (command, " -o size=" & HT.trim (max_size_M'Img) & "M");
      end if;
      case platform_type is
         when sunos     => HT.SU.Append (command, " swap " & mount_point);
         when freebsd   |
              dragonfly |
              netbsd    |
              openbsd   |
              linux     => HT.SU.Append (command, " tmpfs " & mount_point);
         when macos     => null;
      end case;
      execute (HT.USS (command));
   end mount_tmpfs;


   --------------------------------------------------------------------------------------------
   --  mount_devices
   --------------------------------------------------------------------------------------------
   procedure mount_devices (path_to_dev : String)
   is
      bsd_command : constant String := "/sbin/mount -t devfs devfs " & path_to_dev;
      lin_command : constant String := "/usr/bin/mount --bind /dev " & path_to_dev;
   begin
      case platform_type is
         when dragonfly |
              freebsd   => execute (bsd_command);
         when linux     => execute (lin_command);
         when netbsd    |
              openbsd   |
              macos     |
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
      lin_command : constant String := "/usr/bin/mount --bind /proc " & path_to_proc;
   begin
      case platform_type is
         when dragonfly |
              freebsd   => execute (bsd_command);
         when netbsd    |
              openbsd   => execute (net_command);
         when linux     => execute (lin_command);
         when sunos     => mount_nullfs (target => "/proc", mount_point => path_to_proc);
         when macos =>
            raise scenario_unexpected with
              "Null mounting not supported on " & platform_type'Img;
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
         when tmp         => return mount_base & root_tmp;
         when var         => return mount_base & root_var;
         when home        => return mount_base & root_home;
         when proc        => return mount_base & root_proc;
         when root        => return mount_base & root_root;
         when xports      => return mount_base & root_xports;
         when port        => return mount_base & root_port;
         when libexec     => return mount_base & root_libexec;
         when packages    => return mount_base & root_packages;
         when distfiles   => return mount_base & root_distfiles;
         when wrkdirs     => return mount_base & root_wrkdirs;
         when ccache      => return mount_base & root_ccache;
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
         when packages  => return HT.USS (PM.configuration.dir_packages);
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
      cmd_freebsd   : constant String := "/bin/chflags";
      cmd_dragonfly : constant String := "/usr/bin/chflags";
      cmd_linux     : constant String := "/usr/bin/chattr";
      cmd_solaris   : constant String := "/usr/bin/chmod";
      flag_lock     : constant String := " schg ";
      flag_unlock   : constant String := " noschg ";
      chattr_lock   : constant String := " +i ";
      chattr_unlock : constant String := " -i ";
      sol_lock      : constant String := " S+ci ";
      sol_unlock    : constant String := " S-ci ";
      command       : HT.Text;
   begin
      if not DIR.Exists (path) then
         --  e.g. <slave>/var/empty does not exist on NetBSD
         return;
      end if;
      case platform_type is
         when freebsd   => command := HT.SUS (cmd_freebsd);
         when dragonfly |
              netbsd    |
              openbsd   |
              macos     => command := HT.SUS (cmd_dragonfly);
         when linux     => command := HT.SUS (cmd_linux);
         when sunos     => command := HT.SUS (cmd_solaris);
      end case;
      case platform_type is
         when freebsd | dragonfly | netbsd | openbsd | macos =>
            case operation is
               when lock   => HT.SU.Append (command, flag_lock & path);
               when unlock => HT.SU.Append (command, flag_unlock & path);
            end case;
         when linux =>
            case operation is
               when lock   => HT.SU.Append (command, chattr_lock & path);
               when unlock => HT.SU.Append (command, chattr_unlock & path);
            end case;
         when sunos =>
            case operation is
               when lock   => HT.SU.Append (command, sol_lock & path);
               when unlock => HT.SU.Append (command, sol_unlock & path);
            end case;
      end case;
      execute (HT.USS (command));
   end folder_access;


   --------------------------------------------------------------------------------------------
   --  launch_slave
   --------------------------------------------------------------------------------------------
   procedure launch_slave  (id : builders; need_procfs : Boolean := False)
    is
      function clean_mount_point (point : folder) return String;

      slave_base  : constant String := get_slave_mount (id);
      slave_local : constant String := slave_base & "_localbase";
      dir_system  : constant String := HT.USS (PM.configuration.dir_sysroot);
      lbase       : constant String := HT.USS (PM.configuration.dir_localbase);
      etc_path    : constant String := location (slave_base, etc);

      function clean_mount_point (point : folder) return String is
      begin
         return location (dir_system, point);
      end clean_mount_point;

   begin
      forge_directory (slave_base);

      if PM.configuration.avoid_tmpfs then
         if lbase = bsd_localbase then
            forge_directory (location (slave_local, toolchain));
            mount_nullfs (slave_local, slave_base & lbase, readwrite);
         else
            forge_directory (location (slave_base, toolchain));
         end if;
      else
         --  Limit slave to 16Gb, covers localbase + construction mainly
         mount_tmpfs (slave_base, 16 * 1024);
         if lbase = bsd_localbase then
            mount_tmpfs (slave_base & bsd_localbase, 12 * 1024);
         end if;
         forge_directory (location (slave_base, toolchain));
      end if;

      for mnt in safefolders'Range loop
         forge_directory (location (slave_base, mnt));
      end loop;

      for mnt in subfolder'Range loop
         mount_nullfs (target      => clean_mount_point (mnt),
                       mount_point => location (slave_base, mnt));
      end loop;

      folder_access (location (slave_base, home), lock);
      folder_access (location (slave_base, root), lock);

      mount_nullfs (mount_target (xports),    location (slave_base, xports));
      mount_nullfs (mount_target (toolchain), location (slave_base, toolchain));
      mount_nullfs (mount_target (packages),  location (slave_base, packages),  mode => readwrite);
      mount_nullfs (mount_target (distfiles), location (slave_base, distfiles), mode => readwrite);

      if need_procfs then
         mount_procfs (path_to_proc => location (slave_base, proc));
      end if;

      if DIR.Exists (mount_target (ccache)) then
         mount_nullfs (mount_target (ccache), location (slave_base, ccache),
                       mode => readwrite);
      end if;

      mount_devices (location (slave_base, dev));

      populate_var_folder      (location (slave_base, var));
      copy_rc_default          (etc_path);
      copy_resolv_conf         (etc_path);
      copy_ldconfig_hints      (slave_base & "/var/run");
      create_make_conf         (etc_path);
      install_passwd_and_group (etc_path);
      create_etc_services      (etc_path);
      create_etc_shells        (etc_path);

   exception
      when hiccup : others =>
         EX.Reraise_Occurrence (hiccup);
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
      counter     : Natural := 0;
   begin

       unmount_devices (location (slave_base, dev));

      if DIR.Exists (mount_target (ccache)) then
         unmount (location (slave_base, ccache));
      end if;

      if need_procfs then
         unmount_procfs (location (slave_base, proc));
      end if;

      unmount (location (slave_base, distfiles));
      unmount (location (slave_base, packages));
      unmount (location (slave_base, xports));

      if DIR.Exists (location (slave_base, toolchain) & "/bin") then
         unmount (location (slave_base, toolchain));
      end if;

      folder_access (location (slave_base, root), unlock);
      folder_access (location (slave_base, home), unlock);
      folder_access (location (slave_base, var) & "/empty", unlock);

      for mnt in subfolder'Range loop
         unmount (location (slave_base, mnt));
      end loop;

      if PM.configuration.avoid_tmpfs then
         unmount (slave_base & lbase);
         annihilate_directory_tree (slave_local);
      else
         if lbase = bsd_localbase then
            unmount (slave_base & lbase);
         end if;
         unmount (slave_base);
      end if;
      annihilate_directory_tree (slave_base);

   exception
      when hiccup : others =>
         EX.Reraise_Occurrence (hiccup);
   end destroy_slave;


   --------------------------------------------------------------------------------------------
   --  unhook_toolchain
   --------------------------------------------------------------------------------------------
   procedure unhook_toolchain (id : builders)
   is
      slave_base : constant String := get_slave_mount (id);
   begin
      unmount (location (slave_base, toolchain));
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
      forge_directory (path & "/run");
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
         when dragonfly | freebsd =>
            DIR.Copy_File (mm & hints, path_to_varrun & hints);
         when netbsd | openbsd =>
            DIR.Copy_File (mm & nhints, path_to_varrun & nhints);
         when macos | linux | sunos => null;
      end case;
   end copy_ldconfig_hints;


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
      original : constant String := "/etc/resolv.conf";
   begin
      if DIR.Exists (original) then
         DIR.Copy_File (Source_Name => original,
                        Target_Name => path_to_etc & "/resolv.conf");
      end if;
   end copy_resolv_conf;


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
   end install_passwd_and_group;


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
            TIO.Put_Line (shells, "/bin/sh" & LAT.LF);
      TIO.Close (shells);
   end create_etc_shells;


   --------------------------------------------------------------------------------------------
   --  create_make_conf
   --------------------------------------------------------------------------------------------
   procedure create_make_conf (path_to_etc : String)
   is
      makeconf  : TIO.File_Type;
      profilemc : constant String := PM.raven_confdir & "/" &
                  HT.USS (PM.configuration.profile) & "-make.conf";
      profile   : constant String := HT.USS (PM.configuration.profile);
      mjnum     : constant Integer := Integer (PM.configuration.jobs_limit);
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
           "NUMBER_CPUS=" & HT.int2str (Integer (smp_cores)) & LAT.LF &
           "MAKE_JOBS_NUMBER_LIMIT=" & HT.int2str (mjnum));

      if developer_mode then
         TIO.Put_Line (makeconf, "DEVELOPER=1");
      end if;
      if DIR.Exists (HT.USS (PM.configuration.dir_ccache)) then
         TIO.Put_Line (makeconf, "BUILD_WITH_CCACHE=yes");
         TIO.Put_Line (makeconf, "CCACHE_DIR=/ccache");
      end if;
      concatenate_makeconf (makeconf, profilemc);
      TIO.Close (makeconf);

   end create_make_conf;


   --------------------------------------------------------------------------------------------
   --  create_make_conf
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

end Replicant;
