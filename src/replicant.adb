--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Directories;
with Ada.Containers.Vectors;
with Ada.Characters.Latin_1;
with Parameters;
with Unix;

package body Replicant is

   package PM  renames Parameters;
   package CON renames Ada.Containers;
   package DIR renames Ada.Directories;
   package LAT renames Ada.Characters.Latin_1;

   --------------------------------------------------------------------------------------------
   --  initialize
   --------------------------------------------------------------------------------------------
   procedure initialize
     (testmode  : Boolean;
      num_cores : cpu_range;
      localbase : String)
   is
      mm     : constant String := get_master_mount;
      sretc  : constant String := raven_sysroot & "/usr/share/etc";
      maspas : constant String := "/master.passwd";
      passwd : constant String := "/passwd";
      spwd   : constant String := "/spwd.db";
      pwd    : constant String := "/pwd.db";
   begin
      smp_cores      := num_cores;
      developer_mode := testmode;
      ravenbase      := HT.SUS (localbase);

      start_abnormal_logging;

      if DIR.Exists (mm) then
         annihilate_directory_tree (mm);
      end if;

      DIR.Create_Path (mm & "/etc");
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
   begin
      silent_exec (command);
   exception
      when others => null;
   end annihilate_directory_tree;


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
      DIR.Start_Search (Search    => Search,
                        Directory => buildbase,
                        Filter    => (DIR.Directory => True, others => False),
                        Pattern   => "SL*_*");

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
   begin
      DIR.Start_Search (Search    => Search,
                       Directory => buildbase,
                       Filter    => (DIR.Directory => True, others => False),
                       Pattern   => "SL*_*");
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
              openbsd   => return "/bin/df -h";
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

end Replicant;
