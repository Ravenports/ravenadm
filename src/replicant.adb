--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Directories;
with Ada.Characters.Latin_1;
with Parameters;
with Unix;

package body Replicant is

   package PM  renames Parameters;
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
      comres    : constant String := HT.USS (internal_system_command ("/bin/df -h"));
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
end Replicant;
