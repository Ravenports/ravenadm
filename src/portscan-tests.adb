--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with File_Operations;
with PortScan.Log;
with Parameters;
with Unix;
with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Exceptions;

package body PortScan.Tests is

   package FOP renames File_Operations;
   package LOG renames PortScan.Log;
   package PM  renames Parameters;
   package LAT renames Ada.Characters.Latin_1;
   package DIR renames Ada.Directories;
   package EX  renames Ada.Exceptions;

   --------------------------------------------------------------------------------------------
   --  exec_phase_check_plist
   --------------------------------------------------------------------------------------------
   function exec_check_plist
     (specification : PSP.Portspecs;
      log_handle    : TIO.File_Type;
      phase_name    : String;
      seq_id        : port_id;
      port_prefix   : String;
      rootdir       : String) return Boolean
   is
      passed_check : Boolean := True;
      namebase     : constant String := specification.get_namebase;
      directory_list : entry_crate.Map;
      dossier_list   : entry_crate.Map;
   begin
      LOG.log_phase_begin (log_handle, phase_name);
      TIO.Put_Line (log_handle, "====> Checking for package manifest issues");

      if not ingest_manifests (specification  => specification,
                               log_handle     => log_handle,
                               directory_list => directory_list,
                               dossier_list   => dossier_list,
                               seq_id         => seq_id,
                               namebase       => namebase,
                               port_prefix    => port_prefix,
                               rootdir        => rootdir)
      then
         passed_check := False;
      end if;

      if orphaned_directories_detected (log_handle     => log_handle,
                                        directory_list => directory_list,
                                        namebase       => namebase,
                                        port_prefix    => port_prefix,
                                        rootdir        => rootdir)
      then
         passed_check := False;
      end if;

      if missing_directories_detected (log_handle, directory_list) then
         passed_check := False;
      end if;

      if orphaned_files_detected (log_handle   => log_handle,
                                  dossier_list => dossier_list,
                                  namebase     => namebase,
                                  port_prefix  => port_prefix,
                                  rootdir      => rootdir)
      then
         passed_check := False;
      end if;

      if missing_files_detected (log_handle, dossier_list) then
         passed_check := False;
      end if;

      if passed_check then
         TIO.Put_Line (log_handle, "====> No manifest issues found");
      end if;

      LOG.log_phase_end (log_handle);
      return passed_check;
   end exec_check_plist;


   --------------------------------------------------------------------------------------------
   --  ingest_manifests
   --------------------------------------------------------------------------------------------
   function ingest_manifests
     (specification  : PSP.Portspecs;
      log_handle     : TIO.File_Type;
      directory_list : in out entry_crate.Map;
      dossier_list   : in out entry_crate.Map;
      seq_id         : port_id;
      namebase       : String;
      port_prefix    : String;
      rootdir        : String) return Boolean
   is
      procedure eat_plist (position : subpackage_crate.Cursor);
      procedure insert_directory (directory : String; subpackage : HT.Text);

      result   : Boolean := True;

      procedure insert_directory (directory : String; subpackage : HT.Text)
      is
         numsep : Natural := HT.count_char (directory, LAT.Solidus);
         canvas : HT.Text := HT.SUS (directory);
      begin
         for x in 1 .. numsep + 1 loop
            declare
               paint      : String := HT.USS (canvas);
               my_new_rec : entry_record := (subpackage, False);
            begin
               if paint /= "" then
                  if not directory_list.Contains (canvas) then
                     directory_list.Insert (canvas, my_new_rec);
                  end if;
                  canvas := HT.SUS (HT.head (paint, "/"));
               end if;
            end;
         end loop;
      end insert_directory;

      procedure eat_plist (position : subpackage_crate.Cursor)
      is
         subpackage    : HT.Text := subpackage_crate.Element (position).subpackage;
         manifest_file : String := "/construction/" & namebase & "/.manifest." &
                         HT.USS (subpackage) & ".mktmp";
         contents      : String := FOP.get_file_contents (rootdir & manifest_file);
         identifier    : constant String := HT.USS (subpackage) & " manifest: ";
         markers       : HT.Line_Markers;
      begin
         HT.initialize_markers (contents, markers);
         loop
            exit when not HT.next_line_present (contents, markers);
            declare
               line      : constant String := HT.extract_line (contents, markers);
               line_text : HT.Text := HT.SUS (line);
               new_rec   : entry_record := (subpackage, False);
            begin
               if HT.leads (line, "@comment ") or else
                 HT.leads (line, "@terminfo") or else
                 HT.leads (line, "@rmtry ") or else
                 HT.leads (line, "@postexec ")
               then
                  null;
               elsif HT.leads (line, "@dir ") then
                  declare
                     dir : String :=
                           convert_to_absolute_path (port_prefix, HT.substring (line, 5, 0));
                     dir_text  : HT.Text := HT.SUS (dir);
                     excludeit : Boolean;
                  begin
                     if directory_list.Contains (dir_text) then
                        --  There is one case where a redundant @dir symbol is desired:
                        --  *) when a non-standard PREFIX is used.  Pkg(8) needs to be given an
                        --     explicit command to remove the package's root directory.
                        excludeit := (LAT.Solidus & dir = port_prefix) and then
                          (port_prefix /= HT.USS (PM.configuration.dir_localbase));

                        if not excludeit then
                           result := False;
                           declare
                              spkg : String :=
                                HT.USS (directory_list.Element (dir_text).subpackage);
                           begin
                              if spkg /= "" then
                                 TIO.Put_Line
                                   (log_handle,
                                    "Redundant @dir symbol, " & identifier & dir &
                                      " will already be created by the " & spkg & " manifest");
                              else
                                 TIO.Put_Line
                                   (log_handle,
                                    "Redundant @dir symbol, " & identifier & dir &
                                      " will already be created by another manifest");
                              end if;
                           end;
                        end if;
                     else
                        insert_directory (dir, subpackage);
                     end if;
                  end;
               else
                  declare
                     modline : String  := modify_file_if_necessary (port_prefix, line);
                     ml_text : HT.Text := HT.SUS (modline);
                  begin
                     if dossier_list.Contains (ml_text) then
                        result := False;
                        declare
                           spkg : String := HT.USS (dossier_list.Element (ml_text).subpackage);
                        begin
                           TIO.Put_Line
                             (log_handle,
                              "Duplicate file entry, " & identifier & modline &
                                " already present in " & spkg & " manifest");
                        end;
                     else
                        dossier_list.Insert (ml_text, new_rec);
                        declare
                           plistdir : String := DIR.Containing_Directory (modline);
                        begin
                           insert_directory (plistdir, subpackage);
                        end;
                     end if;
                  end;
               end if;
            end;
         end loop;
      exception
         when issue : others =>
            TIO.Put_Line (log_handle, "check-plist error: " & EX.Exception_Message (issue));
      end eat_plist;
   begin
      all_ports (seq_id).subpackages.Iterate (eat_plist'Access);
      return result;
   end ingest_manifests;


   --------------------------------------------------------------------------------------------
   --  directory_excluded
   --------------------------------------------------------------------------------------------
   function directory_excluded (port_prefix, candidate : String) return Boolean
   is
      --  mandatory candidate has ${STAGEDIR}/ stripped (no leading slash)
      localbase : constant String  := HT.substring (port_prefix, 1, 0);
      lblen     : constant Natural := localbase'Length;
   begin
      if candidate = localbase then
         return True;
      end if;

      declare
         shortcan : String := HT.substring (candidate, lblen + 1, 0);
      begin
         if shortcan = "bin" or else
           shortcan = "etc" or else
           shortcan = "etc/rc.d" or else
           shortcan = "include" or else
           shortcan = "lib" or else
           shortcan = "lib/pkgconfig" or else
           shortcan = "libdata" or else
           shortcan = "libexec" or else
           shortcan = "sbin" or else
           shortcan = "share" or else
           shortcan = "www"
         then
            return True;
         end if;
         if not HT.leads (shortcan, "share/") then
            return False;
         end if;
      end;

      declare
         shortcan : String := HT.substring (candidate, lblen + 7, 0);
      begin
         if shortcan = "doc" or else
           shortcan = "examples" or else
           shortcan = "info" or else
           shortcan = "locale" or else
           shortcan = "man" or else
           shortcan = "nls"
         then
            return True;
         end if;
         if shortcan'Length /= 8 or else
           not HT.leads (shortcan, "man/man")
         then
            return False;
         end if;
         case shortcan (shortcan'Last) is
            when '1' .. '9' | 'l' | 'n' => return True;
            when others => return False;
         end case;
      end;
   end directory_excluded;


   --------------------------------------------------------------------------------------------
   --  file_excluded
   --------------------------------------------------------------------------------------------
   function file_excluded (port_prefix, candidate : String) return Boolean is
   begin
      if HT.trails (candidate, "info/dir") then
         return True;
      end if;

      return False;
   end file_excluded;


   --------------------------------------------------------------------------------------------
   --  orphaned_directories_detected
   --------------------------------------------------------------------------------------------
   function orphaned_directories_detected
     (log_handle     : TIO.File_Type;
      directory_list : in out entry_crate.Map;
      namebase       : String;
      port_prefix    : String;
      rootdir        : String) return Boolean
   is
      localbase : constant String  := HT.substring (port_prefix, 1, 0);
      stagedir  : String := rootdir & "/construction/" & namebase & "/stage";
      command   : String := rootdir & "/usr/bin/find " & stagedir & " -type d -printf " &
                  LAT.Quotation & "%P\n" & LAT.Quotation;
      status    : Integer;
      comres    : String := HT.USS (Unix.piped_command (command, status));
      markers   : HT.Line_Markers;
      lblen     : constant Natural := localbase'Length;
      result    : Boolean := False;
      errprefix : constant String := "Orphaned directory detected: ";
   begin
      if status /= 0 then
         TIO.Put_Line ("orphaned_directories_detected: command error: " & comres);
         return True;
      end if;
      HT.initialize_markers (comres, markers);
      loop
         exit when not HT.next_line_present (comres, markers);
         declare
            line      : constant String := HT.extract_line (comres, markers);
            plist_dir : HT.Text := HT.SUS (line);
         begin
            if line /= "" then
               if directory_list.Contains (plist_dir) then
                  directory_list.Update_Element (Position => directory_list.Find (plist_dir),
                                                 Process  => mark_verified'Access);
               else
                  if not directory_excluded (port_prefix, line) then
                     if HT.leads (line, localbase) then
                        TIO.Put_Line (log_handle, errprefix & HT.substring (line, lblen + 1, 0));
                     else
                        TIO.Put_Line (log_handle, errprefix & line);
                     end if;
                     result := True;
                  end if;
               end if;
            end if;
         end;
      end loop;
      return result;
   end orphaned_directories_detected;


   --------------------------------------------------------------------------------------------
   --  mark_verified
   --------------------------------------------------------------------------------------------
   procedure mark_verified (key : HT.Text; Element : in out entry_record) is
   begin
      Element.verified := True;
   end mark_verified;


   --------------------------------------------------------------------------------------------
   --  missing_directories_detected
   --------------------------------------------------------------------------------------------
   function missing_directories_detected
     (log_handle     : TIO.File_Type;
      directory_list : in out entry_crate.Map) return Boolean
   is
      procedure check (position : entry_crate.Cursor);

      result : Boolean := False;

      procedure check (position : entry_crate.Cursor)
      is
         rec       : entry_record renames entry_crate.Element (position);
         plist_dir : String := HT.USS (entry_crate.Key (position));
      begin
         if not rec.verified then
            TIO.Put_Line
              (log_handle,
               "Directory " & plist_dir & " listed on " & HT.USS (rec.subpackage) &
                 " manifest is not present in the stage directory.");
            result := True;
         end if;
      end check;
   begin
      directory_list.Iterate (check'Access);
      return result;
   end missing_directories_detected;


   --------------------------------------------------------------------------------------------
   --  missing_files_detected
   --------------------------------------------------------------------------------------------
   function missing_files_detected
     (log_handle   : TIO.File_Type;
      dossier_list : in out entry_crate.Map) return Boolean
   is
      procedure check (position : entry_crate.Cursor);

      result : Boolean := False;

      procedure check (position : entry_crate.Cursor)
      is
         rec        : entry_record renames entry_crate.Element (position);
         plist_file : String := HT.USS (entry_crate.Key (position));
      begin
         if not rec.verified then
            TIO.Put_Line
              (log_handle,
               "File " & plist_file & " listed on " & HT.USS (rec.subpackage) &
                 " manifest is not present in the stage directory.");
            result := True;
         end if;
      end check;
   begin
      dossier_list.Iterate (check'Access);
      return result;
   end missing_files_detected;


   --------------------------------------------------------------------------------------------
   --  orphaned_files_detected
   --------------------------------------------------------------------------------------------
   function orphaned_files_detected
     (log_handle     : TIO.File_Type;
      dossier_list   : in out entry_crate.Map;
      namebase       : String;
      port_prefix    : String;
      rootdir        : String) return Boolean
   is
      localbase : constant String  := HT.substring (port_prefix, 1, 0);
      stagedir  : String := rootdir & "/construction/" & namebase & "/stage";
      command   : String := rootdir & "/usr/bin/find " & stagedir &
                  " \( -type f -o -type l \) -printf " &
                  LAT.Quotation & "%P\n" & LAT.Quotation;
      status    : Integer;
      comres    : String := HT.USS (Unix.piped_command (command, status));
      markers   : HT.Line_Markers;
      lblen     : constant Natural := localbase'Length;
      result    : Boolean := False;
      errprefix : constant String := "Orphaned file detected: ";
   begin
      if status /= 0 then
         TIO.Put_Line ("orphaned_files_detected: command error: " & comres);
         return True;
      end if;
      HT.initialize_markers (comres, markers);
      loop
         exit when not HT.next_line_present (comres, markers);
         declare
            line       : constant String := HT.extract_line (comres, markers);
            plist_file : HT.Text := HT.SUS (line);
         begin
            if not HT.IsBlank (plist_file) then
               if dossier_list.Contains (plist_file) then
                  dossier_list.Update_Element (Position => dossier_list.Find (plist_file),
                                               Process  => mark_verified'Access);
               else
                  if not file_excluded (port_prefix, line) then
                     if HT.leads (line, localbase) then
                        TIO.Put_Line (log_handle, errprefix & HT.substring (line, lblen + 1, 0));
                     else
                        TIO.Put_Line (log_handle, errprefix & line);
                     end if;
                     result := True;
                  end if;
               end if;
            end if;
         end;
      end loop;
      return result;
   end orphaned_files_detected;


   --------------------------------------------------------------------------------------------
   --  modify_file_if_necessary
   --------------------------------------------------------------------------------------------
   function modify_file_if_necessary (port_prefix, original : String) return String is
   begin
      if HT.leads (original, "@info ") then
         return convert_to_absolute_path (port_prefix, HT.substring (original, 6, 0));
      elsif HT.leads (original, "@sample ") then
         return convert_to_absolute_path (port_prefix, HT.substring (original, 8, 0));
      else
         return convert_to_absolute_path (port_prefix, original);
      end if;
   end modify_file_if_necessary;


   --------------------------------------------------------------------------------------------
   --  convert_to_absolute_path
   --------------------------------------------------------------------------------------------
   function convert_to_absolute_path (port_prefix, raw : String) return String is
   begin
      if raw'Length < 2 then
         return raw;
      end if;
      if raw (raw'First) = LAT.Solidus then
         return HT.substring (raw, 1, 0);
      end if;
      return HT.substring (port_prefix, 1, 0) & LAT.Solidus & raw;
   end convert_to_absolute_path;

end PortScan.Tests;
