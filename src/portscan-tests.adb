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
      rootdir       : String) return Boolean
   is
      passed_check : Boolean := True;
      namebase     : constant String := specification.get_namebase;
   begin
      directory_list.Clear;
      dossier_list.Clear;

      LOG.log_phase_begin (log_handle, phase_name);
      TIO.Put_Line (log_handle, "====> Checking for package manifest issues");

      if not ingest_manifests (specification, log_handle, seq_id, namebase, rootdir) then
         passed_check := False;
      end if;

      if orphaned_directories_detected (log_handle, namebase, rootdir) then
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
     (specification : PSP.Portspecs;
      log_handle    : TIO.File_Type;
      seq_id        : port_id;
      namebase      : String;
      rootdir       : String) return Boolean
   is
      procedure eat_plist (position : subpackage_crate.Cursor);

      result   : Boolean := True;

      procedure eat_plist (position : subpackage_crate.Cursor)
      is
         subpackage    : HT.Text := subpackage_crate.Element (position).subpackage;
         manifest_file : String := "/construction/" & namebase & "/.manifest." &
                         HT.USS (subpackage) & ".mktmp";
         contents      : String := FOP.get_file_contents (rootdir & manifest_file);
         identifier    : constant String := HT.USS (subpackage) & " manifest: ";

         markers : HT.Line_Markers;
      begin
         HT.initialize_markers (contents, markers);
         loop
            exit when not HT.next_line_present (contents, markers);
            declare
               line      : constant String := HT.extract_line (contents, markers);
               line_text : HT.Text := HT.SUS (line);
               new_rec   : entry_record := (subpackage, False);
            begin
               if HT.leads (line, "@dir ") then
                  declare
                     dir      : String := line (line'First + 5 .. line'Last);
                     dir_text : HT.Text := HT.SUS (dir);
                  begin
                     if directory_list.Contains (dir_text) then
                        result := False;
                        declare
                           spkg : String := HT.USS (dossier_list.Element (line_text).subpackage);
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
                     else
                        directory_list.Insert (dir_text, new_rec);
                     end if;
                  end;
               else
                  if dossier_list.Contains (line_text) then
                     result := False;
                     declare
                        spkg : String := HT.USS (dossier_list.Element (line_text).subpackage);
                     begin
                        TIO.Put_Line
                          (log_handle,
                           "Duplicate file entry, " & identifier & line & " already present in "
                           & spkg & " manifest");
                     end;
                  else
                     dossier_list.Insert (line_text, new_rec);
                     declare
                        plistdir : String := DIR.Containing_Directory (line);
                        dir_text : HT.Text := HT.SUS (plistdir);
                     begin
                        if not directory_list.Contains (dir_text) then
                           directory_list.Insert (dir_text, new_rec);
                        end if;
                     end;
                  end if;
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
   function directory_excluded (candidate : String) return Boolean
   is
      --  mandatory candidate has ${STAGEDIR}/ stripped (no leading slash)
      rawlbase  : constant String  := HT.USS (PM.configuration.dir_localbase);
      localbase : constant String  := rawlbase (rawlbase'First + 1 .. rawlbase'Last);
      lblen     : constant Natural := localbase'Length;
   begin
      if candidate = localbase then
         return True;
      end if;
      if not HT.leads (candidate, localbase & "/") then
         --  This should never happen
         return False;
      end if;

      declare
         shortcan : String := candidate (candidate'First + lblen + 1 .. candidate'Last);
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
         shortcan : String := candidate (candidate'First + lblen + 7 .. candidate'Last);
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
   --  orphaned_directories_detected
   --------------------------------------------------------------------------------------------
   function orphaned_directories_detected
     (log_handle    : TIO.File_Type;
      namebase      : String;
      rootdir       : String) return Boolean
   is
      rawlbase  : constant String  := HT.USS (PM.configuration.dir_localbase);
      localbase : constant String  := rawlbase (rawlbase'First + 1 .. rawlbase'Last);
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
            line : constant String := HT.extract_line (comres, markers);
         begin
            if line /= "" then
               if HT.leads (line, localbase) then
                  declare
                     plist_dir : HT.Text := HT.SUS (line (line'First + lblen + 1 .. line'Last));
                  begin
                     if directory_list.Contains (plist_dir) then
                        directory_list.Update_Element (Position => directory_list.Find (plist_dir),
                                                       Process  => mark_verified'Access);
                     else
                        if not directory_excluded (line) then
                           TIO.Put_Line (log_handle, errprefix & line);
                           result := True;
                        end if;
                     end if;
                  end;
               else
                  if not directory_list.Contains (HT.SUS (line)) then
                     TIO.Put_Line (log_handle, errprefix & line);
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

end PortScan.Tests;
