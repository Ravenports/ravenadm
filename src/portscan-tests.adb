--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with File_Operations;
with PortScan.Log;
with Ada.Exceptions;

package body PortScan.Tests is

   package FOP renames File_Operations;
   package LOG renames PortScan.Log;
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
   begin
      directory_list.Clear;
      dossier_list.Clear;

      LOG.log_phase_begin (log_handle, phase_name);
      TIO.Put_Line (log_handle, "====> Checking for package manifest issues");

      if not ingest_manifests (specification, log_handle, seq_id, rootdir) then
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
      rootdir       : String) return Boolean
   is
      procedure eat_plist (position : subpackage_crate.Cursor);

      namebase : constant String := specification.get_namebase;
      result   : Boolean := True;

      procedure eat_plist (position : subpackage_crate.Cursor)
      is
         subpackage    : HT.Text := subpackage_crate.Element (position).subpackage;
         manifest_file : String := "/construction/" & namebase & "/.manifest." &
                         HT.USS (subpackage) & ".mktmp";
         contents      : String := FOP.get_file_contents (rootdir & manifest_file);
         identifier    : constant String := HT.USS (subpackage) & " manifest: ";

         markers : HT.Line_Markers;
         linenum : Natural := 0;
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

end PortScan.Tests;
