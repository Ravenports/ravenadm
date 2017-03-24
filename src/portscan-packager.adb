--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with File_Operations;
with PortScan.Log;
with Parameters;
with Unix;
with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Text_IO;

package body PortScan.Packager is

   package FOP renames File_Operations;
   package LOG renames PortScan.Log;
   package LAT renames Ada.Characters.Latin_1;
   package DIR renames Ada.Directories;
   package TIO renames Ada.Text_IO;
   package PM  renames Parameters;

   --------------------------------------------------------------------------------------------
   --  exec_phase_package
   --------------------------------------------------------------------------------------------
   function exec_phase_package
     (specification : PSP.Portspecs;
      log_handle    : in out TIO.File_Type;
      log_name      : String;
      phase_name    : String;
      seq_id        : port_id;
      rootdir       : String) return Boolean
   is
      procedure metadata   (position : subpackage_crate.Cursor);
      procedure package_it (position : subpackage_crate.Cursor);

      namebase   : String  := HT.USS (all_ports (seq_id).port_namebase);
      conbase    : String  := "/construction/" & namebase;
      spkgdir    : String  := rootdir & "/construction/metadata/";
      wrkdir     : String  := rootdir & conbase;
      chspkgdir  : String  := "/construction/metadata/";
      newpkgdir  : String  := "/construction/new_packages";
      stagedir   : String  := conbase & "/stage";
      display    : String  := "/+DISPLAY";
      pkgvers    : String  := HT.USS (all_ports (seq_id).pkgversion);
      still_good : Boolean := True;

      procedure metadata (position : subpackage_crate.Cursor)
      is
         type three is range 1 .. 3;
         function convert_prepost (prepost : three) return String;
         function convert_stage   (stage   : three) return String;
         subpackage    : constant String :=
                         HT.USS (subpackage_crate.Element (position).subpackage);
         message_file  : constant String := wrkdir & "/PKG_DISPLAY." & subpackage;
         descript_file : constant String := wrkdir & "/PKG_DESC." & subpackage;
         descript : String := "/+DESC";
         manifest : String := "/+MANIFEST";

         function convert_prepost (prepost : three) return String is
         begin
            case prepost is
               when 1 => return "pre-";
               when 2 => return "";
               when 3 => return "post-";
            end case;
         end convert_prepost;

         function convert_stage (stage   : three) return String is
         begin
            case stage is
               when 1 => return "install";
               when 2 => return "upgrade";
               when 3 => return "desinstall";
            end case;
         end convert_stage;
      begin
         DIR.Create_Path (spkgdir & subpackage);
         if DIR.Exists (message_file) then
            DIR.Copy_File (Source_Name => message_file,
                           Target_Name => spkgdir & subpackage & display);
         end if;
         if DIR.Exists (descript_file) then
            DIR.Copy_File (Source_Name => descript_file,
                           Target_Name => spkgdir & subpackage & descript);
         end if;

         for action in three'Range loop
            for psp in three'Range loop
               declare
                  script_file : String := wrkdir & "/pkg-" & convert_prepost (psp) &
                                          convert_stage (action) & "." & subpackage;
                  pkg_script  : String := spkgdir & subpackage & "/pkg-" & convert_prepost (psp) &
                                          convert_stage (action);
               begin
                  if DIR.Exists (script_file) then
                     DIR.Copy_File (Source_Name => script_file,
                                    Target_Name => pkg_script);
                  end if;
               end;
            end loop;
         end loop;

         write_package_manifest (spec       => specification,
                                 subpackage => subpackage,
                                 variant    => HT.USS (all_ports (seq_id).port_variant),
                                 pkgversion => pkgvers,
                                 filename   => spkgdir & subpackage & manifest);
      end metadata;

      procedure package_it (position : subpackage_crate.Cursor)
      is
         subpackage   : constant String := HT.USS (subpackage_crate.Element (position).subpackage);
         package_list : constant String := conbase & "/.manifest." & subpackage & ".mktmp";
         FORCE_POST_PATTERNS : constant String := "rmdir mkfontscale mkfontdir fc-cache " &
           "fonts.dir fonts.scale gtk-update-icon-cache gio-querymodules gtk-query-immodules " &
           "load-octave-pkg ocamlfind update-desktop-database update-mime-database " &
           "gdk-pixbuf-query-loaders catalog.ports glib-compile-schemas ccache-update-links";
         MORE_ENV : constant String :=
           " PKG_DBDIR=/var/db/pkg8" &
           " PLIST_KEYWORDS_DIR=/xports/Mk/Keywords ";
         PKG_CREATE : constant String := "/usr/bin/pkg-static create";
         PKG_CREATE_ARGS : constant String :=
           " --root-dir " & stagedir &
           " --metadata " & chspkgdir & subpackage &
           " --plist " & package_list &
           " --format txz" &
           " --out-dir " & newpkgdir &
           " --verbose ";
         namebase : constant String := specification.get_namebase;
         pkgname : String := namebase & "-" & subpackage & "-" &
           HT.USS (all_ports (seq_id).port_variant) & "-" & pkgvers;
         package_cmd : constant String := chroot & rootdir & " /usr/bin/env FORCE_POST=" &
           LAT.Quotation & FORCE_POST_PATTERNS & LAT.Quotation & MORE_ENV &
           PKG_CREATE & PKG_CREATE_ARGS & pkgname;
         move_cmd : constant String := chroot & rootdir & " /bin/mv " & newpkgdir & "/" &
           pkgname & ".txz /packages/All/";
         link_cmd : constant String := chroot & rootdir & " /bin/ln -sf /packages/All/" &
           pkgname & ".txz /packages/Latest/" & pkgname & ".txz";
      begin
         if still_good then
            if DIR.Exists (spkgdir & subpackage & display) then
               dump_pkg_message_to_log (display_file => spkgdir & subpackage & display,
                                        log_handle   => log_handle);
            end if;

            if not DIR.Exists (rootdir & package_list) then
               still_good := False;
               TIO.Put_Line
                 (log_handle, "=> The package list " & package_list & " for the " &
                    subpackage & " subpackage does not exist.");
            end if;
            TIO.Put_Line (log_handle, "===>  Building " & pkgname & " subpackage");
            TIO.Close (log_handle);

            still_good := execute_command (package_cmd, log_name);
            if still_good then
               still_good := execute_command (move_cmd, log_name);
            end if;
            if still_good and then namebase = "pkg" then
               still_good := execute_command (link_cmd, log_name);
            end if;

            TIO.Open (File => log_handle,
                      Mode => TIO.Append_File,
                      Name => log_name);
         end if;
      end package_it;
   begin
      LOG.log_phase_begin (log_handle, phase_name);

      all_ports (seq_id).subpackages.Iterate (metadata'Access);

      check_deprecation (specification, log_handle);
      if not create_package_directory_if_necessary (log_handle) or else
        not create_latest_package_directory_too (log_handle)
      then
         return False;
      end if;

      DIR.Create_Directory (rootdir & newpkgdir);
      all_ports (seq_id).subpackages.Iterate (package_it'Access);
      LOG.log_phase_end (log_handle);

      return still_good;

   exception
      when others =>
         return False;
   end exec_phase_package;


   --------------------------------------------------------------------------------------------
   --  create_package_directory_if_necessary
   --------------------------------------------------------------------------------------------
   function create_package_directory_if_necessary (log_handle : TIO.File_Type) return Boolean
   is
      packagedir : String := HT.USS (PM.configuration.dir_repository);
   begin
      if DIR.Exists (packagedir) then
         return True;
      end if;
      DIR.Create_Directory (packagedir);
      return True;
   exception
      when others =>
         TIO.Put_Line (log_handle, "=> Can't create directory " & packagedir);
         return False;
   end create_package_directory_if_necessary;


   --------------------------------------------------------------------------------------------
   --  create_package_directory_if_necessary
   --------------------------------------------------------------------------------------------
   function create_latest_package_directory_too (log_handle : TIO.File_Type) return Boolean
   is
      packagedir : String := HT.USS (PM.configuration.dir_packages) & "/Latest";
   begin
      if DIR.Exists (packagedir) then
         return True;
      end if;
      DIR.Create_Directory (packagedir);
      return True;
   exception
      when others =>
         TIO.Put_Line (log_handle, "=> Can't create directory " & packagedir);
         return False;
   end create_latest_package_directory_too;


   --------------------------------------------------------------------------------------------
   --  dump_pkg_message_to_log
   --------------------------------------------------------------------------------------------
   procedure dump_pkg_message_to_log (display_file : String; log_handle : TIO.File_Type)
   is
      File_Size : Natural := Natural (DIR.Size (display_file));
   begin
      if File_Size = 0 then
         DIR.Delete_File (display_file);
      else
         declare
            contents : String := FOP.get_file_contents (display_file);
         begin
            TIO.Put_Line (log_handle, contents);
         end;
      end if;
   end dump_pkg_message_to_log;


   --------------------------------------------------------------------------------------------
   --  check_deprecation
   --------------------------------------------------------------------------------------------
   procedure check_deprecation (spec : PSP.Portspecs; log_handle : TIO.File_Type)
   is
      deprecated  : String := spec.get_field_value (PSP.sp_deprecated);
      expire_date : String := spec.get_field_value (PSP.sp_expiration);
   begin
      if deprecated /= "" then
         TIO.Put_Line
           (log_handle,
            "===>   NOTICE:" & LAT.LF & LAT.LF &
              "This port is deprecated; you may wish to consider avoiding its packages." & LAT.LF &
              LAT.LF & deprecated & LAT.Full_Stop & LAT.LF &
              "It is scheduled to be removed on or after " & expire_date & LAT.LF
           );
      end if;
   end check_deprecation;


   --------------------------------------------------------------------------------------------
   --  quote
   --------------------------------------------------------------------------------------------
   function quote (thetext : String) return String is
   begin
      return LAT.Quotation & thetext & LAT.Quotation;
   end quote;


   --------------------------------------------------------------------------------------------
   --  write_package_manifest
   --------------------------------------------------------------------------------------------
   procedure write_package_manifest
     (spec       : PSP.Portspecs;
      subpackage : String;
      variant    : String;
      pkgversion : String;
      filename   : String)
   is
      function get_prefix return String;
      procedure single_if_defined (name, value : String);
      procedure array_if_defined (name, value : String);

      file_handle : TIO.File_Type;

      function get_prefix return String
      is
         prefix : String := spec.get_field_value (PSP.sp_prefix);
      begin
         if prefix = "" then
            return HT.USS (PM.configuration.dir_localbase);
         else
            return prefix;
         end if;
      end get_prefix;

      procedure single_if_defined (name, value : String) is
      begin
         if value /= "" then
            TIO.Put_Line (file_handle, name & ": " & quote (value));
         end if;
      end single_if_defined;

      procedure array_if_defined (name, value : String)
      is
         --  No identified need for quoting
      begin
         if value /= "" then
            TIO.Put_Line (file_handle, name & ": [ " & value & " ]");
         end if;
      end array_if_defined;

      name    : String := quote (spec.get_namebase & "-" & subpackage & "-" & variant);
      origin  : String := quote (spec.get_namebase & ":" & variant);

   begin
      TIO.Create (File => file_handle,
                  Mode => TIO.Out_File,
                  Name => filename);
      TIO.Put_Line
        (file_handle,
         LAT.Left_Curly_Bracket & LAT.LF &
           "name: " & name & LAT.LF &
           "version: " & quote (pkgversion) & LAT.LF &
           "origin: " & origin & LAT.LF &
           "comment: <<EOD" & LAT.LF &
           spec.get_tagline (variant) & " (" & subpackage & ")" & LAT.LF &
           "EOD" & LAT.LF &
           "maintainer: " & quote (spec.get_field_value (PSP.sp_contacts)) & LAT.LF &
           "prefix: " & quote (get_prefix) & LAT.LF &
           "categories: [ " & spec.get_field_value (PSP.sp_keywords) & " ]" & LAT.LF &
           "licenselogic: " & quote ("single")  -- TODO: IMPLEMENT
        );
      single_if_defined ("www",      spec.get_field_value (PSP.sp_homepage));
      array_if_defined  ("licenses", spec.get_field_value (PSP.sp_licenses));
      array_if_defined  ("users",    spec.get_field_value (PSP.sp_users));
      array_if_defined  ("groups",   spec.get_field_value (PSP.sp_groups));
      TIO.Put_Line (file_handle, "deps: {");
      if subpackage = spkg_complete then
         --  special handling: this is automatically converted to a metaport
         write_complete_metapackage_deps (spec, file_handle, variant, pkgversion);
      else
         --  TODO: dependencies
         null;
      end if;
      TIO.Put_Line (file_handle, "}");
      TIO.Put_Line (file_handle, "options: {"  & spec.get_field_value (PSP.sp_opt_helper) & " }");
      write_package_annotations (spec, file_handle);
      TIO.Close (file_handle);

   exception
      when others =>
         if TIO.Is_Open (file_handle) then
            TIO.Close (file_handle);
         end if;
   end write_package_manifest;


   --------------------------------------------------------------------------------------------
   --  write_complete_metapackage_deps
   --------------------------------------------------------------------------------------------
   procedure write_complete_metapackage_deps
     (spec        : PSP.Portspecs;
      file_handle : TIO.File_Type;
      variant     : String;
      pkgversion  : String)
   is
      namebase        : constant String  := spec.get_namebase;
      num_subpackages : constant Natural := spec.get_subpackage_length (variant);
   begin
      for spkg in 1 .. num_subpackages loop
         declare
            subpackage : constant String := spec.get_subpackage_item (variant, spkg);
         begin
            if subpackage /= spkg_complete then
               TIO.Put_Line
                 (file_handle, "  " &
                    quote (namebase & LAT.Hyphen & subpackage & LAT.Hyphen & variant) & " : {");
               TIO.Put_Line
                 (file_handle,
                    "    version : " & quote (pkgversion) & "," & LAT.LF &
                    "    origin : " & quote (namebase & LAT.Colon & variant) & LAT.LF &
                    "  },");
            end if;
         end;
      end loop;
   end write_complete_metapackage_deps;


   --------------------------------------------------------------------------------------------
   --  write_package_annotations
   --------------------------------------------------------------------------------------------
   procedure write_package_annotations
     (spec        : PSP.Portspecs;
      file_handle : TIO.File_Type)
   is
      num_notes : constant Natural := spec.get_list_length (PSP.sp_notes);
   begin
      if num_notes = 0 then
         return;
      end if;
      TIO.Put_Line (file_handle, "annotations: {");
      for note in 1 .. num_notes loop
         declare
            nvpair : String := spec.get_list_item (PSP.sp_notes, note);
            key    : String := HT.part_1 (nvpair, "=");
            value  : String := HT.part_2 (nvpair, "=");
         begin
            TIO.Put_Line (file_handle, key & ": <<EOD" & LAT.LF & value & LAT.LF & "EOD");
         end;
      end loop;
      TIO.Put_Line (file_handle, "}");
   end write_package_annotations;


   --------------------------------------------------------------------------------------------
   --  execute_command
   --------------------------------------------------------------------------------------------
   function execute_command (command : String; name_of_log : String) return Boolean
   is
      use type Unix.process_exit;
      pid         : Unix.pid_t;
      status      : Unix.process_exit;
      hangmonitor : constant Boolean := True;
      ravenexec   : constant String := host_localbase & "/libexec/ravenexec";
      truecommand : constant String := ravenexec & " " & name_of_log & " " & command;
   begin
      pid := Unix.launch_process (truecommand);
      if Unix.fork_failed (pid) then
         return False;
      end if;
      loop
         delay 0.25;
         status := Unix.process_status (pid);
         if status = Unix.exited_normally then
            return True;
         end if;
         if status = Unix.exited_with_error then
            return False;
         end if;
      end loop;
   end execute_command;

end PortScan.Packager;
