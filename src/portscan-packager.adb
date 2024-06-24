--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with File_Operations;
with PortScan.Log;
with Parameters;
with ThickUCL.Emitter;
with ThickUCL.Files;
with Unix;
with ucl_operations;
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
      port_prefix   : String;
      rootdir       : String) return Boolean
   is
      procedure create_metadata_file (position : subpackage_crate.Cursor);
      procedure package_it (position : subpackage_crate.Cursor);
      procedure move_it_outside_sysroot (position : subpackage_crate.Cursor);

      namebase   : constant String := HT.USS (all_ports (seq_id).port_namebase);
      construct  : constant String := "/construction/";
      conbase    : constant String := construct & namebase;
      wrkdir     : constant String := rootdir & conbase;
      chspkgdir  : constant String := construct & "metadata/";
      newpkgdir  : constant String := construct & "new_packages";
      spkgdir    : constant String := rootdir & chspkgdir;
      sysroot    : constant String := HT.USS (PM.configuration.dir_sysroot);
      realpkgdir : constant String := HT.USS (PM.configuration.dir_packages);
      stagedir   : constant String := conbase & "/stage";
      pkgvers    : constant String := HT.USS (all_ports (seq_id).pkgversion);
      usrgrp_pkg : constant String := specification.get_field_value (PSP.sp_ug_pkg);
      still_good : Boolean := True;

      --  [[ Metadata format ]]
      --  namebase        : pkg-namebase <string>
      --  subpackage      : pkg-subpackage <string>
      --  variant         : pkg-variant <string>
      --  version         : pkg-version <string>
      --  comment         : comment-string <string>
      --  desc            : description <multiline string>
      --  www             : url <string>
      --  maintainer      : mail-address <string>
      --  prefix          : <skip, autoset>
      --  flatsize        : <skip, autoset>
      --  abi             : <skip, autoset>
      --  deps            : object { n-s-v => version }
      --  options         : object { option-name => on/off or T/F }
      --  categories      : array of <string>
      --  licencelogic    : ("dual"|"single"|"multi")
      --  licenses        : array of <string>
      --  annotations     : object { tag => note }
      --  users           : array of <string>
      --  groups          : array of <string>
      --  shlibs_provided : <skip, autoset>  array of <string>
      --  shlibs_required : <skip, autoset>  array of <string>
      --  shlibs_adjacent : <skip, autoset>  array of <string>
      --  scripts         : object { script-type => { args => <string>, code => <string> }}
      --  directories     : <skip, autoset> array of object { group, owner, perms, path }
      --  messages        : array of objects
      --  triggers        : array of object { cleanup, trigger,
      --                                      (dir_path|file_path|file_glob|file_regexp)}

      procedure create_metadata_file (position : subpackage_crate.Cursor)
      is
         function short_description return String;
         function long_description return String;
         procedure conditional_single (name : String; value : String);
         procedure conditional_array (name : String; concatenated : String; quoted : Boolean);
         procedure insert_annotations;
         procedure insert_options;
         procedure insert_dependencies;
         procedure insert_trigger_set;

         myrec : subpackage_record renames subpackage_crate.Element (position);
         subpackage : constant String := HT.USS (myrec.subpackage);
         variant    : constant String := HT.USS (all_ports (seq_id).port_variant);
         metatree : ThickUCL.UclTree;

         function short_description return String is
         begin
            if specification.get_subpackage_length (variant) = 1 then
               return specification.get_tagline (variant);
            end if;
            return specification.get_tagline (variant) & " (" & subpackage & ")";
         end short_description;

         procedure conditional_single (name : String; value : String) is
         begin
            if value = "" then
               return;
            end if;
            metatree.insert (name, value);
         end conditional_single;

         procedure conditional_array (name : String; concatenated : String; quoted : Boolean)
         is
            function stripped_field (field : Natural) return String;
            function stripped_field (field : Natural) return String
            is
               namestr : constant String :=  HT.specific_field (concatenated, field, ", ");
            begin
               if quoted then
                  if namestr'Length <= 2 then
                     return "";
                  end if;
                  return namestr (namestr'First + 1 .. namestr'Last - 1);
               end if;
               return namestr;
            end stripped_field;

         begin
            if concatenated = "" then
               return;
            end if;
            metatree.start_array (name);
            declare
               num_fields : constant Natural := HT.count_char (concatenated, ',') + 1;
            begin
               for field in 1 .. num_fields loop
                  metatree.insert ("", stripped_field (field));
               end loop;
            end;
            metatree.close_array;
         end conditional_array;

         function long_description return String
         is
            description_filename : constant String := wrkdir & "/.PKG_DISPLAY." & subpackage;
         begin
            if DIR.Exists (description_filename) then
               return FOP.get_file_contents (description_filename);
            end if;
            return "Developer error: " & subpackage & " subpackage description missing.";
         end long_description;

         procedure insert_options
         is
            function opt_value (raw_value : String) return Boolean;
            function opt_value (raw_value : String) return Boolean is
            begin
               return raw_value (raw_value'First .. raw_value'First + 1) = "on";
            end opt_value;
         begin
            metatree.start_object ("options");
            if specification.global_options_present then
               declare
                  --  examples: "PERL_536: off, PERL_538: on,"   (trailing comma)
                  opts : constant String := specification.get_options_list (variant);
                  num_opts : constant Natural := HT.count_char (opts, ',');
               begin
                  for index in 1 .. num_opts loop
                     declare
                        field : constant String := HT.specific_field (opts, index, ", ");
                        key   : constant String := HT.part_1 (field, ": ");
                        value : constant Boolean := opt_value (HT.part_2 (field, ": "));
                     begin
                        metatree.insert (key, value);
                     end;
                  end loop;
               end;
            end if;
            metatree.close_object;
         end insert_options;

         procedure insert_annotations
         is
            num_notes : constant Natural := specification.get_list_length (PSP.sp_notes);
         begin
            if num_notes = 0 then
               return;
            end if;
            metatree.start_object ("annotations");
            for note in 1 .. num_notes loop
               declare
                  nvpair : constant String := specification.get_list_item (PSP.sp_notes, note);
                  key    : constant String := HT.part_1 (nvpair, "=");
                  value  : constant String := HT.part_2 (nvpair, "=");
               begin
                  metatree.insert (key, value);
               end;
            end loop;
            metatree.close_object;
         end insert_annotations;

         procedure insert_dependencies
         is
            procedure scan (position : subpackage_crate.Cursor);
            procedure assemble_origins (dep_position : spkg_id_crate.Cursor);

            found_subpackage : Boolean := False;

            procedure scan (position : subpackage_crate.Cursor)
            is
               rec : subpackage_record renames subpackage_crate.Element (position);
            begin
               if not found_subpackage then
                  if HT.equivalent (rec.subpackage, subpackage) then
                     found_subpackage := True;
                     rec.spkg_run_deps.Iterate (assemble_origins'Access);
                  end if;
               end if;
            end scan;

            procedure assemble_origins (dep_position : spkg_id_crate.Cursor)
            is
               sprec : subpackage_identifier renames spkg_id_crate.Element (dep_position);
               n     : constant String := HT.USS (all_ports (sprec.port).port_namebase);
               s     : constant String := HT.USS (sprec.subpackage);
               v     : constant String := HT.USS (all_ports (sprec.port).port_variant);
               ver   : constant String := HT.USS (all_ports (sprec.port).pkgversion);
            begin
               metatree.insert (n & LAT.Hyphen & s & LAT.Hyphen & v, ver);
            end assemble_origins;
         begin
            metatree.start_object ("deps");
            all_ports (seq_id).subpackages.Iterate (scan'Access);
            metatree.close_object;
         end insert_dependencies;

         procedure insert_trigger_set
         is
            trigger_metadata : ThickUCL.UclTree;
            file_location : constant String := wrkdir & "/.PKG_TRIGGER." & subpackage;
         begin
            if not DIR.Exists (file_location) then
               return;
            end if;
            ThickUCL.Files.parse_ucl_file (trigger_metadata, file_location, "");
            if ucl_operations.trigger_file_is_valid (trigger_metadata) then
               ucl_operations.transfer_triggers (trigger_metadata, metatree);
            end if;
         exception
            when ThickUCL.Files.ucl_file_unparseable =>
               null;  --  should not happen since it's already been validated
         end insert_trigger_set;
      begin
         metatree.insert ("namebase", namebase);
         metatree.insert ("subpackage", subpackage);
         metatree.insert ("variant", variant);
         metatree.insert ("version", HT.USS (all_ports (seq_id).pkgversion));
         metatree.insert ("comment", short_description);
         metatree.insert ("maintainer", specification.get_field_value (PSP.sp_contacts));
         metatree.insert ("licenselogic", specification.get_license_scheme);
         metatree.insert ("desc", long_description);
         conditional_single ("www", specification.get_field_value (PSP.sp_homepage));
         conditional_array ("categories", specification.get_field_value (PSP.sp_keywords), False);
         conditional_array ("licenses", specification.get_field_value (PSP.sp_licenses), True);
         conditional_array ("users", specification.get_field_value (PSP.sp_users), False);
         conditional_array ("groups", specification.get_field_value (PSP.sp_groups), False);
         insert_options;
         insert_annotations;
         insert_trigger_set;

         declare
            meta_contents : constant String := ThickUCL.Emitter.emit_ucl (metatree);
            filename      : constant String := spkgdir & subpackage & ".ucl";
         begin
            FOP.dump_contents_to_file (meta_contents, filename);
         end;

      end create_metadata_file;


      procedure package_it (position : subpackage_crate.Cursor)
      is
         subpackage   : constant String := HT.USS (subpackage_crate.Element (position).subpackage);
         package_list : constant String := conbase & "/.manifest." & subpackage & ".mktmp";
         FORCE_POST_PATTERNS : constant String := "rmdir mkfontscale mkfontdir fc-cache " &
           "fonts.dir fonts.scale gtk-update-icon-cache gio-querymodules gtk-query-immodules " &
           "load-octave-pkg ocamlfind update-desktop-database update-mime-database " &
           "gdk-pixbuf-query-loaders catalog.ports glib-compile-schemas ccache-update-links";
         MORE_ENV : constant String :=
           " RAVENSW_DBDIR=/var/db/pkg8" &
           " PLIST_KEYWORDS_DIR=/xports/Mk/Keywords ";
         PKG_CREATE : constant String := "/usr/bin/ravensw create";
         PKG_CREATE_ARGS : constant String :=
           " --root-dir " & stagedir &
           " --metadata " & chspkgdir & subpackage &
           " --plist " & package_list &
           " --out-dir " & newpkgdir &
           " --verbose ";
         namebase : constant String := specification.get_namebase;
         pkgname : String := namebase & "-" & subpackage & "-" &
           HT.USS (all_ports (seq_id).port_variant) & "-" & pkgvers;
         package_cmd : constant String := PM.chroot_cmd & rootdir & " /usr/bin/env FORCE_POST=" &
           LAT.Quotation & FORCE_POST_PATTERNS & LAT.Quotation & MORE_ENV &
           PKG_CREATE & PKG_CREATE_ARGS & pkgname;
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

            TIO.Open (File => log_handle,
                      Mode => TIO.Append_File,
                      Name => log_name);
         end if;
      end package_it;

      procedure move_it_outside_sysroot (position : subpackage_crate.Cursor)
      is
         subpackage : constant String := HT.USS (subpackage_crate.Element (position).subpackage);
         namebase   : constant String := specification.get_namebase;
         pkgarchive : String := namebase & "-" & subpackage & "-" &
                      HT.USS (all_ports (seq_id).port_variant) & "-" & pkgvers & arc_ext;
         built_loc  : constant String := rootdir & newpkgdir & "/" & pkgarchive;
         final_loc  : constant String := realpkgdir & "/All/" & pkgarchive;
         link_loc   : constant String := realpkgdir & "/Latest/" & pkgarchive;
         mv_program : constant String := sysroot & "/bin/mv ";
         mv_command : constant String := mv_program & " " & built_loc & " " & final_loc;
         cmd_output : HT.Text;
      begin
         if still_good then
            --  DIR.Rename fails.  The exception doesn't indicate why.  Use mv instead.
            if not Unix.piped_mute_command (mv_command, cmd_output) then
               still_good := False;
               TIO.Put_Line (log_handle, "Failed to move " & built_loc & " to " & final_loc);
               TIO.Put_Line (log_handle, "Message: " & HT.USS (cmd_output));
            end if;
            if still_good and then namebase = "ravensw" then
               if DIR.Exists (link_loc) then
                  DIR.Delete_File (link_loc);
               end if;
               still_good := Unix.create_symlink
                 (actual_file => "../All/" & pkgarchive,
                  link_to_create => link_loc);
               if not still_good then
                  TIO.Put_Line (log_handle, "Failed to create link " & link_loc &
                                  " to ../All/" & pkgarchive);
               end if;
            end if;
         end if;
      end move_it_outside_sysroot;

   begin
      LOG.log_phase_begin (log_handle, phase_name);

      DIR.Create_Path (spkgdir);
      all_ports (seq_id).subpackages.Iterate (create_metadata_file'Access);

      check_deprecation (specification, log_handle);
      if not create_package_directory_if_necessary (log_handle) then
         return False;
      end if;

      DIR.Create_Directory (rootdir & newpkgdir);
      all_ports (seq_id).subpackages.Iterate (package_it'Access);
      all_ports (seq_id).subpackages.Iterate (move_it_outside_sysroot'Access);
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
   --  execute_command
   --------------------------------------------------------------------------------------------
   function execute_command (command : String; name_of_log : String) return Boolean
   is
      use type Unix.process_exit;
      pid         : Unix.pid_t;
      status      : Unix.process_exit;
      hangmonitor : constant Boolean := True;
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
