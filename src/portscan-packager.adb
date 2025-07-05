--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with File_Operations;
with PortScan.Log;
with Parameters;
with ThickUCL.Emitter;
with ThickUCL.Files;
with Unix;
with UCL_Operations;
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
      log_fd        : RAX.File_Descriptor;
      builder_id    : builders;
      phase_name    : String;
      seq_id        : port_id;
      port_prefix   : String;
      rootdir       : String;
      environ       : String) return Boolean
   is
      procedure create_metadata_file (position : subpackage_crate.Cursor);
      procedure package_it (position : subpackage_crate.Cursor);
      procedure copy_it_outside_sysroot (position : subpackage_crate.Cursor);

      namebase   : constant String := HT.USS (all_ports (seq_id).port_namebase);
      construct  : constant String := "/construction/";
      conbase    : constant String := construct & namebase;
      wrkdir     : constant String := rootdir & conbase;
      chspkgdir  : constant String := construct & "metadata/";
      rfilesdir  : constant String := "/repo/files";
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
         procedure insert_message_set;
         procedure insert_script_set;
         procedure insert_script (scriptree : in out ThickUCL.UclTree; phase : String;
                                  code_file : String);

         myrec : subpackage_record renames subpackage_crate.Element (position);
         subpackage : constant String := HT.USS (myrec.subpackage);
         variant    : constant String := HT.USS (all_ports (seq_id).port_variant);
         metatree : ThickUCL.UclTree;

         procedure insert_script (scriptree : in out ThickUCL.UclTree; phase : String;
                                  code_file : String)
         is
            code : constant String := FOP.get_file_contents (code_file);
            andx : ThickUCL.array_index;
            num_elements : Natural;
         begin
            if scriptree.key_exists (phase) then
               andx := scriptree.get_index_of_base_array (phase);
               num_elements := scriptree.get_number_of_array_elements (andx);
               scriptree.reopen_array (phase, num_elements);
               scriptree.start_object ("");
               scriptree.insert ("args", "");
               scriptree.insert ("code", code);
               scriptree.close_object;
               scriptree.close_array;
            else
               scriptree.start_array (phase);
               scriptree.start_object ("");
               scriptree.insert ("args", "");
               scriptree.insert ("code", code);
               scriptree.close_object;
               scriptree.close_array;
            end if;
         end insert_script;

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
            description_filename : constant String := wrkdir & "/.PKG_DESC." & subpackage;
         begin
            if DIR.Exists (description_filename) then
               return FOP.get_file_contents (description_filename);
            end if;
            still_good := False;
            RAX.writeln (log_fd, "=> The package description file for the " & subpackage &
                           " subpackage does not exist.");
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
                        key   : constant String := HT.trim (HT.part_1 (field, ": "));
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

            already_inserted : string_crate.Vector;

            procedure scan (position : subpackage_crate.Cursor)
            is
               rec : subpackage_record renames subpackage_crate.Element (position);
            begin
               if HT.equivalent (rec.subpackage, subpackage) then
                  rec.spkg_run_deps.Iterate (assemble_origins'Access);
               end if;
            end scan;

            procedure assemble_origins (dep_position : spkg_id_crate.Cursor)
            is
               sprec : subpackage_identifier renames spkg_id_crate.Element (dep_position);
               n     : constant String := HT.USS (all_ports (sprec.port).port_namebase);
               s     : constant String := HT.USS (sprec.subpackage);
               v     : constant String := HT.USS (all_ports (sprec.port).port_variant);
               dkey  : constant String := n & LAT.Tilde & s & LAT.Tilde & v;
               ver   : constant String := HT.USS (all_ports (sprec.port).pkgversion);
            begin
               if not already_inserted.Contains (HT.SUS (dkey)) then
                  metatree.insert (dkey, ver);
                  already_inserted.Append (HT.SUS (dkey));
               end if;
            end assemble_origins;
         begin
            metatree.start_object ("deps");
            all_ports (seq_id).subpackages.Iterate (scan'Access);
            metatree.close_object;
         end insert_dependencies;

         procedure insert_trigger_set
         is
            trigger_metadata : ThickUCL.UclTree;
            file_location : constant String := wrkdir & "/.PKG_TRIGGERS." & subpackage;
         begin
            if not DIR.Exists (file_location) then
               return;
            end if;
            ThickUCL.Files.parse_ucl_file (trigger_metadata, file_location, "");
            if UCL_Operations.trigger_file_is_valid (trigger_metadata) then
               UCL_Operations.transfer_triggers (trigger_metadata, metatree);
            end if;
         exception
            when ThickUCL.Files.ucl_file_unparseable =>
               null;  --  should not happen since it's already been validated
         end insert_trigger_set;

         procedure insert_message_set
         is
            message_metadata : ThickUCL.UclTree;
            file_location : constant String := wrkdir & "/.PKG_MESSAGES." & subpackage;
         begin
            if not DIR.Exists (file_location) then
               return;
            end if;
            ThickUCL.Files.parse_ucl_file (message_metadata, file_location, "");
            if UCL_Operations.message_file_is_valid (message_metadata) then
               UCL_Operations.transfer_messages (message_metadata, metatree);
            end if;
         exception
            when ThickUCL.Files.ucl_file_unparseable =>
               null;  --  should not happen since it's already been validated
         end insert_message_set;

         procedure insert_script_set
         is
            script_metadata : ThickUCL.UclTree;
            file_location : constant String := wrkdir & "/.PKG_SCRIPTS." & subpackage;
            ug_install    : constant String := wrkdir & "/users-groups-install.sh";
            ug_deinstall  : constant String := wrkdir & "/users-groups-deinstall.sh";
            any_script    : Boolean := False;
         begin
            if DIR.Exists (file_location) then
               ThickUCL.Files.parse_ucl_file (script_metadata, file_location, "");
               any_script := True;
            end if;
            if subpackage = usrgrp_pkg then
               if DIR.Exists (ug_install) then
                  insert_script (script_metadata, "pre-install", ug_install);
                  any_script := True;
               end if;

               if DIR.Exists (ug_deinstall) then
                  insert_script (script_metadata, "post-deinstall", ug_deinstall);
                  any_script := True;
               end if;
            end if;

            if any_script then
               if UCL_Operations.script_file_is_valid (script_metadata) then
                  UCL_Operations.transfer_scripts (script_metadata, metatree);
               end if;
            end if;
         exception
            when ThickUCL.Files.ucl_file_unparseable =>
               null;  --  should not happen since it's already been validated
         end insert_script_set;
      begin
         if not still_good then
            return;
         end if;
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
         conditional_array ("licenses",
                            specification.get_field_value (PSP.sp_licenses, subpackage), True);
         conditional_array ("users",
                            specification.get_field_value (PSP.sp_users, subpackage), False);
         conditional_array ("groups",
                            specification.get_field_value (PSP.sp_groups, subpackage), False);
         insert_dependencies;
         insert_options;
         insert_annotations;
         insert_trigger_set;
         insert_message_set;
         insert_script_set;

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
         RVN_CREATE : constant String := "/usr/bin/rvn create";
         RVN_CREATE_ARGS : constant String :=
           " --out-dir " & rfilesdir &
           " --root-dir " & stagedir &
           " --prefix " & port_prefix &
           " --whitelist " & package_list &
           " --metadata " & chspkgdir & subpackage & ".ucl ";
         namebase : constant String := specification.get_namebase;
         filename : constant String := namebase & LAT.Tilde & subpackage & LAT.Tilde &
           HT.USS (all_ports (seq_id).port_variant) & LAT.Tilde & pkgvers & arc_ext;
         arguments : constant String := rootdir & environ & RVN_CREATE & RVN_CREATE_ARGS;
      begin
         if still_good then

            if not DIR.Exists (rootdir & package_list) then
               still_good := False;
               RAX.writeln (log_fd, "=> The package list " & package_list & " for the " &
                              subpackage & " subpackage does not exist.");
            end if;
         end if;
         if still_good then
            RAX.writeln (log_fd, "===>  Creating " & filename & " package");

            still_good := execute_command (builder_id, log_fd, PM.chroot_program, arguments);

         end if;
      end package_it;

      procedure copy_it_outside_sysroot (position : subpackage_crate.Cursor)
      is
         subpackage : constant String := HT.USS (subpackage_crate.Element (position).subpackage);
         namebase   : constant String := specification.get_namebase;
         pkgarchive : String := namebase & LAT.Tilde & subpackage & LAT.Tilde &
                      HT.USS (all_ports (seq_id).port_variant) & LAT.Tilde & pkgvers & arc_ext;
         built_loc  : constant String := rootdir & rfilesdir & "/" & pkgarchive;
         final_loc  : constant String := realpkgdir & "/files/" & pkgarchive;
         cp_program : constant String := sysroot & "/bin/cp -RpP ";
         cp_command : constant String := cp_program & " " & built_loc & " " & final_loc;
         cmd_output : HT.Text;
      begin
         if still_good then
            if not Unix.piped_mute_command (cp_command, cmd_output) then
               still_good := False;
               RAX.writeln (log_fd, "Failed to copy " & built_loc & " to " & final_loc);
               RAX.writeln (log_fd, "Message: " & HT.USS (cmd_output));
            end if;
         end if;
      end copy_it_outside_sysroot;

   begin
      LOG.log_phase_begin (log_fd, phase_name);

      DIR.Create_Path (spkgdir);

      check_deprecation (specification, log_fd);
      if not create_package_directory_if_necessary (log_fd) then
         return False;
      end if;

      all_ports (seq_id).subpackages.Iterate (create_metadata_file'Access);
      all_ports (seq_id).subpackages.Iterate (package_it'Access);
      all_ports (seq_id).subpackages.Iterate (copy_it_outside_sysroot'Access);
      LOG.log_phase_end (log_fd);

      return still_good;

   exception
      when others =>
         return False;
   end exec_phase_package;


   --------------------------------------------------------------------------------------------
   --  create_package_directory_if_necessary
   --------------------------------------------------------------------------------------------
   function create_package_directory_if_necessary (log_fd : RAX.File_Descriptor) return Boolean
   is
      packagedir : constant String := HT.USS (PM.configuration.dir_repository);
      packagedir_files : constant String := packagedir;
   begin
      if DIR.Exists (packagedir_files) then
         return True;
      end if;
      DIR.Create_Path (packagedir_files);
      return True;
   exception
      when others =>
         RAX.writeln (log_fd, "=> Can't create directory " & packagedir_files);
         return False;
   end create_package_directory_if_necessary;


   --------------------------------------------------------------------------------------------
   --  check_deprecation
   --------------------------------------------------------------------------------------------
   procedure check_deprecation (spec : PSP.Portspecs; log_fd : RAX.File_Descriptor)
   is
      deprecated  : String := spec.get_field_value (PSP.sp_deprecated);
      expire_date : String := spec.get_field_value (PSP.sp_expiration);
   begin
      if deprecated /= "" then
         RAX.writeln
           (log_fd,
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
   function execute_command
     (builder_id : builders;
      log_fd     : RAX.File_Descriptor;
      program    : String;
      arguments  : String) return Boolean
   is
      use type Unix.process_exit;
      pid         : Unix.pid_t;
      status      : Unix.process_exit;
   begin
      pid := RAX.launch_separate_process (builder_id, log_fd, program, arguments);
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
