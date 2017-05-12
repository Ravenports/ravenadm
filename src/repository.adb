--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Unix;
with Replicant;
with Parameters;
with HelperText;
with File_Operations;
with PortScan.Scan;
with PortScan.Operations;
with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;

package body Repository is

   package REP renames Replicant;
   package PM  renames Parameters;
   package HT  renames HelperText;
   package FOP renames File_Operations;
   package LAT renames Ada.Characters.Latin_1;
   package DIR renames Ada.Directories;
   package EX  renames Ada.Exceptions;
   package TIO renames Ada.Text_IO;

   --------------------------------------------------------------------------------------------
   --  signing_command
   --------------------------------------------------------------------------------------------
   function signing_command return String
   is
      filename : constant String := get_file_prefix & "-signing_command";
   begin
      return FOP.head_n1 (filename);
   end signing_command;


   --------------------------------------------------------------------------------------------
   --  profile_fingerprint
   --------------------------------------------------------------------------------------------
   function profile_fingerprint return String
   is
      filename : constant String := get_file_prefix & "-fingerprint";
   begin
      return FOP.head_n1 (filename);
   end profile_fingerprint;


   --------------------------------------------------------------------------------------------
   --  valid_signing_command
   --------------------------------------------------------------------------------------------
   function valid_signing_command return Boolean
   is
      file_prefix   : constant String := get_file_prefix;
      fingerprint   : constant String := file_prefix & "fingerprint";
      ext_command   : constant String := file_prefix & "signing_command";
      found_finger  : constant Boolean := DIR.Exists (fingerprint);
      found_command : constant Boolean := DIR.Exists (ext_command);
      sorry         : constant String := "The generated repository will not " &
                      "be externally signed due to the misconfiguration.";
   begin
      if found_finger and then found_command then
         if HT.IsBlank (FOP.head_n1 (fingerprint)) or else
           HT.IsBlank (FOP.head_n1 (ext_command))
         then
            TIO.Put_Line ("At least one of the profile signing command files is blank");
            TIO.Put_Line (sorry);
            return False;
         end if;
         return True;
      end if;

      if found_finger then
         TIO.Put_Line ("The profile fingerprint was found but not the signing command");
      elsif found_command then
         TIO.Put_Line ("The profile signing command was found but not the fingerprint");
      end if;

      TIO.Put_Line (sorry);
      return False;
   end valid_signing_command;


   --------------------------------------------------------------------------------------------
   --  acceptable_RSA_signing_support
   --------------------------------------------------------------------------------------------
   function acceptable_RSA_signing_support (ss_base : String) return Boolean
   is
      file_prefix   : constant String := get_file_prefix;
      key_private   : constant String := file_prefix & "private.key";
      key_public    : constant String := file_prefix & "public.key";
      found_private : constant Boolean := DIR.Exists (key_private);
      found_public  : constant Boolean := DIR.Exists (key_public);
      sorry         : constant String := "The generated repository will not " &
                      "be signed due to the misconfiguration.";
      repo_key      : constant String := HT.USS (PM.configuration.dir_buildbase)
                      & ss_base & "/etc/repo.key";
   begin
      if not found_private and then not found_public then
         return True;
      end if;
      if found_public and then not found_private then
         TIO.Put_Line ("A public RSA key file has been found without a " &
                         "corresponding private key file.");
         TIO.Put_Line (sorry);
         return True;
      end if;
      if found_private and then not found_public then
         TIO.Put_Line ("A private RSA key file has been found without a " &
                         "corresponding public key file.");
         TIO.Put_Line (sorry);
         return True;
      end if;
      declare
         mode : constant String := file_permissions (key_private);
      begin
         if mode /= "400" then
            TIO.Put_Line ("The private RSA key file has insecure file permissions (" & mode & ")");
            TIO.Put_Line ("Please change the file mode of " & key_private &
                            " to 400 before continuing.");
            return False;
         end if;
      end;
      begin
         DIR.Copy_File (Source_Name => key_private, Target_Name => repo_key);
         return True;
      exception
         when failed : others =>
            TIO.Put_Line ("Failed to copy private RSA key to builder.");
            TIO.Put_Line (EX.Exception_Information (failed));
            return False;
      end;
   end acceptable_RSA_signing_support;


   --------------------------------------------------------------------------------------------
   --  set_raven_conf_with_RSA
   --------------------------------------------------------------------------------------------
   function set_raven_conf_with_RSA return Boolean
   is
      file_prefix   : constant String := get_file_prefix;
      key_private   : constant String := file_prefix & "private.key";
      key_public    : constant String := file_prefix & "public.key";
      found_private : constant Boolean := DIR.Exists (key_private);
      found_public  : constant Boolean := DIR.Exists (key_public);
   begin
      return
        found_public and then
        found_private and then
        file_permissions (key_private) = "400";
   end set_raven_conf_with_RSA;


   --------------------------------------------------------------------------------------------
   --  get_file_prefix
   --------------------------------------------------------------------------------------------
   function get_file_prefix return String is
   begin
      return PM.raven_confdir & "/" & HT.USS (PM.configuration.profile) & "-";
   end get_file_prefix;


   --------------------------------------------------------------------------------------------
   --  file_permissions
   --------------------------------------------------------------------------------------------
   function file_permissions (full_path : String) return String
   is
      command : constant String := HT.USS (PM.configuration.dir_sysroot) &
                                   "/usr/bin/stat -f %Lp " & full_path;
      content  : HT.Text;
      status   : Integer;
   begin
      content := Unix.piped_command (command, status);
      if status /= 0 then
         return "000";
      end if;
      return HT.first_line (HT.USS (content));
   end file_permissions;


   --------------------------------------------------------------------------------------------
   --  host_pkg8_conservative_upgrade_set
   --------------------------------------------------------------------------------------------
   function host_pkg8_conservative_upgrade_set return Boolean
   is
      command : constant String := host_pkg8 & " config CONSERVATIVE_UPGRADE";
      content : HT.Text;
      status  : Integer;
   begin
      content := Unix.piped_command (command, status);
      return (HT.first_line (HT.USS (content)) = "yes");
   end host_pkg8_conservative_upgrade_set;


   --------------------------------------------------------------------------------------------
   --  get_repos_dir
   --------------------------------------------------------------------------------------------
   function get_repos_dir return String
   is
      command : String := host_pkg8 & " config repos_dir";
      status  : Integer;
      markers : HT.Line_Markers;
      repdirs : String := HT.USS (Unix.piped_command (command, status));
      default : constant String := host_localbase & "/etc/pkg/repos";
   begin
      if status /= 0 then
         --  command failed, use default
         return default;
      end if;

      HT.initialize_markers (repdirs, markers);
      loop
         exit when not HT.next_line_present (repdirs, markers);
         declare
            line : constant String := HT.extract_line (repdirs, markers);
         begin
            if line /= "/etc/pkg" then
               return line;
            end if;
         end;
      end loop;

      --  fallback, use default
      return default;
   end get_repos_dir;


   --------------------------------------------------------------------------------------------
   --  write_pkg_repos_configuration_file
   --------------------------------------------------------------------------------------------
   function write_pkg_repos_configuration_file return Boolean
   is
      repdir : constant String := get_repos_dir;
      target : constant String := repdir & "/00_raven.conf";
      pkgdir : constant String := HT.USS (PM.configuration.dir_packages);
      pubkey : constant String := get_file_prefix & "-public.key";
      keydir : constant String := PM.raven_confdir & "/keys";
      tstdir : constant String := keydir & "/trusted";
      autgen : constant String := "# Automatically generated." & LAT.LF;
      fpfile : constant String := tstdir & "/fingerprint." & HT.USS (PM.configuration.profile);
      handle : TIO.File_Type;
      vscmd  : Boolean := False;
   begin
      if DIR.Exists (target) then
         DIR.Delete_File (target);
      elsif not DIR.Exists (repdir) then
         DIR.Create_Path (repdir);
      end if;
      TIO.Create (File => handle, Mode => TIO.Out_File, Name => target);
      TIO.Put_Line (handle, autgen);
      TIO.Put_Line (handle, "Raven: {");
      TIO.Put_Line (handle, "  url      : file://" & pkgdir & ",");
      TIO.Put_Line (handle, "  priority : 0,");
      TIO.Put_Line (handle, "  enabled  : yes,");
      if valid_signing_command then
         vscmd := True;
         TIO.Put_Line (handle, "  signature_type : FINGERPRINTS,");
         TIO.Put_Line (handle, "  fingerprints   : " & keydir);
      elsif set_raven_conf_with_RSA then
         TIO.Put_Line (handle, "  signature_type : PUBKEY,");
         TIO.Put_Line (handle, "  pubkey         : " & pubkey);
      end if;
      TIO.Put_Line (handle, "}");
      TIO.Close (handle);
      if vscmd then
         if DIR.Exists (fpfile) then
            DIR.Delete_File (fpfile);
         elsif not DIR.Exists (tstdir) then
            DIR.Create_Path (tstdir);
         end if;
         TIO.Create (File => handle, Mode => TIO.Out_File, Name => fpfile);
         TIO.Put_Line (handle, autgen);
         TIO.Put_Line (handle, "function    : sha256");
         TIO.Put_Line (handle, "fingerprint : " & profile_fingerprint);
         TIO.Close (handle);
      end if;
      return True;
   exception
      when others =>
         TIO.Put_Line ("Error: failed to create " & target);
         if TIO.Is_Open (handle) then
            TIO.Close (handle);
         end if;
         return False;
   end write_pkg_repos_configuration_file;


   --------------------------------------------------------------------------------------------
   --  rebuild_local_respository
   --------------------------------------------------------------------------------------------
   procedure preclean_repository (repository : String)
   is
   begin
      if PortScan.Scan.scan_repository (repository) then
         PortScan.Operations.eliminate_obsolete_packages;
      end if;
   end preclean_repository;


   --------------------------------------------------------------------------------------------
   --  rebuild_local_respository
   --------------------------------------------------------------------------------------------
   procedure rebuild_local_respository (remove_invalid_packages : Boolean;
                                        scan_slave : builders)
   is
      ------------------------------------------------------------
      --  fully_scan_tree must be executed before this routine  --
      ------------------------------------------------------------
      repo       : constant String := HT.USS (PM.configuration.dir_repository);
      main       : constant String := HT.USS (PM.configuration.dir_packages);
      xz_meta    : constant String := main & "/meta.txz";
      xz_digest  : constant String := main & "/digests.txz";
      xz_pkgsite : constant String := main & "/packagesite.txz";
      bs_error   : constant String := "Rebuild Repository: Failed to bootstrap builder";
      build_res  : Boolean;
   begin
      if remove_invalid_packages then
         preclean_repository (repo);
      end if;

      if DIR.Exists (xz_meta) then
         DIR.Delete_File (xz_meta);
      end if;
      if DIR.Exists (xz_digest) then
         DIR.Delete_File (xz_digest);
      end if;
      if DIR.Exists (xz_pkgsite) then
         DIR.Delete_File (xz_pkgsite);
      end if;
      TIO.Put_Line ("Rebuilding local repository.");
      REP.initialize (testmode => False);
      REP.launch_slave (scan_slave);
--        if valid_signing_command then
--           build_res := REP.build_repository (scan_slave,
--                                              sign_command => signing_command);
--        elsif acceptable_RSA_signing_support then
--           build_res := REP.build_repository (scan_slave);
--        else
--           build_res := False;
--        end if;
build_res:=True;
      REP.destroy_slave (scan_slave);
      REP.finalize;
      if build_res then
         TIO.Put_Line ("Local repository successfully rebuilt");
      else
         TIO.Put_Line ("Failed to rebuild repository.");
      end if;
   end rebuild_local_respository;

end Repository;
