--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Unix;
with File_Operations;
with PortScan.Scan;
with PortScan.Operations;
with Ada.Directories;
with Ada.Text_IO;

package body Repository is

   package FOP renames File_Operations;
   package DIR renames Ada.Directories;
   package TIO renames Ada.Text_IO;


   --------------------------------------------------------------------------------------------
   --  valid_signing_command
   --------------------------------------------------------------------------------------------
   function valid_signing_command return Boolean
   is
      found_command : constant Boolean := DIR.Exists (cfg_sign_command);
   begin
      if found_command then
         --  The signing command must contain "{}" which is a replacement token for the digest
         --  to be signed.  Verify the command contains that token.

         declare
            sc : constant String := FOP.head_n1 (cfg_sign_command);
         begin
            if not HT.contains (sc, "{}") then
               TIO.Put_Line ("Invalid signing command found: missing {} token.");
               return False;
            end if;
            if HT.contains (sc, "'") then
               TIO.Put_Line ("Invalid signing command found: contains apostrophe.");
               return False;
            end if;
            return True;
         end;
      end if;
      return False;
   end valid_signing_command;


   --------------------------------------------------------------------------------------------
   --  acceptable_RSA_signing_support
   --------------------------------------------------------------------------------------------
   function acceptable_RSA_signing_support return Boolean
   is
      found_private : constant Boolean := DIR.Exists (cfg_key_private);
   begin
      if found_private then
         if not Unix.file_secure (cfg_key_private) then
            TIO.Put_Line ("The private RSA key file has insecure file permissions");
            TIO.Put_Line ("Please change the file mode of " & cfg_key_private &
                            " to 400 before continuing.");
            return False;
         end if;
      else
         return False;
      end if;

      return True;
   end acceptable_RSA_signing_support;


   --------------------------------------------------------------------------------------------
   --  rebuild_local_respository
   --------------------------------------------------------------------------------------------
   procedure preclean_repository (repository    : String;
                                  major_release : String;
                                  architecture  : supported_arch)
   is
   begin
      if PortScan.Scan.scan_repository (repository) then
         PortScan.Operations.eliminate_obsolete_packages (major_release, architecture);
      end if;
   end preclean_repository;


   --------------------------------------------------------------------------------------------
   --  rebuild_local_respository
   --------------------------------------------------------------------------------------------
   procedure rebuild_local_respository (remove_invalid_packages : Boolean;
                                        major_release : String;
                                        architecture  : supported_arch)
   is
      ------------------------------------------------------------
      --  fully_scan_tree must be executed before this routine  --
      ------------------------------------------------------------
      repo       : constant String := HT.USS (PM.configuration.dir_repository);
      main       : constant String := HT.USS (PM.configuration.dir_packages);
      bs_error   : constant String := "Rebuild Repository: Failed to bootstrap builder";
      build_res  : Boolean;
   begin
      if remove_invalid_packages then
         preclean_repository (repo, major_release, architecture);
      end if;

      --  Priority:
      --  1.  Build with signing server (fingerprint generated)
      --  2.  Build with public and private key (fingerprint generated)
      --  3.  Build with private key only
      --  4.  Build without signature

      if valid_signing_command then
         build_res := externally_sign_repository;
      elsif acceptable_RSA_signing_support then
         build_res := locally_sign_repository;
      else
         build_res := build_repository;
      end if;
      if build_res then
         TIO.Put_Line ("Local repository successfully generated.");
      else
         TIO.Put_Line ("Failed to generate local repository.");
      end if;
   end rebuild_local_respository;


   --------------------------------------------------------------------------------------------
   --  externally_sign_repository
   --------------------------------------------------------------------------------------------
   function externally_sign_repository return Boolean
   is
      sc       : constant String := FOP.head_n1 (cfg_sign_command);
      repopath : constant String := HT.USS (PM.configuration.dir_packages);
      command  : constant String := host_rvn & " genrepo --external '" & sc &
        "' --fingerprint " & cfg_fingerprint & " " & repopath;
   begin
      TIO.Put_Line ("Assembling externally-signed repository ...");
      return Unix.external_command (command);
   end externally_sign_repository;


   --------------------------------------------------------------------------------------------
   --  locally_sign_repository
   --------------------------------------------------------------------------------------------
   function locally_sign_repository return Boolean
   is
      function get_command return String;

      repopath : constant String := HT.USS (PM.configuration.dir_packages);
      found_public : constant Boolean := DIR.Exists (cfg_key_public);

      function get_command return String is
      begin
         if found_public then
            return host_rvn & " genrepo --key " & cfg_key_private &
              " --pubkey " & cfg_key_public &
              " --fingerprint " & cfg_fingerprint & " " & repopath;
         end if;
         return host_rvn & " genrepo --key " & cfg_key_private & " " & repopath;
      end get_command;
   begin
      declare
         command : constant String := get_command;
      begin
         TIO.Put_Line ("Assembling RSA-signed repository ...");
         return Unix.external_command (command);
      end;
   end locally_sign_repository;


   --------------------------------------------------------------------------------------------
   --  build_repository
   --------------------------------------------------------------------------------------------
   function build_repository return Boolean
   is
      pkgdir  : constant String := HT.USS (PM.configuration.dir_packages);
      command : constant String := host_rvn & " genrepo " & pkgdir;
   begin
      TIO.Put_Line ("Assembling unsigned repository ...");
      return Unix.external_command (command);
   end build_repository;


   ------------------------
   --  pseudo constants  --
   ------------------------
   function cfgfile_prefix return String is begin
      return PM.raven_confdir & "/" & HT.USS (PM.configuration.profile) & "-";
   end cfgfile_prefix;

   function cfg_sign_command return String is begin
      return cfgfile_prefix & "signing_command";
   end cfg_sign_command;

   function cfg_fingerprint return String is begin
      return cfgfile_prefix & "fingerprint";
   end cfg_fingerprint;

   function cfg_key_private return String is begin
      return cfgfile_prefix & "private.key";
   end cfg_key_private;

   function cfg_key_public return String is begin
      return cfgfile_prefix & "public.key";
   end cfg_key_public;

end Repository;
