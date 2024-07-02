--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions; use Definitions;

private with Parameters;
private with HelperText;

package Repository is

   --  Rebuild the local repository with rvn(8)
   procedure rebuild_local_respository
     (remove_invalid_packages : Boolean;
      major_release : String;
      architecture  : supported_arch);

private

   package PM renames Parameters;
   package HT renames HelperText;

   bad_command : exception;

   --  ${LOCALBASE}/etc/raven/[profile]-
   --  cfgfile_prefix : constant String :=
   --  PM.raven_confdir & "/" & HT.USS (PM.configuration.profile) & "-";

   --  cfg_sign_command : constant String := cfgfile_prefix & "signing_command";
   --  cfg_fingerprint  : constant String := cfgfile_prefix & "fingerprint";
   --  cfg_key_private  : constant String := cfgfile_prefix & "private.key";
   --  cfg_key_public   : constant String := cfgfile_prefix & "public.key";

   function cfgfile_prefix return String;
   function cfg_sign_command return String;
   function cfg_fingerprint return String;
   function cfg_key_private return String;
   function cfg_key_public return String;

   --  The check for existence of both [profile]-signing_command and
   --  [profile]-fingerprint.  If only one exists, a non-fatal notice is
   --  emitted without signing the repository.  Returns True if both files
   --  exist and aren't empty.
   function valid_signing_command return Boolean;

   --  This checks for existence of both [profile]-public.key and
   --  [profile]-private.key.  If only one exists, a non-fatal notice is
   --  emitted saying signing configuration is incomplete (repo will not be
   --  signed).  The permissions for the private key will be checked, and if
   --  not 400 and owned by root, it will fail fatally.
   --  Returns False with fatal fail, otherwises it always returns True
   function acceptable_RSA_signing_support return Boolean;

   --  This routine first removes all invalid packages (package from removed
   --  port or older version) and inserts the origins of the remaining packages
   --  into the port list for a limited tree scan.
   procedure preclean_repository
     (repository    : String;
      major_release : String;
      architecture  : supported_arch);

   --  The actual command to assemble a catalog and sign it via an external server
   function externally_sign_repository return Boolean;

   --  The actual command to assemble a catalog and sign it with a private RSA key
   --  and generate a fingerprint file if the public RSA key is also provided.
   function locally_sign_repository return Boolean;

   --  The actual command to assemble an unsigned repository
   function build_repository return Boolean;

end Repository;
