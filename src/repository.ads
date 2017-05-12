--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions; use Definitions;

package Repository is

   --  returns True if CONSERVATIVE_UPGRADE option on host pkg(8) is enabled
   function host_pkg8_conservative_upgrade_set return Boolean;

   --  Create a pkg repo conf file (requires root permission)
   function write_pkg_repos_configuration_file return Boolean;

   --  Rebuild the local repository with pkg(8)
   procedure rebuild_local_respository (remove_invalid_packages : Boolean);

private

   bad_command : exception;

   sorry : constant String := "The generated repository will not be signed due " &
                              "to the misconfiguration.";

   --  Return the contents of the profile's signing command
   function signing_command return String;

   --  Return the contents of the profile's fingerprint
   function profile_fingerprint return String;

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

      --  Return True if repo is configured to be built with RSA
   function set_raven_conf_with_RSA return Boolean;

   --  Returns ${LOCALBASE}/etc/raven/[profile]-
   function get_file_prefix return String;

   --  Returns octal failure of file permissions or "000" upon command failure
   function file_permissions (full_path : String) return String;

   --  Query pkg(8)'s repos_dir configuration instead of assuming default
   function get_repos_dir return String;

   --  This routine first removes all invalid packages (package from removed
   --  port or older version) and inserts the origins of the remaining packages
   --  into the port list for a limited tree scan.
   procedure preclean_repository (repository : String);

   --  The actual command to build a local repository (Returns True on success)
   function build_repository (sign_command : String := "") return Boolean;

   --  generic command, throws exception if exit code is not zero
   procedure silent_exec (command : String);

end Repository;
