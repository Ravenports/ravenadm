--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Ravenports is

   --  Provides information to user via standard out to determine if newer version of Ravenports
   --  (as compared to what is installed) is available.
   procedure check_version_available;

   --  If a new version of Ravenports is available, this first retrieves the tarball and
   --  then installs the set (replacing an old set if necessary).
   procedure retrieve_latest_ravenports;

private

   remote_version : constant String :=
     "https://raw.githubusercontent.com/jrmarino/Ravenports/master/Mk/Misc/latest_release.txt";

   conspiracy_version : constant String := "/Mk/Misc/latest_release.txt";
   temporary_ver_loc  : constant String := "/tmp/rp_latest_version.txt";
   github_archive     : constant String := "https://github.com/jrmarino/Ravenports/archive/";

   --  Returns True if the latest Ravenports version information was successfully retrieved
   function fetch_latest_version_info (tmp_location : String) return Boolean;

   --  Returns True if the latest Ravenports tarball was successfully retrieved
   function fetch_latest_tarball (latest_version : String) return Boolean;

   --  Returns True if there's a version to install
   function later_version_available (available : out Boolean) return String;

   --  Returns True if mv command from conspiracy to conspiracy.old was successful
   function relocate_existing_ravenports return Boolean;

   --  Return true if successful at creating new, fully populated conspiracy directory
   function explode_tarball_into_conspiracy (latest_version : String) return Boolean;

   --  Try removing conspiracy.old and downloaded tarball
   procedure clean_up (latest_version : String);

end Ravenports;
