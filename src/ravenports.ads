--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Ravenports is

   --  Provides information to user via standard out to determine if newer version of Ravenports
   --  (as compared to what is installed) is available.
   procedure check_version_available;

private

   remote_version : constant String :=
     "https://raw.githubusercontent.com/jrmarino/Ravenports/master/Mk/Misc/latest_release.txt";

   conspiracy_version : constant String := "/Mk/Misc/latest_release.txt";

   --  Returns True if the latest Ravenports version information was successfully retrieved
   function fetch_latest_version_info (tmp_location : String) return Boolean;

end Ravenports;
