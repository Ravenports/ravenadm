--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Parameters;
with HelperText;
with Unix;
with File_Operations;
with Ada.Directories;
with Ada.Text_IO;

package body Ravenports is

   package PM  renames Parameters;
   package HT  renames HelperText;
   package FOP renames File_Operations;
   package TIO renames Ada.Text_IO;
   package DIR renames Ada.Directories;

   --------------------------------------------------------------------------------------------
   --  check_version_available
   --------------------------------------------------------------------------------------------
   procedure check_version_available
   is
      consdir       : constant String := HT.USS (PM.configuration.dir_conspiracy);
      version_loc   : constant String := consdir & conspiracy_version;
      temp_loc      : constant String := "/tmp/rp_latest_version.txt";
   begin
      if DIR.Exists (version_loc) then
         if fetch_latest_version_info (temp_loc) then
            declare
               currentver : constant String := FOP.head_n1 (version_loc);
               latestver  : constant String := FOP.head_n1 (temp_loc);
            begin
               if latestver = currentver then
                  TIO.Put_Line ("The latest version of Ravenports, " & latestver &
                                  ", is currently installed.");
               else
                  TIO.Put_Line ("A newer version of Ravenports is available: " & latestver);
                  TIO.Put_Line ("The " & currentver & " version is currently installed.");
               end if;
            end;
            DIR.Delete_File (temp_loc);
         end if;
      else
         TIO.Put_Line ("It appears that Ravenports has not been installed at " & consdir);
      end if;
   exception
      when others => null;
   end check_version_available;


   --------------------------------------------------------------------------------------------
   --  fetch_latest_version_info
   --------------------------------------------------------------------------------------------
   function fetch_latest_version_info (tmp_location : String) return Boolean
   is
      sysroot       : constant String := HT.USS (PM.configuration.dir_sysroot);
      fetch_program : constant String := sysroot & "/usr/bin/fetch";
      cmd_output    : HT.Text;
      cmd : constant String := fetch_program & " -q " & remote_version & " -o " & tmp_location;
   begin
      if not DIR.Exists (fetch_program) then
         TIO.Put_Line ("It appears that the system root has not yet been installed at " & sysroot);
         TIO.Put_Line ("This must be rectified before Ravenports can be checked or fetched.");
         return False;
      end if;

      return Unix.piped_mute_command (cmd, cmd_output);
   end fetch_latest_version_info;

end Ravenports;
