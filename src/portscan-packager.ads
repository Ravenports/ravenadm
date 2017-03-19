--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Port_Specification;

package PortScan.Packager is

   package PSP renames Port_Specification;

   --  Executes package phase (run sometime after stage)
   function exec_phase_package
     (specification : PSP.Portspecs;
      log_handle    : in out TIO.File_Type;
      log_name      : String;
      phase_name    : String;
      seq_id        : port_id;
      rootdir       : String) return Boolean;

   --  check-plist target verifies manifest
   --  It takes into account the manifest for all subpackages (for a given variant).
   --  If any files are left over or unaccounted for according to manifest, return false.
   --  It
   function exec_check_plist
     (log_handle : TIO.File_Type;
      seq_id     : port_id) return Boolean;

private

   --  create +MANIFEST file for each subpackage
   procedure write_package_manifest
     (spec       : PSP.Portspecs;
      subpackage : String;
      variant    : String;
      filename   : String);

   --  Alert if port is deprecated
   procedure check_deprecation (spec : PSP.Portspecs; log_handle : TIO.File_Type);

   --  remove filename if it's zero-length, otherwise dump it to the log
   procedure dump_pkg_message_to_log (display_file : String; log_handle : TIO.File_Type);

   --  Returns true if package directory exists or if it was successfully created.
   function create_package_directory_if_necessary (log_handle : TIO.File_Type) return Boolean;

   --  Returns true if the latest package directory exists or if it was successfully created.
   function create_latest_package_directory_too (log_handle : TIO.File_Type) return Boolean;

   --  Calculate the surprisingly complex pkgversion string
   function get_pkg_version (spec : PSP.Portspecs) return String;

   --  Used to launch root commands (no watchdog)
   function execute_command (command : String; name_of_log : String) return Boolean;

end PortScan.Packager;
