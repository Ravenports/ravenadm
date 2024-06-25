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
      port_prefix   : String;
      rootdir       : String) return Boolean;

private

   --  Alert if port is deprecated
   procedure check_deprecation (spec : PSP.Portspecs; log_handle : TIO.File_Type);

   --  Returns true if package directory exists or if it was successfully created.
   function create_package_directory_if_necessary (log_handle : TIO.File_Type) return Boolean;

   --  Used to launch root commands (no watchdog)
   function execute_command (command : String; name_of_log : String) return Boolean;

end PortScan.Packager;
