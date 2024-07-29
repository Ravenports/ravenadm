--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Port_Specification;
with Unix.Ravexec;

package PortScan.Packager is

   package PSP renames Port_Specification;
   package RAX renames Unix.Ravexec;

   --  Executes package phase (run sometime after stage)
   function exec_phase_package
     (specification : PSP.Portspecs;
      log_fd        : RAX.File_Descriptor;
      builder_id    : builders;
      phase_name    : String;
      seq_id        : port_id;
      port_prefix   : String;
      rootdir       : String;
      environ       : String) return Boolean;

private

   --  Alert if port is deprecated
   procedure check_deprecation (spec : PSP.Portspecs; log_fd : RAX.File_Descriptor);

   --  Returns true if package directory exists or if it was successfully created.
   function create_package_directory_if_necessary (log_fd : RAX.File_Descriptor) return Boolean;

   --  Used to launch root commands (no watchdog)
   function execute_command
     (builder_id : builders;
      log_fd     : RAX.File_Descriptor;
      program    : String;
      arguments  : String) return Boolean;

end PortScan.Packager;
