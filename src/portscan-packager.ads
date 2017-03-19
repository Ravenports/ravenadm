--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Port_Specification;

package PortScan.Packager is

   package PSP renames Port_Specification;

   --  Executes package phase (run sometime after stage)
   function exec_phase_package
     (specification : PSP.Portspecs;
      log_handle    : TIO.File_Type;
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

   procedure write_package_manifest
     (spec       : PSP.Portspecs;
      subpackage : String;
      variant    : String;
      filename   : String);


end PortScan.Packager;
