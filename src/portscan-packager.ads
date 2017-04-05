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

private

   --  create +MANIFEST file for each subpackage
   procedure write_package_manifest
     (spec       : PSP.Portspecs;
      subpackage : String;
      variant    : String;
      pkgversion : String;
      filename   : String;
      prime_pkg  : Boolean);

   --  Alert if port is deprecated
   procedure check_deprecation (spec : PSP.Portspecs; log_handle : TIO.File_Type);

   --  remove filename if it's zero-length, otherwise dump it to the log
   procedure dump_pkg_message_to_log (display_file : String; log_handle : TIO.File_Type);

   --  Returns true if package directory exists or if it was successfully created.
   function create_package_directory_if_necessary (log_handle : TIO.File_Type) return Boolean;

   --  Returns true if the latest package directory exists or if it was successfully created.
   function create_latest_package_directory_too (log_handle : TIO.File_Type) return Boolean;

   --  Used to launch root commands (no watchdog)
   function execute_command (command : String; name_of_log : String) return Boolean;

   --  Puts quotation marks around given string
   function quote (thetext : String) return String;

   --  Handle "complete" metapackage deps case for the metadata file
   procedure write_complete_metapackage_deps
     (spec        : PSP.Portspecs;
      file_handle : TIO.File_Type;
      variant     : String;
      pkgversion  : String);

   --  Document buildrun + run dependencies in the "deps" category of the manifest.
   procedure write_down_run_dependencies
     (spec        : PSP.Portspecs;
      file_handle : TIO.File_Type;
      prime_pkg   : Boolean);

   --  If there are any package notes, write them to the manifest
   procedure write_package_annotations
     (spec        : PSP.Portspecs;
      file_handle : TIO.File_Type);

end PortScan.Packager;
