--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Port_Specification;

package PortScan.Tests is

   package PSP renames Port_Specification;

   --  check-plist target verifies manifest
   --  It takes into account the manifest for all subpackages (for a given variant).
   --  If any files are left over or unaccounted for according to manifest, return false.
   --  It also objects to the same file present on multiple manifests.

   function exec_check_plist
     (specification : PSP.Portspecs;
      log_handle    : TIO.File_Type;
      phase_name    : String;
      seq_id        : port_id;
      port_prefix   : String;
      rootdir       : String) return Boolean;

private

   type entry_record is
      record
         subpackage  : HT.Text := HT.blank;
         verified    : Boolean := False;
      end record;

   package entry_crate is new CON.Hashed_Maps
     (Key_Type        => HT.Text,
      Element_Type    => entry_record,
      Hash            => port_hash,
      Equivalent_Keys => HT.equivalent);

   --  Iterates though subpackages and pulls in associated manifest
   --  If any files are represented by multiple manifests, output the culprit and set result
   --  to false.  Even when error is found, all files are still checked.
   function ingest_manifests
     (specification  : PSP.Portspecs;
      log_handle     : TIO.File_Type;
      directory_list : in out entry_crate.Map;
      dossier_list   : in out entry_crate.Map;
      seq_id         : port_id;
      namebase       : String;
      port_prefix    : String;
      rootdir        : String) return Boolean;

   --  Scans all directories in stage and returns true if any non-standard orphans are found
   function orphaned_directories_detected
     (log_handle     : TIO.File_Type;
      directory_list : in out entry_crate.Map;
      namebase       : String;
      port_prefix    : String;
      rootdir        : String) return Boolean;

   --  Scans all files in stage and returns true if any orphans are found
   function orphaned_files_detected
     (log_handle     : TIO.File_Type;
      dossier_list   : in out entry_crate.Map;
      namebase       : String;
      port_prefix    : String;
      rootdir        : String) return Boolean;

   --  Iterates through directory list.  If any directories are listed that weren't in
   --  the stage directory, output the issue and set result to false.
   function missing_directories_detected
     (log_handle     : TIO.File_Type;
      directory_list : in out entry_crate.Map) return Boolean;

   --  Iterates through file list.  If any file are listed that weren't in
   --  the stage directory, output the issue and set result to false.
   function missing_files_detected
     (log_handle   : TIO.File_Type;
      dossier_list : in out entry_crate.Map) return Boolean;

   --  Return True if directory was pre-created by ravenports
   function directory_excluded (port_prefix, candidate : String) return Boolean;

   --  Return True if the file is a created during an automated process
   function file_excluded (localbase, candidate : String) return Boolean;

   --  Set verified to true
   procedure mark_verified (key : HT.Text; Element : in out entry_record);

   --  Remove keywords and other alterations
   function modify_file_if_necessary (port_prefix, original : String) return String;

   --  If "raw" starts with "/" then assume it's already an absolute path, but remove the
   --  leading slash before returning.  Otherwise assume it's missing the prefix and
   --  prepend "raw" with the prefix minus the leading slash.
   function convert_to_absolute_path (port_prefix, raw : String) return String;

end PortScan.Tests;
