--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: /License.txt

with ThickUCL;

package ucl_operations is

   --  Returns true if trigger_metadata represents a set of valid triggers
   function trigger_file_is_valid
     (trigger_metadata : ThickUCL.UclTree) return Boolean;

   --  Transfer data from trigger file to the package metadata
   procedure transfer_triggers
     (trigger_metadata : ThickUCL.UclTree;
      metatree         : in out ThickUCL.UclTree);

private

   KEY_CLEANUP   : constant String := "cleanup";
   KEY_TRIGGER   : constant String := "trigger";
   KEY_DIR_PATH  : constant String := "dir_path";
   KEY_FILE_PATH : constant String := "file_path";
   KEY_FILE_GLOB : constant String := "file_glob";
   KEY_FILE_REGX : constant String := "file_regexp";

   function valid_trigger_object
     (trigger_metadata : ThickUCL.UclTree;
      ondx : ThickUCL.object_index) return Boolean;

end ucl_operations;
