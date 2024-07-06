--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: /License.txt

with admtypes;
with ThickUCL;

use admtypes;

package UCL_Operations is

   --  Returns true if trigger_metadata represents a set of valid triggers
   function trigger_file_is_valid
     (trigger_metadata : ThickUCL.UclTree) return Boolean;

   function message_file_is_valid
     (message_metadata : ThickUCL.UclTree) return Boolean;

   function script_file_is_valid
     (script_metadata : ThickUCL.UclTree) return Boolean;

   --  Transfer data from trigger file to the package metadata
   procedure transfer_triggers
     (trigger_metadata : ThickUCL.UclTree;
      metatree         : in out ThickUCL.UclTree);

   --  Transfer data from message file to the package metadata
   procedure transfer_messages
     (message_metadata : ThickUCL.UclTree;
      metatree         : in out ThickUCL.UclTree);

   --  Transfer data from script file to the package metadata
   procedure transfer_scripts
     (script_metadata  : ThickUCL.UclTree;
      metatree         : in out ThickUCL.UclTree);

   --  Scans for files in <ravensrcdir>/files
   --  Looks for triggers-*.ucl(.in)
   --  Looks for messages-*.ucl(.in)
   --  Looks for scripts-*.ucl(.in)
   --  For each found, runs validation check.
   --  If any check fails, send error message to standard out and set to False.
   function port_ucl_files_valid
     (ravensrcdir : String) return Boolean;

   --  Parses metadata string to convert to UCL tree, then populates the metadata needed
   --  for archive file checks.
   procedure extract_ADO
     (metadata_string : String;
      metadata        : in out ADO_Data);

private

   KEY_CLEANUP   : constant String := "cleanup";
   KEY_TRIGGER   : constant String := "trigger";
   KEY_DIR_PATH  : constant String := "dir_path";
   KEY_FILE_PATH : constant String := "file_path";
   KEY_FILE_GLOB : constant String := "file_glob";
   KEY_FILE_REGX : constant String := "file_regexp";

   KEY_MESSAGE   : constant String := "message";
   KEY_TYPE      : constant String := "type";
   KEY_MINVER    : constant String := "min_version";
   KEY_MAXVER    : constant String := "max_version";

   KEY_CODE      : constant String := "code";
   KEY_ARGS      : constant String := "args";

   function valid_trigger_object
     (trigger_metadata : ThickUCL.UclTree;
      ondx : ThickUCL.object_index) return Boolean;

   function valid_message_object
     (message_metadata : ThickUCL.UclTree;
      ondx : ThickUCL.object_index) return Boolean;

   function valid_script_object
     (script_metadata : ThickUCL.UclTree;
      ondx : ThickUCL.object_index) return Boolean;

end UCL_Operations;
