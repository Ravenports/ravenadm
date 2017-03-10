--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions;   use Definitions;

private with Ada.Text_IO;
private with HelperText;
private with Utilities;

package Replicant is

   scenario_unexpected : exception;

   --  This procedure needs to be run once and sets key variables.
   --  It creates a set of files that each slave will copy in during launch.
   --  It also creates the password database
   procedure initialize
     (testmode  : Boolean;
      num_cores : cpu_range;
      localbase : String);

   --  This removes the password database
   procedure finalize;

   --  Returns True if any mounts are detected (used by pilot)
   function ravenadm_mounts_exist return Boolean;

private

   package HT  renames HelperText;
   package TIO renames Ada.Text_IO;
   package UTL renames Utilities;

   smp_cores      : cpu_range       := cpu_range'First;
   developer_mode : Boolean;
   abn_log_ready  : Boolean;
   abnormal_log   : TIO.File_Type;
   ravenbase      : HT.Text;

   abnormal_cmd_logname : constant String := "05_abnormal_command_output.log";

   reference_base   : constant String := "Base";
   raven_sysroot    : constant String := host_localbase & "/share/raven-sysroot/" &
                                         UTL.mixed_opsys (platform_type);

   --  Query configuration to determine the master mount
   function get_master_mount return String;

   --  capture unexpected output while setting up builders (e.g. mount)
   procedure start_abnormal_logging;
   procedure stop_abnormal_logging;

   --  generic command, throws exception if exit code is not 0
   procedure execute (command : String);
   procedure silent_exec (command : String);
   function internal_system_command (command : String) return HT.Text;

   --  Wrapper for rm -rf <directory>
   procedure annihilate_directory_tree (tree : String);

   --  Used to generic mtree exclusion files
   procedure create_mtree_exc_preconfig (path_to_mm : String);
   procedure create_mtree_exc_preinst (path_to_mm : String);
   procedure write_common_mtree_exclude_base (mtreefile : TIO.File_Type);
   procedure write_preinstall_section (mtreefile : TIO.File_Type);

end Replicant;
