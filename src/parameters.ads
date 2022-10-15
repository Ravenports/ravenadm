--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions;  use Definitions;

with HelperText;
private with Utilities;

package Parameters is

   package HT renames HelperText;

   no_ccache     : constant String := "none";
   no_unkindness : constant String := "none";
   raven_confdir : constant String := host_localbase & "/etc/ravenadm";
   latest_tag    : constant String := "LATEST";

   type configuration_record is
      record
         profile         : HT.Text;
         dir_sysroot     : HT.Text;
         dir_toolchain   : HT.Text;
         dir_localbase   : HT.Text;
         dir_conspiracy  : HT.Text;
         dir_unkindness  : HT.Text;
         dir_distfiles   : HT.Text;
         dir_packages    : HT.Text;
         dir_ccache      : HT.Text;
         dir_buildbase   : HT.Text;
         dir_profile     : HT.Text;
         dir_logs        : HT.Text;
         dir_options     : HT.Text;
         num_builders    : builders;
         jobs_limit      : builders;
         avoid_tmpfs     : Boolean;
         defer_prebuilt  : Boolean;
         avec_ncurses    : Boolean;
         record_options  : Boolean;
         batch_mode      : Boolean;

         --  defaults
         def_firebird    : HT.Text;
         def_lua         : HT.Text;
         def_mysql_group : HT.Text;
         def_perl        : HT.Text;
         def_php         : HT.Text;
         def_postgresql  : HT.Text;
         def_python3     : HT.Text;
         def_ruby        : HT.Text;
         def_ssl         : HT.Text;
         def_tcl_tk      : HT.Text;

         --  Computed, not saved
         number_cores    : cpu_range;
         dir_repository  : HT.Text;
         sysroot_pkg8    : HT.Text;
      end record;

   configuration  : configuration_record;
   active_profile : HT.Text;

   --  Gentoo linux puts chroot in /usr/bin when ever other system has the program or
   --  a symlink at /usr/sbin.  Precreate the longest string for the command.
   chroot_cmd     : String := "/usr/sbin/chroot ";

   --  Return true if configuration file exists.
   --  Reason: if it doesn't, we need to check privileges because root is needed
   --          to pre-create the first version
   function configuration_exists return Boolean;

   --  This procedure will create a default configuration file if one
   --  does not already exist, otherwise it will it load it.  In every case,
   --  the "configuration" record will be populated after this is run.
   --  returns "True" on success
   function load_configuration return Boolean;

   --  Maybe a previously valid directory path has been removed.  This
   --  function returns true when all the paths still work.
   --  The configuration must be loaded before it's run, of course.
   function all_paths_valid (skip_mk_check : Boolean) return Boolean;

   --  Return true if the localbase is set to someplace it really shouldn't be
   function forbidden_localbase (candidate : String) return Boolean;

   --  Return a profile record filled with dynamic defaults.
   function default_profile (new_profile : String) return configuration_record;

   --  Delete any existing profile data and create a new profile.
   --  Typically a save operation follows.
   procedure insert_profile (confrec : configuration_record);

   --  Create or overwrite a complete ravenadm.ini file using internal data at IFM
   procedure rewrite_configuration;

   --  Return True if 3 or more sections exist (1 is global, the rest must be profiles)
   function alternative_profiles_exist return Boolean;

   --  Return a LF-delimited list of profiles contained in ravenadm.ini
   function list_profiles return String;

   --  Remove an entire profile from the configuration and save it.
   procedure delete_profile (profile : String);

   --  Updates master section with new profile name and initiates a transfer
   procedure switch_profile (to_profile : String);

   --  Returns SSL selection (converts "floating" to default)
   function ssl_selection (confrec : in configuration_record) return String;

private

   package UTL renames Utilities;

   memory_probe : exception;
   profile_DNE  : exception;

   memory_megs   : Natural := 0;

    --  Default Sizing by number of CPUS
   --      1 CPU ::  1 Builder,  1 job  per builder
   --    2/3 CPU ::  2 builders, 2 jobs per builder
   --    4/5 CPU ::  3 builders, 3 jobs per builder
   --    6/7 CPU ::  4 builders, 3 jobs per builder
   --    8/9 CPU ::  6 builders, 4 jobs per builder
   --  10/11 CPU ::  8 builders, 4 jobs per builder
   --    12+ CPU :: floor (75% * CPU), 5 jobs per builder

   Field_01 : constant String := "directory_sysroot";
   Field_16 : constant String := "directory_toolchain";
   Field_02 : constant String := "directory_localbase";
   Field_03 : constant String := "directory_conspiracy";
   Field_04 : constant String := "directory_unkindness";
   Field_05 : constant String := "directory_distfiles";
   Field_06 : constant String := "directory_profile";
   Field_07 : constant String := "directory_packages";
   Field_08 : constant String := "directory_ccache";
   Field_09 : constant String := "directory_buildbase";
   Field_10 : constant String := "number_of_builders";
   Field_11 : constant String := "max_jobs_per_builder";
   Field_12 : constant String := "avoid_tmpfs";
   Field_13 : constant String := "leverage_prebuilt";
   Field_14 : constant String := "display_with_ncurses";
   Field_15 : constant String := "record_default_options";
   Field_27 : constant String := "assume_default_options";

   Field_17 : constant String := "default_firebird";
   Field_18 : constant String := "default_lua";
   Field_19 : constant String := "default_mysql_group";
   Field_20 : constant String := "default_perl";
   Field_21 : constant String := "default_php";
   Field_22 : constant String := "default_postgresql";
   Field_23 : constant String := "default_python3";
   Field_24 : constant String := "default_ruby";
   Field_25 : constant String := "default_ssl";
   Field_26 : constant String := "default_tcl_tk";

   global_01 : constant String := "profile_selected";
   global_02 : constant String := "url_conspiracy";

   first_profile  : constant String := "primary";
   master_section : constant String := "Global Configuration";
   pri_packages   : constant String := raven_var & "/[X]/packages";
   pri_profile    : constant String := raven_var & "/[X]";
   pri_buildbase  : constant String := raven_var & "/builders";
   ravenadm_ini   : constant String := "ravenadm.ini";
   conf_location  : constant String := raven_confdir & "/" & ravenadm_ini;
   std_localbase  : constant String := "/raven";
   std_distfiles  : constant String := raven_var & "/distfiles";
   std_conspiracy : constant String := raven_var & "/conspiracy";
   std_sysroot    : constant String := std_localbase & "/share/raven/sysroot/" &
                                       UTL.mixed_opsys (platform_type);
   std_toolchain  : constant String := std_localbase & "/share/raven/toolchain";

   procedure query_physical_memory;
   procedure query_physical_memory_linux;
   procedure query_physical_memory_sunos;
   function enough_memory (num_builders : builders) return Boolean;

   procedure default_parallelism
     (num_cores        : cpu_range;
      num_builders     : out Integer;
      jobs_per_builder : out Integer);

   --  Copy from IFM to configuration record, updating type as necessary.
   --  If values are missing, use default values.
   --  If profile in global does not exist, throw exception
   procedure transfer_configuration;

   --  Determine and store number of cores.  It's needed for dynamic configuration and
   --  the value is used in the build cycle as well.
   procedure set_cores;

   --  Platform-specific routines to determine ncpu
   function get_number_cpus return Positive;

   --  Updates the global section to indicate active profile
   procedure change_active_profile (new_active_profile : String);

   --  Set chroot to /usr/bin/chroot if program is found
   --  If not, chroot remains set to /usr/sbin/chroot
   procedure set_chroot;

end Parameters;
