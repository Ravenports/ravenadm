--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Directories;
with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Text_IO;
with INI_File_Manager;
with GNAT.OS_Lib;
with Unix;

package body Parameters is

   package EX  renames Ada.Exceptions;
   package DIR renames Ada.Directories;
   package LAT renames Ada.Characters.Latin_1;
   package TIO renames Ada.Text_IO;
   package OSL renames GNAT.OS_Lib;
   package IFM renames INI_File_Manager;

   --------------------------------------------------------------------------------------------
   --  all_paths_valid
   --------------------------------------------------------------------------------------------
   function all_paths_valid return Boolean
   is
      function invalid_directory (folder : HT.Text; desc : String) return Boolean;

      use type DIR.File_Kind;

      localbase : String := HT.USS (configuration.dir_localbase);

      function invalid_directory (folder : HT.Text; desc : String) return Boolean
      is
         dossier : constant String := HT.USS (folder);
         errmsg : constant String := "Configuration invalid: ";
      begin
         if DIR.Exists (dossier) and then DIR.Kind (dossier) = DIR.Directory then
            return False;
         else
            TIO.Put_Line (errmsg & desc & " directory: " & dossier);
            return True;
         end if;
      end invalid_directory;
   begin
      if HT.USS (configuration.dir_sysroot) = "/" then
         TIO.Put_Line ("[A] System root cannot be " & LAT.Quotation & "/" & LAT.Quotation);
         return False;
      elsif forbidden_localbase (localbase) then
         TIO.Put_Line ("[B] Localbase set to standard system folder '" & localbase & "'");
         return False;
      elsif invalid_directory (configuration.dir_sysroot, "[A] System root") then
         return False;
      elsif invalid_directory (configuration.dir_conspiracy, "[C] Conspiracy") then
         return False;
      elsif invalid_directory (configuration.dir_distfiles, "[E] Distfiles") then
         return False;
      elsif invalid_directory (configuration.dir_packages, "[F] Packages") then
         return False;
      elsif invalid_directory (configuration.dir_logs, "[I] Build logs") then
         return False;
      end if;

      if not (HT.USS (configuration.dir_ccache) = no_ccache) and then
        invalid_directory (configuration.dir_ccache, "[G] Compiler cache")
      then
         return False;
      end if;

      if not (HT.USS (configuration.dir_unkindness) = no_unkindness) and then
        invalid_directory (configuration.dir_unkindness, "[D] Custom ports")
      then
         return False;
      end if;

      return True;
   end all_paths_valid;


   --------------------------------------------------------------------------------------------
   --  load_configuration
   --------------------------------------------------------------------------------------------
   function load_configuration return Boolean is
   begin
      set_cores;
      if DIR.Exists (conf_location) then
         begin
            IFM.scan_file (raven_confdir, ravenadm_ini);
         exception
            when ini : others =>
               TIO.Put_Line (ravenadm_ini & " parse issue: " & EX.Exception_Message (ini));
               return False;
         end;
         declare
            selected_profile : String := IFM.show_value (master_section, global_01);
            envprofile       : String := OSL.Getenv ("SYNTHPROFILE").all;
         begin
            if envprofile /= "" and then IFM.section_exists (envprofile) then
               change_active_profile (envprofile);
            else
               if selected_profile = "" then
                  TIO.Put_Line (ravenadm_ini & ": missing global profile_selected field.");
                  return False;
               end if;
               if not IFM.section_exists (selected_profile) then
                  TIO.Put_Line (ravenadm_ini & ": profile_selected set to non-existent profile.");
                  return False;
               end if;
            end if;
         end;
      else
         insert_profile (default_profile (first_profile));
         change_active_profile (first_profile);
         rewrite_configuration;
      end if;
      transfer_configuration;
      return True;
   end load_configuration;


   --------------------------------------------------------------------------------------------
   --  forbidden_localbase
   --------------------------------------------------------------------------------------------
   function forbidden_localbase (candidate : String) return Boolean
   is
      function downstream (path : String) return Boolean;

      function downstream (path : String) return Boolean is
      begin
         return candidate = path or else HT.leads (candidate, path & "/");
      end downstream;
   begin
      if candidate (candidate'First) /= LAT.Solidus then
         return True;
      end if;
      return
        downstream ("/bin") or else
        downstream ("/boot") or else
        downstream ("/dev") or else
        downstream ("/etc") or else
        downstream ("/lib") or else
        downstream ("/libexec") or else
        downstream ("/proc") or else
        downstream ("/sbin") or else
        downstream ("/sys") or else
        downstream ("/tmp") or else
        downstream ("/var") or else
        downstream ("/usr/bin") or else
        downstream ("/usr/dports") or else
        downstream ("/usr/games") or else
        downstream ("/usr/include") or else
        downstream ("/usr/lib") or else
        downstream ("/usr/libdata") or else
        downstream ("/usr/libexec") or else
        downstream ("/usr/obj") or else
        downstream ("/usr/pkgsrc") or else
        downstream ("/usr/ports") or else
        downstream ("/usr/sbin") or else
        downstream ("/usr/share") or else
        downstream ("/usr/src");
   end forbidden_localbase;


   --------------------------------------------------------------------------------------------
   --  query_physical_memory
   --------------------------------------------------------------------------------------------
   function alternative_profiles_exist return Boolean is
   begin
      return IFM.section_count > 2;
   end alternative_profiles_exist;


   --------------------------------------------------------------------------------------------
   --  query_physical_memory
   --------------------------------------------------------------------------------------------
   procedure query_physical_memory is
      --  Works for *BSD, DragonFly, Bitrig
      --  DF/Free "hw.physmem: 8525971456"  (8G)
      --  NetBSD  "hw.physmem = 1073278976" (1G)
      command : constant String := "/sbin/sysctl hw.physmem";
      content : HT.Text;
      status  : Integer;
   begin
      memory_megs := 1024;
      content := Unix.piped_command (command, status);
      if status /= 0 then
         TIO.Put_Line ("command failed: " & command);
         return;
      end if;
      declare
         type styles is (unknown, dragonfly, netbsd);
         function get_number_string return String;
         style    : styles := unknown;
         response : constant String :=
           HT.USS (content) (1 .. HT.SU.Length (content) - 1);
         SP1 : constant String (1 .. 2) := (1 => LAT.Colon,
                                            2 => LAT.Space);
         SP2 : constant String (1 .. 2) := (1 => LAT.Equals_Sign,
                                            2 => LAT.Space);
         function get_number_string return String is
         begin
            case style is
               when dragonfly => return HT.part_2 (response, SP1);
               when netbsd    => return HT.part_2 (response, SP2);
               when unknown   => return "1073741824";
            end case;
         end get_number_string;
      begin
         if HT.contains (response, SP1) then
            style := dragonfly;
         elsif HT.contains (response, SP2) then
            style := netbsd;
         else
            TIO.Put_Line ("Anomaly, unable to detect physical memory");
            TIO.Put_Line (response);
            return;
         end if;

         declare
            type memtype is mod 2**64;
            numbers : String := get_number_string;
            bytes   : constant memtype := memtype'Value (numbers);
            megs    : constant memtype := bytes / 1024 / 1024;
         begin
            memory_megs := Natural (megs);
         end;
      end;
   end query_physical_memory;


   --------------------------------------------------------------------------------------------
   --  query_physical_memory_linux
   --------------------------------------------------------------------------------------------
   procedure query_physical_memory_linux
   is
      --  On linux, MemTotal should be on first line and that's what we're looking for
      command  : constant String := "/usr/bin/head /proc/meminfo";
      found    : Boolean := False;
      status   : Integer;
      comres   : String := HT.USS (Unix.piped_command (command, status));
      markers  : HT.Line_Markers;
   begin
      if status /= 0 then
         raise memory_probe with command;
      end if;
      HT.initialize_markers (comres, markers);
      loop
         exit when found;
         exit when not HT.next_line_present (comres, markers);
         declare
            line : constant String := HT.trim (HT.extract_line (comres, markers));
         begin
            if line'Length > 12 and then
              line (line'First .. line'First + 8) = "MemTotal:"
            then
               declare
                  type memtype is mod 2**64;
                  sequence  : String := HT.trim (line (line'First + 9 .. line'Last));
                  numbers   : String := HT.part_1 (sequence, " ");
                  kilobytes : constant memtype := memtype'Value (numbers);
                  megs      : constant memtype := kilobytes / 1024;
               begin
                  memory_megs := Natural (megs);
               end;
               found := True;
            end if;
         end;
      end loop;
   end query_physical_memory_linux;


   --------------------------------------------------------------------------------------------
   --  query_physical_memory_sunos
   --------------------------------------------------------------------------------------------
   procedure query_physical_memory_sunos
   is
         --  On Solaris, we're looking for "Memory size" which should be on second line
      command  : constant String := "/usr/sbin/prtconf";
      found    : Boolean := False;
      status   : Integer;
      comres   : String := HT.USS (Unix.piped_command (command, status));
      markers  : HT.Line_Markers;
   begin
      if status /= 0 then
         raise memory_probe with command;
      end if;
      HT.initialize_markers (comres, markers);
      loop
         exit when found;
         exit when not HT.next_line_present (comres, markers);
         declare
            line : constant String := HT.extract_line (comres, markers);
         begin
            if line'Length > 15 and then
              line (line'First .. line'First + 11) = "Memory size:"
            then
               declare
                  type memtype is mod 2**64;
                  numbers   : String := HT.part_1 (line (line'First + 13 .. line'Last), " ");
                  megabytes : constant memtype := memtype'Value (numbers);
               begin
                  memory_megs := Natural (megabytes);
               end;
               found := True;
            end if;
         end;
      end loop;
   end query_physical_memory_sunos;


   --------------------------------------------------------------------------------------------
   --  enough_memory
   --------------------------------------------------------------------------------------------
   function enough_memory (num_builders : builders) return Boolean
   is
      megs_per_slave : Natural;
   begin
      if memory_megs = 0 then
         case platform_type is
            when linux => query_physical_memory_linux;
            when sunos => query_physical_memory_sunos;
            when dragonfly |
                 freebsd   |
                 macos     |
                 netbsd    |
                 openbsd   => query_physical_memory;
         end case;
      end if;
      megs_per_slave := memory_megs / Positive (num_builders);
      return megs_per_slave >= 1280;
   end enough_memory;


   --------------------------------------------------------------------------------------------
   --  enough_memory
   --------------------------------------------------------------------------------------------
   procedure default_parallelism
     (num_cores        : cpu_range;
      num_builders     : out Integer;
      jobs_per_builder : out Integer) is
   begin
      case num_cores is
         when 1 =>
            num_builders := 1;
            jobs_per_builder := 1;
         when 2 | 3 =>
            num_builders := 2;
            jobs_per_builder := 2;
         when 4 | 5 =>
            num_builders := 3;
            jobs_per_builder := 3;
         when 6 | 7 =>
            num_builders := 4;
            jobs_per_builder := 3;
         when 8 | 9 =>
            num_builders := 6;
            jobs_per_builder := 4;
         when 10 | 11 =>
            num_builders := 8;
            jobs_per_builder := 4;
         when others =>
            num_builders := (Integer (num_cores) * 3) / 4;
            jobs_per_builder := 5;
      end case;
   end default_parallelism;


   --------------------------------------------------------------------------------------------
   --  default_profile
   --------------------------------------------------------------------------------------------
   function default_profile (new_profile : String) return configuration_record
   is
      result       : configuration_record;
      def_builders : Integer;
      def_jlimit   : Integer;
   begin
      default_parallelism (num_cores        => configuration.number_cores,
                           num_builders     => def_builders,
                           jobs_per_builder => def_jlimit);

      result.profile        := HT.SUS (new_profile);
      result.dir_sysroot    := HT.SUS (std_sysroot);
      result.dir_localbase  := HT.SUS (std_localbase);
      result.dir_conspiracy := HT.SUS (std_conspiracy);
      result.dir_unkindness := HT.SUS (no_unkindness);
      result.dir_distfiles  := HT.SUS (std_distfiles);
      result.dir_packages   := HT.replace_substring (HT.SUS (pri_packages), "[X]", new_profile);
      result.dir_ccache     := HT.SUS (no_ccache);
      result.dir_buildbase  := HT.SUS (pri_buildbase);
      result.dir_logs       := HT.replace_substring (HT.SUS (pri_logs), "[X]", new_profile);
      result.num_builders   := builders (def_builders);
      result.jobs_limit     := builders (def_jlimit);
      result.avoid_tmpfs    := not enough_memory (builders (def_builders));
      result.avec_ncurses   := True;
      result.defer_prebuilt := False;
      result.record_options := False;

      return result;
   end default_profile;


   --------------------------------------------------------------------------------------------
   --  insert_profile
   --------------------------------------------------------------------------------------------
   procedure insert_profile (confrec : configuration_record)
   is
      function may_be_disabled (directory : HT.Text; disabled_value : String) return String;
      function set_builder (X : builders) return String;
      function set_boolean (value : Boolean) return String;

      profile_name : constant String := HT.USS (confrec.profile);

      function may_be_disabled (directory : HT.Text; disabled_value : String) return String is
      begin
         if HT.equivalent (directory, HT.blank) then
            return disabled_value;
         else
            return HT.USS (directory);
         end if;
      end may_be_disabled;

      function set_builder (X : builders) return String is
      begin
         return HT.int2str (Integer (X));
      end set_builder;

      function set_boolean (value : Boolean) return String is
      begin
         case value is
            when True  => return "true";
            when False => return "false";
         end case;
      end set_boolean;
   begin
      IFM.delete_section (profile_name);
      IFM.insert_or_update (profile_name, Field_01, HT.USS (confrec.dir_sysroot));
      IFM.insert_or_update (profile_name, Field_02, HT.USS (confrec.dir_localbase));
      IFM.insert_or_update (profile_name, Field_03, HT.USS (confrec.dir_conspiracy));
      IFM.insert_or_update (profile_name, Field_04,
                            may_be_disabled (confrec.dir_unkindness, no_unkindness));
      IFM.insert_or_update (profile_name, Field_05, HT.USS (confrec.dir_distfiles));
      IFM.insert_or_update (profile_name, Field_06, HT.USS (confrec.dir_packages));
      IFM.insert_or_update (profile_name, Field_07,
                            may_be_disabled (confrec.dir_ccache, no_ccache));
      IFM.insert_or_update (profile_name, Field_08, HT.USS (confrec.dir_buildbase));
      IFM.insert_or_update (profile_name, Field_09, HT.USS (confrec.dir_logs));
      IFM.insert_or_update (profile_name, Field_10, set_builder (confrec.num_builders));
      IFM.insert_or_update (profile_name, Field_11, set_builder (confrec.jobs_limit));
      IFM.insert_or_update (profile_name, Field_12, set_boolean (confrec.avoid_tmpfs));
      IFM.insert_or_update (profile_name, Field_13, set_boolean (confrec.record_options));
      IFM.insert_or_update (profile_name, Field_14, set_boolean (confrec.avec_ncurses));
      IFM.insert_or_update (profile_name, Field_15, set_boolean (confrec.defer_prebuilt));
   end insert_profile;


   --------------------------------------------------------------------------------------------
   --  change_active_profile
   --------------------------------------------------------------------------------------------
   procedure change_active_profile (new_active_profile : String) is
   begin
      IFM.insert_or_update (master_section, global_01, new_active_profile);
   end change_active_profile;


   --------------------------------------------------------------------------------------------
   --  rewrite_configuration
   --------------------------------------------------------------------------------------------
   procedure rewrite_configuration
   is
      comment : String := "This ravenadm configuration file is automatically generated";
   begin
      IFM.scribe_file (directory     => raven_confdir,
                       filename      => ravenadm_ini,
                       first_comment => comment);
   end rewrite_configuration;


   --------------------------------------------------------------------------------------------
   --  transfer_configuration
   --------------------------------------------------------------------------------------------
   procedure transfer_configuration
   is
      function default_string  (field_name : String; default : String) return HT.Text;
      function default_builder (field_name : String; default : Integer) return builders;
      function default_boolean (field_name : String; default : Boolean) return Boolean;
      function tmpfs_transfer return Boolean;

      def_builders : Integer;
      def_jlimit   : Integer;
      profile      : constant String := IFM.show_value (section => master_section,
                                                        name    => global_01);

      function default_string (field_name : String; default : String) return HT.Text
      is
         value : String := IFM.show_value (profile, field_name);
      begin
         if value = "" then
            return HT.SUS (default);
         else
            return HT.SUS (value);
         end if;
      end default_string;

      function default_builder (field_name : String; default : Integer) return builders
      is
         value     : String := IFM.show_value (profile, field_name);
         converted : Integer;
      begin
         if value = "" then
            return builders (default);
         else
            converted := Integer'Value (value);
            return builders (converted);
         end if;
      exception
         when others =>
            return builders (default);
      end default_builder;

      function default_boolean (field_name : String; default : Boolean) return Boolean
      is
         value       : String := IFM.show_value (profile, field_name);
         lower_value : String := HT.lowercase (value);
      begin
         if value = "" then
            return default;
         else
            if lower_value = "true" then
               return True;
            elsif lower_value = "false" then
               return False;
            else
               return default;
            end if;
         end if;
      end default_boolean;

      function tmpfs_transfer return Boolean
      is
         value       : String := IFM.show_value (profile, Field_12);
         lower_value : String := HT.lowercase (value);
      begin
         if value = "" then
            return not enough_memory (builders (def_builders));
         else
            if lower_value = "true" then
               return True;
            elsif lower_value = "false" then
               return False;
            else
               return not enough_memory (builders (def_builders));
            end if;
         end if;
      end tmpfs_transfer;
   begin
      if profile = "" then
         raise profile_DNE;
      end if;

      default_parallelism (num_cores        => configuration.number_cores,
                           num_builders     => def_builders,
                           jobs_per_builder => def_jlimit);

      active_profile := HT.SUS (profile);
      configuration.profile        := active_profile;
      configuration.dir_sysroot    := default_string (Field_01, std_sysroot);
      configuration.dir_localbase  := default_string (Field_02, std_localbase);
      configuration.dir_conspiracy := default_string (Field_03, std_conspiracy);
      configuration.dir_unkindness := default_string (Field_04, no_unkindness);
      configuration.dir_distfiles  := default_string (Field_05, std_distfiles);
      configuration.dir_packages   := default_string (Field_06, pri_packages);
      configuration.dir_ccache     := default_string (Field_07, no_ccache);
      configuration.dir_buildbase  := default_string (Field_08, pri_buildbase);
      configuration.dir_logs       := default_string (Field_09, pri_logs);
      configuration.num_builders   := default_builder (Field_10, def_builders);
      configuration.jobs_limit     := default_builder (Field_11, def_jlimit);
      configuration.avoid_tmpfs    := tmpfs_transfer;
      configuration.record_options := default_boolean (Field_13, False);
      configuration.avec_ncurses   := default_boolean (Field_14, True);
      configuration.defer_prebuilt := default_boolean (Field_15, False);

   end transfer_configuration;


   --------------------------------------------------------------------------------------------
   --  set_cores
   --------------------------------------------------------------------------------------------
   procedure set_cores
   is
      number : constant Positive := get_number_cpus;
   begin
      if number > Positive (cpu_range'Last) then
         configuration.number_cores := cpu_range'Last;
      else
         configuration.number_cores := cpu_range (number);
      end if;
   end set_cores;


   --------------------------------------------------------------------------------------------
   --  get_number_cpus
   --------------------------------------------------------------------------------------------
   function get_number_cpus return Positive
   is
      bsd_cmd : constant String := "/sbin/sysctl hw.ncpu";
      lin_cmd : constant String := "/usr/bin/nproc";
      sol_cmd : constant String := "/usr/sbin/psrinfo -p";
      comres  : HT.Text;
      status  : Integer;
      start   : Positive;
   begin
      --  DF/Free: expected output: "hw.ncpu: C" where C is integer
      --  NetBSD:  expected output: "hw.ncpu = C"
      --  OpenBSD: expected output: "hw.ncpu=C"
      --  Linux:   expected output: "C"
      --  Solaris: expected output: "C"

      case platform_type is
         when dragonfly | freebsd | macos | netbsd | openbsd =>
            comres := Unix.piped_command (bsd_cmd, status);
         when linux =>
            comres := Unix.piped_command (lin_cmd, status);
         when sunos =>
            comres := Unix.piped_command (sol_cmd, status);
      end case;

      case platform_type is
         when dragonfly | freebsd | macos => start := 10;
         when sunos | linux               => start := 1;
         when netbsd                      => start := 11;
         when openbsd                     => start := 9;
      end case;

      if status /= 0 then
         return 1;
      end if;
      declare
         resstr : String := HT.USS (comres);
         ncpu   : String := resstr (start .. resstr'Last - 1);
         number : Positive := Integer'Value (ncpu);
      begin
         return number;
      exception
         when others => return 1;
      end;
   end get_number_cpus;


   --------------------------------------------------------------------------------------------
   --  list_profiles
   --------------------------------------------------------------------------------------------
   function list_profiles return String
   is
      result : HT.Text;
      total_sections : Natural := IFM.section_count;
   begin
      for index in Positive range 1 .. total_sections loop
         declare
            section : String := IFM.section_name (index);
         begin
            if section /= master_section then
               HT.SU.Append (result, section & LAT.LF);
            end if;
         end;
      end loop;
      return HT.USS (result);
   end list_profiles;


   --------------------------------------------------------------------------------------------
   --  delete_profile
   --------------------------------------------------------------------------------------------
   procedure delete_profile (profile : String) is
   begin
      IFM.delete_section (profile);
      rewrite_configuration;
   end delete_profile;


   --------------------------------------------------------------------------------------------
   --  switch_profile
   --------------------------------------------------------------------------------------------
   procedure switch_profile (to_profile : String) is
   begin
      change_active_profile (new_active_profile => to_profile);
      transfer_configuration;
   end switch_profile;

end Parameters;
