--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Directories;
with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Text_IO;
with INI_File_Manager;
with Unix;

package body Parameters is

   package EX  renames Ada.Exceptions;
   package DIR renames Ada.Directories;
   package LAT renames Ada.Characters.Latin_1;
   package TIO renames Ada.Text_IO;
   package IFM renames INI_File_Manager;

   --------------------------------------------------------------------------------------------
   --  all_paths_valid
   --------------------------------------------------------------------------------------------
   function all_paths_valid return Boolean
   is
      function invalid_directory (folder : HT.Text; desc : String) return Boolean;

      use type DIR.File_Kind;

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
         TIO.Put_Line ("[G] System root cannot be " & LAT.Quotation & "/" & LAT.Quotation);
         return False;
      elsif invalid_directory (configuration.dir_sysroot, "[G] System root") then
         return False;
      elsif invalid_directory (configuration.dir_packages, "[B] Packages") then
         return False;
      elsif invalid_directory (configuration.dir_conspiracy, "[A] Conspiracy") then
         return False;
      elsif invalid_directory (configuration.dir_distfiles, "[C] Distfiles") then
         return False;
      elsif invalid_directory (configuration.dir_logs, "[E] Build logs") then
         return False;
      end if;

      if HT.USS (configuration.dir_ccache) = no_ccache then
         return True;
      else
         return not invalid_directory (configuration.dir_ccache, "[H] Compiler cache");
      end if;
   end all_paths_valid;


   --------------------------------------------------------------------------------------------
   --  load_configuration
   --------------------------------------------------------------------------------------------
   function load_configuration (num_cores : cpu_range) return Boolean
   is
      if DIR.Exists (conf_location) then
         begin
            IFM.scan_file (raven_confdir, ravenadm_ini);
         exception
            when ini : others =>
               TIO.Put_Line (ravenadm_ini & " parse issue: " & EX.Exception_Message (ini));
               return False;
         end;
      else

      end if;
   begin

   end load_configuration;

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
   function default_profile
     (new_profile : String;
      num_cores   : cpu_range) return configuration_record
   is
      result       : configuration_record;
      def_builders : Integer;
      def_jlimit   : Integer;
   begin
      default_parallelism (num_cores        => num_cores,
                           num_builders     => def_builders,
                           jobs_per_builder => def_jlimit);

      result.profile        := HT.SUS (new_profile);
      result.dir_sysroot    := HT.SUS (std_sysroot);
      result.dir_localbase  := HT.SUS (std_localbase);
      result.dir_conspiracy := HT.SUS (std_conspiracy);
      result.dir_unkindness := HT.SUS (no_unkindness);
      result.dir_distfiles  := HT.SUS (std_distfiles);
      result.dir_packages   := HT.SUS (pri_packages);
      result.dir_ccache     := HT.SUS (no_ccache);
      result.dir_buildbase  := HT.SUS (pri_buildbase);
      result.dir_logs       := HT.SUS (pri_logs);
      result.num_builders   := builders (def_builders);
      result.jobs_limit     := builders (def_jlimit);
      result.avoid_tmpfs    := not enough_memory (result.num_builders);
      result.avec_ncurses   := True;
      result.defer_prebuilt := False;

      return result;
   end default_profile;

end Parameters;
