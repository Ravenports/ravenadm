--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package PortScan.Scan is

   missing_index  : exception;
   bad_index_data : exception;
   bsheet_parsing : exception;
   populate_error : exception;

   --  Scan the entire conspiracy and unkindness directories in order with a single,
   --  non-recursive pass using up to 32 parallel scanners.  Return True on success
   function scan_entire_ports_tree (sysrootver : sysroot_characteristics) return Boolean;

   --  Scan entire conspiracy and unkindness directories using parallel scanners in order
   --  to compile a complete unique set of distribution files.  Used for distfile purging.
   --  Return True on success.
   function gather_distfile_set (sysrootver : sysroot_characteristics) return Boolean;

   --  Starting with a single port, recurse to determine a limited but complete
   --  dependency tree.  Repeated calls will augment already existing data.
   --  Return True on success
   function scan_single_port
     (namebase     : String;
      variant      : String;
      always_build : Boolean;
      sysrootver   : sysroot_characteristics;
      fatal        : out Boolean) return Boolean;

   --  This procedure causes the reverse dependencies to be calculated, and
   --  then the extended (recursive) reverse dependencies.  The former is
   --  used progressively to determine when a port is free to build and the
   --  latter sets the build priority.
   procedure set_build_priority;

   --  Iterate through portlist and scan each individual port recursively.
   --  May be interrupted with a a singal.  Returns False if any port fails scan or if
   --  the process was interrupted by a signal.
   function scan_provided_list_of_ports
     (always_build : Boolean;
      sysrootver   : sysroot_characteristics) return Boolean;

   --  Linearly scan through entire conspiracy directory and generate a new index
   procedure generate_conspiracy_index (sysrootver : sysroot_characteristics);

   --  List every port to be built and the final tally.
   procedure display_results_of_dry_run;

   --  Scan distfiles directory, then purge all obsolete distfiles.
   procedure purge_obsolete_distfiles;

   --  Scan directory that contains the packages (*.txz) and stores the
   --  file names in the container.  Returns False if no packages are found.
   function scan_repository (repository : String) return Boolean;

private

   type dependency_type is (build, buildrun, runtime, extra_runtime);
   subtype LR_set is dependency_type range buildrun .. extra_runtime;
   type verdiff is (newbuild, rebuild, change);
   subtype AF is Integer range 0 .. 15;
   type disktype is mod 2**64;

   conspindex    : constant String := "/Mk/Misc/conspiracy_variants";

   --  subroutines for populate_port_data
   procedure prescan_ports_tree
     (conspiracy : String;
      unkindness : String;
      sysrootver : sysroot_characteristics);

   procedure prescan_conspiracy_index_for_distfiles
     (conspiracy : String;
      unkindness : String;
      sysrootver : sysroot_characteristics);

   procedure prescan_unkindness (unkindness : String);

   procedure prescan_custom
     (unkindness    : String;
      bucket        : bucket_code;
      namebase      : String;
      max_lots      : scanners);

   procedure populate_port_data
     (conspiracy    : String;
      unkindness    : String;
      target        : port_index;
      always_build  : Boolean;
      sysrootver    : sysroot_characteristics);

   procedure parallel_deep_scan
     (conspiracy    : String;
      unkindness    : String;
      sysrootver    : sysroot_characteristics;
      success       : out Boolean;
      show_progress : Boolean);

   procedure populate_set_depends
     (target        : port_index;
      tuple         : String;
      dtype         : dependency_type);

   procedure populate_option
     (target        : port_index;
      option_name   : String;
      setting       : Boolean);

   procedure skeleton_compiler_data
     (conspiracy    : String;
      unkindness    : String;
      target        : port_index;
      sysrootver    : sysroot_characteristics);

   procedure iterate_reverse_deps;
   procedure iterate_drill_down;

   procedure drill_down (next_target : port_index; original_target : port_index);

    --  some helper routines
   function get_max_lots return scanners;
   function convert_tuple_to_portkey (tuple : String) return String;
   function extract_subpackage (tuple : String) return String;
   function tohex (value : AF) return Character;
   function display_kmg (number : disktype) return String;

   --  Given a port ID, search for existing package in the packages directory
   --  If the exact package exists, return " (rebuild <version>)"
   --  If no package exists, return " (new)"
   --  If previous package exists, return " (<oldversion> => <version>)"
   function version_difference (id : port_id; kind : out verdiff) return String;

   --  Don't bother with parallel scan on unkindness, just get the distfiles now.
   procedure linear_scan_unkindness_for_distfiles (unkindness : String);

   --  Split conspiracy up equally between available scanners looking for distfiles
   procedure parallel_distfile_scan
     (conspiracy    : String;
      sysrootver    : sysroot_characteristics;
      success       : out Boolean;
      show_progress : Boolean);

end PortScan.Scan;
