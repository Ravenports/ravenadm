--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package PortScan.Scan is

   missing_index  : exception;
   bad_index_data : exception;
   bsheet_parsing : exception;
   populate_error : exception;

   --  Scan the entire conspiracy directory in order with a single, non-recursive pass
   --  Return True on success
   function scan_entire_ports_tree (sysrootver : sysroot_characteristics) return Boolean;

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

private

   type dependency_type is (build, buildrun, runtime, extra_runtime);
   subtype LR_set is dependency_type range buildrun .. extra_runtime;

   --  subroutines for populate_port_data
   procedure prescan_ports_tree
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

   procedure iterate_reverse_deps;
   procedure iterate_drill_down;

   procedure drill_down (next_target : port_index; original_target : port_index);

    --  some helper routines
   function get_max_lots return scanners;
   function convert_tuple_to_portkey (tuple : String) return String;
   function extract_subpackage (tuple : String) return String;

end PortScan.Scan;
