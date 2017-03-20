--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package PortScan.Scan is

   missing_index  : exception;
   bad_index_data : exception;
   bsheet_parsing : exception;
   populate_error : exception;

   --  Scan the entire conspiracy directory in order with a single, non-recursive pass
   --  Return True on success
   function scan_entire_ports_tree
     (conspiracy : String;
      unkindness : String;
      sysrootver : sysroot_characteristics) return Boolean;

private

   lot_number   : scanners   := 1;
   lot_counter  : port_index := 0;
   prescanned   : Boolean    := False;

   type dependency_type is (build, buildrun, runtime);
   subtype LR_set is dependency_type range buildrun .. runtime;

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

    --  some helper routines
   function get_max_lots return scanners;
   function scan_progress return String;
   function convert_tuple_to_portkey (tuple : String) return String;

end PortScan.Scan;
