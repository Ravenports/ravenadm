--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package PortScan.Scan is

   missing_index  : exception;
   bad_index_data : exception;
   bsheet_parsing : exception;

   --  Scan the entire conspiracy directory in order with a single, non-recursive pass
   --  Return True on success
   function scan_entire_ports_tree (conspiracy, unkindness : String) return Boolean;

private

   lot_number   : scanners   := 1;
   lot_counter  : port_index := 0;
   prescanned   : Boolean    := False;

   --  subroutines for populate_port_data
   procedure prescan_ports_tree (conspiracy, unkindness : String);
   procedure prescan_custom (unkindness, buildsheet : String; max_lots : scanners);
   procedure prescan_unkindness (unkindness : String);
   procedure parallel_deep_scan (success : out Boolean; show_progress : Boolean);
--   procedure walk_the_bucket    (conspiracy, unkindness : String; bucket : bucket_code);

    --  some helper routines
   function get_max_lots return scanners;

end PortScan.Scan;
