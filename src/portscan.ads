--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions; use Definitions;
with Ada.Text_IO;

private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Ordered_Sets;
private with Ada.Containers.Vectors;
private with HelperText;

--  GCC 6.0 only
--  pragma Suppress (Tampering_Check);

package PortScan is

   package TIO renames Ada.Text_IO;

   scan_log_error : exception;

   type count_type is (total, success, failure, ignored, skipped);

   type port_id is private;

   --  DELETE-ME-LATER
   first_port : constant port_id;
   procedure crash_test_dummy;

private

   package CON renames Ada.Containers;
   package HT  renames HelperText;

   max_ports  : constant := 2000;

   type port_id is range -1 .. max_ports - 1;
   subtype port_index is port_id range 0 .. port_id'Last;

   --  DELETE-ME-LATER
   first_port : constant port_id := port_index'First;

   port_match_failed : constant port_id := port_id'First;

   type dependency_type is (build, buildrun, runtime);
   subtype bucket_code is String (1 .. 2);

   bmake_execution  : exception;
   pkgng_execution  : exception;
   make_garbage     : exception;
   nonexistent_port : exception;
   circular_logic   : exception;
   seek_failure     : exception;
   unknown_format   : exception;

   type queue_record is
      record
         ap_index      : port_index;
         reverse_score : port_index;
      end record;

   --  Functions for ranking_crate definitions
   function "<" (L, R : queue_record) return Boolean;

   --  Functions for portkey_crate and package_crate definitions
   function port_hash (key : HT.Text) return CON.Hash_Type;

   --  Functions for block_crate definitions
   function block_hash (key : port_index) return CON.Hash_Type;
   function block_ekey (left, right : port_index) return Boolean;

   package subqueue is new CON.Vectors
     (Element_Type => port_index,
      Index_Type   => port_index);

   package string_crate is new CON.Vectors
     (Element_Type => HT.Text,
      Index_Type   => port_index,
      "="          => HT.SU."=");

   package ranking_crate is new CON.Ordered_Sets
     (Element_Type => queue_record);

   package portkey_crate is new CON.Hashed_Maps
     (Key_Type        => HT.Text,
      Element_Type    => port_index,
      Hash            => port_hash,
      Equivalent_Keys => HT.equivalent);

   package package_crate is new CON.Hashed_Maps
     (Key_Type        => HT.Text,
      Element_Type    => Boolean,
      Hash            => port_hash,
      Equivalent_Keys => HT.equivalent);

   package block_crate is new CON.Hashed_Maps
     (Key_Type        => port_index,
      Element_Type    => port_index,
      Hash            => block_hash,
      Equivalent_Keys => block_ekey);

   type port_record is
      record
         sequence_id   : port_index           := 0;
         key_cursor    : portkey_crate.Cursor := portkey_crate.No_Element;
         ignore_reason : HT.Text              := HT.blank;
         pkgversion    : HT.Text              := HT.blank;
         port_variant  : HT.Text              := HT.blank;
         port_namebase : HT.Text              := HT.blank;
         bucket        : bucket_code          := "00";
         unkind_custom : Boolean              := False;
         ignored       : Boolean              := False;
         scanned       : Boolean              := False;
         rev_scanned   : Boolean              := False;
         unlist_failed : Boolean              := False;
         work_locked   : Boolean              := False;
         scan_locked   : Boolean              := False;
         pkg_present   : Boolean              := False;
         remote_pkg    : Boolean              := False;
         never_remote  : Boolean              := False;
         deletion_due  : Boolean              := False;
         use_procfs    : Boolean              := False;
         reverse_score : port_index           := 0;
         min_librun    : Natural              := 0;
         librun        : block_crate.Map;
         blocked_by    : block_crate.Map;
         blocks        : block_crate.Map;
         all_reverse   : block_crate.Map;
         options       : package_crate.Map;
         subpackages   : string_crate.Vector;
      end record;
   type port_record_access is access all port_record;

   type dim_make_queue is array (scanners) of subqueue.Vector;
   type dim_progress   is array (scanners) of port_index;
   type dim_all_ports  is array (port_index) of aliased port_record;

   all_ports    : dim_all_ports;
   ports_keys   : portkey_crate.Map;
   portlist     : portkey_crate.Map;
   make_queue   : dim_make_queue;
   mq_progress  : dim_progress := (others => 0);
   rank_queue   : ranking_crate.Set;
   last_port    : port_index := 0;

   discerr      : constant String := "Discovery error";
   chroot       : constant String := "/usr/sbin/chroot ";

   function get_port_variant (PR : port_record) return String;

end PortScan;
