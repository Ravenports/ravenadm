--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions; use Definitions;
with HelperText;
with Ada.Text_IO;

with admtypes; use admtypes;

private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Ordered_Sets;
private with Ada.Containers.Vectors;
private with Unix;

package PortScan is

   package TIO renames Ada.Text_IO;
   package HT  renames HelperText;

   scan_log_error : exception;

   type count_type is (total, success, failure, ignored, skipped);

   type port_id is private;

   type sysroot_characteristics is
      record
         major   : HT.Text;
         release : HT.Text;
         version : HT.Text;
         arch    : supported_arch;
      end record;

   --  Wipe out all scan data so new scan can be performed
   procedure reset_ports_tree;

   --  Sometimes there is nothing left to do after the sanity check.  Let's provide a way to
   --  detect that case.
   function queue_is_empty return Boolean;

   --  Returns the size of the queue before it was pared down.
   function original_queue_size return Natural;

   --  Returns the current queue size
   function queue_length return Integer;

   --  Return the length of the original build request
   function build_request_length return Integer;

   --  Given a port ID, attempt to display the portkey
   function get_port_variant (id : port_id) return String;

   --  Given a port ID, attempt to display the "ignore" reason
   function ignore_reason (id : port_id) return String;

   --  Returns true if the given ID is valid (e.g. not port_match_failed)
   function valid_port_id (id : port_id) return Boolean;

   --  Given a port ID, attempt to return 2-character bucket
   function get_bucket (id : port_id) return String;

   --  Given an ID and specifying a subpackage, this function returns the package file name (NSVV).
   function calculate_package_name (id : port_id; subpackage : String) return String;

   --  Given an ID and specifying a subpackage, this function returns the NSV identifier
   function calculate_nsv (id : port_id; subpackage : String) return String;

   --  Takes origin tuplet (namebase:subpkg:variant) and returns namebase:variant
   function convert_colon_nsv_to_portkey (origin : String) return String;

   --  Takes nvvv package name (namebase-subpkg-variant-version.rvn) and returns subpkg
   function subpackage_from_pkgname (pkgname : String) return String;

   --  Takes nsvv package name (namebase-subpkg-variant-version) and returns portkey
   function convert_pkgname_to_portkey (pkgname : String) return String;

   --  Insert unique NV pair into portlist and dupelist.
   procedure insert_into_portlist (port_variant : String);

   --  Checks all_ports (id) and returns value of use_procfs
   function requires_procfs (id : port_id) return Boolean;

   --  Given an index, returns a buildsheet location wrt unkindness
   function get_buildsheet_from_origin_list (index : Positive) return String;

   --  Returns true if ports binutils (binutils:ravensys) is present in stored origins
   --  The pkg version also has to match $binutilsversion
   function jail_port_binutils_specified return Boolean;

   --  Returns true if ports compiler (gcc9:standard) is present in stored origins
   --  The pkg version also has to match $compiler_version
   function jail_port_compiler_specified return Boolean;

private

   package CON renames Ada.Containers;

   max_ports    : constant := 8000;
   max_packages : constant := 24000;

   type port_id is range -1 .. max_ports - 1;
   subtype port_index is port_id range 0 .. port_id'Last;

   port_match_failed : constant port_id := port_id'First;

   subtype bucket_code is String (1 .. 2);

   type built_package_id is range 0 .. max_packages;

   pkgng_execution  : exception;
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

   package built_package_crate is new CON.Vectors
     (Index_Type   => built_package_id,
      Element_Type => HT.Text,
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

   type subpackage_identifier is
      record
         port          : port_index;
         subpackage    : HT.Text;
      end record;

   package spkg_id_crate is new CON.Vectors
     (Element_Type => subpackage_identifier,
      Index_Type   => Positive);

   type subpackage_record is
      record
         subpackage    : HT.Text := HT.blank;
         pkg_dep_query : string_crate.Vector;
         pkg_present   : Boolean := False;
         remote_pkg    : Boolean := False;
         never_remote  : Boolean := False;
         deletion_due  : Boolean := False;
         spkg_run_deps : spkg_id_crate.Vector;
      end record;

   package subpackage_crate is new CON.Vectors
     (Element_Type => subpackage_record,
      Index_Type   => Positive);

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
         work_started  : Unix.int64           := 0;

         use_procfs    : Boolean              := False;
         reverse_score : port_index           := 0;
         run_deps      : block_crate.Map;
         blocked_by    : block_crate.Map;
         blocks        : block_crate.Map;
         all_reverse   : block_crate.Map;
         options       : package_crate.Map;
         subpackages   : subpackage_crate.Vector;
      end record;

   type dim_make_queue is array (scanners) of subqueue.Vector;
   type dim_progress   is array (scanners) of port_index;
   type dim_all_ports  is array (port_index) of port_record;

   all_ports    : dim_all_ports;
   ports_keys   : portkey_crate.Map;
   make_queue   : dim_make_queue;
   mq_progress  : dim_progress := (others => 0);
   rank_queue   : ranking_crate.Set;
   last_port    : port_index := 0;
   lot_number   : scanners   := 1;
   lot_counter  : port_index := 0;
   prescanned   : Boolean    := False;
   log_list     : string_crate.Vector;
   portlist     : string_crate.Vector;
   dupelist     : string_crate.Vector;
   package_list : built_package_crate.Vector;
   distfile_set : portkey_crate.Map;

   original_queue_len : CON.Count_Type;

   discerr      : constant String := "Discovery error";

   function get_port_variant (PR : port_record) return String;
   function scan_progress return String;
   procedure wipe_make_queue;

end PortScan;
