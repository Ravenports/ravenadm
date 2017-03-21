--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Strings.Hash;
with Ada.Characters.Latin_1;

package body PortScan is

   package LAT renames Ada.Characters.Latin_1;

   --------------------------------------------------------------------------------------------
   --  port_hash
   --------------------------------------------------------------------------------------------
   function port_hash (key : HT.Text) return CON.Hash_Type is
   begin
      return Ada.Strings.Hash (HT.USS (key));
   end port_hash;


   --------------------------------------------------------------------------------------------
   --  block_ekey
   --------------------------------------------------------------------------------------------
   function block_hash (key : port_index) return CON.Hash_Type is
       preresult : constant CON.Hash_Type := CON.Hash_Type (key);
      use type CON.Hash_Type;
   begin
      --  Equivalent to mod 128
      return preresult and 2#1111111#;
   end block_hash;


   --------------------------------------------------------------------------------------------
   --  block_ekey
   --------------------------------------------------------------------------------------------
   function block_ekey (left, right : port_index) return Boolean is
   begin
      return left = right;
   end block_ekey;


   --------------------------------------
   --  "<" function for ranking_crate  --
   --------------------------------------
   function "<" (L, R : queue_record) return Boolean is
   begin
      if L.reverse_score = R.reverse_score then
         return R.ap_index > L.ap_index;
      end if;
      return L.reverse_score > R.reverse_score;
   end "<";


   --------------------------------------------------------------------------------------------
   --  get_port_variant #1
   --------------------------------------------------------------------------------------------
   function get_port_variant (PR : port_record) return String
   is
      use type portkey_crate.Cursor;
   begin
      if PR.key_cursor = portkey_crate.No_Element then
         return "get_port_variant: invalid key_cursor";
      end if;
      return HT.USS (portkey_crate.Key (PR.key_cursor));
   end get_port_variant;


   --------------------------------------------------------------------------------------------
   --  get_port_variant #2
   --------------------------------------------------------------------------------------------
   function get_port_variant (id : port_id) return String is
   begin
      if id = port_match_failed or else id > last_port then
         return "Invalid port ID";
      end if;
      return get_port_variant (all_ports (id));
   end get_port_variant;


   --------------------------------------------------------------------------------------------
   --  ignore_reason
   --------------------------------------------------------------------------------------------
   function ignore_reason (id : port_id) return String is
   begin
      if id = port_match_failed or else id > last_port then
         return "Invalid port ID";
      end if;
      return HT.USS (all_ports (id).ignore_reason);
   end ignore_reason;


   --------------------------------------------------------------------------------------------
   --  valid_port_id
   --------------------------------------------------------------------------------------------
   function valid_port_id (id : port_id) return Boolean is
   begin
      return id /= port_match_failed;
   end valid_port_id;


   --------------------------------------------------------------------------------------------
   --  wipe_make_queue
   --------------------------------------------------------------------------------------------
   procedure wipe_make_queue is
   begin
      for j in scanners'Range loop
         make_queue (j).Clear;
      end loop;
   end wipe_make_queue;


   --------------------------------------------------------------------------------------------
   --  reset_ports_tree
   --------------------------------------------------------------------------------------------
   procedure reset_ports_tree is
   begin
      for k in dim_all_ports'Range loop
         declare
            rec : port_record renames all_ports (k);
         begin

            rec.sequence_id   := 0;
            rec.key_cursor    := portkey_crate.No_Element;
            rec.ignore_reason := HT.blank;
            rec.pkgversion    := HT.blank;
            rec.port_variant  := HT.blank;
            rec.port_namebase := HT.blank;
            rec.bucket        := "00";
            rec.unkind_custom := False;
            rec.ignored       := False;
            rec.scanned       := False;
            rec.rev_scanned   := False;
            rec.unlist_failed := False;
            rec.work_locked   := False;
            rec.scan_locked   := False;
            rec.use_procfs    := False;
            rec.reverse_score := 0;
            rec.run_deps.Clear;
            rec.blocked_by.Clear;
            rec.blocks.Clear;
            rec.all_reverse.Clear;
            rec.options.Clear;
            rec.subpackages.Clear;
         end;
      end loop;
      ports_keys.Clear;
      rank_queue.Clear;
      lot_number  := 1;
      lot_counter := 0;
      last_port   := 0;
      prescanned  := False;
      wipe_make_queue;
      for m in scanners'Range loop
         mq_progress (m) := 0;
      end loop;
   end reset_ports_tree;


   --------------------------------------------------------------------------------------------
   --  queue_is_empty
   --------------------------------------------------------------------------------------------
   function queue_is_empty return Boolean is
   begin
      return rank_queue.Is_Empty;
   end queue_is_empty;



   --------------------------------------------------------------------------------------------
   --  queue_is_empty
   --------------------------------------------------------------------------------------------
   function original_queue_size return Natural is
   begin
      return Natural (original_queue_len);
   end original_queue_size;


   --------------------------------------------------------------------------------------------
   --  queue_length
   --------------------------------------------------------------------------------------------
   function queue_length return Integer is
   begin
      return Integer (rank_queue.Length);
   end queue_length;


   --------------------------------------------------------------------------------------------
   --  calculate_package_name
   --------------------------------------------------------------------------------------------
   function calculate_package_name (id : port_id; subpackage : String) return String
   is
      namebase   : constant String := HT.USS (all_ports (id).port_namebase);
      variant    : constant String := HT.USS (all_ports (id).port_variant);
      pkgversion : constant String := HT.USS (all_ports (id).pkgversion);
   begin
      return namebase & "-" & subpackage & "-" & variant & "-" & pkgversion;
   end calculate_package_name;


   --------------------------------------------------------------------------------------------
   --  convert_origin_to_portkey
   --------------------------------------------------------------------------------------------
   function convert_origin_to_portkey (origin : String) return String
   is
      numcolons : Natural := HT.count_char (origin, LAT.Colon);
   begin
      if numcolons < 2 then
         return "error";
      end if;
      declare
         namebase : String := HT.specific_field (origin, 1, ":");
         variant  : String := HT.specific_field (origin, 3, ":");
      begin
         return namebase & LAT.Colon & variant;
      end;
   end convert_origin_to_portkey;


   --------------------------------------------------------------------------------------------
   --  subpackage_from_origin
   --------------------------------------------------------------------------------------------
   function subpackage_from_origin (origin : String) return String
   is
      numcolons : Natural := HT.count_char (origin, LAT.Colon);
   begin
      if numcolons < 2 then
         return "error";
      end if;
      return HT.specific_field (origin, 2, ":");
   end subpackage_from_origin;

end PortScan;
