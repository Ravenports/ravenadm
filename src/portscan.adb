--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Strings.Hash;

package body PortScan is

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
   --  get_port_variant
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
            rec.pkg_present   := False;
            rec.remote_pkg    := False;
            rec.never_remote  := False;
            rec.deletion_due  := False;
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


   --  DELETE ME
   procedure crash_test_dummy
   is
      rec : port_record renames all_ports (0);
   begin
      ports_keys.Insert (Key      => HT.SUS ("nawk:standard"),
                         New_Item => 0);

      rec.sequence_id := 0;
      rec.key_cursor    := ports_keys.Find (HT.SUS ("nawk:standard"));
      rec.pkgversion    := HT.SUS ("20121220_1");
      rec.port_variant  := HT.SUS ("standard");
      rec.port_namebase := HT.SUS ("nawk");
      rec.scanned       := True;
      rec.rev_scanned   := True;
      rec.subpackages.Append (HT.SUS ("single"));


   end crash_test_dummy;

end PortScan;
