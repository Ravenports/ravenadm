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

end PortScan;
