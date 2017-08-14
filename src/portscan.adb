--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Strings.Hash;
with Ada.Characters.Latin_1;
with Ada.Directories;
with Parameters;
with Utilities;

package body PortScan is

   package LAT renames Ada.Characters.Latin_1;
   package DIR renames Ada.Directories;
   package UTL renames Utilities;

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
   --  get_bucket
   --------------------------------------------------------------------------------------------
   function get_bucket (id : port_id) return String is
   begin
      if id = port_match_failed or else id > last_port then
         return "XX";
      end if;
      return all_ports (id).bucket;
   end get_bucket;


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
   --  convert_depend_origin_to_portkey
   --------------------------------------------------------------------------------------------
   function convert_depend_origin_to_portkey (origin : String) return String
   is
      --  expected format: namebase:subpackage:variant
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
   end convert_depend_origin_to_portkey;


   --------------------------------------------------------------------------------------------
   --  subpackage_from_pkgname
   --------------------------------------------------------------------------------------------
   function subpackage_from_pkgname (pkgname : String) return String
   is
      --  expected format: namebase-subpackage-variant-version.txz
      --  support namebase-subpackage-variant too
      numcolons : Natural := HT.count_char (pkgname, LAT.Hyphen);
   begin
      if numcolons < 3 then
         return "error";
      end if;
      return HT.tail (HT.head (HT.head (pkgname, "-"), "-"), "-");
   end subpackage_from_pkgname;


   --------------------------------------------------------------------------------------------
   --  scan_progress
   --------------------------------------------------------------------------------------------
   function scan_progress return String
   is
      type percent is delta 0.01 digits 5;
      complete : port_index := 0;
      pc : percent;
      maximum : Float := Float (last_port + 1);
   begin
      for k in scanners'Range loop
         complete := complete + mq_progress (k);
      end loop;
      pc := percent (100.0 * Float (complete) / maximum);
      return " progress:" & pc'Img & "%              " & LAT.CR;
   end scan_progress;


   --------------------------------------------------------------------------------------------
   --  insert_into_portlist
   --------------------------------------------------------------------------------------------
   procedure insert_into_portlist (port_variant : String)
   is
      pv_text : HT.Text := HT.SUS (port_variant);
   begin
      if not portlist.Contains (pv_text) then
         portlist.Append (pv_text);
         dupelist.Append (pv_text);
      end if;
   end insert_into_portlist;


   --------------------------------------------------------------------------------------------
   --  jail_env_port_specified
   --------------------------------------------------------------------------------------------
   function jail_env_port_specified return Boolean is
   begin
      return
        portlist.Contains (HT.SUS (default_compiler & ":standard")) or else
        portlist.Contains (HT.SUS ("binutils:ravensys"));
   end jail_env_port_specified;


   --------------------------------------------------------------------------------------------
   --  requires_procfs
   --------------------------------------------------------------------------------------------
   function requires_procfs (id : port_id) return Boolean is
   begin
      return all_ports (id).use_procfs;
   end requires_procfs;


   --------------------------------------------------------------------------------------------
   --  build_request_length
   --------------------------------------------------------------------------------------------
   function build_request_length return Integer is
   begin
      return Integer (dupelist.Length);
   end build_request_length;


   --------------------------------------------------------------------------------------------
   --  get_buildsheet_from_origin_list
   --------------------------------------------------------------------------------------------
   function get_buildsheet_from_origin_list (index : Positive) return String
   is
      procedure search (position : string_crate.Cursor);

      conspiracy   : constant String := HT.USS (Parameters.configuration.dir_conspiracy);
      unkindness   : constant String := HT.USS (Parameters.configuration.dir_unkindness);
      counter : Natural := 0;
      answer  : HT.Text;

      procedure search (position : string_crate.Cursor) is
      begin
         counter := counter + 1;
         if counter = index then
            declare
               namebase : String := HT.part_1 (HT.USS (string_crate.Element (position)), ":");
               bsheetname : String := "/bucket_" & UTL.bucket (namebase) & "/" & namebase;
            begin
               if DIR.Exists (unkindness & bsheetname) then
                  answer := HT.SUS (unkindness & bsheetname);
               else
                  answer := HT.SUS (conspiracy & bsheetname);
               end if;
            end;
         end if;
      end search;
   begin
      dupelist.Iterate (search'Access);
      return HT.USS (answer);
   end get_buildsheet_from_origin_list;

end PortScan;
