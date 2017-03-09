--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Characters.Latin_1;
with Ada.Text_IO;

package body Port_Specification.Buildsheet is

   package TIO renames Ada.Text_IO;
   package LAT renames Ada.Characters.Latin_1;

   --------------------------------------------------------------------------------------------
   --  generator
   --------------------------------------------------------------------------------------------
   procedure generator
     (specs         : Portspecs;
      output_file   : String)
   is
      procedure send (data : String; use_put : Boolean := False);
      procedure send (varname : String; value, default : Integer);
      procedure send (varname, value : String);
      procedure send (varname : String; value : HT.Text);
      procedure send (varname : String; crate : string_crate.Vector; flavor : Positive);
      procedure send (varname : String; crate : def_crate.Map);
      procedure send (varname : String; crate : list_crate.Map);
      procedure print_item (position : string_crate.Cursor);
      procedure print_adjacent (position : string_crate.Cursor);
      procedure dump_sdesc (position : def_crate.Cursor);
      procedure dump_sites (position : list_crate.Cursor);
      procedure dump_distfiles (position : string_crate.Cursor);
      procedure blank_line;

      write_to_file   : constant Boolean := (output_file /= "");
      makefile_handle : TIO.File_Type;
      varname_prefix  : HT.Text;
      current_len     : Natural;

      procedure send (data : String;  use_put : Boolean := False) is
      begin
         if write_to_file then
            if use_put then
               TIO.Put (makefile_handle, data);
            else
               TIO.Put_Line (makefile_handle, data);
            end if;
         else
            if use_put then
               TIO.Put (data);
            else
               TIO.Put_Line (data);
            end if;
         end if;
      end send;

      procedure send (varname, value : String) is
      begin
         send (align24 (varname & LAT.Equals_Sign) & value);
      end send;

      procedure send (varname : String; value : HT.Text) is
      begin
         send (align24 (varname & LAT.Equals_Sign) & HT.USS (value));
      end send;

      procedure send (varname : String; value, default : Integer) is
      begin
         if value /= default then
            send (align24 (varname & LAT.Equals_Sign) & HT.int2str (value));
         end if;
      end send;

      procedure send (varname : String; crate : string_crate.Vector; flavor : Positive) is
      begin
         if crate.Is_Empty then
            return;
         end if;
         case flavor is
            when 1 =>
               send (align24 (varname & "="), True);
               crate.Iterate (Process => print_item'Access);
            when 2 =>
               current_len := 0;
               send (align24 (varname & "="), True);
               crate.Iterate (Process => print_adjacent'Access);
               send ("");
            when 3 =>
               varname_prefix := HT.SUS (varname);
               crate.Iterate (Process => dump_distfiles'Access);
            when others =>
               null;
         end case;
      end send;

      procedure send (varname : String; crate : def_crate.Map) is
      begin
         varname_prefix := HT.SUS (varname);
         crate.Iterate (Process => dump_sdesc'Access);
      end send;

      procedure send (varname : String; crate : list_crate.Map) is
      begin
         varname_prefix := HT.SUS (varname);
         crate.Iterate (Process => dump_sites'Access);
      end send;

      procedure print_item (position : string_crate.Cursor)
      is
         index : Natural := string_crate.To_Index (position);
         item  : String  := HT.USS (string_crate.Element (position));
      begin
         if index = 1 then
            send (item);
         else
            send (LAT.HT & LAT.HT & LAT.HT & item);
         end if;
      end print_item;

      procedure print_adjacent (position : string_crate.Cursor)
      is
         index : Natural := string_crate.To_Index (position);
         item  : String  := HT.USS (string_crate.Element (position));
         len   : Natural := item'Length;
      begin
         if current_len + len + 1 > 76 then
            current_len := 0;
            send ("");
         end if;
         if current_len > 0 then
            send (" ", True);
            current_len := current_len + 1;
         end if;
         send (item, True);
         current_len := current_len + len;

      end print_adjacent;

      procedure dump_sdesc (position : def_crate.Cursor)
      is
         varname : String := HT.USS (varname_prefix)  & LAT.Left_Square_Bracket &
                   HT.USS (def_crate.Key (position)) & LAT.Right_Square_Bracket & LAT.Equals_Sign;
      begin
         send (align24 (varname) & HT.USS (def_crate.Element (position)));
      end dump_sdesc;

      procedure dump_sites (position : list_crate.Cursor)
      is
         rec     : group_list renames list_crate.Element (position);
         varname : String := HT.USS (varname_prefix)  & LAT.Left_Square_Bracket &
                   HT.USS (rec.group) & LAT.Right_Square_Bracket & LAT.Equals_Sign;
      begin
         send (align24 (varname), True);
         rec.list.Iterate (Process => print_item'Access);
      end dump_sites;

      procedure dump_distfiles (position : string_crate.Cursor)
      is
         index : Natural := string_crate.To_Index (position);
         NDX   : String  := HT.USS (varname_prefix)  & LAT.Left_Square_Bracket &
           HT.int2str (index) & LAT.Right_Square_Bracket & LAT.Equals_Sign;
      begin
         send (align24 (NDX) & HT.USS (string_crate.Element (position)));
      end dump_distfiles;

      procedure blank_line is
      begin
         send ("");
      end blank_line;

   begin
      send ("# Buildsheet autogenerated by ravenadm tool -- Do not edit." & LAT.LF);

      send ("NAMEBASE",          specs.namebase);
      send ("VERSION",           specs.version);
      send ("REVISION",          specs.revision, 0);
      send ("EPOCH",             specs.epoch, 0);
      send ("KEYWORDS",          specs.keywords, 2);
      send ("VARIANTS",          specs.variants, 2);
      send ("SDESC",             specs.taglines);
      send ("HOMEPAGE",          specs.homepage);
      send ("CONTACT",           specs.contacts, 1);
      blank_line;

      --      send ("DOWNLOAD_GROUPS=", specs.d
      send ("SITES",             specs.dl_sites);
      send ("DISTFILE",          specs.distfiles, 3);
      send ("DIST_SUBDIR",       specs.dist_subdir);
      send ("DF_INDEX",          specs.df_index, 2);
      send ("SPKGS",             specs.subpackages);

      blank_line;
      send ("OPTIONS_AVAILABLE", specs.ops_avail, 2);
      send ("OPTIONS_STANDARD",  specs.ops_standard, 2);
      send ("VOPTS",             specs.variantopts);
      send ("OPT_ON",            specs.options_on);
      blank_line;
      send ("BROKEN",            specs.broken);
      send ("NOT_FOR_OPSYS",     specs.exc_opsys, 2);
      send ("ONLY_FOR_OPSYS",    specs.inc_opsys, 2);
      send ("NOT_FOR_ARCH",      specs.exc_arch, 2);
      send ("DEPRECATED",        specs.deprecated);
      send ("EXPIRATION_DATE",   specs.expire_date);

      if write_to_file then
         TIO.Close (makefile_handle);
      end if;
   exception
      when others =>
         if TIO.Is_Open (makefile_handle) then
            TIO.Close (makefile_handle);
         end if;

   end generator;


   --------------------------------------------------------------------------------------------
   --  align24
   --------------------------------------------------------------------------------------------
   function align24 (payload : String) return String
   is
      len : Natural := payload'Length;
   begin
      if len < 8 then
         return payload & LAT.HT & LAT.HT & LAT.HT;
      elsif len < 16 then
         return payload & LAT.HT & LAT.HT;
      elsif len < 24 then
         return payload & LAT.HT;
      else
         return payload;
      end if;
   end align24;


   --------------------------------------------------------------------------------------------
   --  align40
   --------------------------------------------------------------------------------------------
   function align40 (payload : String) return String
   is
      len : Natural := payload'Length;
   begin
      if len < 8 then
         return payload & LAT.HT & LAT.HT & LAT.HT & LAT.HT & LAT.HT;
      elsif len < 16 then
         return payload & LAT.HT & LAT.HT & LAT.HT & LAT.HT;
      elsif len < 24 then
         return payload & LAT.HT & LAT.HT & LAT.HT;
      elsif len < 32 then
         return payload & LAT.HT & LAT.HT;
      elsif len < 40 then
         return payload & LAT.HT;
      else
         return payload;
      end if;
   end align40;

end Port_Specification.Buildsheet;
