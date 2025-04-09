--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Utilities;
with Ada.Characters.Latin_1;
with HelperText;

package body Port_Specification.Json is

   package LAT renames Ada.Characters.Latin_1;
   package UTL renames Utilities;
   package HT  renames HelperText;

   --------------------------------------------------------------------------------------------
   --  describe_port
   --------------------------------------------------------------------------------------------
   procedure describe_port
     (specs   : Portspecs;
      dossier : TIO.File_Type;
      bucket  : String;
      index   : Positive)
   is
      nvar : constant Natural := specs.get_number_of_variants;
   begin
      if specs.infrastructure then
         return;
      end if;
      begin
         TIO.Put
           (dossier,
            UTL.json_object (True, 3, index) &
              UTL.json_nvpair_string  ("bucket",   bucket, 1, pad) &
              UTL.json_nvpair_string  ("namebase", specs.get_namebase, 2, pad) &
              UTL.json_nvpair_string  ("version",  specs.get_field_value (sp_version), 3, pad) &
              homepage_line (specs) &
              UTL.json_nvpair_complex ("keywords", describe_keywords (specs), 3, pad) &
              UTL.json_nvpair_complex ("distfile", describe_distfiles (specs), 3, pad) &
              specs.get_json_contacts &
              describe_Common_Platform_Enumeration (specs) &
              describe_patched_vulnerabilities (specs) &
              UTL.json_name_complex   ("variants", 4, pad) &
              UTL.json_array (True, pad + 1)
           );
      exception
         when problem : others =>
            TIO.Put_Line ("Failed json description main text, " & HT.DQ (specs.get_namebase));
            return;
      end;
      for x in 1 .. nvar loop
         declare
            varstr : constant String := specs.get_list_item (Port_Specification.sp_variants, x);
            sdesc  : constant String := specs.get_tagline (varstr);
            spray  : constant String := describe_subpackages (specs, varstr);
         begin
            TIO.Put
              (dossier,
               UTL.json_object (True, pad + 2, x) &
                 UTL.json_nvpair_string  ("label", varstr, 1, pad + 3) &
                 UTL.json_nvpair_string  ("sdesc", sdesc, 2, pad + 3) &
                 UTL.json_nvpair_complex ("spkgs", spray, 3, pad + 3) &
                 UTL.json_object (False, pad + 2, x)
              );
         exception
            when others =>
               TIO.Put_Line ("Failed json description variants, " & HT.DQ (specs.get_namebase));
               return;
         end;
      end loop;
      TIO.Put
        (dossier,
           UTL.json_array (False, pad + 1) &
           UTL.json_object (False, 3, index)
        );
   exception
      when others =>
         TIO.Put_Line ("Failed json description declaration, " & bucket & specs.get_namebase);

   end describe_port;


   --------------------------------------------------------------------------------------------
   --  fpc_value
   --------------------------------------------------------------------------------------------
   function fpc_value (is_generated : Boolean; raw_value : String) return String is
   begin
      if is_generated then
         return "generated";
      else
         return raw_value;
      end if;
   end fpc_value;


   --------------------------------------------------------------------------------------------
   --  describe_keywords
   --------------------------------------------------------------------------------------------
   function describe_keywords (specs : Portspecs) return String
   is
      raw : constant String := specs.get_field_value (Port_Specification.sp_keywords);
      innerquote : constant String :=
        HT.replace_char
          (S         => HT.replace_char (raw, LAT.Comma, LAT.Quotation & LAT.Comma),
           focus     => LAT.Space,
           substring => LAT.Space & LAT.Quotation);
   begin
      return "[ " & LAT.Quotation & innerquote  & LAT.Quotation & " ]";
   end describe_keywords;


   --------------------------------------------------------------------------------------------
   --  describe_distfiles
   --------------------------------------------------------------------------------------------
   function describe_distfiles (specs : Portspecs) return String
   is
      numfiles : constant Natural := specs.get_list_length (Port_Specification.sp_distfiles);
      result   : HT.Text := HT.SUS ("[ ");
   begin
      for x in 1 .. numfiles loop
         if x > 1 then
            HT.SU.Append (result, ", ");
         end if;
         HT.SU.Append (result, LAT.Quotation & specs.get_repology_distfile (x) & LAT.Quotation);
      end loop;
      return HT.USS (result) & " ]";
   end describe_distfiles;


   --------------------
   --  describe_cve  --
   --------------------
   function describe_cve (specs : Portspecs) return String
   is
      numitems : constant Natural := specs.get_list_length (Port_Specification.sp_cve);
      result   : HT.Text := HT.SUS ("[ ");
   begin
      for x in 1 .. numitems loop
         if x > 1 then
            HT.SU.Append (result, ", ");
         end if;
         HT.SU.Append (result,
                       LAT.Quotation & specs.get_list_item (Port_Specification.sp_cve, x) &
                         LAT.Quotation);
      end loop;
      return HT.USS (result) & " ]";
   end describe_cve;


   --------------------------------------------------------------------------------------------
   --  describe_subpackages
   --------------------------------------------------------------------------------------------
   function describe_subpackages (specs : Portspecs; variant : String) return String
   is
      numpkg : constant Natural := specs.get_subpackage_length (variant);
      result   : HT.Text := HT.SUS ("[ ");
   begin
      for x in 1 .. numpkg loop
         if x > 1 then
            HT.SU.Append (result, ", ");
         end if;
         HT.SU.Append (result, LAT.Quotation &
                         specs.get_subpackage_item (variant, x) & LAT.Quotation);
      end loop;
      return HT.USS (result) & " ]";
   end describe_subpackages;


   --------------------------------------------------------------------------------------------
   --  homepage_line
   --------------------------------------------------------------------------------------------
   function homepage_line (specs : Portspecs) return String
   is
      homepage : constant String := specs.get_field_value (sp_homepage);
   begin
      if homepage = homepage_none then
         return "";
      end if;
      return UTL.json_nvpair_string  ("homepage", specs.get_field_value (sp_homepage), 3, pad);
   end homepage_line;


   --------------------------------------------------------------------------------------------
   --  describe_Common_Platform_Enumeration
   --------------------------------------------------------------------------------------------
   function describe_Common_Platform_Enumeration (specs : Portspecs) return String
   is
      function retrieve (key : String; default_value : String) return String;
      function form_object return String;
      procedure maybe_push (json_key, cpe_key, default_value : String);
      procedure always_push (json_key, value : String);

      pairs : string_crate.Vector;

      function retrieve (key : String; default_value : String) return String
      is
         key_text : HT.Text := HT.SUS (key);
      begin
         if specs.catch_all.Contains (key_text) then
            return HT.USS (specs.catch_all.Element (key_text).list.First_Element);
         else
            return default_value;
         end if;
      end retrieve;

      procedure always_push (json_key, value : String) is
      begin
         pairs.Append (HT.SUS (json_key & "=" & value));
      end always_push;

      procedure maybe_push (json_key, cpe_key, default_value : String)
      is
         cpe_key_text : HT.Text := HT.SUS (cpe_key);
         cpe_value    : HT.Text;
      begin
         if specs.catch_all.Contains (cpe_key_text) then
            cpe_value := specs.catch_all.Element (cpe_key_text).list.First_Element;
            if not HT.equivalent (cpe_value, default_value) then
               pairs.Append (HT.SUS (json_key & "=" & HT.USS (cpe_value)));
            end if;
         end if;
      end maybe_push;

      function form_object return String
      is
         procedure dump (position : string_crate.Cursor);

         tracker : Natural := 0;
         guts    : HT.Text;

         procedure dump (position : string_crate.Cursor)
         is
            both  : String := HT.USS (string_crate.Element (position));
            key   : String := HT.part_1 (both, "=");
            value : String := HT.part_2 (both, "=");
            data  : String := UTL.json_nvpair_string (key, value, 1, 0);  -- trailing LF
         begin
            tracker := tracker + 1;
            if tracker > 1 then
               HT.SU.Append (guts, LAT.Comma);
            end if;
            HT.SU.Append (guts, data (data'First .. data'Last - 1));
         end dump;
      begin
         pairs.Iterate (dump'Access);
         return "{" & HT.USS (guts) & " }";
      end form_object;
   begin
      if not specs.uses_base.Contains (HT.SUS ("cpe")) then
         return "";
      end if;

      --  maybe_push  ("part", "CPE_PART", "a");

      declare
         cpe_product : String := retrieve ("CPE_PRODUCT", HT.lowercase (specs.get_namebase));
         cpe_vendor  : String := retrieve ("CPE_VENDOR", cpe_product);
      begin
         always_push ("vendor", cpe_vendor);
         always_push ("product", cpe_product);
      end;

      maybe_push ("version", "CPE_VERSION", HT.USS (specs.version));
      maybe_push ("update", "CPE_UPDATE", "");
      maybe_push ("edition", "CPE_EDITION", "");
      maybe_push ("lang", "CPE_LANG", "");
      maybe_push ("sw_edition", "CPE_SW_EDITION", "");
      maybe_push ("target_sw", "CPE_TARGET_SW", "take-anything");
      maybe_push ("target_hw", "CPE_TARGET_HW", "x86-x86-arm64");
      maybe_push ("other", "CPE_OTHER",  HT.USS (specs.version));

      return UTL.json_nvpair_complex ("cpe", form_object, 3, pad);
   exception
      when others =>
         TIO.Put_Line ("Failed json CPE enum, " & HT.DQ (specs.get_namebase));
         return "";
   end describe_Common_Platform_Enumeration;


   ----------------------------------------
   --  describe_patched_vulnerabilities  --
   ----------------------------------------
   function describe_patched_vulnerabilities (specs : Portspecs) return String is
   begin
      --  Should not be possible to have fixed_cve without cpe module, but check anyway
      if specs.fixed_cve.Is_Empty or else
        not specs.uses_base.Contains (HT.SUS ("cpe"))
      then
         return "";
      end if;
      return UTL.json_nvpair_complex ("vulnaddressed", describe_cve (specs), 3, pad);
   end describe_patched_vulnerabilities;

end Port_Specification.Json;
