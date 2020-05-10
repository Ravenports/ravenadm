--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Utilities;
with Ada.Characters.Latin_1;

package body Port_Specification.Json is

   package UTL renames Utilities;
   package LAT renames Ada.Characters.Latin_1;

   --------------------------------------------------------------------------------------------
   --  describe_port
   --------------------------------------------------------------------------------------------
   procedure describe_port
     (specs   : Portspecs;
      dossier : TIO.File_Type;
      bucket  : String;
      index   : Positive)
   is
      fpcval : constant String := fpc_value (specs.generated, specs.equivalent_fpc_port);
      nvar   : constant Natural := specs.get_number_of_variants;
   begin
      TIO.Put
        (dossier,
           UTL.json_object (True, 3, index) &
           UTL.json_nvpair_string  ("bucket",   bucket, 1, pad) &
           UTL.json_nvpair_string  ("namebase", specs.get_namebase, 2, pad) &
           UTL.json_nvpair_string  ("version",  specs.get_field_value (sp_version), 3, pad) &
           homepage_line (specs) &
           UTL.json_nvpair_string  ("FPC",      fpcval, 3, pad) &
           UTL.json_nvpair_complex ("keywords", describe_keywords (specs), 3, pad) &
           UTL.json_nvpair_complex ("distfile", describe_distfiles (specs), 3, pad) &
           specs.get_json_contacts &
           describe_Common_Platform_Enumeration (specs) &
           UTL.json_name_complex   ("variants", 4, pad) &
           UTL.json_array (True, pad + 1)
        );
      for x in 1 .. nvar loop
         declare
            varstr : constant String := specs.get_list_item (Port_Specification.sp_variants, x);
            sdesc  : constant String := escape_tagline (specs.get_tagline (varstr));
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
         end;
      end loop;
      TIO.Put
        (dossier,
           UTL.json_array (False, pad + 1) &
           UTL.json_object (False, 3, index)
        );

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
   --  escape_tagline
   --------------------------------------------------------------------------------------------
   function escape_tagline (raw : String) return String
   is
      focus : constant String :=
        LAT.Reverse_Solidus &
        LAT.Quotation &
        LAT.Solidus &
        LAT.BS &
        LAT.FF &
        LAT.LF &
        LAT.CR &
        LAT.HT;
      curlen : Natural := raw'Length;
      result : String (1 .. raw'Length * 2) := (others => ' ');
   begin
      result (1 .. curlen) := raw;
      for x in focus'Range loop
         if HT.count_char (result (1 .. curlen), focus (x)) > 0 then
            declare
               newstr : String := HT.replace_char (result (1 .. curlen), focus (x),
                                                   LAT.Reverse_Solidus & focus (x));
            begin
               curlen := newstr'Length;
               result (1 .. curlen) := newstr;
            end;
         end if;
      end loop;
      return result (1 .. curlen);
   end escape_tagline;


   --------------------------------------------------------------------------------------------
   --  homepage_line
   --------------------------------------------------------------------------------------------
   function homepage_line (specs : Portspecs) return String
   is
      homepage : constant String := specs.get_field_value (sp_homepage);
   begin
      if homepage = homepage_none or else
        specs.repology_sucks
      then
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
      function form_object (product, vendor : String) return String;

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

      function form_object (product, vendor : String) return String
      is
         data1 : String := UTL.json_nvpair_string ("product", product, 1, 0);  -- trailing LF
         data2 : String := UTL.json_nvpair_string ("vendor", vendor, 1, 0);    -- trailing LF
      begin
         return "{" & data1 (data1'First .. data1'Last - 1) & LAT.Comma
                     & data2 (data2'First .. data2'Last - 1) & " }";
      end form_object;
   begin
      if not specs.uses.Contains (HT.SUS ("cpe")) then
         return "";
      end if;

      declare
         cpe_product : String := retrieve ("CPE_PRODUCT", HT.lowercase (specs.get_namebase));
         cpe_vendor  : String := retrieve ("CPE_VENDOR", cpe_product);
      begin
         return UTL.json_nvpair_complex ("cpe", form_object (cpe_product, cpe_vendor), 3, pad);
      end;
   end describe_Common_Platform_Enumeration;

end Port_Specification.Json;
