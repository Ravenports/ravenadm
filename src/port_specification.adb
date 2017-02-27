--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions; use Definitions;

package body Port_Specification is


   --------------------------------------------------------------------------------------------
   --  initialize
   --------------------------------------------------------------------------------------------
   procedure initialize (specs : out Portspecs) is
   begin
      specs.namebase := HT.blank;
      specs.version  := HT.blank;
      specs.revision := 0;
      specs.epoch    := 0;
      specs.keywords.Clear;
      specs.variants.Clear;

      specs.last_set := so_initialized;
   end initialize;


   --------------------------------------------------------------------------------------------
   --  set_single_string
   --------------------------------------------------------------------------------------------
   procedure set_single_string
     (specs : in out Portspecs;
      field : spec_field;
      value : String) is
   begin
      if HT.contains (S => value, fragment => " ") then
         raise contains_spaces;
      end if;
      case field is
         when sp_namebase =>
            if specs.last_set /= so_initialized then
               raise misordered with field'Img;
            end if;
            specs.namebase := HT.SUS (value);
            specs.last_set := so_namebase;
         when sp_version =>
            if specs.last_set /= so_namebase then
               raise misordered with field'Img;
            end if;
            specs.version := HT.SUS (value);
            specs.last_set := so_version;
         when others =>
            raise wrong_type with field'Img;
      end case;

   end set_single_string;


   --------------------------------------------------------------------------------------------
   --  append_list
   --------------------------------------------------------------------------------------------
   procedure append_list
     (specs : in out Portspecs;
      field : spec_field;
      value : String) is
   begin
      if HT.contains (S => value, fragment => " ") then
         raise contains_spaces;
      end if;
      case field is
         when sp_keywords =>
            if specs.last_set /= so_keywords or else
              specs.last_set /= so_epoch or else
              specs.last_set /= so_revision or else
              specs.last_set /= so_version
            then
               raise misordered with field'Img;
            end if;
            specs.keywords.Append (HT.SUS (value));
            specs.last_set := so_keywords;
         when sp_variants =>
            if specs.last_set /= so_variants or else
              specs.last_set /= so_keywords
            then
               raise misordered with field'Img;
            end if;
            if specs.variants.Is_Empty and then
              value /= variant_standard
            then
               raise wrong_type with "First variant must be '" & variant_standard & "'";
            end if;
            specs.variants.Append (HT.SUS (value));
            specs.last_set := so_variants;
         when others =>
            raise wrong_type with field'Img;
      end case;

   end append_list;


   --------------------------------------------------------------------------------------------
   --  set_natural_integer
   --------------------------------------------------------------------------------------------
   procedure set_natural_integer
     (specs : in out Portspecs;
      field : spec_field;
      value : Natural) is
   begin
      case field is
         when sp_revision =>
            if specs.last_set /= so_version then
               raise misordered with field'Img;
            end if;
            specs.revision := value;
            specs.last_set := so_revision;
         when sp_epoch =>
            if specs.last_set /= so_revision or else
              specs.last_set /= so_version
            then
               raise misordered with field'Img;
            end if;
            specs.epoch := value;
            specs.last_set := so_epoch;
         when others =>
            raise wrong_type with field'Img;
      end case;
   end set_natural_integer;


end Port_Specification;
