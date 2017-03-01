--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions; use Definitions;
with Ada.Text_IO;
with Ada.Characters.Latin_1;

package body Port_Specification is

   package TIO renames Ada.Text_IO;
   package LAT renames Ada.Characters.Latin_1;

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
            if specs.last_set /= so_keywords and then
              specs.last_set /= so_epoch and then
              specs.last_set /= so_revision and then
              specs.last_set /= so_version
            then
               raise misordered with field'Img;
            end if;
            if not keyword_is_valid (value) then
               raise wrong_value with "Keyword '" & value & "' is not recognized";
            end if;
            specs.keywords.Append (HT.SUS (value));
            specs.last_set := so_keywords;
         when sp_variants =>
            if specs.last_set /= so_variants and then
              specs.last_set /= so_keywords
            then
               raise misordered with field'Img;
            end if;
            if specs.variants.Is_Empty and then
              value /= variant_standard
            then
               raise wrong_value with "First variant must be '" & variant_standard & "'";
            end if;
            if value'Length > 15 then
               raise wrong_value with "'" & value & "' value is too long (15-char limit)";
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
            if specs.last_set /= so_revision and then
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


   --------------------------------------------------------------------------------------------
   --  keyword_is_valid
   --------------------------------------------------------------------------------------------
   function keyword_is_valid (keyword : String) return Boolean is
   begin
      return (keyword = "accessibility" or else
              keyword = "archivers" or else
              keyword = "astro" or else
              keyword = "audio" or else
              keyword = "benchmarks" or else
              keyword = "biology" or else
              keyword = "cad" or else
              keyword = "comms" or else
              keyword = "converters" or else
              keyword = "databases" or else
              keyword = "deskutils" or else
              keyword = "devel" or else
              keyword = "dns" or else
              keyword = "editors" or else
              keyword = "emulators" or else
              keyword = "finance" or else
              keyword = "ftp" or else
              keyword = "games" or else
              keyword = "graphics" or else
              keyword = "irc" or else
              keyword = "lang" or else
              keyword = "mail" or else
              keyword = "math" or else
              keyword = "misc" or else
              keyword = "multimedia" or else
              keyword = "net" or else
              keyword = "net_im" or else
              keyword = "net_mgmt" or else
              keyword = "net_p2p" or else
              keyword = "news" or else
              keyword = "print" or else
              keyword = "raven" or else
              keyword = "science" or else
              keyword = "security" or else
              keyword = "shells" or else
              keyword = "sysutils" or else
              keyword = "textproc" or else
              keyword = "www" or else
              keyword = "x11" or else
              keyword = "x11_clocks" or else
              keyword = "x11_drivers" or else
              keyword = "x11_fm" or else
              keyword = "x11_fonts" or else
              keyword = "x11_servers" or else
              keyword = "x11_toolkits" or else
              keyword = "x11_wm" or else
              keyword = "ada" or else
              keyword = "c++" or else
              keyword = "csharp" or else
              keyword = "java" or else
              keyword = "javascript" or else
              keyword = "lisp" or else
              keyword = "perl" or else
              keyword = "php" or else
              keyword = "python" or else
              keyword = "ruby" or else
              keyword = "scheme" or else
              keyword = "Arabic" or else
              keyword = "Chinese" or else
              keyword = "French" or else
              keyword = "German" or else
              keyword = "Italian" or else
              keyword = "Japanese" or else
              keyword = "Russian" or else
              keyword = "Spanish" or else
              keyword = "Vietnamese");
   end keyword_is_valid;


   --------------------------------------------------------------------------------------------
   --  dump_specification
   --------------------------------------------------------------------------------------------
   procedure dump_specification (specs : Portspecs)
   is
      procedure print_item (position : string_crate.Cursor);
      procedure print_item (position : string_crate.Cursor)
      is
         index : Natural := string_crate.To_Index (position);
      begin
         if index > 1 then
            TIO.Put (" ");
         end if;
         TIO.Put (HT.USS (string_crate.Element (position)));
      end print_item;
   begin
      TIO.Put_Line ("NAMEBASE=" & LAT.HT & LAT.HT & HT.USS (specs.namebase));
      TIO.Put_Line ("VERSION="  & LAT.HT & LAT.HT & HT.USS (specs.version));
      TIO.Put_Line ("REVISION=" & LAT.HT & LAT.HT & HT.int2str (specs.revision));
      TIO.Put_Line ("EPOCH="    & LAT.HT & LAT.HT & LAT.HT & HT.int2str (specs.epoch));
      TIO.Put      ("KEYWORDS=" & LAT.HT & LAT.HT);
      specs.keywords.Iterate (Process => print_item'Access);
      TIO.Put      (LAT.LF);
      TIO.Put      ("VARIANTS=" & LAT.HT & LAT.HT);
      specs.variants.Iterate (Process => print_item'Access);
      TIO.Put      (LAT.LF);
   end dump_specification;

end Port_Specification;
