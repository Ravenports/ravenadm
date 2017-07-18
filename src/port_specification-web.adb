--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Characters.Latin_1;

package body Port_Specification.Web is

   package LAT renames Ada.Characters.Latin_1;


   --------------------------------------------------------------------------------------------
   --  produce_page
   --------------------------------------------------------------------------------------------
   procedure produce_page
     (specs   : Portspecs;
      variant : String;
      dossier : TIO.File_Type) is
   begin
      TIO.Put_Line (dossier, page_header ("Ravenport: " & specs.get_namebase));
      TIO.Put_Line (dossier, generate_body (specs, variant));
      TIO.Put_Line (dossier, page_footer);
   end produce_page;

   --------------------------------------------------------------------------------------------
   --  escape_value
   --------------------------------------------------------------------------------------------
   function escape_value (raw : String) return String
   is
      function htmlval (rawchar : Character) return String;

      focus : constant String :=
        LAT.Quotation &
        LAT.Less_Than_Sign &
        LAT.Greater_Than_Sign &
        LAT.Ampersand;
      curlen : Natural := raw'Length;
      result : String (1 .. raw'Length * 6) := (others => ' ');

      function htmlval (rawchar : Character) return String is
      begin
         case rawchar is
            when LAT.Quotation         => return "&quot;";
            when LAT.Less_Than_Sign    => return "&lt;";
            when LAT.Greater_Than_Sign => return "&gt;";
            when LAT.Ampersand         => return "&amp;";
            when others => return "";
         end case;
      end htmlval;
   begin
      result (1 .. curlen) := raw;
      for x in focus'Range loop
         if HT.count_char (result (1 .. curlen), focus (x)) > 0 then
            declare
               newstr : constant String :=
                 HT.replace_char (result (1 .. curlen), focus (x), htmlval (focus (x)));
            begin
               curlen := newstr'Length;
               result (1 .. curlen) := newstr;
            end;
         end if;
      end loop;
      return result (1 .. curlen);
   end escape_value;


   --------------------------------------------------------------------------------------------
   --  nvpair
   --------------------------------------------------------------------------------------------
   function nvpair (name, value : String) return String is
   begin
      return " " & name & LAT.Equals_Sign & LAT.Quotation & escape_value (value) & LAT.Quotation;
   end nvpair;


   --------------------------------------------------------------------------------------------
   --  page_header
   --------------------------------------------------------------------------------------------
   function page_header (title : String) return String
   is
      bing : constant String := LAT.Greater_Than_Sign & LAT.LF;
      content : constant String := "Ravenports individual port description";
      csslink : constant String := "../style/ravenports.css";
   begin
      return
        "<!doctype html" & bing &
        "<html" & nvpair ("lang", "en") & bing &
        "<head" & bing &
        " <title>" & escape_value (title) & "</title" & bing &
        " <meta" & nvpair ("charset", "utf-8") & bing &
        " <meta" & nvpair ("name", "description") & nvpair ("content", content) & bing &
        " <link" & nvpair ("rel", "stylesheet") & nvpair ("href", csslink) & bing &
        "</head" & bing &
        "<body>";
   end page_header;


   --------------------------------------------------------------------------------------------
   --  page_footer
   --------------------------------------------------------------------------------------------
   function page_footer return String is
   begin
      return "</body>" & LAT.LF & "</html>";
   end page_footer;


   --------------------------------------------------------------------------------------------
   --  div
   --------------------------------------------------------------------------------------------
   function div (id, value : String) return String is
   begin
      return "<div" & nvpair ("id", id) & ">" & escape_value (value) & "</div>" & LAT.LF;
   end div;


   --------------------------------------------------------------------------------------------
   --  body_template
   --------------------------------------------------------------------------------------------
   function body_template return String
   is
      ediv : constant String := "</div>" & LAT.LF;
      etd  : constant String := "</td>" & LAT.LF;
      etr  : constant String := "</tr>" & LAT.LF;
      btr  : constant String := "<tr>" & LAT.LF;
      raw : constant String :=
        " <div id='namebase'>@NAMEBASE@" & ediv &
        " <div id='shortblock'>" & LAT.LF &
        "  <table id='sbt1'>" & LAT.LF &
        "   <tbody>" & LAT.LF &
        "    " & btr &
        "     <td>Port Variant" & etd &
        "     <td id='variant'>@VARIANT@" & etd &
        "    " & etr &
        "    " & btr &
        "     <td>Summary" & etd &
        "     <td id='summary'>@TAGLINE@" & etd &
        "    " & etr &
        "    " & btr &
        "     <td>Package version" & etd &
        "     <td id='pkgversion'>@PKGVERSION@" & etd &
        "    " & etr &
        "    " & btr &
        "     <td>Homepage" & etd &
        "     <td id='homepage'>@HOMEPAGE@" & etd &
        "    " & etr &
        "    " & btr &
        "     <td>Keywords" & etd &
        "     <td id='keywords'>@KEYWORDS@" & etd &
        "    " & etr &
        "    " & btr &
        "     <td>Maintainers" & etd &
        "     <td id='maintainers'>@MAINTAINER@" & etd &
        "    " & etr &
        "    " & btr &
        "     <td>License" & etd &
        "     <td id='license'>@LICENSE@" & etd &
        "    " & etr &
        "   </tbody>" & LAT.LF &
        "  </table>" & LAT.LF &
        " " & ediv &
        " <div id='pkgdesc'>@PKGDESC@" & ediv;
   begin
      return HT.replace_all (S => raw, reject => LAT.Apostrophe, shiny  => LAT.Quotation);
   end body_template;


   --------------------------------------------------------------------------------------------
   --  format_homepage
   --------------------------------------------------------------------------------------------
   function format_homepage (homepage : String) return String is
   begin
      if homepage = homepage_none then
         return "No known homepage";
      end if;
      return "<a" & nvpair ("href", homepage) & nvpair ("class", "hplink") & ">" &
        homepage & "</a>";
   end format_homepage;


   --------------------------------------------------------------------------------------------
   --  list_scheme
   --------------------------------------------------------------------------------------------
   function list_scheme (licenses, scheme : String) return String
   is
      stripped : constant String := HT.replace_all (licenses, LAT.Quotation, ' ');
   begin
      if HT.IsBlank (licenses) then
         return "Not yet specified";
      end if;
      if scheme = "single" then
         return stripped;
      end if;
      return stripped & LAT.Space & LAT.Left_Parenthesis & scheme & LAT.Right_Parenthesis;
   end list_scheme;


   --------------------------------------------------------------------------------------------
   --  generate_body
   --------------------------------------------------------------------------------------------
   function generate_body (specs : Portspecs; variant : String) return String
   is
      result   : HT.Text := HT.SUS (body_template);
      subject  : constant String := "Ravenports:%20" & specs.get_namebase & "%20port";
      homepage : constant String := format_homepage (specs.get_field_value (sp_homepage));
      tagline  : constant String := escape_value (specs.get_tagline (variant));
      licenses : constant String := list_scheme (specs.get_field_value (sp_licenses),
                                                 specs.get_license_scheme);
   begin
      result := HT.replace_substring (result, "@NAMEBASE@", specs.get_namebase);
      result := HT.replace_substring (result, "@VARIANT@", variant);
      result := HT.replace_substring (result, "@HOMEPAGE@", homepage);
      result := HT.replace_substring (result, "@TAGLINE@", tagline);
      result := HT.replace_substring (result, "@PKGVERSION@", specs.calculate_pkgversion);
      result := HT.replace_substring (result, "@MAINTAINER@", specs.get_web_contacts (subject));
      result := HT.replace_substring (result, "@KEYWORDS@", specs.get_field_value (sp_keywords));
      result := HT.replace_substring (result, "@LICENSE@", licenses);
      return HT.USS (result);
   end generate_body;

end Port_Specification.Web;
