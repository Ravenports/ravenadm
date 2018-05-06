--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Exceptions;
with File_Operations;
with Utilities;
with PortScan.Log;

package body Port_Specification.Web is

   package LAT renames Ada.Characters.Latin_1;
   package DIR renames Ada.Directories;
   package FOP renames File_Operations;
   package LOG renames PortScan.Log;
   package UTL renames Utilities;


   --------------------------------------------------------------------------------------------
   --  produce_page
   --------------------------------------------------------------------------------------------
   procedure produce_page
     (specs   : Portspecs;
      variant : String;
      dossier : TIO.File_Type;
      portdir : String;
      blocked : String;
      created : CAL.Time;
      changed : CAL.Time;
      devscan : Boolean) is
   begin
      TIO.Put_Line (dossier, page_header ("Ravenport: " & specs.get_namebase));
      TIO.Put_Line (dossier, generate_body (specs   => specs,
                                            variant => variant,
                                            portdir => portdir,
                                            blocked => blocked,
                                            created => created,
                                            changed => changed,
                                            devscan => devscan));
      TIO.Put_Line (dossier, page_footer);
   end produce_page;


   --------------------------------------------------------------------------------------------
   --  escape_value
   --------------------------------------------------------------------------------------------
   function escape_value (raw : String) return String
   is
      function htmlval (rawchar : Character) return String;

      focus : constant String :=
        LAT.Ampersand &
        LAT.Quotation &
        LAT.Less_Than_Sign &
        LAT.Greater_Than_Sign;
      curlen : Natural := raw'Length;
      result : String (1 .. raw'Length * 6) := (others => ' ');

      function htmlval (rawchar : Character) return String is
      begin
         case rawchar is
            when LAT.Ampersand         => return "&amp;";
            when LAT.Quotation         => return "&quot;";
            when LAT.Less_Than_Sign    => return "&lt;";
            when LAT.Greater_Than_Sign => return "&gt;";
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
      csslink : constant String := "../../../style/ravenports.css";
      cctrl   : constant String := "public, max-age=21600";  --  valid 6 hours
   begin
      return
        "<!doctype html" & bing &
        "<html" & nvpair ("lang", "en") & bing &
        "<head" & bing &
        " <title>" & escape_value (title) & "</title" & bing &
        " <meta" & nvpair ("charset", "utf-8") & bing &
        " <meta" & nvpair ("name", "description") & nvpair ("content", content) & bing &
        " <meta" & nvpair ("http-equiv", "Cache-Control") & nvpair ("content", cctrl) & bing &
        " <link" & nvpair ("rel", "stylesheet") & nvpair ("href", csslink) & bing &
        "</head" & bing &
        "<body>";
   end page_header;


   --------------------------------------------------------------------------------------------
   --  page_footer
   --------------------------------------------------------------------------------------------
   function page_footer return String
   is
      bing     : constant String := LAT.Greater_Than_Sign & LAT.LF;
      link1val : constant String := "Ravenports catalog";
      link2val : constant String := "Ravenports official site";
   begin
      return
        " <div" & nvpair ("id", "footer") & bing &
        "  <div" & nvpair ("id", "catlink") & ">" &
        link ("../../../index.html", "footlink", link1val) & " | " &
        link ("http://www.ravenports.com/", "footlink", link2val) &
        "</div" & bing &
        " </div" & bing &
        "</body>" & LAT.LF & "</html>";
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
        "     <td>Port variant" & etd &
        "     <td id='variant'>@VARIANT@" & etd &
        "    " & etr &
        "    " & btr &
        "     <td>Summary" & etd &
        "     <td id='summary'>@TAGLINE@" & etd &
        "    " & etr &
        "@BROKEN@" &
        "@DEPRECATED@" &
        "@ONLY_PLATFORM@" &
        "@EXC_PLATFORM@" &
        "@EXC_ARCH@" &
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
        "     <td>Maintainer" & etd &
        "     <td id='maintainer'>@MAINTAINER@" & etd &
        "    " & etr &
        "    " & btr &
        "     <td>License" & etd &
        "     <td id='license'>@LICENSE@" & etd &
        "    " & etr &
        "    " & btr &
        "     <td>Other variants" & etd &
        "     <td id='othervar'>@OTHERVAR@" & etd &
        "    " & etr &
        "    " & btr &
        "     <td>Ravenports" & etd &
        "     <td id='ravenports'>@LNK_BUILDSHEET@ | @LNK_HISTORY_BS@" & etd &
        "    " & etr &
        "    " & btr &
        "     <td>Ravensource" & etd &
        "     <td id='ravensource'>@LNK_PORT@ | @LNK_HISTORY_PORT@" & etd &
        "    " & etr &
        "    " & btr &
        "     <td>Last modified" & etd &
        "     <td id='mdate'>@MDATETIME@" & etd &
        "    " & etr &
        "    " & btr &
        "     <td>Port created" & etd &
        "     <td id='cdate'>@CDATETIME@" & etd &
        "    " & etr &
        "   </tbody>" & LAT.LF &
        "  </table>" & LAT.LF &
        " " & ediv &
        " <div id='pkgdesc'>" & LAT.LF &
        "  <div id='pdtitle'>Subpackage Descriptions" & ediv &
        "  <table id='pdt2'>" & LAT.LF &
        "   <tbody>" & LAT.LF &
        "@DESCBODY@" &
        "   </tbody>" & LAT.LF &
        "  </table>" & LAT.LF &
        " " & ediv &
        " <div id='options'>" & LAT.LF &
        "  <div id='optiontitle'>" &
        "Configuration Switches (platform-specific settings discarded)" & ediv &
        "  <div id='optionblock'>@OPTIONBLOCK@" & ediv &
        " " & ediv &
        " <div id='dependencies'>" & LAT.LF &
        "  <div id='deptitle'>Package Dependencies by Type" & ediv &
        "  <table id='dpt3'>" & LAT.LF &
        "   <tbody>" & LAT.LF &
        "@DEPBODY@" &
        "   </tbody>" & LAT.LF &
        "  </table>" & LAT.LF &
        " " & ediv &
        " <div id='master_sites'>" & LAT.LF &
        "  <div id='mstitle'>Download groups" & ediv &
        "  <table id='dlt4'>" & LAT.LF &
        "   <tbody>" & LAT.LF &
        "@SITES@" &
        "   </tbody>" & LAT.LF &
        "  </table>" & LAT.LF &
        " " & ediv &
        " <div id='distinfo'>" & LAT.LF &
        "  <div id='disttitle'>Distribution File Information" & ediv &
        "  <div id='distblock'>@DISTINFO@" & ediv &
        " " & ediv &
        " <div id='upstream'>" & LAT.LF &
        "  <div id='ustitle'>Ports that require @NAMEBASE@:@VARIANT@" & ediv &
        "  <div id='upstream_inner'>" & LAT.LF &
        "  <table id='ust5'>" & LAT.LF &
        "   <tbody>" & LAT.LF &
        "@UPSTREAM@" &
        "   </tbody>" & LAT.LF &
        "  </table>" & LAT.LF &
        " " & ediv &
        " </div>";
   begin
      return HT.replace_all (S => raw, reject => LAT.Apostrophe, shiny  => LAT.Quotation);
   end body_template;


   --------------------------------------------------------------------------------------------
   --  two_cell_row_template
   --------------------------------------------------------------------------------------------
   function two_cell_row_template return String is
   begin
      return
        "    <tr>" & LAT.LF &
        "     <td>@CELL1@</td>" & LAT.LF &
        "     <td>@CELL2@</td>" & LAT.LF &
        "    </tr>" & LAT.LF;
   end two_cell_row_template;


   --------------------------------------------------------------------------------------------
   --  link
   --------------------------------------------------------------------------------------------
   function link (href, link_class, value : String) return String is
   begin
      return "<a" & nvpair ("href", href) & nvpair ("class", link_class) & ">" & value & "</a>";
   end link;


   --------------------------------------------------------------------------------------------
   --  format_homepage
   --------------------------------------------------------------------------------------------
   function format_homepage (homepage : String) return String is
   begin
      if homepage = homepage_none then
         return "No known homepage";
      end if;
      return link (homepage, "hplink", homepage);
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
   --  other_variants
   --------------------------------------------------------------------------------------------
   function other_variants (specs : Portspecs; variant : String) return String
   is
      nvar    : Natural := specs.get_number_of_variants;
      counter : Natural := 0;
      result  : HT.Text;
   begin
      if nvar = 1 then
         return "There are no other variants.";
      end if;
      for x in 1 .. nvar loop
         declare
            nextvar : constant String := specs.get_list_item (sp_variants, x);
         begin
            if nextvar /= variant then
               counter := counter + 1;
               if counter > 1 then
                  HT.SU.Append (result, " | ");
               end if;
               HT.SU.Append (result, link ("../" & nextvar & "/", "ovlink", nextvar));
            end if;
         end;
      end loop;
      return HT.USS (result);
   end other_variants;


   --------------------------------------------------------------------------------------------
   --  subpackage_description_block
   --------------------------------------------------------------------------------------------
   function subpackage_description_block
     (specs    : Portspecs;
      namebase : String;
      variant  : String;
      portdir  : String) return String
   is
      function description (variant, subpackage : String) return String;

      num_pkgs : Natural := specs.get_subpackage_length (variant);
      result   : HT.Text;
      id2      : constant String := namebase & LAT.Hyphen & variant;

      function description (variant, subpackage : String) return String
      is
         trunk : constant String := portdir & "/descriptions/desc.";
         desc1 : constant String := trunk & subpackage & "." & variant;
         desc2 : constant String := trunk & subpackage;
      begin
         if DIR.Exists (desc1) then
            return FOP.get_file_contents (desc1);
         elsif DIR.Exists (desc2) then
            return FOP.get_file_contents (desc2);
         end if;
         if subpackage = "docs" then
            return "This is the documents subpackage of the " & id2 & " port.";
         elsif subpackage = "examples" then
            return "This is the examples subpackage of the " & id2 & " port.";
         elsif subpackage = "complete" then
            return
              "This is the " & id2 & " metapackage." & LAT.LF &
              "It pulls in all subpackages of " & id2 & ".";
         else
            return "Subpackage description undefined (port maintainer error).";
         end if;
      end description;
   begin
      for x in 1 .. num_pkgs loop
         declare
            row  : HT.Text := HT.SUS (two_cell_row_template);
            spkg : constant String  := specs.get_subpackage_item (variant, x);
         begin
            --  Don't escape CELL2, it's preformatted
            row := HT.replace_substring (row, "@CELL1@", spkg);
            row := HT.replace_substring (row, "@CELL2@", description (variant, spkg));
            HT.SU.Append (result, row);
         end;
      end loop;
      return HT.USS (result);
   end subpackage_description_block;


   --------------------------------------------------------------------------------------------
   --  dependency_block
   --------------------------------------------------------------------------------------------
   function dependency_block (specs : Portspecs) return String
   is
      function link_block (field : spec_field) return String;
      procedure add_row (field : spec_field; listlen : Natural);

      result : HT.Text;

      nb  : constant Natural := specs.get_list_length (sp_build_deps);
      nbr : constant Natural := specs.get_list_length (sp_buildrun_deps);
      nr  : constant Natural := specs.get_list_length (sp_run_deps);
      xr  : constant Natural := Natural (specs.extra_rundeps.Length);
      sb  : constant Natural := Natural (specs.opsys_b_deps.Length);
      sbr : constant Natural := Natural (specs.opsys_br_deps.Length);
      sr  : constant Natural := Natural (specs.opsys_r_deps.Length);

      procedure add_row (field : spec_field; listlen : Natural) is
      begin
         if listlen > 0 then
            declare
               row  : HT.Text := HT.SUS (two_cell_row_template);
            begin
               if field = sp_build_deps then
                  row := HT.replace_substring (row, "@CELL1@", "Build (only)");
               elsif field = sp_buildrun_deps then
                  row := HT.replace_substring (row, "@CELL1@", "Build and Runtime");
               else
                  row := HT.replace_substring (row, "@CELL1@", "Runtime (only)");
               end if;
               row := HT.replace_substring (row, "@CELL2@", link_block (field));
               HT.SU.Append (result, row);
            end;
         end if;
      end add_row;

      function link_block (field : spec_field) return String
      is
         procedure spkg_scan         (position : list_crate.Cursor);
         procedure opsys_scan        (position : list_crate.Cursor);
         procedure dump_dep          (position : string_crate.Cursor);
         procedure process_opsys_dep (position : string_crate.Cursor);
         procedure dump_opsys_dep    (position : def_crate.Cursor);

         listlen : constant Natural := specs.get_list_length (field);
         cell : HT.Text;
         spkg : HT.Text;
         ostr : HT.Text;
         tempstore : def_crate.Map;

         procedure spkg_scan (position : list_crate.Cursor)
         is
            rec : group_list renames list_crate.Element (position);
         begin
            spkg := rec.group;
            rec.list.Iterate (dump_dep'Access);
         end spkg_scan;

         procedure opsys_scan (position : list_crate.Cursor)
         is
            rec : group_list renames list_crate.Element (position);
         begin
            ostr := rec.group;
            rec.list.Iterate (process_opsys_dep'Access);
         end opsys_scan;

         procedure dump_dep (position : string_crate.Cursor)
         is
            dep      : String := HT.USS (string_crate.Element (position));
            namebase : String := HT.specific_field (dep, 1, ":");
            bucket   : String := UTL.bucket (namebase);
            variant  : String := HT.specific_field (dep, 3, ":");
            href     : String := "../../../bucket_" & bucket & "/" & namebase & "/" & variant;
            value    : String := dep & " (" & HT.USS (spkg) & " subpackage)";
            lnk      : String := link (href, "deplink", value);
         begin
            if HT.IsBlank (cell) then
               HT.SU.Append (cell, LAT.LF & lnk);
            else
               HT.SU.Append (cell, "<br/>" & LAT.LF & lnk);
            end if;
         end dump_dep;

         procedure dump_opsys_dep (position : def_crate.Cursor)
         is
            dep      : String := HT.USS (def_crate.Key (position));
            namebase : String := HT.specific_field (dep, 1, ":");
            bucket   : String := UTL.bucket (namebase);
            variant  : String := HT.specific_field (dep, 3, ":");
            href     : String := "../../../bucket_" & bucket & "/" & namebase & "/" & variant;
            value    : String := dep & " (" & HT.USS (def_crate.Element (position)) & ")";
            lnk      : String := link (href, "deplink", value);
         begin
            if HT.IsBlank (cell) then
               HT.SU.Append (cell, LAT.LF & lnk);
            else
               HT.SU.Append (cell, "<br/>" & LAT.LF & lnk);
            end if;
         end dump_opsys_dep;

         procedure process_opsys_dep (position : string_crate.Cursor)
         is
            new_index : HT.Text renames string_crate.Element (position);
            new_value : HT.Text;
         begin
            if tempstore.Contains (new_index) then
               new_value := tempstore.Element (new_index);
               HT.SU.Append (new_value, ", " & HT.USS (ostr));
               tempstore.Delete (new_index);
               tempstore.Insert (new_index, new_value);
            else
               tempstore.Insert (new_index, ostr);
            end if;
         end process_opsys_dep;

      begin
         for x in 1 .. listlen loop
            declare
               dep      : String := specs.get_list_item (field, x);
               namebase : String := HT.specific_field (dep, 1, ":");
               bucket   : String := UTL.bucket (namebase);
               variant  : String := HT.specific_field (dep, 3, ":");
               href     : String := "../../../bucket_" & bucket & "/" & namebase & "/" & variant;
               lnk      : String := link (href, "deplink", dep);
            begin
               if x = 1 then
                  HT.SU.Append (cell, LAT.LF & lnk);
               else
                  HT.SU.Append (cell, "<br/>" & LAT.LF & lnk);
               end if;
            end;
         end loop;
         if field = sp_build_deps then
            specs.opsys_b_deps.Iterate (opsys_scan'Access);
         end if;
         if field = sp_buildrun_deps then
            specs.opsys_br_deps.Iterate (opsys_scan'Access);
         end if;
         if field = sp_run_deps then
            specs.opsys_r_deps.Iterate (opsys_scan'Access);
            specs.extra_rundeps.Iterate (spkg_scan'Access);
         end if;
         tempstore.Iterate (dump_opsys_dep'Access);
         return HT.USS (cell);
      end link_block;

   begin
      if nb + nr + nbr + xr = 0 then
         return "    <tr><td>This package has no dependency requirements of any kind.</td></tr>";
      end if;
      add_row (sp_build_deps, nb + sb);
      add_row (sp_buildrun_deps, nbr + sbr);
      add_row (sp_run_deps, nr + sr + xr);
      return HT.USS (result);
   end dependency_block;


   --------------------------------------------------------------------------------------------
   --  retrieve_distinfo
   --------------------------------------------------------------------------------------------
   function retrieve_distinfo (specs : Portspecs; portdir : String) return String
   is
      distinfo : String := portdir & "/distinfo";
   begin
      if DIR.Exists (distinfo) then
         return FOP.get_file_contents (distinfo);
      else
         return "This port does not contain distinfo information.";
      end if;
   end retrieve_distinfo;


   --------------------------------------------------------------------------------------------
   --  master_sites_block
   --------------------------------------------------------------------------------------------
   function master_sites_block (specs : Portspecs) return String
   is
      procedure group_scan (position : list_crate.Cursor);
      procedure dump_sites (position : string_crate.Cursor);
      function make_link (site : String) return String;
      num_groups  : constant Natural := Natural (specs.dl_sites.Length);
      first_group : constant String  := HT.USS (list_crate.Element (specs.dl_sites.First).group);

      cell2  : HT.Text;
      result : HT.Text;

      function make_link (site : String) return String
      is
         lnk : constant String := link (site, "sitelink", site);
      begin
         if HT.contains (site, "://") then
            return lnk;
         else
            return "mirror://" & site;
         end if;
      end make_link;

      procedure dump_sites (position : string_crate.Cursor)
      is
         site_string : HT.Text renames string_crate.Element (position);
         lnk : constant String := make_link (HT.USS (site_string));
      begin
         if HT.IsBlank (cell2) then
            HT.SU.Append (cell2, LAT.LF & lnk);
         else
            HT.SU.Append (cell2, "<br/>" & LAT.LF & lnk);
         end if;
      end dump_sites;

      procedure group_scan (position : list_crate.Cursor)
      is
         rec   : group_list renames list_crate.Element (position);
         cell1 : constant String := HT.USS (rec.group);
         row   : HT.Text := HT.SUS (two_cell_row_template);
      begin
         cell2 := HT.SU.Null_Unbounded_String;
         rec.list.Iterate (dump_sites'Access);
         row := HT.replace_substring (row, "@CELL1@", cell1);
         row := HT.replace_substring (row, "@CELL2@", HT.USS (cell2));
         HT.SU.Append (result, row);
      end group_scan;

   begin
      if num_groups = 1 and then
        first_group = dlgroup_none
      then
         return "    <tr><td>This port does not download anything.</td></tr>";
      end if;
      specs.dl_sites.Iterate (group_scan'Access);
      return HT.USS (result);
   end master_sites_block;


   --------------------------------------------------------------------------------------------
   --  deprecated_message
   --------------------------------------------------------------------------------------------
   function deprecated_message (specs : Portspecs) return String
   is
      row1 : HT.Text := HT.SUS (two_cell_row_template);
      row2 : HT.Text := HT.SUS (two_cell_row_template);
   begin
      if HT.IsBlank (specs.deprecated) then
         return "";
      end if;
      row1 := HT.replace_substring (row1, "@CELL1@", "DEPRECATED");
      row1 := HT.replace_substring (row1, "@CELL2@", HT.USS (specs.deprecated));
      row2 := HT.replace_substring (row2, "@CELL1@", "Expiration Date");
      row2 := HT.replace_substring (row2, "@CELL2@", HT.USS (specs.expire_date));
      return HT.USS (row1) & HT.USS (row2);
   end deprecated_message;


   --------------------------------------------------------------------------------------------
   --  broken_attributes
   --------------------------------------------------------------------------------------------
   function broken_attributes (specs : Portspecs) return String
   is
      procedure group_scan (position : list_crate.Cursor);
      procedure dump_messages (position : string_crate.Cursor);

      row1    : HT.Text := HT.SUS (two_cell_row_template);
      content : HT.Text;
      index   : HT.Text;

      procedure group_scan (position : list_crate.Cursor)
      is
         rec : group_list renames list_crate.Element (position);
      begin
         index := rec.group;
         rec.list.Iterate (dump_messages'Access);
      end group_scan;

      procedure dump_messages (position : string_crate.Cursor)
      is
         message : HT.Text renames string_crate.Element (position);
         esc_msg : String := "[" & HT.USS (index) & "] " & escape_value (HT.USS (message));
      begin
         if HT.IsBlank (content) then
            HT.SU.Append (content, LAT.LF & esc_msg);
         else
            HT.SU.Append (content, "<br/>" & LAT.LF & esc_msg);
         end if;
      end dump_messages;
   begin
      if specs.broken.Is_Empty then
         return "";
      end if;
      specs.broken.Iterate (group_scan'Access);
      row1 := HT.replace_substring (row1, "@CELL1@", "BROKEN");
      row1 := HT.replace_substring (row1, "@CELL2@", HT.USS (content));
      return HT.USS (row1);
   end broken_attributes;


   --------------------------------------------------------------------------------------------
   --  inclusive_platform
   --------------------------------------------------------------------------------------------
   function inclusive_platform (specs : Portspecs) return String
   is
      procedure dump (position : string_crate.Cursor);

      row1    : HT.Text := HT.SUS (two_cell_row_template);
      content : HT.Text;

      procedure dump (position : string_crate.Cursor)
      is
         message : HT.Text renames string_crate.Element (position);
      begin
         if HT.IsBlank (content) then
            HT.SU.Append (content, message);
         else
            HT.SU.Append (content, " | " & HT.USS (message));
         end if;
      end dump;
   begin
      if specs.inc_opsys.Is_Empty then
         return "";
      end if;
      specs.inc_opsys.Iterate (dump'Access);
      row1 := HT.replace_substring (row1, "@CELL1@", "Only for platform");
      row1 := HT.replace_substring (row1, "@CELL2@", HT.USS (content));
      return HT.USS (row1);
   end inclusive_platform;


   --------------------------------------------------------------------------------------------
   --  exclusive_platform
   --------------------------------------------------------------------------------------------
   function exclusive_platform (specs : Portspecs) return String
   is
      procedure dump (position : string_crate.Cursor);

      row1    : HT.Text := HT.SUS (two_cell_row_template);
      content : HT.Text;

      procedure dump (position : string_crate.Cursor)
      is
         message : HT.Text renames string_crate.Element (position);
      begin
         if HT.IsBlank (content) then
            HT.SU.Append (content, message);
         else
            HT.SU.Append (content, " | " & HT.USS (message));
         end if;
      end dump;
   begin
      if specs.exc_opsys.Is_Empty then
         return "";
      end if;
      specs.exc_opsys.Iterate (dump'Access);
      row1 := HT.replace_substring (row1, "@CELL1@", "Exclude platform");
      row1 := HT.replace_substring (row1, "@CELL2@", HT.USS (content));
      return HT.USS (row1);
   end exclusive_platform;


   --------------------------------------------------------------------------------------------
   --  exclusive_arch
   --------------------------------------------------------------------------------------------
   function exclusive_arch (specs : Portspecs) return String
   is
      procedure dump (position : string_crate.Cursor);

      row1    : HT.Text := HT.SUS (two_cell_row_template);
      content : HT.Text;

      procedure dump (position : string_crate.Cursor)
      is
         message : HT.Text renames string_crate.Element (position);
      begin
         if HT.IsBlank (content) then
            HT.SU.Append (content, message);
         else
            HT.SU.Append (content, " | " & HT.USS (message));
         end if;
      end dump;
   begin
      if specs.exc_arch.Is_Empty then
         return "";
      end if;
      specs.exc_arch.Iterate (dump'Access);
      row1 := HT.replace_substring (row1, "@CELL1@", "Exclude architecture");
      row1 := HT.replace_substring (row1, "@CELL2@", HT.USS (content));
      return HT.USS (row1);
   end exclusive_arch;


   --------------------------------------------------------------------------------------------
   --  upstream
   --------------------------------------------------------------------------------------------
   function upstream (blocked : String) return String
   is
      markers : HT.Line_Markers;
      result  : HT.Text;
   begin
      if HT.IsBlank (blocked) then
         return "<tr><td>No other ports depend on this one.</td></tr>" & LAT.LF;
      end if;
      HT.initialize_markers (blocked, markers);
      loop
         exit when not HT.next_line_present (blocked, markers);
         declare
            line : constant String := HT.extract_line (blocked, markers);
            cell : constant String := HT.specific_field (line, 1, ";");
            href : constant String := HT.specific_field (line, 2, ";");
            lnk  : constant String := link (href, "upslink", cell);
            row1 : HT.Text := HT.SUS (two_cell_row_template);
         begin
            row1 := HT.replace_substring (row1, "@CELL1@", lnk);
            row1 := HT.replace_substring (row1, "@CELL2@", HT.specific_field (line, 3, ";"));
            HT.SU.Append (result, row1);
         end;
      end loop;
      return HT.USS (result);
   exception
      when issue : others =>
         return "<tr><td>" & Ada.Exceptions.Exception_Message (issue) & "</td></tr>" & LAT.LF &
           "<tr><td>" & blocked & "</td></tr>" & LAT.LF;
   end upstream;


   --------------------------------------------------------------------------------------------
   --  generate_body
   --------------------------------------------------------------------------------------------
   function generate_body
     (specs   : Portspecs;
      variant : String;
      portdir : String;
      blocked : String;
      created : CAL.Time;
      changed : CAL.Time;
      devscan : Boolean) return String
   is
      result   : HT.Text := HT.SUS (body_template);
      namebase : constant String := specs.get_namebase;
      bucket   : constant String := UTL.bucket (namebase);
      catport  : constant String := "bucket_" & bucket & "/" & namebase;
      subject  : constant String := "Ravenports:%20" & specs.get_namebase & "%20port";
      homepage : constant String := format_homepage (specs.get_field_value (sp_homepage));
      tagline  : constant String := escape_value (specs.get_tagline (variant));
      isocdate : constant String := LOG.timestamp (created, True);
      isomdate : constant String := LOG.timestamp (changed, True);
      licenses : constant String := list_scheme (specs.get_field_value (sp_licenses),
                                                 specs.get_license_scheme);
      lnk_bs   : constant String :=
        link ("https://raw.githubusercontent.com/jrmarino/Ravenports/master/" & catport,
              "ghlink", "Buildsheet");
      lnk_bshy : constant String :=
        link ("https://github.com/jrmarino/Ravenports/commits/master/" & catport,
              "histlink", "History");
      lnk_port : constant String :=
        link ("https://github.com/jrmarino/ravensource/tree/master/" & catport,
              "ghlink", "Port Directory");
      lnk_pthy : constant String :=
        link ("https://github.com/jrmarino/ravensource/commits/master/" & catport,
              "histlink", "History");
   begin
      result := HT.replace_substring (result, "@NAMEBASE@", namebase);
      result := HT.replace_substring (result, "@NAMEBASE@", namebase);
      result := HT.replace_substring (result, "@VARIANT@", variant);
      result := HT.replace_substring (result, "@VARIANT@", variant);
      result := HT.replace_substring (result, "@HOMEPAGE@", homepage);
      result := HT.replace_substring (result, "@TAGLINE@", tagline);
      result := HT.replace_substring (result, "@PKGVERSION@", specs.calculate_pkgversion);
      result := HT.replace_substring (result, "@MAINTAINER@", specs.get_web_contacts (subject));
      result := HT.replace_substring (result, "@KEYWORDS@", specs.get_field_value (sp_keywords));
      result := HT.replace_substring (result, "@LICENSE@", licenses);
      result := HT.replace_substring (result, "@CDATETIME@", isocdate);
      result := HT.replace_substring (result, "@MDATETIME@", isomdate);
      result := HT.replace_substring (result, "@LNK_BUILDSHEET@", lnk_bs);
      result := HT.replace_substring (result, "@LNK_HISTORY_BS@", lnk_bshy);
      result := HT.replace_substring (result, "@LNK_PORT@", lnk_port);
      result := HT.replace_substring (result, "@LNK_HISTORY_PORT@", lnk_pthy);
      result := HT.replace_substring (result, "@OTHERVAR@", other_variants (specs, variant));
      result := HT.replace_substring (result, "@OPTIONBLOCK@", specs.options_summary (variant));
      result := HT.replace_substring (result, "@DISTINFO@", retrieve_distinfo (specs, portdir));
      result := HT.replace_substring (result, "@DEPBODY@", dependency_block (specs));
      result := HT.replace_substring (result, "@SITES@", master_sites_block (specs));
      result := HT.replace_substring (result, "@DEPRECATED@", deprecated_message (specs));
      result := HT.replace_substring (result, "@BROKEN@", broken_attributes (specs));
      result := HT.replace_substring (result, "@ONLY_PLATFORM@", inclusive_platform (specs));
      result := HT.replace_substring (result, "@EXC_PLATFORM@", exclusive_platform (specs));
      result := HT.replace_substring (result, "@EXC_ARCH@", exclusive_arch (specs));
      result := HT.replace_substring (result, "@UPSTREAM@", upstream (blocked));
      result := HT.replace_substring
        (result, "@DESCBODY@", subpackage_description_block (specs, namebase, variant, portdir));
      return HT.USS (result);
   end generate_body;


   --------------------------------------------------------------------------------------------
   --  generate_catalog_index
   --------------------------------------------------------------------------------------------
   function generate_catalog_index
     (dossier : TIO.File_Type;
      row_assembly_block : String) return Boolean is
   begin
      declare
         template_file : String := host_localbase & "/share/ravenadm/catalog.template";
         template      : constant String := FOP.get_file_contents (template_file);
         fullpage      : HT.Text := HT.SUS (template);
      begin
         fullpage := HT.replace_substring (fullpage, "@ROW_ASSY@", row_assembly_block);
         TIO.Put_Line (dossier, HT.USS (fullpage));
         return True;
      end;
   exception
      when others =>
         TIO.Put_Line ("Failed to create the web site index");
         return False;
   end generate_catalog_index;

end Port_Specification.Web;
