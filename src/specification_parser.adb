--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with File_Operations;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Exceptions;

package body Specification_Parser is

   package FOP renames File_Operations;
   package LAT renames Ada.Characters.Latin_1;
   package AS  renames Ada.Strings;
   package EX  renames Ada.Exceptions;

   --------------------------------------------------------------------------------------------
   --  parse_specification_file
   --------------------------------------------------------------------------------------------
   procedure parse_specification_file
     (dossier : String;
      success : out Boolean)
   is
      contents : constant String := FOP.get_file_contents (dossier);
      markers  : HT.Line_Markers;
      linenum  : Natural := 0;
      seen_namebase : Boolean := False;
      line_array    : spec_array;
      line_singlet  : spec_singlet;
      last_array    : spec_array    := not_array;
      last_singlet  : spec_singlet  := not_singlet;
      last_seen     : type_category := cat_none;
   begin
      success := False;
      specification.initialize;
      HT.initialize_markers (contents, markers);
      loop
         exit when not HT.next_line_present (contents, markers);
         linenum := linenum + 1;
         declare
            line : constant String := HT.extract_line (contents, markers);
            LN   : constant String := "Line" & linenum'Img & ": ";
         begin
            if HT.IsBlank (line) then
               goto line_done;
            end if;
            if HT.trailing_whitespace_present (line) then
               last_parse_error := HT.SUS (LN & "Detected trailing white space");
               exit;
            end if;
            if HT.trapped_space_character_present (line) then
               last_parse_error := HT.SUS (LN & "Detected trapped space before hard tab");
               exit;
            end if;
            if line (line'First) = '#' then
               if line'Length = 1 then
                  goto line_done;
               end if;
               if line'Length > 79 then
                  last_parse_error := HT.SUS (LN & "Comment length exceeds 79 columns");
                  exit;
               end if;
               if line (line'First + 1) /= LAT.Space then
                  last_parse_error := HT.SUS (LN & "Space does not follow hash in comment");
                  exit;
               end if;
               goto line_done;
            end if;
            line_array := determine_array (line);
            if line_array = not_array then
               line_singlet := determine_singlet (line);
            else
               line_singlet := not_singlet;
            end if;

            if line_singlet = not_singlet and then
              line_array = not_array
            then
               case last_seen is
                  when cat_array   => line_array   := last_array;
                  when cat_singlet => line_singlet := last_singlet;
                  when cat_none    => null;
               end case;
            end if;

            begin
               if line_array = def then
                  if seen_namebase then
                     last_parse_error := HT.SUS (LN & "DEF can't appear after NAMEBASE");
                     exit;
                  end if;
                  declare
                     defkey   : HT.Text := retrieve_key (line);
                     defvalue : HT.Text := HT.SUS (retrieve_single_value (line));
                  begin
                     if spec_definitions.Contains (defkey) then
                        raise duplicate_key with HT.USS (defkey);
                     else
                        spec_definitions.Insert (Key      => defkey,
                                                 New_Item => defvalue);
                     end if;
                  end;
               end if;

               case line_singlet is
                  when namebase =>
                     seen_namebase := True;
                     specification.set_single_string (PSP.sp_namebase,
                                                      retrieve_single_value (line));
                  when version =>
                     specification.set_single_string (PSP.sp_version,
                                                      retrieve_single_value (line));
                  when revision =>
                     specification.set_natural_integer (PSP.sp_revision,
                                                       retrieve_single_integer (line));
                  when epoch =>
                     specification.set_natural_integer (PSP.sp_epoch,
                                                       retrieve_single_integer (line));
                  when keywords =>
                     build_list (PSP.sp_keywords, line);
                  when variants =>
                     build_list (PSP.sp_variants, line);
                  when not_singlet =>
                     null;
               end case;
               last_singlet := line_singlet;
               last_seen := cat_singlet;
            exception
               when F1 : PSP.misordered =>
                  last_parse_error := HT.SUS (LN & "Field " & EX.Exception_Message (F1) &
                                                " appears out of order");
                  exit;
               when F2 : PSP.contains_spaces =>
                  last_parse_error := HT.SUS (LN & "Multiple values found");
                  exit;
               when F3 : PSP.wrong_type =>
                  last_parse_error := HT.SUS (LN & "Field " & EX.Exception_Message (F3) &
                                                " DEV ISSUE: matched to wrong type");
                  exit;
               when F4 : PSP.wrong_value =>
                  last_parse_error := HT.SUS (LN & EX.Exception_Message (F4));
                  exit;
               when F5 : mistabbed =>
                  last_parse_error := HT.SUS (LN & "value not aligned to column-24 (tab issue)");
                  exit;
               when F6 : missing_definition =>
                  last_parse_error := HT.SUS (LN & "Variable expansion: definition missing.");
                  exit;
               when F7 : extra_spaces =>
                  last_parse_error := HT.SUS (LN & "extra spaces detected between list items.");
                  exit;
               when F8 : expansion_too_long =>
                  last_parse_error := HT.SUS (LN & "expansion exceeds 512-char maximum");
                  exit;
               when F9 : duplicate_key =>
                  last_parse_error := HT.SUS (LN & "array key '" & EX.Exception_Message (F9) &
                                                "' duplicated");
                  exit;
            end;
            <<line_done>>
         end;
      end loop;
      if HT.IsBlank (last_parse_error) then
         success := True;
      end if;
   exception
      when FOP.file_handling =>
         success := False;
         last_parse_error := HT.SUS ("Failed to dump contents of " & dossier);
   end parse_specification_file;


   --------------------------------------------------------------------------------------------
   --  get_parse_error
   --------------------------------------------------------------------------------------------
   function get_parse_error return String is
   begin
      return HT.USS (last_parse_error);
   end get_parse_error;


   --------------------------------------------------------------------------------------------
   --  expand_value
   --------------------------------------------------------------------------------------------
   function expand_value (value : String) return String
   is
      function translate (variable : String) return String;
      function will_fit  (CL, CR : Natural; new_text : String) return Boolean;
      procedure exchange (CL, CR : Natural; new_text : String);
      function modify    (curvalue : HT.Text;
                          modifier : String;
                          valid    : out Boolean) return HT.Text;

      canvas       : String (1 .. 512);
      found        : Boolean;
      front_marker : Natural;
      curly_left   : Natural;
      curly_right  : Natural;
      trans_error  : Natural;
      delimiter    : HT.Text := HT.SUS ("/");

      function will_fit (CL, CR : Natural; new_text : String) return Boolean
      is
         old_length : Natural := CR - CL + 1;
         new_marker : Natural := front_marker - old_length + new_text'Length;
      begin
         return (new_marker < canvas'Length);
      end will_fit;

      procedure exchange (CL, CR : Natural; new_text : String)
      is
         old_length : Natural := CR - CL + 1;
      begin
         if old_length = new_text'Length then
            canvas (CL .. CR) := new_text;
            return;
         end if;

         declare
            final_segment : String := canvas (CR + 1 .. front_marker);
         begin
            --  if old_length < new_text'Length then
            --     Shift later text to the right from the end (expand)
            --  else
            --     Shift later text to the left from the beginning (shrink)

            front_marker := front_marker + (new_text'Length - old_length);
            canvas (CL .. front_marker) := new_text & final_segment;
         end;
      end exchange;

      function translate (variable : String) return String
      is
         basename   : HT.Text;
         result     : HT.Text := HT.blank;
         colon_pos  : Natural := AS.Fixed.Index (variable, ":");
         colon_next : Natural;
         end_mark   : Natural;
         found      : Boolean;
         valid      : Boolean;
      begin
         if colon_pos = 0 then
            basename := HT.SUS (variable);
         else
            basename := HT.SUS (variable (variable'First .. colon_pos - 1));
         end if;

         if spec_definitions.Contains (basename) then
            result := spec_definitions.Element (basename);
         else
            trans_error := 1;  -- missing_definition
            return "";
         end if;

         loop
            exit when colon_pos = 0;
            colon_next := colon_pos + 1;
            found := False;
            loop
               exit when colon_next > variable'Last;
               if variable (colon_next) = LAT.Colon then
                  found := True;
                  exit;
               end if;
               colon_next := colon_next + 1;
            end loop;
            if found then
               end_mark := colon_next - 1;
            else
               end_mark := variable'Last;
               colon_next := 0;
            end if;
            if end_mark = colon_pos then
               trans_error := 2;  --  bad_modifier
               return "";
            end if;
            result := modify (result, variable (colon_pos + 1 .. end_mark), valid);
            if not valid then
               trans_error := 2;  --  bad_modifier
               return "";
            end if;
            colon_pos := colon_next;
         end loop;
         trans_error := 0;
         return HT.USS (result);
      end translate;

      function modify (curvalue : HT.Text;
                       modifier : String;
                       valid    : out Boolean) return HT.Text
      is
         ml : Natural := modifier'Length;
         dot : HT.Text := HT.SUS (".");
      begin
         valid := True;
         if modifier = "LC" then
            return HT.lowercase (curvalue);
         elsif modifier = "UC" then
            return HT.uppercase (curvalue);
         elsif modifier = "H" then
            return HT.head (curvalue, delimiter);
         elsif modifier = "T" then
            return HT.tail (curvalue, delimiter);
         elsif modifier = "R" then
            return HT.head (curvalue, dot);
         elsif modifier = "E" then
            return HT.tail (curvalue, dot);
         elsif ml >= 6 and then
           modifier (modifier'First .. modifier'First + 3) = "DL=" & LAT.Quotation and then
           modifier (modifier'Last) = LAT.Quotation
         then
            delimiter := HT.SUS (modifier (modifier'First + 4 .. modifier'Last - 1));
            return curvalue;
         elsif ml >= 5 and then modifier (modifier'First) = 'S' then
            declare
               separator  : Character;
               position_2 : Natural := 0;
               position_3 : Natural := 0;
               repeat     : Boolean := False;
               its_bad    : Boolean := False;
               new_value  : HT.Text := curvalue;
            begin
               if modifier (modifier'First + 1) = LAT.Solidus or else
                 modifier (modifier'First + 1) = LAT.Vertical_Line
               then
                  separator := modifier (modifier'First + 1);
                  for arrow in Natural range modifier'First + 2 .. modifier'Last loop
                     if modifier (arrow) = separator then
                        if position_2 = 0 then
                           position_2 := arrow;
                        elsif position_3 = 0 then
                           position_3 := arrow;
                        else
                           its_bad := True;
                        end if;
                     end if;
                  end loop;
                  if position_3 = 0 then
                     its_bad := True;
                  else
                     if position_3 < modifier'Last then
                        if (position_3 = modifier'Last - 1) and then
                          modifier (modifier'Last) = 'g'
                        then
                           repeat := True;
                        else
                           its_bad := True;
                        end if;
                     end if;
                  end if;
                  if not its_bad then
                     declare
                        oldst : String := modifier (modifier'First + 2 .. position_2 - 1);
                        newst : String := modifier (position_2 + 1 .. position_3 - 1);
                     begin
                        loop
                           exit when not HT.contains (new_value, oldst);
                           new_value := HT.replace_substring (US         => new_value,
                                                              old_string => oldst,
                                                              new_string => newst);
                           exit when not repeat;
                        end loop;
                     end;
                     return new_value;
                  end if;
               end if;
            end;
         end if;
         valid := False;
         return HT.blank;
      end modify;

   begin
      if not HT.contains (S => value, fragment => "${") then
         return value;
      end if;
      if value'Length > canvas'Length then
         raise expansion_too_long;
      end if;
      front_marker := value'Length;
      canvas (1 .. front_marker) := value;
      loop
         curly_left := AS.Fixed.Index (canvas (1 .. front_marker), "${");
         if curly_left = 0 then
            return canvas (1 .. front_marker);
         end if;
         found := False;
         curly_right := curly_left + 2;
         loop
            exit when curly_right > front_marker;
            if canvas (curly_right) = LAT.Right_Curly_Bracket then
               found := True;
               exit;
            end if;
            curly_right := curly_right + 1;
         end loop;
         if found then
            if curly_right - curly_left - 2 = 0 then
               raise missing_definition with "zero-length variable name";
            end if;
            declare
               expanded : String := translate (canvas (curly_left + 2 .. curly_right - 1));
            begin
               if trans_error = 0 then
                  if will_fit (curly_left, curly_right, expanded) then
                     exchange (curly_left, curly_right, expanded);
                  else
                     raise expansion_too_long;
                  end if;
               elsif trans_error = 1 then
                  raise missing_definition;
               elsif trans_error = 2 then
                  raise bad_modifier;
               end if;
            end;
         end if;
      end loop;
   end expand_value;


   --------------------------------------------------------------------------------------------
   --  determine_array
   --------------------------------------------------------------------------------------------
   function determine_array (line : String) return spec_array
   is
      function known (array_name : String) return Boolean;
      function known (array_name : String) return Boolean
      is
         len : constant Natural := array_name'Length;
      begin
         return (line'Length > len + 6) and then
           line (line'First .. line'First + len) = array_name & LAT.Left_Square_Bracket;
      end known;
   begin
      if not HT.contains (S => line, fragment => "]=" & LAT.HT) then
         return not_array;
      end if;
      if known ("DEF") then
         return def;
      elsif known ("SDESC") then
         return sdesc;
      else
         return not_array;
      end if;
   end determine_array;


   --------------------------------------------------------------------------------------------
   --  determine_singlet
   --------------------------------------------------------------------------------------------
   function determine_singlet (line : String) return spec_singlet
   is
      function known (varname : String) return Boolean;
      function known (varname : String) return Boolean
      is
         len : constant Natural := varname'Length;
      begin
         return (line'Length > len + 2) and then
           line (line'First .. line'First + len) = varname & "=";
      end known;
   begin
      if not HT.contains (S => line, fragment => "=" & LAT.HT) then
         return not_singlet;
      end if;
      if known ("NAMEBASE") then
         return namebase;
      elsif known ("VERSION") then
         return version;
      elsif known ("REVISION") then
         return revision;
      elsif known ("EPOCH") then
         return epoch;
      elsif known ("KEYWORDS") then
         return keywords;
      elsif known ("VARIANTS") then
         return variants;
      else
         return not_singlet;
      end if;
   end determine_singlet;


   --------------------------------------------------------------------------------------------
   --  retrieve_single_value
   --------------------------------------------------------------------------------------------
   function retrieve_single_value (line : String) return String
   is
      wrkstr : String (1 .. line'Length) := line;
      equals : Natural := AS.Fixed.Index (wrkstr, LAT.Equals_Sign & LAT.HT);
      c81624 : Natural := ((equals / 8) + 1) * 8;
      --  f(4)  =  8    ( 2 ..  7)
      --  f(8)  = 16;   ( 8 .. 15)
      --  f(18) = 24;   (16 .. 23)
      --  We are looking for an exact number of tabs starting at equals + 2:
      --  if c81624 = 8, then we need 2 tabs.  IF it's 16 then we need 1 tab,
      --  if it's 24 then there can be no tabs, and if it's higher, that's a problem.
   begin
      if equals = 0 then
         --  Support triple-tab line too.
         if wrkstr'Length > 3 and then
           wrkstr (wrkstr'First .. wrkstr'First + 2) = LAT.HT & LAT.HT & LAT.HT
         then
            equals := wrkstr'First + 1;
            c81624 := 24;
         else
            raise missing_definition;
         end if;
      end if;
      if c81624 > 24 then
         raise mistabbed;
      end if;
      declare
         rest : constant String := wrkstr (equals + 2 .. wrkstr'Last);
         contig_tabs : Natural := 0;
         arrow : Natural := rest'First;
      begin
         loop
            exit when arrow > rest'Last;
            exit when rest (arrow) /= LAT.HT;
            contig_tabs := contig_tabs + 1;
            arrow := arrow + 1;
         end loop;
         if ((c81624 = 8) and then (contig_tabs /= 2)) or else
           ((c81624 = 16) and then (contig_tabs /= 1)) or else
           ((c81624 = 24) and then (contig_tabs /= 0))
         then
            raise mistabbed;
         end if;
         return expand_value (rest (rest'First + contig_tabs .. rest'Last));
      end;
   end retrieve_single_value;


   --------------------------------------------------------------------------------------------
   --  retrieve_single_integer
   --------------------------------------------------------------------------------------------
   function retrieve_single_integer (line : String) return Natural
   is
      result   : Natural;
      strvalue : constant String := retrieve_single_value (line);
      --  let any exceptions cascade
   begin
      result := Integer'Value (strvalue);
      return result;
   exception
      when Constraint_Error =>
         raise integer_expected;
   end retrieve_single_integer;


   --------------------------------------------------------------------------------------------
   --  retrieve_key
   --------------------------------------------------------------------------------------------
   function retrieve_key (line : String) return HT.Text
   is
      LB : Natural := AS.Fixed.Index (line, "[");
      RB : Natural := AS.Fixed.Index (line, "]");
   begin
      if LB = 0 or else
        RB = 0 or else
        RB <= LB + 1
      then
         return HT.SUS ("BOGUS");  --  should be impossible
      end if;
      return HT.SUS (line (LB + 1 .. RB - 1));
   end retrieve_key;


   --------------------------------------------------------------------------------------------
   --  build_list
   --------------------------------------------------------------------------------------------
   procedure build_list (field : PSP.spec_field; line : String)
   is
      arrow      : Natural;
      word_start : Natural;
      strvalue   : constant String := retrieve_single_value (line);
      --  let any exceptions cascade
   begin
      --  Handle single item case
      if not HT.contains (S => strvalue, fragment => " ") then
         specification.append_list (field, strvalue);
         return;
      end if;

      --  Check for multiple space error or leading space error
      if HT.contains (S => strvalue, fragment => "  ") or else
        strvalue (strvalue'First) = ' '
      then
         raise extra_spaces;
      end if;

      --  Now we have multiple list items separated by single spaces
      --  We know the original line has no trailing spaces too, btw.
      word_start := strvalue'First;
      arrow := word_start;
      loop
         exit when arrow > strvalue'Last;
         if strvalue (arrow) = ' ' then
            specification.append_list (field, strvalue (word_start .. arrow - 1));
            word_start := arrow + 1;
         end if;
         arrow := arrow + 1;
      end loop;
      specification.append_list (field, strvalue (word_start .. strvalue'Last));

   end build_list;


end Specification_Parser;
