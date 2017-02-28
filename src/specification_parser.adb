--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with File_Operations;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;

package body Specification_Parser is

   package FOP renames File_Operations;
   package LAT renames Ada.Characters.Latin_1;
   package AS  renames Ada.Strings;

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
         end;
         <<line_done>>
      end loop;
      success := True;
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
               arrow      : Natural;
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

end Specification_Parser;
