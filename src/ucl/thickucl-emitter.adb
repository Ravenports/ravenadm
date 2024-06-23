--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

package body ThickUCL.Emitter is

   package ASF renames Ada.Strings.Fixed;
   package ASU renames Ada.Strings.Unbounded;


   ----------------
   --  emit_ucl  --
   ----------------
   function emit_ucl (tree : UclTree) return String
   is
      procedure scan_key (Position : jar_string.Cursor);
      procedure dive_into_array (vndx : array_index; indent_len : Natural);
      procedure dive_into_object (vndx : object_index; indent_len : Natural);

      canvas : ASU.Unbounded_String;
      stumpkeys : jar_string.Vector;

      procedure scan_key (Position : jar_string.Cursor)
      is
         raw_key : constant String := ASU.To_String (jar_string.Element (Position).payload);
         valtype : Leaf_type;
      begin
         ASU.Append (canvas, format_key (raw_key, False) & ": ");
         valtype := tree.get_data_type (raw_key);
         case valtype is
            when data_not_present =>
               --  This should not be possible
               ASU.Append (canvas, "null");
            when data_string =>
               ASU.Append (canvas, format_string_value (tree.get_base_value (raw_key), True));
            when data_boolean =>
               ASU.Append (canvas, format_boolean_value (tree.get_base_value (raw_key)));
            when data_integer =>
               ASU.Append (canvas, format_integer_value (tree.get_base_value (raw_key)));
            when data_float =>
               ASU.Append (canvas, format_float_value (tree.get_base_value (raw_key)));
            when data_time =>
               ASU.Append (canvas, format_time_value (tree.get_base_value (raw_key)));
            when data_array =>
               ASU.Append (canvas, '[' & LF);
               dive_into_array (tree.get_index_of_base_array (raw_key), 2);
               ASU.Append (canvas, ']' & LF);
            when data_object =>
               ASU.Append (canvas, '{' & LF);
               dive_into_object (tree.get_index_of_base_ucl_object (raw_key), 2);
               ASU.Append (canvas, '}' & LF);
         end case;
      end scan_key;

      procedure dive_into_array (vndx : array_index; indent_len : Natural)
      is
         array_len : constant Natural := tree.get_number_of_array_elements (vndx);
         indent    : constant String (1 .. indent_len) := (others => ' ');
      begin
         for elndx in 0 .. array_len - 1 loop
            declare
               valtype : Leaf_type;
            begin
               valtype := tree.get_array_element_type (vndx, elndx);
               case valtype is
                  when data_not_present =>
                     null;  -- should be impossible
                  when data_integer =>
                     ASU.Append (canvas, indent & format_integer_value
                                 (tree.get_array_element_value (vndx, elndx)));
                  when data_float =>
                     ASU.Append (canvas, indent & format_float_value
                                 (tree.get_array_element_value (vndx, elndx)));
                  when data_string =>
                     ASU.Append (canvas, indent & format_string_value
                                 (tree.get_array_element_value (vndx, elndx), True));
                  when data_boolean =>
                     ASU.Append (canvas, indent & format_boolean_value
                                 (tree.get_array_element_value (vndx, elndx)));
                  when data_time =>
                     ASU.Append (canvas, indent & format_time_value
                                 (tree.get_array_element_value (vndx, elndx)));
                  when data_array =>
                     ASU.Append (canvas, indent & '[' & LF);
                     dive_into_array (tree.get_array_element_vector_index (vndx, elndx),
                                      indent_len + 2);
                     ASU.Append (canvas, indent & ']' & LF);
                  when data_object =>
                     ASU.Append (canvas, indent & '{' & LF);
                     dive_into_object (tree.get_array_element_object (vndx, elndx),
                                       indent_len + 2);
                     ASU.Append (canvas, indent & '}' & LF);
               end case;
            end;
         end loop;
      end dive_into_array;

      procedure dive_into_object (vndx : object_index; indent_len : Natural)
      is
         procedure scan_deeper_key (Position : jar_string.Cursor);

         keys   : jar_string.Vector;
         indent : constant String (1 .. indent_len) := (others => ' ');

         procedure scan_deeper_key (Position : jar_string.Cursor)
         is
            this_key : constant String := ASU.To_String (jar_string.Element (Position).payload);
            valtype  : Leaf_type;
         begin
            valtype := tree.get_object_data_type (vndx, this_key);
            ASU.Append (canvas, indent & format_key (this_key, False) & ": ");
            case valtype is
               when data_not_present =>
                  ASU.Append (canvas, "null");  -- should be impossible
               when data_boolean =>
                  ASU.Append (canvas, format_boolean_value
                              (tree.get_object_value (vndx, this_key)));
               when data_float =>
                  ASU.Append (canvas, format_float_value
                              (tree.get_object_value (vndx, this_key)));
               when data_integer =>
                  ASU.Append (canvas, format_integer_value
                              (tree.get_object_value (vndx, this_key)));
               when data_string =>
                  ASU.Append (canvas, format_string_value
                              (tree.get_object_value (vndx, this_key), True));
               when data_time =>
                  ASU.Append (canvas, format_time_value
                              (tree.get_object_value (vndx, this_key)));
               when data_array =>
                  ASU.Append (canvas, '[' & LF);
                  dive_into_array (tree.get_object_array (vndx, this_key), indent_len + 2);
                  ASU.Append (canvas, indent & ']' & LF);
               when data_object =>
                  ASU.Append (canvas, '{' & LF);
                  dive_into_object (tree.get_object_object (vndx, this_key), indent_len + 2);
                  ASU.Append (canvas, indent & '}' & LF);
            end case;
         end scan_deeper_key;
      begin
         tree.get_object_object_keys (vndx, keys);
         keys.Iterate (scan_deeper_key'Access);
      end dive_into_object;

   begin
      tree.get_base_object_keys (stumpkeys);
      stumpkeys.Iterate (scan_key'Access);
      return ASU.To_String (canvas);
   end emit_ucl;


   ------------------------
   --  emit_compact_ucl  --
   ------------------------
   function emit_compact_ucl (tree : UclTree; as_json : Boolean := False) return String
   is
      procedure scan_key (Position : jar_string.Cursor);
      procedure dive_into_array (vndx : array_index);
      procedure dive_into_object (vndx : object_index);
      procedure close_structure (bracket : Character);

      canvas : ASU.Unbounded_String;
      stumpkeys : jar_string.Vector;
      cm : constant Character := ',';

      procedure close_structure (bracket : Character) is
      begin
         if ASU.Element (canvas, ASU.Length (canvas)) = cm then
            ASU.Replace_Element (canvas, ASU.Length (canvas), bracket);
            ASU.Append (canvas, cm);
         else
            ASU.Append (canvas, bracket & cm);
         end if;
      end close_structure;

      procedure scan_key (Position : jar_string.Cursor)
      is
         raw_key : constant String := ASU.To_String (jar_string.Element (Position).payload);
         valtype : Leaf_type;
      begin
         ASU.Append (canvas, format_key (raw_key, as_json) & ":");
         valtype := tree.get_data_type (raw_key);
         case valtype is
            when data_not_present =>
               --  This should not be possible
               ASU.Append (canvas, "null" & cm);
            when data_string =>
               ASU.Append (canvas, format_string_value (tree.get_base_value (raw_key), False, cm));
            when data_boolean =>
               ASU.Append (canvas, format_boolean_value (tree.get_base_value (raw_key), cm));
            when data_integer =>
               ASU.Append (canvas, format_integer_value (tree.get_base_value (raw_key), cm));
            when data_float =>
               ASU.Append (canvas, format_float_value (tree.get_base_value (raw_key), cm));
            when data_time =>
               ASU.Append (canvas, format_time_value (tree.get_base_value (raw_key), cm));
            when data_array =>
               ASU.Append (canvas, '[');
               dive_into_array (tree.get_index_of_base_array (raw_key));
               close_structure (']');
            when data_object =>
               ASU.Append (canvas, '{');
               dive_into_object (tree.get_index_of_base_ucl_object (raw_key));
               close_structure ('}');
         end case;
      end scan_key;

      procedure dive_into_array (vndx : array_index)
      is
         array_len : constant Natural := tree.get_number_of_array_elements (vndx);
      begin
         for elndx in 0 .. array_len - 1 loop
            declare
               valtype : Leaf_type;
            begin
               valtype := tree.get_array_element_type (vndx, elndx);
               case valtype is
                  when data_not_present =>
                     null;  -- should be impossible
                  when data_integer =>
                     ASU.Append (canvas, format_integer_value
                                 (tree.get_array_element_value (vndx, elndx), cm));
                  when data_float =>
                     ASU.Append (canvas, format_float_value
                                 (tree.get_array_element_value (vndx, elndx), cm));
                  when data_string =>
                     ASU.Append (canvas, format_string_value
                                 (tree.get_array_element_value (vndx, elndx), False, cm));
                  when data_boolean =>
                     ASU.Append (canvas, format_boolean_value
                                 (tree.get_array_element_value (vndx, elndx), cm));
                  when data_time =>
                     ASU.Append (canvas, format_time_value
                                 (tree.get_array_element_value (vndx, elndx), cm));
                  when data_array =>
                     ASU.Append (canvas, '[');
                     dive_into_array (tree.get_array_element_vector_index (vndx, elndx));
                     close_structure (']');
                  when data_object =>
                     ASU.Append (canvas, '{');
                     dive_into_object (tree.get_array_element_object (vndx, elndx));
                     close_structure ('}');
               end case;
            end;
         end loop;
      end dive_into_array;

      procedure dive_into_object (vndx : object_index)
      is
         procedure scan_deeper_key (Position : jar_string.Cursor);

         keys   : jar_string.Vector;

         procedure scan_deeper_key (Position : jar_string.Cursor)
         is
            this_key : constant String := ASU.To_String (jar_string.Element (Position).payload);
            valtype  : Leaf_type;
         begin
            valtype := tree.get_object_data_type (vndx, this_key);
            ASU.Append (canvas,  format_key (this_key, as_json) & ":");
            case valtype is
               when data_not_present =>
                  ASU.Append (canvas, "null" & cm);  -- should be impossible
               when data_boolean =>
                  ASU.Append (canvas, format_boolean_value
                              (tree.get_object_value (vndx, this_key), cm));
               when data_float =>
                  ASU.Append (canvas, format_float_value
                              (tree.get_object_value (vndx, this_key), cm));
               when data_integer =>
                  ASU.Append (canvas, format_integer_value
                              (tree.get_object_value (vndx, this_key), cm));
               when data_string =>
                  ASU.Append (canvas, format_string_value
                              (tree.get_object_value (vndx, this_key), False, cm));
               when data_time =>
                  ASU.Append (canvas, format_time_value
                              (tree.get_object_value (vndx, this_key), cm));
               when data_array =>
                  ASU.Append (canvas, '[');
                  dive_into_array (tree.get_object_array (vndx, this_key));
                  close_structure (']');
               when data_object =>
                  ASU.Append (canvas, '{');
                  dive_into_object (tree.get_object_object (vndx, this_key));
                  close_structure ('}');
            end case;
         end scan_deeper_key;
      begin
         tree.get_object_object_keys (vndx, keys);
         keys.Iterate (scan_deeper_key'Access);
      end dive_into_object;

   begin
      ASU.Append (canvas, '{');
      tree.get_base_object_keys (stumpkeys);
      stumpkeys.Iterate (scan_key'Access);
      if ASU.Element (canvas, ASU.Length (canvas)) = cm then
         ASU.Replace_Element (canvas, ASU.Length (canvas), '}');
      else
         ASU.Append (canvas, '}');
      end if;
      return ASU.To_String (canvas);
   end emit_compact_ucl;


   -------------------------
   --  format_time_value  --
   -------------------------
   function format_time_value (raw : RT.Time_Span; terminator : Character := LF) return String
   is
      rawduration : Duration;
   begin
      rawduration := RT.To_Duration (raw);
      return ASF.Trim (rawduration'Img, Ada.Strings.Left) & 's' & terminator;
   end format_time_value;


   --------------------------
   --  format_float_value  --
   --------------------------
   function format_float_value (raw : Float; terminator : Character := LF) return String
   is
      function trimzero (decimal_string : String) return String;

      int_part : Ucl.ucl_integer;
      dec_part : Ucl.ucl_integer;
      negative : Boolean;
      stripped : Float;
      abs_raw  : constant Float := abs (raw);

      function trimzero (decimal_string : String) return String
      is
         arrow : Natural := decimal_string'Last;
      begin
         loop
            exit when arrow = decimal_string'First;
            exit when decimal_string (arrow) /= '0';
            arrow := arrow - 1;
         end loop;
         return decimal_string (decimal_string'First .. arrow);
      end trimzero;
   begin
      negative := raw < 0.0;
      int_part := Ucl.ucl_integer (abs_raw);
      stripped := abs_raw - Float (int_part);
      dec_part := Ucl.ucl_integer (stripped * 1_000_000_000.0);
      declare
         sint_part : constant String := ASF.Trim (int_part'Img, Ada.Strings.Left);
         sdec_part : constant String := ASF.Trim (dec_part'Img, Ada.Strings.Left);
      begin
         if negative then
            return '-' & sint_part & "." & trimzero (sdec_part) & terminator;
         end if;
         return sint_part & "." & trimzero (sdec_part) & terminator;
      end;
   end format_float_value;


   ----------------------------
   --  format_integer_value  --
   ----------------------------
   function format_integer_value (raw : Ucl.ucl_integer; terminator : Character := LF) return String
   is
      raw_image : constant String := raw'Img;
   begin
      if raw < 0 then
         return raw_image & terminator;
      end if;
      return raw_image (raw_image'First + 1 .. raw_image'Last) & terminator;
   end format_integer_value;


   ----------------------------
   --  format_boolean_value  --
   ----------------------------
   function format_boolean_value (raw : Boolean; terminator : Character := LF) return String is
   begin
      case raw is
         when True  => return "true" & terminator;
         when False => return "false" & terminator;
      end case;
   end format_boolean_value;


   ---------------------------
   --  format_string_value  --
   ---------------------------
   function format_string_value
     (raw : String;
      heredoc : Boolean;
      terminator : Character := LF) return String
   is
      backspace : constant Character := Character'Val (8);
      tabchar   : constant Character := Character'Val (9);
      formfeed  : constant Character := Character'Val (12);
      carriage  : constant Character := Character'Val (13);
      DQ        : constant Character := '"';
      backslash : constant Character := Character'Val (92);
      newline   : constant String (1 .. 1) := (1 => LF);
   begin
      if heredoc then
         if ASF.Index (raw, newline) > 0 then
            if raw (raw'Last) = LF then
               return "<<EOD" & LF & raw & "EOD" & LF;
            else
               return "<<EOD" & LF & raw & LF & "EOD" & LF;
            end if;
         end if;
      end if;

      declare
         procedure single_copy (char : Character);
         procedure escape_quote;
         procedure escape_linefeed;
         procedure escape_formfeed;
         procedure escape_tab;
         procedure escape_backspace;
         procedure escape_backslash;
         procedure escape_cr;

         canvas : String (1 .. raw'Length * 2 + 2);
         canlen : Natural := 0;

         procedure single_copy (char : Character) is
         begin
            canlen := canlen + 1;
            canvas (canlen) := char;
         end single_copy;

         procedure escape_quote is
         begin
            single_copy (backslash);
            single_copy (DQ);
         end escape_quote;

         procedure escape_linefeed is
         begin
            single_copy (backslash);
            single_copy ('n');
         end escape_linefeed;

         procedure escape_formfeed is
         begin
            single_copy (backslash);
            single_copy ('f');
         end escape_formfeed;

         procedure escape_tab is
         begin
            single_copy (backslash);
            single_copy ('t');
         end escape_tab;

         procedure escape_backspace is
         begin
            single_copy (backslash);
            single_copy ('b');
         end escape_backspace;

         procedure escape_backslash is
         begin
            single_copy (backslash);
            single_copy (backslash);
         end escape_backslash;

         procedure escape_cr is
         begin
            single_copy (backslash);
            single_copy ('r');
         end escape_cr;

      begin
         single_copy (DQ);
         for k in raw'Range loop
            case raw (k) is
               when DQ        => escape_quote;
               when LF        => escape_linefeed;
               when formfeed  => escape_formfeed;
               when tabchar   => escape_tab;
               when backspace => escape_backspace;
               when carriage  => escape_cr;
               when backslash => escape_backslash;
               when others => single_copy (raw (k));
            end case;
         end loop;
         single_copy (DQ);
         return canvas (1 .. canlen) & terminator;
      end;
   end format_string_value;


   ------------------
   --  format_key  --
   ------------------
   function format_key (raw : String;  as_json : Boolean) return String
   is
      procedure single_copy (char : Character);
      procedure escape_quote;
      procedure copy_set_quote (char : Character);

      canvas : String (1 .. raw'Length * 2);
      canlen : Natural := 0;
      quotes : Boolean := False;
      SQ     : constant Character := Character'Val (39);
      DQ     : constant Character := '"';

      procedure single_copy (char : Character) is
      begin
         canlen := canlen + 1;
         canvas (canlen) := char;
      end single_copy;

      procedure escape_quote is
      begin
         quotes := True;
         single_copy ('\');
         single_copy (SQ);
      end escape_quote;

      procedure copy_set_quote (char : Character) is
      begin
         quotes := True;
         single_copy (char);
      end copy_set_quote;
   begin
      if as_json then
         quotes := True;
      end if;
      for k in raw'Range loop
         case raw (k) is
            when '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' | '-' | '_' =>
               single_copy (raw (k));
            when Character'Val (0) .. Character'Val (31) |
                 Character'Val (127) .. Character'Val (255) =>
               null;
            when SQ =>
               escape_quote;
            when ' ' .. '&' | '(' .. ',' | '.' | '/' | ':' .. '@' |
               '[' .. '^' | '`' | '{' .. '~' =>
               copy_set_quote (raw (k));
         end case;
      end loop;
      if quotes then
         return DQ & canvas (1 .. canlen) & DQ;
      end if;
      return canvas (1 .. canlen);
   end format_key;

end ThickUCL.Emitter;
