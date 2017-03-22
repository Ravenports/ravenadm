--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Characters.Latin_1;
with Ada.Characters.Handling;

package body HelperText is

   package AS  renames Ada.Strings;
   package LAT renames Ada.Characters.Latin_1;
   package HAN renames Ada.Characters.Handling;

   --------------------------------------------------------------------------------------------
   --  int2str
   --------------------------------------------------------------------------------------------
   function int2str (A : Integer) return String
   is
      raw : constant String := A'Img;
      len : constant Natural := raw'Length;
   begin
      if A < 0 then
         return raw;
      else
         return raw (2 .. len);
      end if;
   end int2str;


   --------------------------------------------------------------------------------------------
   --  int2text
   --------------------------------------------------------------------------------------------
   function int2text (A : Integer) return Text is
   begin
      return SUS (int2str (A));
   end int2text;


   --------------------------------------------------------------------------------------------
   --  trim
   --------------------------------------------------------------------------------------------
   function trim (S : String) return String is
   begin
      return AS.Fixed.Trim (S, AS.Both);
   end trim;


   --------------------------------------------------------------------------------------------
   --  USS
   --------------------------------------------------------------------------------------------
   function USS (US : Text) return String is
   begin
         return SU.To_String (US);
   end USS;


   --------------------------------------------------------------------------------------------
   --  SUS
   --------------------------------------------------------------------------------------------
   function SUS (S : String) return Text is
   begin
      return SU.To_Unbounded_String (S);
   end SUS;


   --------------------------------------------------------------------------------------------
   --  equivalent #1
   --------------------------------------------------------------------------------------------
   function equivalent (A, B : Text) return Boolean
   is
      use type Text;
   begin
      return A = B;
   end equivalent;


   --------------------------------------------------------------------------------------------
   --  equivalent #2
   --------------------------------------------------------------------------------------------
   function equivalent (A : Text; B : String) return Boolean
   is
      A2S : constant String := USS (A);
   begin
      return A2S = B;
   end equivalent;


   --------------------------------------------------------------------------------------------
   --  hash
   --------------------------------------------------------------------------------------------
   function hash (key : Text) return CON.Hash_Type is
   begin
      return AS.Hash (USS (key));
   end hash;


   --------------------------------------------------------------------------------------------
   --  IsBlank #1
   --------------------------------------------------------------------------------------------
   function IsBlank (US : Text)   return Boolean is
   begin
      return SU.Length (US) = 0;
   end IsBlank;


   --------------------------------------------------------------------------------------------
   --  IsBlank #2
   --------------------------------------------------------------------------------------------
   function IsBlank (S  : String) return Boolean is
   begin
      return S'Length = 0;
   end IsBlank;


   --------------------------------------------------------------------------------------------
   --  contains #1
   --------------------------------------------------------------------------------------------
   function contains (S : String; fragment : String) return Boolean is
   begin
      return (AS.Fixed.Index (Source => S, Pattern => fragment) > 0);
   end contains;


   --------------------------------------------------------------------------------------------
   --  contains #2
   --------------------------------------------------------------------------------------------
   function contains (US : Text; fragment : String) return Boolean is
   begin
      return (SU.Index (Source => US, Pattern => fragment) > 0);
   end contains;


   --------------------------------------------------------------------------------------------
   --  start_index
   --------------------------------------------------------------------------------------------
   function start_index (S : String; fragment : String) return Natural
   is
   begin
      return AS.Fixed.Index (Source => S, Pattern => fragment);
   end start_index;


   --------------------------------------------------------------------------------------------
   --  leads #1
   --------------------------------------------------------------------------------------------
   function leads (S : String; fragment : String) return Boolean is
   begin
      if fragment'Length > S'Length then
         return False;
      end if;
      return (S (S'First .. S'First + fragment'Length - 1) = fragment);
   end leads;


   --------------------------------------------------------------------------------------------
   --  leads #2
   --------------------------------------------------------------------------------------------
   function leads (US : Text; fragment : String) return Boolean is
   begin
      return leads (USS (US), fragment);
   end leads;


   --------------------------------------------------------------------------------------------
   --  trails #1
   --------------------------------------------------------------------------------------------
   function trails (S : String; fragment : String) return Boolean is
   begin
      if fragment'Length > S'Length then
         return False;
      end if;
      return (S (S'Last - fragment'Length + 1 .. S'Last) = fragment);
   end trails;


   --------------------------------------------------------------------------------------------
   --  trails #2
   --------------------------------------------------------------------------------------------
   function trails (US : Text; fragment : String) return Boolean is
   begin
      return trails (USS (US), fragment);
   end trails;


   --------------------------------------------------------------------------------------------
   --  uppercase #1
   --------------------------------------------------------------------------------------------
   function uppercase (US : Text) return Text
   is
      tall : String := uppercase (USS (US));
   begin
      return SUS (tall);
   end uppercase;


   --------------------------------------------------------------------------------------------
   --  uppercase #2
   --------------------------------------------------------------------------------------------
   function uppercase (S : String) return String is
   begin
      return HAN.To_Upper (S);
   end uppercase;


   --------------------------------------------------------------------------------------------
   --  lowercase #1
   --------------------------------------------------------------------------------------------
   function lowercase (US : Text) return Text
   is
      short : String := lowercase (USS (US));
   begin
      return SUS (short);
   end lowercase;


   --------------------------------------------------------------------------------------------
   --  lowercase #2
   --------------------------------------------------------------------------------------------
   function lowercase (S : String) return String is
   begin
      return HAN.To_Lower (S);
   end lowercase;


   --------------------------------------------------------------------------------------------
   --  head #1
   --------------------------------------------------------------------------------------------
   function head (US : Text; delimiter : Text) return Text
   is
      result : constant String := head (USS (US), USS (delimiter));
   begin
      return SUS (result);
   end head;


   --------------------------------------------------------------------------------------------
   --  head #2
   --------------------------------------------------------------------------------------------
   function head (S  : String; delimiter : String) return String
   is
      dl_size      : constant Natural := delimiter'Length;
      back_marker  : constant Natural := S'First;
      front_marker : Natural := S'Last - dl_size + 1;
   begin
      loop
         if front_marker < back_marker then
            --  delimiter never found
            return "";
         end if;
         if S (front_marker .. front_marker + dl_size - 1) = delimiter then
            return S (back_marker .. front_marker - 1);
         end if;
         front_marker := front_marker - 1;
      end loop;
   end head;


   --------------------------------------------------------------------------------------------
   --  tail #1
   --------------------------------------------------------------------------------------------
   function tail (US : Text; delimiter : Text) return Text
   is
      result : constant String := tail (USS (US), USS (delimiter));
   begin
      return SUS (result);
   end tail;


   --------------------------------------------------------------------------------------------
   --  tail #2
   --------------------------------------------------------------------------------------------
   function tail (S : String; delimiter : String) return String
   is
      dl_size      : constant Natural := delimiter'Length;
      back_marker  : constant Natural := S'First;
      front_marker : Natural := S'Last - dl_size + 1;
   begin
      loop
         if front_marker < back_marker then
            --  delimiter never found
            return S;
         end if;
         if S (front_marker .. front_marker + dl_size - 1) = delimiter then
            return S (front_marker + dl_size .. S'Last);
         end if;
         front_marker := front_marker - 1;
      end loop;
   end tail;


   --------------------------------------------------------------------------------------------
   --  part_1
   --------------------------------------------------------------------------------------------
   function part_1 (S : String; separator : String := "/") return String
   is
      slash : Integer := AS.Fixed.Index (S, separator);
   begin
      if slash = 0 then
         return S;
      end if;
      return S (S'First .. slash - 1);
   end part_1;


   --------------------------------------------------------------------------------------------
   --  part_2
   --------------------------------------------------------------------------------------------
   function part_2 (S : String; separator : String := "/") return String
   is
      slash : Integer := AS.Fixed.Index (S, separator);
   begin
      if slash = 0 then
         return S;
      end if;
      return S (slash + separator'Length .. S'Last);
   end part_2;


   --------------------------------------------------------------------------------------------
   --  zeropad
   --------------------------------------------------------------------------------------------
   function zeropad (N : Natural; places : Positive) return String
   is
      template : String (1 .. places) := (others => '0');
      myimage  : constant String := trim (N'Img);
      startpos : constant Natural := 1 + places - myimage'Length;
   begin
      template (startpos .. places) := myimage;
      return template;
   end zeropad;


   --------------------------------------------------------------------------------------------
   --  replace_substring
   --------------------------------------------------------------------------------------------
   function replace_substring (US : Text;
                               old_string : String;
                               new_string : String) return Text
   is
      back_marker  : Natural := SU.Index (Source => US, Pattern => old_string);
      front_marker : Natural := back_marker + old_string'Length - 1;
   begin
      if back_marker = 0 then
         return US;
      end if;
      return SU.Replace_Slice (Source => US,
                               Low    => back_marker,
                               High   => front_marker,
                               By     => new_string);
   end replace_substring;


   --------------------------------------------------------------------------------------------
   --  initialize_markers
   --------------------------------------------------------------------------------------------
   procedure initialize_markers
     (block_text : in String;
      shuttle    : out Line_Markers) is
   begin
      shuttle.back_marker  := block_text'First;
      shuttle.front_marker := block_text'First;
      shuttle.zero_length  := block_text (shuttle.back_marker) = ASCII.LF;
      shuttle.utilized     := False;
   end initialize_markers;


   --------------------------------------------------------------------------------------------
   --  extract_line
   --------------------------------------------------------------------------------------------
   function extract_line
     (block_text : in String;
      shuttle    : in Line_Markers)
      return String is
   begin
      if shuttle.zero_length or else
        shuttle.back_marker < block_text'First or else
        shuttle.front_marker < shuttle.back_marker or else
        shuttle.front_marker > block_text'Last
      then
         return "";
      end if;
      return block_text (shuttle.back_marker .. shuttle.front_marker);
   end extract_line;


   --------------------------------------------------------------------------------------------
   --  next_line_present
   --------------------------------------------------------------------------------------------
   function next_line_present
     (block_text : in String;
      shuttle    : in out Line_Markers)
      return Boolean is
   begin
      if shuttle.front_marker + 2 > block_text'Last then
         return False;
      end if;
      if shuttle.utilized then
         if shuttle.zero_length then
            shuttle.back_marker  := shuttle.front_marker + 1;
         else
            shuttle.back_marker  := shuttle.front_marker + 2;
         end if;
         shuttle.front_marker := shuttle.back_marker;
         shuttle.zero_length  := block_text (shuttle.back_marker) = ASCII.LF;
      end if;
      loop
         shuttle.utilized := True;
         exit when shuttle.front_marker = block_text'Last;
         exit when block_text (shuttle.back_marker) = ASCII.LF;
         exit when block_text (shuttle.front_marker + 1) = ASCII.LF;
         shuttle.front_marker := shuttle.front_marker + 1;
      end loop;
      return True;
   end next_line_present;


   --------------------------------------------------------------------------------------------
   --  extract_file
   --------------------------------------------------------------------------------------------
   function extract_file
     (block_text : in String;
      shuttle    : in out Line_Markers;
      file_size  : in Natural)
      return String
   is
      --  Nomally executed after next_line_present followed by extract_line
      --  Needs to adjust adjust shuttle to prepare for another next_line_present after
      --  file is extracted
   begin
      if shuttle.front_marker + 2 > block_text'Last or else
        not shuttle.utilized or else
        shuttle.zero_length or else
        file_size = 0
      then
         --  User seems to have to screwed up since none of these should be true.
         return "";
      end if;
      shuttle.back_marker  := shuttle.front_marker + 2;
      shuttle.front_marker := shuttle.back_marker + file_size - 1;
      shuttle.zero_length  := False;
      return block_text (shuttle.back_marker .. shuttle.front_marker);
   end extract_file;


   --------------------------------------------------------------------------------------------
   --  trailing_whitespace_present
   --------------------------------------------------------------------------------------------
   function trailing_whitespace_present (line : String) return Boolean
   is
      --  For now, whitespace is considered a hard tab and a space character
      last_char : constant Character := line (line'Last);
   begin
      case last_char is
         when LAT.Space | LAT.HT => return True;
         when others => return False;
      end case;
   end trailing_whitespace_present;


   --------------------------------------------------------------------------------------------
   --  trapped_space_character_present
   --------------------------------------------------------------------------------------------
   function trapped_space_character_present (line : String) return Boolean
   is
      back_marker : Natural := line'First;
   begin
      loop
         exit when back_marker + 1 >= line'Last;
         if line (back_marker) = LAT.Space and then
           line (back_marker + 1) = LAT.HT
         then
            return True;
         end if;
         back_marker := back_marker + 1;
      end loop;
      return False;
   end trapped_space_character_present;


   --------------------------------------------------------------------------------------------
   --  count_char
   --------------------------------------------------------------------------------------------
   function count_char (S : String; focus : Character) return Natural
   is
      result : Natural := 0;
   begin
      for x in S'Range loop
         if S (x) = focus then
            result := result + 1;
         end if;
      end loop;
      return result;
   end count_char;


   --------------------------------------------------------------------------------------------
   --  partial_search
   --------------------------------------------------------------------------------------------
   function partial_search
     (fullstr    : String;
      offset     : Natural;
      end_marker : String) return String
   is
      newstr : String := fullstr (fullstr'First + offset .. fullstr'Last);
      marker : Natural := AS.Fixed.Index (Source => newstr, Pattern => end_marker);
   begin
      if marker = 0 then
         return "";
      end if;
      return newstr (newstr'First .. marker - 1);
   end partial_search;


   --------------------------------------------------------------------------------------------
   --  first_line
   --------------------------------------------------------------------------------------------
   function first_line (S : String) return String
   is
      LF_position : Integer;
   begin
      LF_position := AS.Fixed.Index (Source => S, Pattern => single_LF);
      if LF_position > 0 then
         return S (S'First .. LF_position - 1);
      else
         return S;
      end if;
   end first_line;


   --------------------------------------------------------------------------------------------
   --  bool2str
   --------------------------------------------------------------------------------------------
   function bool2str  (A : Boolean) return String is
   begin
      if A then
         return "true";
      end if;
      return "false";
   end bool2str;


   --------------------------------------------------------------------------------------------
   --  bool2text
   --------------------------------------------------------------------------------------------
   function bool2text (A : Boolean) return Text is
   begin
      return SUS (bool2str (A));
   end bool2text;


   --------------------------------------------------------------------------------------------
   --  specific_line
   --------------------------------------------------------------------------------------------
   function specific_line (S : String; line_number : Positive) return String
   is
      back  : Integer;
      front : Integer := S'First;
   begin
      for line in 1 .. line_number - 1 loop
         back := AS.Fixed.Index (Source => S, Pattern => single_LF, From => front);
         if back <= 0 then
            return "";
         end if;
         front := back + 1;
      end loop;
      back := AS.Fixed.Index (Source => S, Pattern => single_LF, From => front);
      if back > 0 then
         return S (front .. back - 1);
      else
         return S (front .. S'Last);
      end if;
   end specific_line;


   --------------------------------------------------------------------------------------------
   --  specific_field
   --------------------------------------------------------------------------------------------
   function specific_field
     (S            : String;
      field_number : Positive;
      delimiter    : String := " ") return String
   is
      back  : Integer;
      dsize : Natural := delimiter'Length;
      front : Integer := S'First;
   begin
      for field in 1 .. field_number - 1 loop
         back := AS.Fixed.Index (Source => S, Pattern => delimiter, From => front);
         if back <= 0 then
            return "";
         end if;
         front := back + dsize;
      end loop;
      back := AS.Fixed.Index (Source => S, Pattern => delimiter, From => front);
      if back > 0 then
         return S (front .. back - 1);
      else
         return S (front .. S'Last);
      end if;
   end specific_field;


   --------------------------------------------------------------------------------------------
   --  replace
   --------------------------------------------------------------------------------------------
   function replace (S : String; reject, shiny : Character) return String
   is
      rejectstr : constant String (1 .. 1) := (1 => reject);
      focus     : constant Natural :=
                           AS.Fixed.Index (Source => S, Pattern => rejectstr);
      returnstr : String := S;
   begin
      if focus > 0 then
        returnstr (focus) := shiny;
      end if;
      return returnstr;
   end replace;


   --------------------------------------------------------------------------------------------
   --  replace_all
   --------------------------------------------------------------------------------------------
   function replace_all (S : String; reject, shiny : Character) return String
   is
      rejectstr : constant String (1 .. 1) := (1 => reject);
      returnstr : String := S;
      focus     : Natural;
   begin
      loop
         focus := AS.Fixed.Index (Source => returnstr, Pattern => rejectstr);
         exit when focus = 0;
         returnstr (focus) := shiny;
      end loop;
      return returnstr;
   end replace_all;


   --------------------------------------------------------------------------------------------
   --  replace_char
   --------------------------------------------------------------------------------------------
   function replace_char (S : String; focus : Character; substring : String) return String
   is
      num_to_replace : constant Natural := count_char (S, focus);
   begin
      if num_to_replace = 0 then
         return S;
      end if;

      declare
         ssm1   : constant Natural := substring'Length - 1;
         strlen : constant Natural := S'Length + (num_to_replace * ssm1);
         product : String (1 .. strlen);
         ndx : Positive := 1;
      begin
         for x in S'Range loop
            if S (x) = focus then
               product (ndx .. ndx + ssm1) := substring;
               ndx := ndx + substring'Length;
            else
               product (ndx) := S (x);
               ndx := ndx + 1;
            end if;
         end loop;
         return product;
      end;
   end replace_char;


   --------------------------------------------------------------------------------------------
   --  strip_control
   --------------------------------------------------------------------------------------------
   function strip_control (S : String) return String
   is
      product : String (1 .. S'Length);
      ndx     : Natural := 0;
   begin
      for x in S'Range loop
         if Character'Pos (S (x)) >= 32 then
            ndx := ndx + 1;
            product (ndx) := S (x);
         end if;
      end loop;
      return product (1 .. ndx);
   end strip_control;


   --------------------------------------------------------------------------------------------
   --  strip_excessive_spaces
   --------------------------------------------------------------------------------------------
   function strip_excessive_spaces (S : String) return String
   is
      result             : String (1 .. S'Length);
      previous_was_space : Boolean := False;
      front_marker       : Natural := 0;
      keep_it            : Boolean;
   begin
      for x in S'Range loop
         keep_it := True;
         if S (x) = LAT.Space then
            if previous_was_space then
               keep_it := False;
            end if;
            previous_was_space := True;
         else
            previous_was_space := False;
         end if;
         if keep_it then
            front_marker := front_marker + 1;
            result (front_marker) := S (x);
         end if;
      end loop;
      return result (1 .. front_marker);
   end strip_excessive_spaces;

end HelperText;
