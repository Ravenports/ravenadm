--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Strings.Unbounded;
with Ada.Containers;

package HelperText is

   package SU  renames Ada.Strings.Unbounded;
   package CON renames Ada.Containers;

   subtype Text is SU.Unbounded_String;
   type Line_Markers is private;

   blank : constant Text := SU.Null_Unbounded_String;

   --  unpadded numeric image
   function int2str (A : Integer) return String;

   --  Trim both sides
   function trim (S : String) return String;

   --  converters : Text <==> String
   function USS (US : Text)   return String;
   function SUS (S  : String) return Text;

   --  True if strings are identical
   function equivalent (A, B : Text) return Boolean;
   function equivalent (A : Text; B : String) return Boolean;

   --  Used for mapped containers
   function hash (key : Text) return CON.Hash_Type;

   --  True if the string is zero length
   function IsBlank (US : Text)   return Boolean;
   function IsBlank (S  : String) return Boolean;

   --  shorthand for index
   function contains (S : String; fragment : String) return Boolean;
   function contains (US : Text; fragment : String) return Boolean;

   --  Return True if S terminates with fragment exactly
   function trails (S  : String; fragment : String) return Boolean;
   function trails (US : Text;   fragment : String) return Boolean;

   --  Return True if S leads with fragment exactly
   function leads (S  : String; fragment : String) return Boolean;
   function leads (US : Text;   fragment : String) return Boolean;

   --  Convert to uppercase
   function uppercase (US : Text)   return Text;
   function uppercase (S  : String) return String;

   --  Convert to lowercase
   function lowercase (US : Text)   return Text;
   function lowercase (S  : String) return String;

   --  Head (keep all but last delimiter and field)
   function head (US : Text;   delimiter : Text)   return Text;
   function head (S  : String; delimiter : String) return String;

   --  Tail (keep only last field)
   function tail (US : Text;   delimiter : Text)   return Text;
   function tail (S  : String; delimiter : String) return String;

   --  Return half of a string split by separator
   function part_1 (S : String; separator : String := "/") return String;
   function part_2 (S : String; separator : String := "/") return String;

   --  Returns index of first character of fragment (0 if not found)
   function start_index (S : String; fragment : String) return Natural;

   --  Replace substring with another string
   function replace_substring (US : Text;
                               old_string : String;
                               new_string : String) return Text;

   --  Iterate though block of text, LF is delimiter
   procedure initialize_markers
     (block_text : in String;
      shuttle    : out Line_Markers);

   function next_line_present
     (block_text : in String;
      shuttle    : in out Line_Markers)
      return Boolean;

   function extract_line
     (block_text : in String;
      shuttle    : in Line_Markers)
      return String;

   function extract_file
     (block_text : in String;
      shuttle    : in out Line_Markers;
      file_size  : in Natural)
     return String;

   --  True when trailing white space detected
   function trailing_whitespace_present (line : String) return Boolean;

   --  True when tab preceded by space character is present
   function trapped_space_character_present (line : String) return Boolean;

   --  Returns number of instances of a given character in a given string
   function count_char (S : String; focus : Character) return Natural;

   --  Returns string that is 'First + offset to index(end_marker) - 1
   function partial_search
     (fullstr    : String;
      offset     : Natural;
      end_marker : String) return String;

   --  returns first character through (not including) the first line feed (if it exists)
   function first_line (S : String) return String;

private

   single_LF : constant String (1 .. 1) := (1 => ASCII.LF);

   type Line_Markers is
      record
         back_marker  : Natural := 0;
         front_marker : Natural := 0;
         zero_length  : Boolean := False;
         utilized     : Boolean := False;
      end record;

end HelperText;
