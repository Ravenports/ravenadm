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

   --  True when trailing white space detected
   function trailing_whitespace_present (line : String) return Boolean;

   --  True when tab preceded by space character is present
   function trapped_space_character_present (line : String) return Boolean;

   --  True if the target character is present anywhere in the provided line
   function contains_specific_character
     (line : String;
      target : Character) return Boolean;

private

   single_LF : constant String (1 .. 1) := (1 => ASCII.LF);

   type Line_Markers is
      record
         back_marker  : Natural := 0;
         front_marker : Natural := 0;
      end record;

end HelperText;
