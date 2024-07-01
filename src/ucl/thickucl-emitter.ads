--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

package ThickUCL.Emitter is

   --  Given a UclTree structure, return a pretty-printed UCL representation of it as a string.
   --  The root object is not wrapped in curly braces
   function emit_ucl (tree : UclTree) return String;

   --  Given a UclTree structure, return the string representation as a single line.
   --  The root object is wrapped in curly braces, removing any trailing commas
   --  Set as_json as true to emit as JSON format instead
   function emit_compact_ucl (tree : UclTree; as_json : Boolean := False) return String;

private

   LF : constant Character := Character'Val (10);

   --  Wrap key in double quotes if it contains a space, comma, colon, period, or several
   --  other non-alphanumeric characters.  Single quotes are escaped.
   --  If as_json is set, key is always wrapped in double quotes.
   --  If the raw string contains a new line (or any character with ASCII < 32),
   --  that character is eliminated.  Also eliminate ASCII > 126
   function format_key
     (raw : String;
      as_json : Boolean) return String;

   --  If heredoc is true, and string contains \n newline, wrap it between "<<EOD\n" and "\nEOD"
   --  Otherwise escape special characters and then write it in single quotes.
   function format_string_value
     (raw : String;
      heredoc : Boolean;
      terminator : Character := LF) return String;

   --  Returns "true" or "false"
   function format_boolean_value
     (raw : Boolean;
      terminator : Character := LF) return String;

   --  Converts integer value to a string
   function format_integer_value
     (raw : Ucl.ucl_integer;
      terminator : Character := LF) return String;

   --  Converts float value to a string
   function format_float_value
     (raw : Float;
     terminator : Character := LF) return String;

   --  Converts time value to a string (seconds)
   function format_time_value
     (raw : RT.Time_Span;
      terminator : Character := LF) return String;

end ThickUCL.Emitter;
