--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with libucl;
with System;
with Ada.Real_Time;
with Interfaces.C;

package Ucl is

   package RT renames Ada.Real_Time;

   subtype T_parser is System.Address;
   type ucl_integer is range -(2**63) .. +(2**63 - 1);
   valid_structure : constant String := "VALID";

   type intermediate_ucl_type is (med_object, med_array, med_int, med_float, med_string,
                                  med_boolean, med_time, med_userdata, med_null);

   function ucl_object_find_key
     (obj : access constant libucl.ucl_object_t;
      key : String) return access constant libucl.ucl_object_t;

   --  Equivalent: ucl_object_typed_new(UCL_OBJECT)
   function ucl_object_typed_new_object return access libucl.ucl_object_t;

   --  Equivalent: ucl_object_typed_new(UCL_ARRAY)
   function ucl_object_typed_new_array return access libucl.ucl_object_t;

   --  Equivalent: ucl_object_fromstring_common (string, size, UCL_STRING_TRIM)
   --  creates and object from text (and trim transform)
   function ucl_object_fromstring_and_trim
     (txt : String) return access libucl.ucl_object_t;

   --  Equivalent: ucl_object_fromstring_common (string, size, UCL_STRING_PARSE_BOOLEAN)
   --  creates and object from text (and trim transform)
   function ucl_object_fromstring_boolean
     (txt : String) return access libucl.ucl_object_t;

   --  Equivalent: ucl_object_fromstring_common (string, size, UCL_STRING_PARSE_INT)
   --  creates and object from text (and trim transform)
   function ucl_object_fromstring_int
     (txt : String) return access libucl.ucl_object_t;

   function ucl_object_insert_key
     (top : access libucl.ucl_object_t;
      elt : access libucl.ucl_object_t;
      key : String;
      copy_key : Boolean) return Boolean;

   --  Returns true if value was inserted into array
   function ucl_array_push
     (top : access libucl.ucl_object_t;
      elt : access libucl.ucl_object_t) return Boolean;

   --  Equivalent: ucl_parser_new(0);
   function ucl_parser_new_basic return T_parser;

   --  Equivalent: ucl_parser_new(UCL_PARSER_NO_FILEVARS);
   function ucl_parser_new_nofilevars return T_parser;

   --  Equivalent: ucl_parser_new(UCL_PARSER_KEY_LOWERCASE);
   function ucl_parser_new_lowercase return T_parser;

   --  Equivalent: ucl_parser_get_error (p) return Interfaces.C.Strings.chars_ptr;
   function ucl_parser_get_error (parser : T_parser) return String;

   --  Equivalent: ucl_parser_get_object (parser : System.Address) return access ucl_object_t;
   function ucl_parser_get_object (parser : T_parser) return access libucl.ucl_object_t;

   --  #define ucl_object_iterate(ob,it,ev) ucl_object_iterate_with_error((ob), (it), (ev), NULL)
   function ucl_object_iterate
     (obj : access constant libucl.ucl_object_t;
      iter : access libucl.ucl_object_iter_t;
      expand_values : Boolean)
      return access constant libucl.ucl_object_t;

   --  Equivalent: ucl_object_key (obj : access constant ucl_object_t) return IC.Strings.chars_ptr
   function ucl_object_key
     (obj : access constant libucl.ucl_object_t) return String;

   --  Equivalent: ucl_object_keyl (obj : access constant ucl_object_t; len : access size_t)
   --  return Interfaces.C.Strings.chars_ptr;  -- ucl.h:748
   function ucl_object_keyl
     (obj : access constant libucl.ucl_object_t;
      key : String) return access constant libucl.ucl_object_t;

   --  Returns true if c_type field of both objects match
   function object_types_equal
     (obj1, obj2 : access constant libucl.ucl_object_t) return Boolean;

   --  Return true if UCL_OBJECT
   function type_is_object
     (obj : access constant libucl.ucl_object_t) return Boolean;

   --  Return true if UCL_STRING
   function type_is_string
     (obj : access constant libucl.ucl_object_t) return Boolean;

   --  Return true if UCL_INT
   function type_is_integer
     (obj : access constant libucl.ucl_object_t) return Boolean;

   --  Return true if UCL_BOOLEAN
   function type_is_boolean
     (obj : access constant libucl.ucl_object_t) return Boolean;

   --  Return true if UCL_ARRAY
   function type_is_array
     (obj : access constant libucl.ucl_object_t) return Boolean;

   --  Return true if UCL_FLOAT
   function type_is_floating_point
     (obj : access constant libucl.ucl_object_t) return Boolean;

   --  Return true if UCL_TIME
   function type_is_time_span
     (obj : access constant libucl.ucl_object_t) return Boolean;

   --  Return C-type to describe object
   function intermediate_type
     (obj : access constant libucl.ucl_object_t) return intermediate_ucl_type;

   function ucl_object_replace_key
     (top : access libucl.ucl_object_t;
      elt : access libucl.ucl_object_t;
      key : String;
      copy_key : Boolean) return Boolean;

   function ucl_object_tostring_forced
     (obj : access constant libucl.ucl_object_t) return String;

   function ucl_object_tostring
     (obj : access constant libucl.ucl_object_t) return String;

   function ucl_object_toint
     (obj : access constant libucl.ucl_object_t) return ucl_integer;

   function ucl_object_toboolean
     (obj : access constant libucl.ucl_object_t) return Boolean;

   function ucl_object_tofloat
     (obj : access constant libucl.ucl_object_t) return Float;

   function ucl_object_totime
     (obj : access constant libucl.ucl_object_t) return RT.Time_Span;

   function ucl_dump
     (obj : access constant libucl.ucl_object_t) return String;

   procedure ucl_parser_register_variable
     (parser : T_parser; key, value : String);

   function ucl_emit_yaml
     (obj : access constant libucl.ucl_object_t) return String;

   --  Returns "valid_structure" constant if the object confirms to the given schema.
   --  If it doesn't confirm, the error string is returned instead.
   function ucl_object_valid_per_schema
     (schema          : access constant libucl.ucl_object_t;
      obj_to_validate : access constant libucl.ucl_object_t) return String;

   --  Returns True if parsing of data string to UCL object is successful
   function ucl_parser_add_chunk
     (parser : T_parser;
      data : String) return Boolean;

   --  Returns True if given file was successfully parsed to UCL object.
   function ucl_parser_add_file
     (parser : T_parser;
      filename : String) return Boolean;

private

   --  internal use
   function common_ucl_object_fromstring
     (txt   : String;
      flags : libucl.ucl_string_flags)
      return access libucl.ucl_object_t;

   function convert_c_boolean (original : Interfaces.C.unsigned_char) return Boolean;

end Ucl;
