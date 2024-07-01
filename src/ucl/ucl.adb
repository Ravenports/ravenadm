--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Interfaces.C.Strings;

package body Ucl is

   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;


   --------------------------------------------------------------------
   --  convert_c_boolean
   --------------------------------------------------------------------
   function convert_c_boolean (original : IC.unsigned_char) return Boolean
   is
      c_true : constant IC.unsigned_char := IC.unsigned_char (1);
   begin
      case original is
         when c_true => return True;
         when others => return False;
      end case;
   end convert_c_boolean;


   --------------------------------------------------------------------
   --  ucl_object_find_key
   --------------------------------------------------------------------
   function ucl_object_find_key (obj : access constant libucl.ucl_object_t;
                                 key : String) return access constant libucl.ucl_object_t
   is
      ckey : ICS.chars_ptr := ICS.New_String (key);
   begin
      return result : constant access constant libucl.ucl_object_t :=
        libucl.ucl_object_lookup (obj, ckey)
      do
         ICS.Free (ckey);
      end return;
   end ucl_object_find_key;


   --------------------------------------------------------------------
   --  ucl_object_typed_new_object
   --------------------------------------------------------------------
   function ucl_object_typed_new_object return access libucl.ucl_object_t is
   begin
      return libucl.ucl_object_typed_new (libucl.UCL_OBJECT);
   end ucl_object_typed_new_object;


   --------------------------------------------------------------------
   --  ucl_object_typed_new_array
   --------------------------------------------------------------------
   function ucl_object_typed_new_array return access libucl.ucl_object_t is
   begin
      return libucl.ucl_object_typed_new (libucl.UCL_ARRAY);
   end ucl_object_typed_new_array;


   --------------------------------------------------------------------
   --  common_ucl_object_fromstring
   --------------------------------------------------------------------
   function common_ucl_object_fromstring (txt   : String;
                                          flags : libucl.ucl_string_flags)
                                          return access libucl.ucl_object_t
   is
      ctxt : ICS.chars_ptr := ICS.New_String (txt);
   begin
      return result : constant access libucl.ucl_object_t :=
        libucl.ucl_object_fromstring_common (ctxt, 0, flags)
      do
         ICS.Free (ctxt);
      end return;
   end common_ucl_object_fromstring;


   --------------------------------------------------------------------
   --  ucl_object_fromstring_and_trim
   --------------------------------------------------------------------
   function ucl_object_fromstring_and_trim (txt : String) return access libucl.ucl_object_t is
   begin
      return (common_ucl_object_fromstring (txt, libucl.UCL_STRING_TRIM));
   end ucl_object_fromstring_and_trim;


   --------------------------------------------------------------------
   --  ucl_object_fromstring_boolean
   --------------------------------------------------------------------
   function ucl_object_fromstring_boolean (txt : String) return access libucl.ucl_object_t is
   begin
      return (common_ucl_object_fromstring (txt, libucl.UCL_STRING_PARSE_BOOLEAN));
   end ucl_object_fromstring_boolean;


   --------------------------------------------------------------------
   --  ucl_object_fromstring_int
   --------------------------------------------------------------------
   function ucl_object_fromstring_int (txt : String) return access libucl.ucl_object_t is
   begin
      return (common_ucl_object_fromstring (txt, libucl.UCL_STRING_PARSE_INT));
   end ucl_object_fromstring_int;


   --------------------------------------------------------------------
   --  ucl_object_fromstring_and_trim
   --------------------------------------------------------------------
   function ucl_object_insert_key (top : access libucl.ucl_object_t;
                                   elt : access libucl.ucl_object_t;
                                   key : String;
                                   copy_key : Boolean) return Boolean
   is
      ckey   : ICS.chars_ptr;
      ccopy  : IC.unsigned_char := IC.unsigned_char (0);
      result : IC.unsigned_char;
   begin
      ckey := ICS.New_String (key);
      if copy_key then
         ccopy := IC.unsigned_char (1);
      end if;
      result := libucl.ucl_object_insert_key (top      => top,
                                              elt      => elt,
                                              key      => ckey,
                                              keylen   => 0,
                                              copy_key => ccopy);
      --  Do not free, libucl doesn't copy
      --  ICS.Free (ckey);
      return convert_c_boolean (result);
   end ucl_object_insert_key;


   --------------------------------------------------------------------
   --  ucl_array_push
   --------------------------------------------------------------------
   function ucl_array_push (top : access libucl.ucl_object_t;
                            elt : access libucl.ucl_object_t) return Boolean
   is
      result : IC.unsigned_char;
   begin
      result := libucl.ucl_array_append (top, elt);
      return convert_c_boolean (result);
   end ucl_array_push;


   --------------------------------------------------------------------
   --  ucl_parser_new_basic
   --------------------------------------------------------------------
   function ucl_parser_new_basic return T_parser
   is
      flags : constant IC.int := 0;
   begin
      return libucl.ucl_parser_new (flags);
   end ucl_parser_new_basic;


   --------------------------------------------------------------------
   --  ucl_parser_new_nofilevars
   --------------------------------------------------------------------
   function ucl_parser_new_nofilevars return T_parser
   is
      flags : constant IC.int := IC.int (libucl.UCL_PARSER_NO_FILEVARS);
   begin
      return libucl.ucl_parser_new (flags);
   end ucl_parser_new_nofilevars;


   --------------------------------------------------------------------
   --  ucl_parser_new_lowercase
   --------------------------------------------------------------------
   function ucl_parser_new_lowercase return T_parser
   is
      flags : constant IC.int := IC.int (libucl.UCL_PARSER_KEY_LOWERCASE);
   begin
      return libucl.ucl_parser_new (flags);
   end ucl_parser_new_lowercase;


   --------------------------------------------------------------------
   --  ucl_parser_get_error
   --------------------------------------------------------------------
   function ucl_parser_get_error (parser : T_parser) return String
   is
      result : ICS.chars_ptr;
   begin
      result := libucl.ucl_parser_get_error (parser);
      --  Don't free result
      return ICS.Value (result);
   end ucl_parser_get_error;


   --------------------------------------------------------------------
   --  ucl_parser_get_object
   --------------------------------------------------------------------
   function ucl_parser_get_object (parser : T_parser) return access libucl.ucl_object_t is
   begin
      return libucl.ucl_parser_get_object (parser);
   end ucl_parser_get_object;


   --------------------------------------------------------------------
   --  ucl_object_iterate
   --------------------------------------------------------------------
   function ucl_object_iterate (obj : access constant libucl.ucl_object_t;
                                iter : access libucl.ucl_object_iter_t;
                                expand_values : Boolean)
                                return access constant libucl.ucl_object_t
   is
      exv : IC.unsigned_char := IC.unsigned_char (0);
   begin
      if expand_values then
         exv := IC.unsigned_char (1);
      end if;
      return libucl.ucl_object_iterate_with_error (obj           => obj,
                                                   iter          => iter,
                                                   expand_values => exv,
                                                   ep            => null);
   end ucl_object_iterate;


   --------------------------------------------------------------------
   --  ucl_object_key
   --------------------------------------------------------------------
   function ucl_object_key (obj : access constant libucl.ucl_object_t) return String
   is
      result : ICS.chars_ptr;

      use type ICS.chars_ptr;
   begin
      result := libucl.ucl_object_key (obj);
      --  Don't free result
      if result = ICS.Null_Ptr then
         return "";
      else
         return ICS.Value (result);
      end if;
   end ucl_object_key;


   --------------------------------------------------------------------
   --  ucl_object_keyl
   --------------------------------------------------------------------
   function ucl_object_keyl (obj : access constant libucl.ucl_object_t;
                             key : String) return access constant libucl.ucl_object_t
   is
      keyx   : ICS.chars_ptr := ICS.New_String (key);
      lenx   : aliased constant IC.size_t := IC.size_t (key'Length);
   begin
      return result : constant access constant libucl.ucl_object_t :=
        libucl.ucl_object_lookup_len (obj, keyx, lenx)
      do
         ICS.Free (keyx);
      end return;
   end ucl_object_keyl;


   --------------------------------------------------------------------
   --  object_types_equal
   --------------------------------------------------------------------
   function object_types_equal (obj1, obj2 : access constant libucl.ucl_object_t) return Boolean
   is
      use type libucl.ucl_type;
      otype1 : libucl.ucl_type_t;
      otype2 : libucl.ucl_type_t;
   begin
      otype1 := libucl.ucl_object_type (obj1);
      otype2 := libucl.ucl_object_type (obj2);

      return otype1 = otype2;
   end object_types_equal;


   --------------------------------------------------------------------
   --  type_is_object
   --------------------------------------------------------------------
   function type_is_object (obj : access constant libucl.ucl_object_t) return Boolean
   is
      use type libucl.ucl_type;
      otype : libucl.ucl_type_t;
   begin
      otype := libucl.ucl_object_type (obj);

      return otype = libucl.UCL_OBJECT;
   end type_is_object;


   --------------------------------------------------------------------
   --  type_is_string
   --------------------------------------------------------------------
   function type_is_string (obj : access constant libucl.ucl_object_t) return Boolean
   is
      use type libucl.ucl_type;
      otype : libucl.ucl_type_t;
   begin
      otype := libucl.ucl_object_type (obj);

      return otype = libucl.UCL_STRING;
   end type_is_string;


   --------------------------------------------------------------------
   --  type_is_integer
   --------------------------------------------------------------------
   function type_is_integer (obj : access constant libucl.ucl_object_t) return Boolean
   is
      use type libucl.ucl_type;
      otype : libucl.ucl_type_t;
   begin
      otype := libucl.ucl_object_type (obj);

      return otype = libucl.UCL_INT;
   end type_is_integer;


   --------------------------------------------------------------------
   --  type_is_boolean
   --------------------------------------------------------------------
   function type_is_boolean (obj : access constant libucl.ucl_object_t) return Boolean
   is
      use type libucl.ucl_type;
      otype : libucl.ucl_type_t;
   begin
      otype := libucl.ucl_object_type (obj);

      return otype = libucl.UCL_BOOLEAN;
   end type_is_boolean;


   --------------------------------------------------------------------
   --  type_is_array
   --------------------------------------------------------------------
   function type_is_array (obj : access constant libucl.ucl_object_t) return Boolean
   is
      use type libucl.ucl_type;
      otype : libucl.ucl_type_t;
   begin
      otype := libucl.ucl_object_type (obj);

      return otype = libucl.UCL_ARRAY;
   end type_is_array;


   --------------------------------------------------------------------
   --  type_is_floating_point
   --------------------------------------------------------------------
   function type_is_floating_point (obj : access constant libucl.ucl_object_t) return Boolean
   is
      use type libucl.ucl_type;
   begin
      return libucl.ucl_object_type (obj) = libucl.UCL_FLOAT;
   end type_is_floating_point;


   --------------------------------------------------------------------
   --  type_is_time_span
   --------------------------------------------------------------------
   function type_is_time_span
     (obj : access constant libucl.ucl_object_t) return Boolean
   is
      use type libucl.ucl_type;
   begin
      return libucl.ucl_object_type (obj) = libucl.UCL_TIME;
   end type_is_time_span;


   --------------------------------------------------------------------
   --  low_level_type
   --------------------------------------------------------------------
   function intermediate_type
     (obj : access constant libucl.ucl_object_t) return intermediate_ucl_type
   is
   begin
      case libucl.ucl_object_type (obj) is
         when libucl.UCL_OBJECT   => return med_object;
         when libucl.UCL_ARRAY    => return med_array;
         when libucl.UCL_INT      => return med_int;
         when libucl.UCL_FLOAT    => return med_float;
         when libucl.UCL_STRING   => return med_string;
         when libucl.UCL_BOOLEAN  => return med_boolean;
         when libucl.UCL_TIME     => return med_time;
         when libucl.UCL_USERDATA => return med_userdata;
         when libucl.UCL_NULL     => return med_null;
      end case;
   end intermediate_type;


   --------------------------------------------------------------------
   --  ucl_object_replace_key
   --------------------------------------------------------------------
   function ucl_object_replace_key (top : access libucl.ucl_object_t;
                                    elt : access libucl.ucl_object_t;
                                    key : String;
                                    copy_key : Boolean) return Boolean
   is
      ckey   : constant ICS.chars_ptr := ICS.New_String (key);
      ccopy  : IC.unsigned_char := IC.unsigned_char (0);
      result : IC.unsigned_char;
   begin
      if copy_key then
         ccopy := IC.unsigned_char (1);
      end if;

      result := libucl.ucl_object_replace_key (top      => top,
                                               elt      => elt,
                                               key      => ckey,
                                               keylen   => 0,
                                               copy_key => ccopy);
      --  Do not free, libucl doesn't copy
      --  ICS.Free (ckey);
      return convert_c_boolean (result);
   end ucl_object_replace_key;


   --------------------------------------------------------------------
   --  ucl_object_tostring_forced
   --------------------------------------------------------------------
   function ucl_object_tostring_forced (obj : access constant libucl.ucl_object_t) return String
   is
      result : ICS.chars_ptr;

      use type ICS.chars_ptr;
   begin
      result := libucl.ucl_object_tostring_forced (obj);
      --  Do NOT free result memory!
      if result = ICS.Null_Ptr then
         return "";
      else
         return ICS.Value (result);
      end if;
   end ucl_object_tostring_forced;


   --------------------------------------------------------------------
   --  ucl_object_tostring
   --------------------------------------------------------------------
   function ucl_object_tostring (obj : access constant libucl.ucl_object_t) return String
   is
      result : ICS.chars_ptr;

      use type ICS.chars_ptr;
   begin
      result := libucl.ucl_object_tostring (obj);
      --  Do NOT free result memory!
      if result = ICS.Null_Ptr then
         return "";
      else
         return ICS.Value (result);
      end if;
   end ucl_object_tostring;


   --------------------------------------------------------------------
   --  ucl_object_toint
   --------------------------------------------------------------------
   function ucl_object_toint (obj : access constant libucl.ucl_object_t) return ucl_integer is
   begin
      return ucl_integer (libucl.ucl_object_toint (obj));
   end ucl_object_toint;


   --------------------------------------------------------------------
   --  ucl_object_tofloat
   --------------------------------------------------------------------
   function ucl_object_tofloat (obj : access constant libucl.ucl_object_t) return Float is
   begin
      return Float (libucl.ucl_object_todouble (obj));
   end ucl_object_tofloat;


   --------------------------------------------------------------------
   --  ucl_object_toboolean
   --------------------------------------------------------------------
   function ucl_object_toboolean (obj : access constant libucl.ucl_object_t) return Boolean
   is
      result : IC.unsigned_char;
   begin
      result := libucl.ucl_object_toboolean (obj);
      return convert_c_boolean (result);
   end ucl_object_toboolean;


   --------------------------------------------------------------------
   --  ucl_object_totime
   --------------------------------------------------------------------
   function ucl_object_totime
     (obj : access constant libucl.ucl_object_t) return RT.Time_Span
   is
      use type IC.double;

      native_res : IC.double;
      part_secs  : Integer;
      part_nsec  : Integer;
      result     : RT.Time_Span;
   begin
      native_res := libucl.ucl_object_todouble (obj);
      part_secs := Integer (native_res);
      part_nsec := Integer (ucl_integer (native_res * 1_000_000_000.0) mod 1_000_000_000);
      result := RT."+" (RT.Seconds (part_secs),  RT.Nanoseconds (part_nsec));
      return result;
   end ucl_object_totime;


   --------------------------------------------------------------------
   --  ucl_dump
   --------------------------------------------------------------------
   function ucl_dump (obj : access constant libucl.ucl_object_t) return String
   is
      result : ICS.chars_ptr;
   begin
      result := libucl.ucl_object_emit (obj, libucl.UCL_EMIT_CONFIG);
      declare
         dump : constant String := ICS.Value (result);
      begin
         ICS.Free (result);
         return dump;
      end;
   end ucl_dump;


   --------------------------------------------------------------------
   --  ucl_parser_register_variable
   --------------------------------------------------------------------
   procedure ucl_parser_register_variable (parser : T_parser; key, value : String)
   is
      ckey : constant ICS.chars_ptr := ICS.New_String (key);
      cval : constant ICS.chars_ptr := ICS.New_String (value);
   begin
      libucl.ucl_parser_register_variable (parser, ckey, cval);

      --  Don't free ckey and cval
   end ucl_parser_register_variable;


   --------------------------------------------------------------------
   --  ucl_emit_yaml
   --------------------------------------------------------------------
   function ucl_emit_yaml (obj : access constant libucl.ucl_object_t) return String
   is
      result : ICS.chars_ptr;
   begin
      result := libucl.ucl_object_emit (obj, libucl.UCL_EMIT_YAML);
      declare
         dump : constant String := ICS.Value (result);
      begin
         ICS.Free (result);
         return dump;
      end;
   end ucl_emit_yaml;


   --------------------------------------------------------------------
   --  ucl_object_valid_per_schema
   --------------------------------------------------------------------
   function ucl_object_valid_per_schema
     (schema          : access constant libucl.ucl_object_t;
      obj_to_validate : access constant libucl.ucl_object_t) return String
   is
      res  : IC.unsigned_char;
      serr : aliased libucl.ucl_schema_error;

   begin
      res := libucl.ucl_object_validate (schema, obj_to_validate, serr'Access);
      if convert_c_boolean (res) then
         return valid_structure;
      end if;

      declare
         msg : ICS.chars_ptr := ICS.New_Char_Array (serr.msg);
         error_message : constant String := ICS.Value (msg);
      begin
         ICS.Free (msg);
         return error_message;
      end;
   end ucl_object_valid_per_schema;


   --------------------------------------------------------------------
   --  ucl_parser_add_chunk
   --------------------------------------------------------------------
   function ucl_parser_add_chunk (parser : T_parser; data : String) return Boolean
   is
      res : IC.unsigned_char;
      c_data : array (data'Range) of aliased IC.unsigned_char;
   begin
      if data'Length = 0 then
         return False;
      end if;
      for x in data'Range loop
         c_data (x) := IC.unsigned_char (Character'Pos (data (x)));
      end loop;
      res := libucl.ucl_parser_add_chunk (parser, c_data (c_data'First)'Access, data'Length);
      return convert_c_boolean (res);
   end ucl_parser_add_chunk;


   --------------------------------------------------------------------
   --  ucl_parser_add_file
   --------------------------------------------------------------------
   function ucl_parser_add_file
     (parser : T_parser;
      filename : String) return Boolean
   is
      cfilename : constant ICS.chars_ptr := ICS.New_String (filename);
      res : IC.unsigned_char;
   begin
      res := libucl.ucl_parser_add_file (parser, cfilename);
      return convert_c_boolean (res);
   end ucl_parser_add_file;

end Ucl;
