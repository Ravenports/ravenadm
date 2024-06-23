pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with Interfaces.C_Streams;
with System;

package libucl is

   package ICS renames Interfaces.C_Streams;

   subtype int64_t is Long_Long_Integer;
   subtype uint32_t is unsigned;
   subtype uint16_t is unsigned_short;

   --  arg-macro: procedure UCL_ALLOC (size)
   --    malloc(size)
   --  arg-macro: procedure UCL_FREE (size, ptr)
   --    free(ptr)
   --  unsupported macro: UCL_WARN_UNUSED_RESULT __attribute__((warn_unused_result))
   --  arg-macro: procedure UCL_DEPRECATED (func)
   --    func __attribute__ ((deprecated))
   --  unsupported macro: ucl_object_find_key ucl_object_lookup
   --  unsupported macro: ucl_object_find_any_key ucl_object_lookup_any
   --  unsupported macro: ucl_object_find_keyl ucl_object_lookup_len
   --  unsupported macro: ucl_lookup_path ucl_object_lookup_path
   --  unsupported macro: ucl_lookup_path_char ucl_object_lookup_path_char
   --  unsupported macro: ucl_iterate_object ucl_object_iterate
   --  arg-macro: procedure ucl_object_iterate (ob, it, ev)
   --    ucl_object_iterate_with_error((ob), (it), (ev), NULL)
   --  unsupported macro: ucl_obj_todouble_safe ucl_object_todouble_safe
   --  unsupported macro: ucl_obj_todouble ucl_object_todouble
   --  unsupported macro: ucl_obj_tostring ucl_object_tostring
   --  unsupported macro: ucl_obj_tostring_safe ucl_object_tostring_safe
   --  unsupported macro: ucl_obj_tolstring ucl_object_tolstring
   --  unsupported macro: ucl_obj_tolstring_safe ucl_object_tolstring_safe
   --  unsupported macro: ucl_obj_toint ucl_object_toint
   --  unsupported macro: ucl_obj_toint_safe ucl_object_toint_safe
   --  unsupported macro: ucl_obj_toboolean ucl_object_toboolean
   --  unsupported macro: ucl_obj_toboolean_safe ucl_object_toboolean_safe
   --  unsupported macro: ucl_obj_get_key ucl_object_find_key
   --  unsupported macro: ucl_obj_get_keyl ucl_object_find_keyl
   --  unsupported macro: ucl_obj_unref ucl_object_unref
   --  unsupported macro: ucl_obj_ref ucl_object_ref
   --  unsupported macro: ucl_obj_free ucl_object_free


  --  * @mainpage
  --  * This is a reference manual for UCL API. You may find the description of UCL format by
  --  * following this [github repository](https://github.com/vstakhov/libucl).
  --  *
  --  * This manual has several main sections:
  --  *  - @ref structures
  --  *  - @ref utils
  --  *  - @ref parser
  --  *  - @ref emitter

  --  * @file ucl.h
  --  * @brief UCL parsing and emitting functions
  --  *
  --  * UCL is universal configuration language, which is a form of
  --  * JSON with less strict rules that make it more comfortable for
  --  * using as a configuration language

  --  * Memory allocation utilities
  --  * UCL_ALLOC(size) - allocate memory for UCL
  --  * UCL_FREE(size, ptr) - free memory of specified size at ptr
  --  * Default: malloc and free

  --  * @defgroup structures Structures and types
  --  * UCL defines several enumeration types used for error reporting or specifying flags
  --  *and attributes.
  --  *
  --  * The common error codes returned by ucl parser

   type ucl_error is
     (UCL_EOK,
      UCL_ESYNTAX,
      UCL_EIO,
      UCL_ESTATE,
      UCL_ENESTED,
      UCL_EUNPAIRED,
      UCL_EMACRO,
      UCL_EINTERNAL,
      UCL_ESSL,
      UCL_EMERGE);
   pragma Convention (C, ucl_error);  -- ucl.h:102

  --  *< No error
  --  *< Syntax error occurred during parsing
  --  *< IO error occurred during parsing
  --  *< Invalid state machine state
  --  *< Input has too many recursion levels
  --  *< Input has too many recursion levels
  --  *< Error processing a macro
  --  *< Internal unclassified error
  --  *< SSL error
  --  *< A merge error occurred
   subtype ucl_error_t is ucl_error;  -- ucl.h:113

  --  * #ucl_object_t may have one of specified types, some types are compatible with each other and some are not.
  --  * For example, you can always convert #UCL_TIME to #UCL_FLOAT. Also you can convert #UCL_FLOAT to #UCL_INTEGER
  --  * by loosing floating point. Every object may be converted to a string by #ucl_object_tostring_forced() function.

   type ucl_type is
     (UCL_OBJECT,
      UCL_ARRAY,
      UCL_INT,
      UCL_FLOAT,
      UCL_STRING,
      UCL_BOOLEAN,
      UCL_TIME,
      UCL_USERDATA,
      UCL_NULL);
   pragma Convention (C, ucl_type);  -- ucl.h:121

  --  *< UCL object - key/value pairs
  --  *< UCL array
  --  *< Integer number
  --  *< Floating point number
  --  *< Null terminated string
  --  *< Boolean value
  --  *< Time value (floating point number of seconds)
  --  *< Opaque userdata pointer (may be used in macros)
  --  *< Null value
   subtype ucl_type_t is ucl_type;  -- ucl.h:131

  --  * You can use one of these types to serialise #ucl_object_t by using ucl_object_emit().

   type ucl_emitter is
     (UCL_EMIT_JSON,
      UCL_EMIT_JSON_COMPACT,
      UCL_EMIT_CONFIG,
      UCL_EMIT_YAML,
      UCL_EMIT_MSGPACK,
      UCL_EMIT_MAX);
   pragma Convention (C, ucl_emitter);  -- ucl.h:136

  --  *< Emit fine formatted JSON
  --  *< Emit compacted JSON
  --  *< Emit human readable config format
  --  *< Emit embedded YAML format
  --  *< Emit msgpack output
  --  *< Unsupported emitter type
   subtype ucl_emitter_t is ucl_emitter;  -- ucl.h:143

  --  * These flags defines parser behaviour. If you specify #UCL_PARSER_ZEROCOPY you must ensure
  --  * that the input memory is not freed if an object is in use. Moreover, if you want to use
  --  * zero-terminated keys and string values then you should not use zero-copy mode, as in this
  --  * case UCL still has to perform copying implicitly.

   subtype ucl_parser_flags is unsigned;
   UCL_PARSER_DEFAULT            : constant ucl_parser_flags := 0;
   UCL_PARSER_KEY_LOWERCASE      : constant ucl_parser_flags := 1;
   UCL_PARSER_ZEROCOPY           : constant ucl_parser_flags := 2;
   UCL_PARSER_NO_TIME            : constant ucl_parser_flags := 4;
   UCL_PARSER_NO_IMPLICIT_ARRAYS : constant ucl_parser_flags := 8;
   UCL_PARSER_SAVE_COMMENTS      : constant ucl_parser_flags := 16;
   UCL_PARSER_DISABLE_MACRO      : constant ucl_parser_flags := 32;
   UCL_PARSER_NO_FILEVARS        : constant ucl_parser_flags := 64;  -- ucl.h:151

  --  *< No special flags
  --  *< Convert all keys to lower case
  --  *< Parse input in zero-copy mode if possible
  --  *< Do not parse time and treat time values as strings
  --  * Create explicit arrays instead of implicit ones
  --  * Save comments in the parser context
  --  * Treat macros as comments
  --  * Do not set file vars
   subtype ucl_parser_flags_t is ucl_parser_flags;  -- ucl.h:160

  --  * String conversion flags, that are used in #ucl_object_fromstring_common function.

   subtype ucl_string_flags is unsigned;
   UCL_STRING_RAW           : constant ucl_string_flags := 0;
   UCL_STRING_ESCAPE        : constant ucl_string_flags := 1;
   UCL_STRING_TRIM          : constant ucl_string_flags := 2;
   UCL_STRING_PARSE_BOOLEAN : constant ucl_string_flags := 4;
   UCL_STRING_PARSE_INT     : constant ucl_string_flags := 8;
   UCL_STRING_PARSE_DOUBLE  : constant ucl_string_flags := 16;
   UCL_STRING_PARSE_TIME    : constant ucl_string_flags := 32;
   UCL_STRING_PARSE_NUMBER  : constant ucl_string_flags := 56;
   UCL_STRING_PARSE         : constant ucl_string_flags := 60;
   UCL_STRING_PARSE_BYTES   : constant ucl_string_flags := 64;  -- ucl.h:165

  --  *< Treat string as is
  --  *< Perform JSON escape
  --  *< Trim leading and trailing whitespaces
  --  *< Parse passed string and detect boolean
  --  *< Parse passed string and detect integer number
  --  *< Parse passed string and detect integer or float number
  --  *< Parse time strings
  --  *< Parse passed string and detect number
  --  *< Parse passed string (and detect booleans and numbers)
  --  *< Treat numbers as bytes
   subtype ucl_string_flags_t is ucl_string_flags;  -- ucl.h:178

  --  * Basic flags for an object (can use up to 12 bits as higher 4 bits are used
  --  * for priorities)

   subtype ucl_object_flags is unsigned;
   UCL_OBJECT_ALLOCATED_KEY   : constant ucl_object_flags := 1;
   UCL_OBJECT_ALLOCATED_VALUE : constant ucl_object_flags := 2;
   UCL_OBJECT_NEED_KEY_ESCAPE : constant ucl_object_flags := 4;
   UCL_OBJECT_EPHEMERAL       : constant ucl_object_flags := 8;
   UCL_OBJECT_MULTILINE       : constant ucl_object_flags := 16;
   UCL_OBJECT_MULTIVALUE      : constant ucl_object_flags := 32;
   UCL_OBJECT_INHERITED       : constant ucl_object_flags := 64;
   UCL_OBJECT_BINARY          : constant ucl_object_flags := 128;
   UCL_OBJECT_SQUOTED         : constant ucl_object_flags := 256;  -- ucl.h:184

  --  *< An object has key allocated internally
  --  *< An object has a string value allocated internally
  --  *< The key of an object need to be escaped on output
  --  *< Temporary object that does not need to be freed really
  --  *< String should be displayed as multiline string
  --  **< Object is a key with multiple values
  --  **< Object has been inherited from another
  --  **< Object contains raw binary data
  --  **< Object has been enclosed in single quotes
   subtype ucl_object_flags_t is ucl_object_flags;  -- ucl.h:194

  --  * Duplicate policy types

   type ucl_duplicate_strategy is
     (UCL_DUPLICATE_APPEND,
      UCL_DUPLICATE_MERGE,
      UCL_DUPLICATE_REWRITE,
      UCL_DUPLICATE_ERROR);
   pragma Convention (C, ucl_duplicate_strategy);  -- ucl.h:199

  --  *< Default policy to merge based on priorities
  --  *< Merge new object with old one
  --  *< Rewrite old keys
  --  *< Stop parsing on duplicate found
  --  *
  --  * Input format type

   type ucl_parse_type is
     (UCL_PARSE_UCL,
      UCL_PARSE_MSGPACK,
      UCL_PARSE_CSEXP,
      UCL_PARSE_AUTO);
   pragma Convention (C, ucl_parse_type);  -- ucl.h:209

  --  *< Default ucl format
  --  *< Message pack input format
  --  *< Canonical S-expressions
  --  *< Try to detect parse type
  --
  --  * UCL object structure. Please mention that the most of fields should not be touched by
  --  * UCL users. In future, this structure may be converted to private one.

  --  * Variant value type

  --  *< Int value of an object
  --  *< String value of an object
  --  *< Double value of an object
  --  *< Array
  --  *< Object
  --  *< Opaque user data
   type ucl_object_s;
   type anon_8 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            iv : aliased int64_t;  -- ucl.h:225
         when 1 =>
            sv : Interfaces.C.Strings.chars_ptr;  -- ucl.h:226
         when 2 =>
            dv : aliased double;  -- ucl.h:227
         when 3 =>
            av : System.Address;  -- ucl.h:228
         when 4 =>
            ov : System.Address;  -- ucl.h:229
         when others =>
            ud : System.Address;  -- ucl.h:230
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, anon_8);
   pragma Unchecked_Union (anon_8);
   type ucl_object_s_trash_stack_array is array (0 .. 1) of access unsigned_char;
   type ucl_object_s is record
      value : aliased anon_8;  -- ucl.h:231
      key : Interfaces.C.Strings.chars_ptr;  -- ucl.h:232
      next : access ucl_object_s;  -- ucl.h:233
      prev : access ucl_object_s;  -- ucl.h:234
      keylen : aliased uint32_t;  -- ucl.h:235
      len : aliased uint32_t;  -- ucl.h:236
      ref : aliased uint32_t;  -- ucl.h:237
      flags : aliased uint16_t;  -- ucl.h:238
      c_type : ucl_type;   --aliased uint16_t;  -- ucl.h:239
      trash_stack : ucl_object_s_trash_stack_array;  -- ucl.h:240
   end record;
   pragma Convention (C_Pass_By_Copy, ucl_object_s);  -- ucl.h:220

  --  *< Key of an object
  --  *< Array handle
  --  *< Array handle
  --  *< Length of a key
  --  *< Size of an object
  --  *< Reference count
  --  *< Object flags
  --  *< Real type
  --  *< Pointer to allocated chunks
   subtype ucl_object_t is ucl_object_s;  -- ucl.h:241

  --  * Destructor type for userdata objects
  --  * @param ud user specified data pointer

   type ucl_userdata_dtor is access procedure (arg1 : System.Address);
   pragma Convention (C, ucl_userdata_dtor);  -- ucl.h:247

   type ucl_userdata_emitter is access function (arg1 : System.Address) return Interfaces.C.Strings.chars_ptr;
   pragma Convention (C, ucl_userdata_emitter);  -- ucl.h:248

  --  * @defgroup utils Utility functions
  --  * A number of utility functions simplify handling of UCL objects

  --  * Copy and return a key of an object, returned key is zero-terminated
  --  * @param obj CL object
  --  * @return zero terminated key

   function ucl_copy_key_trash (obj : access constant ucl_object_t) return Interfaces.C.Strings.chars_ptr;  -- ucl.h:263
   pragma Import (C, ucl_copy_key_trash, "ucl_copy_key_trash");

  --  * Copy and return a string value of an object, returned key is zero-terminated
  --  * @param obj CL object
  --  * @return zero terminated string representation of object value

   function ucl_copy_value_trash (obj : access constant ucl_object_t) return Interfaces.C.Strings.chars_ptr;  -- ucl.h:270
   pragma Import (C, ucl_copy_value_trash, "ucl_copy_value_trash");

  --  * Creates a new object
  --  * @return new object

   function ucl_object_new return access ucl_object_t;  -- ucl.h:276
   pragma Import (C, ucl_object_new, "ucl_object_new");

  --  * Create new object with type specified
  --  * @param type type of a new object
  --  * @return new object

   function ucl_object_typed_new (c_type : ucl_type_t) return access ucl_object_t;  -- ucl.h:283
   pragma Import (C, ucl_object_typed_new, "ucl_object_typed_new");

  --  * Create new object with type and priority specified
  --  * @param type type of a new object
  --  * @param priority priority of an object
  --  * @return new object

   function ucl_object_new_full (c_type : ucl_type_t; priority : unsigned) return access ucl_object_t;  -- ucl.h:291
   pragma Import (C, ucl_object_new_full, "ucl_object_new_full");

  --  * Create new object with userdata dtor
  --  * @param dtor destructor function
  --  * @param emitter emitter for userdata
  --  * @param ptr opaque pointer
  --  * @return new object

   function ucl_object_new_userdata
     (dtor : ucl_userdata_dtor;
      emitter : ucl_userdata_emitter;
      ptr : System.Address) return access ucl_object_t;  -- ucl.h:301
   pragma Import (C, ucl_object_new_userdata, "ucl_object_new_userdata");

  --  * Perform deep copy of an object copying everything
  --  * @param other object to copy
  --  * @return new object with refcount equal to 1

   function ucl_object_copy (other : access constant ucl_object_t) return access ucl_object_t;  -- ucl.h:309
   pragma Import (C, ucl_object_copy, "ucl_object_copy");

  --  * Return the type of an object
  --  * @return the object type

   function ucl_object_type (obj : access constant ucl_object_t) return ucl_type_t;  -- ucl.h:316
   pragma Import (C, ucl_object_type, "ucl_object_type");

  --  * Converts ucl object type to its string representation
  --  * @param type type of object
  --  * @return constant string describing type

   function ucl_object_type_to_string (c_type : ucl_type_t) return Interfaces.C.Strings.chars_ptr;  -- ucl.h:323
   pragma Import (C, ucl_object_type_to_string, "ucl_object_type_to_string");

  --  * Converts string that represents ucl type to real ucl type enum
  --  * @param input C string with name of type
  --  * @param res resulting target
  --  * @return true if `input` is a name of type stored in `res`

   function ucl_object_string_to_type
     (input : Interfaces.C.Strings.chars_ptr;
      res : access ucl_type_t) return unsigned_char;  -- ucl.h:331
   pragma Import (C, ucl_object_string_to_type, "ucl_object_string_to_type");

  --  * Convert any string to an ucl object making the specified transformations
  --  * @param str fixed size or NULL terminated string
  --  * @param len length (if len is zero, than str is treated as NULL terminated)
  --  * @param flags conversion flags
  --  * @return new object

   function ucl_object_fromstring_common
     (str : Interfaces.C.Strings.chars_ptr;
      len : size_t;
      flags : ucl_string_flags) return access ucl_object_t;  -- ucl.h:340
   pragma Import (C, ucl_object_fromstring_common, "ucl_object_fromstring_common");

  --  * Create a UCL object from the specified string
  --  * @param str NULL terminated string, will be json escaped
  --  * @return new object

   function ucl_object_fromstring (str : Interfaces.C.Strings.chars_ptr) return access ucl_object_t;  -- ucl.h:348
   pragma Import (C, ucl_object_fromstring, "ucl_object_fromstring");

  --  * Create a UCL object from the specified string
  --  * @param str fixed size string, will be json escaped
  --  * @param len length of a string
  --  * @return new object

   function ucl_object_fromlstring (str : Interfaces.C.Strings.chars_ptr; len : size_t) return access ucl_object_t;  -- ucl.h:356
   pragma Import (C, ucl_object_fromlstring, "ucl_object_fromlstring");

  --  * Create an object from an integer number
  --  * @param iv number
  --  * @return new object

   function ucl_object_fromint (iv : int64_t) return access ucl_object_t;  -- ucl.h:364
   pragma Import (C, ucl_object_fromint, "ucl_object_fromint");

  --  * Create an object from a float number
  --  * @param dv number
  --  * @return new object

   function ucl_object_fromdouble (dv : double) return access ucl_object_t;  -- ucl.h:371
   pragma Import (C, ucl_object_fromdouble, "ucl_object_fromdouble");

  --  * Create an object from a boolean
  --  * @param bv bool value
  --  * @return new object

   function ucl_object_frombool (bv : unsigned_char) return access ucl_object_t;  -- ucl.h:378
   pragma Import (C, ucl_object_frombool, "ucl_object_frombool");

  --  * Insert a object 'elt' to the hash 'top' and associate it with key 'key'
  --  * @param top destination object (must be of type UCL_OBJECT)
  --  * @param elt element to insert (must NOT be NULL)
  --  * @param key key to associate with this object (either const or preallocated)
  --  * @param keylen length of the key (or 0 for NULL terminated keys)
  --  * @param copy_key make an internal copy of key
  --  * @return true if key has been inserted

   function ucl_object_insert_key
     (top : access ucl_object_t;
      elt : access ucl_object_t;
      key : Interfaces.C.Strings.chars_ptr;
      keylen : size_t;
      copy_key : unsigned_char) return unsigned_char;  -- ucl.h:389

   pragma Import (C, ucl_object_insert_key, "ucl_object_insert_key");

  --  * Replace a object 'elt' to the hash 'top' and associate it with key 'key', old object will be unrefed,
  --  * if no object has been found this function works like ucl_object_insert_key()
  --  * @param top destination object (must be of type UCL_OBJECT)
  --  * @param elt element to insert (must NOT be NULL)
  --  * @param key key to associate with this object (either const or preallocated)
  --  * @param keylen length of the key (or 0 for NULL terminated keys)
  --  * @param copy_key make an internal copy of key
  --  * @return true if key has been inserted

   function ucl_object_replace_key
     (top : access ucl_object_t;
      elt : access ucl_object_t;
      key : Interfaces.C.Strings.chars_ptr;
      keylen : size_t;
      copy_key : unsigned_char) return unsigned_char;  -- ucl.h:402
   pragma Import (C, ucl_object_replace_key, "ucl_object_replace_key");

  --  * Merge the keys from one object to another object. Overwrite on conflict
  --  * @param top destination object (must be of type UCL_OBJECT)
  --  * @param elt element to insert (must be of type UCL_OBJECT)
  --  * @param copy copy rather than reference the elements
  --  * @return true if all keys have been merged

   function ucl_object_merge
     (top : access ucl_object_t;
      elt : access ucl_object_t;
      copy : unsigned_char) return unsigned_char;  -- ucl.h:412
   pragma Import (C, ucl_object_merge, "ucl_object_merge");

  --  * Delete a object associated with key 'key', old object will be unrefered,
  --  * @param top object
  --  * @param key key associated to the object to remove
  --  * @param keylen length of the key (or 0 for NULL terminated keys)

   function ucl_object_delete_keyl
     (top : access ucl_object_t;
      key : Interfaces.C.Strings.chars_ptr;
      keylen : size_t) return unsigned_char;  -- ucl.h:420
   pragma Import (C, ucl_object_delete_keyl, "ucl_object_delete_keyl");

  --  * Delete a object associated with key 'key', old object will be unrefered,
  --  * @param top object
  --  * @param key key associated to the object to remove

   function ucl_object_delete_key
     (top : access ucl_object_t;
      key : Interfaces.C.Strings.chars_ptr) return unsigned_char;  -- ucl.h:428
   pragma Import (C, ucl_object_delete_key, "ucl_object_delete_key");

  --  * Removes `key` from `top` object, returning the object that was removed. This
  --  * object is not released, caller must unref the returned object when it is no
  --  * longer needed.
  --  * @param top object
  --  * @param key key to remove
  --  * @param keylen length of the key (or 0 for NULL terminated keys)
  --  * @return removed object or NULL if object has not been found

   function ucl_object_pop_keyl
     (top : access ucl_object_t;
      key : Interfaces.C.Strings.chars_ptr;
      keylen : size_t) return access ucl_object_t;  -- ucl.h:441
   pragma Import (C, ucl_object_pop_keyl, "ucl_object_pop_keyl");

  --  * Removes `key` from `top` object returning the object that was removed. This
  --  * object is not released, caller must unref the returned object when it is no
  --  * longer needed.
  --  * @param top object
  --  * @param key key to remove
  --  * @return removed object or NULL if object has not been found

   function ucl_object_pop_key (top : access ucl_object_t; key : Interfaces.C.Strings.chars_ptr) return access ucl_object_t;  -- ucl.h:452
   pragma Import (C, ucl_object_pop_key, "ucl_object_pop_key");

  --  * Insert a object 'elt' to the hash 'top' and associate it with key 'key', if
  --  * the specified key exist, try to merge its content
  --  * @param top destination object (must be of type UCL_OBJECT)
  --  * @param elt element to insert (must NOT be NULL)
  --  * @param key key to associate with this object (either const or preallocated)
  --  * @param keylen length of the key (or 0 for NULL terminated keys)
  --  * @param copy_key make an internal copy of key
  --  * @return true if key has been inserted

   function ucl_object_insert_key_merged
     (top : access ucl_object_t;
      elt : access ucl_object_t;
      key : Interfaces.C.Strings.chars_ptr;
      keylen : size_t;
      copy_key : unsigned_char) return unsigned_char;  -- ucl.h:465
   pragma Import (C, ucl_object_insert_key_merged, "ucl_object_insert_key_merged");

  --  * Reserve space in ucl array or object for `elt` elements
  --  * @param obj object to reserve
  --  * @param reserved size to reserve in an object
  --  * @return 0 on success, -1 on failure (i.e. ENOMEM)

   function ucl_object_reserve
     (obj : access ucl_object_t;
      reserved : size_t) return unsigned_char;  -- ucl.h:474
   pragma Import (C, ucl_object_reserve, "ucl_object_reserve");

  --  * Append an element to the end of array object
  --  * @param top destination object (must NOT be NULL)
  --  * @param elt element to append (must NOT be NULL)
  --  * @return true if value has been inserted

   function ucl_array_append
     (top : access ucl_object_t;
      elt : access ucl_object_t) return unsigned_char;  -- ucl.h:482
   pragma Import (C, ucl_array_append, "ucl_array_append");

  --  * Append an element to the start of array object
  --  * @param top destination object (must NOT be NULL)
  --  * @param elt element to append (must NOT be NULL)
  --  * @return true if value has been inserted

   function ucl_array_prepend
     (top : access ucl_object_t;
      elt : access ucl_object_t) return unsigned_char;  -- ucl.h:491
   pragma Import (C, ucl_array_prepend, "ucl_array_prepend");

  --  * Merge all elements of second array into the first array
  --  * @param top destination array (must be of type UCL_ARRAY)
  --  * @param elt array to copy elements from (must be of type UCL_ARRAY)
  --  * @param copy copy elements instead of referencing them
  --  * @return true if arrays were merged

   function ucl_array_merge
     (top : access ucl_object_t;
      elt : access ucl_object_t;
      copy : unsigned_char) return unsigned_char;  -- ucl.h:501
   pragma Import (C, ucl_array_merge, "ucl_array_merge");

  --  * Removes an element `elt` from the array `top`, returning the object that was
  --  * removed. This object is not released, caller must unref the returned object
  --  * when it is no longer needed.
  --  * @param top array ucl object
  --  * @param elt element to remove
  --  * @return removed element or NULL if `top` is NULL or not an array

   function ucl_array_delete
     (top : access ucl_object_t;
      elt : access ucl_object_t) return access ucl_object_t;  -- ucl.h:512
   pragma Import (C, ucl_array_delete, "ucl_array_delete");

  --  * Returns the first element of the array `top`
  --  * @param top array ucl object
  --  * @return element or NULL if `top` is NULL or not an array

   function ucl_array_head
     (top : access constant ucl_object_t) return access constant ucl_object_t;  -- ucl.h:520
   pragma Import (C, ucl_array_head, "ucl_array_head");

  --  * Returns the last element of the array `top`
  --  * @param top array ucl object
  --  * @return element or NULL if `top` is NULL or not an array

   function ucl_array_tail
     (top : access constant ucl_object_t) return access constant ucl_object_t;  -- ucl.h:527
   pragma Import (C, ucl_array_tail, "ucl_array_tail");

  --  * Removes the last element from the array `top`, returning the object that was
  --  * removed. This object is not released, caller must unref the returned object
  --  * when it is no longer needed.
  --  * @param top array ucl object
  --  * @return removed element or NULL if `top` is NULL or not an array

   function ucl_array_pop_last
     (top : access ucl_object_t) return access ucl_object_t;  -- ucl.h:536
   pragma Import (C, ucl_array_pop_last, "ucl_array_pop_last");

  --  * Removes the first element from the array `top`, returning the object that was
  --  * removed. This object is not released, caller must unref the returned object
  --  * when it is no longer needed.
  --  * @param top array ucl object
  --  * @return removed element or NULL if `top` is NULL or not an array

   function ucl_array_pop_first
     (top : access ucl_object_t) return access ucl_object_t;  -- ucl.h:545
   pragma Import (C, ucl_array_pop_first, "ucl_array_pop_first");

  --  * Return size of the array `top`
  --  * @param top object to get size from (must be of type UCL_ARRAY)
  --  * @return size of the array

   function ucl_array_size (top : access constant ucl_object_t) return unsigned;  -- ucl.h:552
   pragma Import (C, ucl_array_size, "ucl_array_size");

  --  * Return object identified by index of the array `top`
  --  * @param top object to get a key from (must be of type UCL_ARRAY)
  --  * @param index array index to return
  --  * @return object at the specified index or NULL if index is not found

   function ucl_array_find_index
     (top : access constant ucl_object_t;
      index : unsigned) return access constant ucl_object_t;  -- ucl.h:560
   pragma Import (C, ucl_array_find_index, "ucl_array_find_index");

  --  * Return the index of `elt` in the array `top`
  --  * @param top object to get a key from (must be of type UCL_ARRAY)
  --  * @param elt element to find index of (must NOT be NULL)
  --  * @return index of `elt` in the array `top or (unsigned int)-1 if `elt` is not found

   function ucl_array_index_of
     (top : access ucl_object_t; elt : access ucl_object_t) return unsigned;  -- ucl.h:569
   pragma Import (C, ucl_array_index_of, "ucl_array_index_of");

  --  * Replace an element in an array with a different element, returning the object
  --  * that was replaced. This object is not released, caller must unref the
  --  * returned object when it is no longer needed.
  --  * @param top destination object (must be of type UCL_ARRAY)
  --  * @param elt element to append (must NOT be NULL)
  --  * @param index array index in destination to overwrite with elt
  --  * @return object that was replaced or NULL if index is not found

   function ucl_array_replace_index
     (top : access ucl_object_t;
      elt : access ucl_object_t;
      index : unsigned) return access ucl_object_t;  -- ucl.h:582
   pragma Import (C, ucl_array_replace_index, "ucl_array_replace_index");

  --  * Append a element to another element forming an implicit array
  --  * @param head head to append (may be NULL)
  --  * @param elt new element
  --  * @return the new implicit array

   function ucl_elt_append
     (head : access ucl_object_t;
      elt : access ucl_object_t) return access ucl_object_t;  -- ucl.h:591
   pragma Import (C, ucl_elt_append, "ucl_elt_append");

  --  * Converts an object to double value
  --  * @param obj CL object
  --  * @param target target double variable
  --  * @return true if conversion was successful

   function ucl_object_todouble_safe
     (obj : access constant ucl_object_t;
      target : access double) return unsigned_char;  -- ucl.h:600
   pragma Import (C, ucl_object_todouble_safe, "ucl_object_todouble_safe");

  --  * Unsafe version of \ref ucl_obj_todouble_safe
  --  * @param obj CL object
  --  * @return double value

   function ucl_object_todouble (obj : access constant ucl_object_t) return double;  -- ucl.h:607
   pragma Import (C, ucl_object_todouble, "ucl_object_todouble");

  --  * Converts an object to integer value
  --  * @param obj CL object
  --  * @param target target integer variable
  --  * @return true if conversion was successful

   function ucl_object_toint_safe
     (obj : access constant ucl_object_t;
      target : access int64_t) return unsigned_char;  -- ucl.h:615
   pragma Import (C, ucl_object_toint_safe, "ucl_object_toint_safe");

  --  * Unsafe version of \ref ucl_obj_toint_safe
  --  * @param obj CL object
  --  * @return int value

   function ucl_object_toint (obj : access constant ucl_object_t) return int64_t;  -- ucl.h:622
   pragma Import (C, ucl_object_toint, "ucl_object_toint");

  --  * Converts an object to boolean value
  --  * @param obj CL object
  --  * @param target target boolean variable
  --  * @return true if conversion was successful

   function ucl_object_toboolean_safe
     (obj : access constant ucl_object_t;
      target : access unsigned_char) return unsigned_char;  -- ucl.h:630
   pragma Import (C, ucl_object_toboolean_safe, "ucl_object_toboolean_safe");

  --  * Unsafe version of \ref ucl_obj_toboolean_safe
  --  * @param obj CL object
  --  * @return boolean value

   function ucl_object_toboolean
     (obj : access constant ucl_object_t) return unsigned_char;  -- ucl.h:637
   pragma Import (C, ucl_object_toboolean, "ucl_object_toboolean");

  --  * Converts an object to string value
  --  * @param obj CL object
  --  * @param target target string variable, no need to free value
  --  * @return true if conversion was successful

   function ucl_object_tostring_safe
     (obj : access constant ucl_object_t;
      target : System.Address) return unsigned_char;  -- ucl.h:645
   pragma Import (C, ucl_object_tostring_safe, "ucl_object_tostring_safe");

  --  * Unsafe version of \ref ucl_obj_tostring_safe
  --  * @param obj CL object
  --  * @return string value

   function ucl_object_tostring
     (obj : access constant ucl_object_t) return Interfaces.C.Strings.chars_ptr;  -- ucl.h:652
   pragma Import (C, ucl_object_tostring, "ucl_object_tostring");

  --  * Convert any object to a string in JSON notation if needed
  --  * @param obj CL object
  --  * @return string value

   function ucl_object_tostring_forced
     (obj : access constant ucl_object_t) return Interfaces.C.Strings.chars_ptr;  -- ucl.h:659
   pragma Import (C, ucl_object_tostring_forced, "ucl_object_tostring_forced");

  --  * Return string as char * and len, string may be not zero terminated, more efficient that \ref ucl_obj_tostring as it
  --  * allows zero-copy (if #UCL_PARSER_ZEROCOPY has been used during parsing)
  --  * @param obj CL object
  --  * @param target target string variable, no need to free value
  --  * @param tlen target length
  --  * @return true if conversion was successful

   function ucl_object_tolstring_safe
     (obj : access constant ucl_object_t;
      target : System.Address;
      tlen : access size_t) return unsigned_char;  -- ucl.h:669
   pragma Import (C, ucl_object_tolstring_safe, "ucl_object_tolstring_safe");

  --  * Unsafe version of \ref ucl_obj_tolstring_safe
  --  * @param obj CL object
  --  * @return string value

   function ucl_object_tolstring
     (obj : access constant ucl_object_t; tlen : access size_t)
      return Interfaces.C.Strings.chars_ptr;  -- ucl.h:677
   pragma Import (C, ucl_object_tolstring, "ucl_object_tolstring");

  --  * Return object identified by a key in the specified object
  --  * @param obj object to get a key from (must be of type UCL_OBJECT)
  --  * @param key key to search
  --  * @return object matching the specified key or NULL if key was not found

   function ucl_object_lookup
     (obj : access constant ucl_object_t;
      key : Interfaces.C.Strings.chars_ptr)
      return access constant ucl_object_t;  -- ucl.h:685
   pragma Import (C, ucl_object_lookup, "ucl_object_lookup");

  --  * Return object identified by a key in the specified object, if the first key is
  --  * not found then look for the next one. This process is repeated unless
  --  * the next argument in the list is not NULL. So, `ucl_object_find_any_key(obj, key, NULL)`
  --  * is equal to `ucl_object_find_key(obj, key)`
  --  * @param obj object to get a key from (must be of type UCL_OBJECT)
  --  * @param key key to search
  --  * @param ... list of alternative keys to search (NULL terminated)
  --  * @return object matching the specified key or NULL if key was not found

   function ucl_object_lookup_any (obj : access constant ucl_object_t; key : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return access constant ucl_object_t;  -- ucl.h:699
   pragma Import (C, ucl_object_lookup_any, "ucl_object_lookup_any");

  --  * Return object identified by a fixed size key in the specified object
  --  * @param obj object to get a key from (must be of type UCL_OBJECT)
  --  * @param key key to search
  --  * @param klen length of a key
  --  * @return object matching the specified key or NULL if key was not found

   function ucl_object_lookup_len
     (obj : access constant ucl_object_t;
      key : Interfaces.C.Strings.chars_ptr;
      klen : size_t) return access constant ucl_object_t;  -- ucl.h:710
   pragma Import (C, ucl_object_lookup_len, "ucl_object_lookup_len");

  --  * Return object identified by dot notation string
  --  * @param obj object to search in
  --  * @param path dot.notation.path to the path to lookup. May use numeric .index on arrays
  --  * @return object matched the specified path or NULL if path is not found

   function ucl_object_lookup_path (obj : access constant ucl_object_t; path : Interfaces.C.Strings.chars_ptr) return access constant ucl_object_t;  -- ucl.h:720
   pragma Import (C, ucl_object_lookup_path, "ucl_object_lookup_path");

  --  * Return object identified by object notation string using arbitrary delimiter
  --  * @param obj object to search in
  --  * @param path dot.notation.path to the path to lookup. May use numeric .index on arrays
  --  * @param sep the sepatorator to use in place of . (incase keys have . in them)
  --  * @return object matched the specified path or NULL if path is not found

   function ucl_object_lookup_path_char
     (obj : access constant ucl_object_t;
      path : Interfaces.C.Strings.chars_ptr;
      sep : char) return access constant ucl_object_t;  -- ucl.h:731
   pragma Import (C, ucl_object_lookup_path_char, "ucl_object_lookup_path_char");

  --  * Returns a key of an object as a NULL terminated string
  --  * @param obj CL object
  --  * @return key or NULL if there is no key

   function ucl_object_key (obj : access constant ucl_object_t) return Interfaces.C.Strings.chars_ptr;  -- ucl.h:740
   pragma Import (C, ucl_object_key, "ucl_object_key");

  --  * Returns a key of an object as a fixed size string (may be more efficient)
  --  * @param obj CL object
  --  * @param len target key length
  --  * @return key pointer

   function ucl_object_keyl (obj : access constant ucl_object_t; len : access size_t) return Interfaces.C.Strings.chars_ptr;  -- ucl.h:748
   pragma Import (C, ucl_object_keyl, "ucl_object_keyl");

  --  * Increase reference count for an object
  --  * @param obj object to ref
  --  * @return the referenced object

   function ucl_object_ref (obj : access constant ucl_object_t) return access ucl_object_t;  -- ucl.h:755
   pragma Import (C, ucl_object_ref, "ucl_object_ref");

  --  * Free ucl object
  --  * @param obj ucl object to free

   procedure ucl_object_free (obj : access ucl_object_t);  -- ucl.h:761
   pragma Import (C, ucl_object_free, "ucl_object_free");

  --  * Decrease reference count for an object
  --  * @param obj object to unref

   procedure ucl_object_unref (obj : access ucl_object_t);  -- ucl.h:767
   pragma Import (C, ucl_object_unref, "ucl_object_unref");

  --  * Compare objects `o1` and `o2`
  --  * @param o1 the first object
  --  * @param o2 the second object
  --  * @return values >0, 0 and <0 if `o1` is more than, equal and less than `o2`.
  --  * The order of comparison:
  --  * 1) Type of objects
  --  * 2) Size of objects
  --  * 3) Content of objects

   function ucl_object_compare (o1 : access constant ucl_object_t;
                                o2 : access constant ucl_object_t) return int;  -- ucl.h:779
   pragma Import (C, ucl_object_compare, "ucl_object_compare");

  --  * Compare objects `o1` and `o2` useful for sorting
  --  * @param o1 the first object
  --  * @param o2 the second object
  --  * @return values >0, 0 and <0 if `o1` is more than, equal and less than `o2`.
  --  * The order of comparison:
  --  * 1) Type of objects
  --  * 2) Size of objects
  --  * 3) Content of objects

   function ucl_object_compare_qsort (o1 : System.Address; o2 : System.Address) return int;  -- ucl.h:792
   pragma Import (C, ucl_object_compare_qsort, "ucl_object_compare_qsort");

  --  * Sort UCL array using `cmp` compare function
  --  * @param ar
  --  * @param cmp

   procedure ucl_object_array_sort (ar : access ucl_object_t; cmp : access function (arg1 : System.Address; arg2 : System.Address) return int);  -- ucl.h:800
   pragma Import (C, ucl_object_array_sort, "ucl_object_array_sort");

  --  * Get the priority for specific UCL object
  --  * @param obj any ucl object
  --  * @return priority of an object

   function ucl_object_get_priority (obj : access constant ucl_object_t) return unsigned;  -- ucl.h:808
   pragma Import (C, ucl_object_get_priority, "ucl_object_get_priority");

  --  * Set explicit priority of an object.
  --  * @param obj any ucl object
  --  * @param priority new priroity value (only 4 least significant bits are considred)

   procedure ucl_object_set_priority (obj : access ucl_object_t; priority : unsigned);  -- ucl.h:815
   pragma Import (C, ucl_object_set_priority, "ucl_object_set_priority");

  --  * Opaque iterator object

   type ucl_object_iter_t is new System.Address;  -- ucl.h:821

  --  * Get next key from an object
  --  * @param obj object to iterate
  --  * @param iter opaque iterator, must be set to NULL on the first call:
  --  * ucl_object_iter_t it = NULL;
  --  * while ((cur = ucl_iterate_object (obj, &it)) != NULL) ...
  --  * @param ep pointer record exception (such as ENOMEM), could be NULL
  --  * @return the next object or NULL

   function ucl_object_iterate_with_error
     (obj : access constant ucl_object_t;
      iter : access ucl_object_iter_t;
      expand_values : unsigned_char;
      ep : access Interfaces.C.int) return access constant ucl_object_t;  -- ucl.h:832
   pragma Import (C, ucl_object_iterate_with_error, "ucl_object_iterate_with_error");

  --  * Create new safe iterator for the specified object
  --  * @param obj object to iterate
  --  * @return new iterator object that should be used with safe iterators API only

   function ucl_object_iterate_new (obj : access constant ucl_object_t) return ucl_object_iter_t;  -- ucl.h:843
   pragma Import (C, ucl_object_iterate_new, "ucl_object_iterate_new");

  --  * Check safe iterator object after performing some operations on it
  --  * (such as ucl_object_iterate_safe()) to see if operation has encountered
  --  * fatal exception while performing that operation (e.g. ENOMEM).
  --  * @param iter opaque iterator
  --  * @return true if exception has occured, false otherwise

   function ucl_object_iter_chk_excpn (it : System.Address) return unsigned_char;  -- ucl.h:852
   pragma Import (C, ucl_object_iter_chk_excpn, "ucl_object_iter_chk_excpn");

  --  * Reset initialized iterator to a new object
  --  * @param obj new object to iterate
  --  * @return modified iterator object

   function ucl_object_iterate_reset
     (it : ucl_object_iter_t;
      obj : access constant ucl_object_t) return ucl_object_iter_t;  -- ucl.h:859
   pragma Import (C, ucl_object_iterate_reset, "ucl_object_iterate_reset");

  --  * Get the next object from the `obj`. This function iterates over arrays, objects
  --  * and implicit arrays
  --  * @param iter safe iterator
  --  * @param expand_values expand explicit arrays and objects
  --  * @return the next object in sequence

   function ucl_object_iterate_safe
     (iter : ucl_object_iter_t;
      expand_values : unsigned_char) return access constant ucl_object_t;  -- ucl.h:869
   pragma Import (C, ucl_object_iterate_safe, "ucl_object_iterate_safe");

  --  * Iteration type enumerator

   subtype ucl_iterate_type is unsigned;
   UCL_ITERATE_EXPLICIT : constant ucl_iterate_type := 1;
   UCL_ITERATE_IMPLICIT : constant ucl_iterate_type := 2;
   UCL_ITERATE_BOTH : constant ucl_iterate_type := 3;  -- ucl.h:874

  --  *< Iterate just explicit arrays and objects
  --  *< Iterate just implicit arrays
  --  *< Iterate both explicit and implicit arrays
  --  *
  --  * Get the next object from the `obj`. This function iterates over arrays, objects
  --  * and implicit arrays if needed
  --  * @param iter safe iterator
  --  * @param
  --  * @return the next object in sequence

   function ucl_object_iterate_full (iter : ucl_object_iter_t; c_type : ucl_iterate_type) return access constant ucl_object_t;  -- ucl.h:887
   pragma Import (C, ucl_object_iterate_full, "ucl_object_iterate_full");

  --  * Free memory associated with the safe iterator
  --  * @param it safe iterator object

   procedure ucl_object_iterate_free (it : ucl_object_iter_t);  -- ucl.h:894
   pragma Import (C, ucl_object_iterate_free, "ucl_object_iterate_free");

  --  * @defgroup parser Parsing functions
  --  * These functions are used to parse UCL objects
  --  *

  --  * Macro handler for a parser
  --  * @param data the content of macro
  --  * @param len the length of content
  --  * @param arguments arguments object
  --  * @param ud opaque user data
  --  * @param err error pointer
  --  * @return true if macro has been parsed

   type ucl_macro_handler is access function
        (arg1 : access unsigned_char;
         arg2 : size_t;
         arg3 : access constant ucl_object_t;
         arg4 : System.Address) return unsigned_char;
   pragma Convention (C, ucl_macro_handler);  -- ucl.h:915

  --  * Context dependent macro handler for a parser
  --  * @param data the content of macro
  --  * @param len the length of content
  --  * @param arguments arguments object
  --  * @param context previously parsed context
  --  * @param ud opaque user data
  --  * @param err error pointer
  --  * @return true if macro has been parsed

   type ucl_context_macro_handler is access function
        (arg1 : access unsigned_char;
         arg2 : size_t;
         arg3 : access constant ucl_object_t;
         arg4 : access constant ucl_object_t;
         arg5 : System.Address) return unsigned_char;
   pragma Convention (C, ucl_context_macro_handler);  -- ucl.h:929

  --  Opaque parser
  --  skipped empty struct ucl_parser

  --  * Creates new parser object
  --  * @param pool pool to allocate memory from
  --  * @return new parser object

   function ucl_parser_new (flags : int) return System.Address;  -- ucl.h:942
   pragma Import (C, ucl_parser_new, "ucl_parser_new");

  --  * Sets the default priority for the parser applied to chunks that do not
  --  * specify priority explicitly
  --  * @param parser parser object
  --  * @param prio default priority (0 .. 16)
  --  * @return true if parser's default priority was set

   function ucl_parser_set_default_priority
     (parser : System.Address; prio : unsigned) return unsigned_char;  -- ucl.h:951
   pragma Import (C, ucl_parser_set_default_priority, "ucl_parser_set_default_priority");

  --  * Gets the default priority for the parser applied to chunks that do not
  --  * specify priority explicitly
  --  * @param parser parser object
  --  * @return true default priority (0 .. 16), -1 for failure

   function ucl_parser_get_default_priority (parser : System.Address) return int;  -- ucl.h:959
   pragma Import (C, ucl_parser_get_default_priority, "ucl_parser_get_default_priority");

  --  * Register new handler for a macro
  --  * @param parser parser object
  --  * @param macro macro name (without leading dot)
  --  * @param handler handler (it is called immediately after macro is parsed)
  --  * @param ud opaque user data for a handler
  --  * @return true on success, false on failure (i.e. ENOMEM)

   function ucl_parser_register_macro
     (parser : System.Address;
      macro : Interfaces.C.Strings.chars_ptr;
      handler : ucl_macro_handler;
      ud : System.Address) return unsigned_char;  -- ucl.h:969
   pragma Import (C, ucl_parser_register_macro, "ucl_parser_register_macro");

  --  * Register new context dependent handler for a macro
  --  * @param parser parser object
  --  * @param macro macro name (without leading dot)
  --  * @param handler handler (it is called immediately after macro is parsed)
  --  * @param ud opaque user data for a handler
  --  * @return true on success, false on failure (i.e. ENOMEM)

   function ucl_parser_register_context_macro
     (parser : System.Address;
      macro : Interfaces.C.Strings.chars_ptr;
      handler : ucl_context_macro_handler;
      ud : System.Address) return unsigned_char;  -- ucl.h:981
   pragma Import (C, ucl_parser_register_context_macro, "ucl_parser_register_context_macro");

  --  * Handler to detect unregistered variables
  --  * @param data variable data
  --  * @param len length of variable
  --  * @param replace (out) replace value for variable
  --  * @param replace_len (out) replace length for variable
  --  * @param need_free (out) UCL will free `dest` after usage
  --  * @param ud opaque userdata
  --  * @return true if variable

   type ucl_variable_handler is access function
        (arg1 : access unsigned_char;
         arg2 : size_t;
         arg3 : System.Address;
         arg4 : access size_t;
         arg5 : access unsigned_char;
         arg6 : System.Address) return unsigned_char;
   pragma Convention (C, ucl_variable_handler);  -- ucl.h:996

  --  * Register new parser variable
  --  * @param parser parser object
  --  * @param var variable name
  --  * @param value variable value

   procedure ucl_parser_register_variable
     (parser : System.Address;
      var : Interfaces.C.Strings.chars_ptr;
      value : Interfaces.C.Strings.chars_ptr);  -- ucl.h:1005
   pragma Import (C, ucl_parser_register_variable, "ucl_parser_register_variable");

  --  * Set handler for unknown variables
  --  * @param parser parser structure
  --  * @param handler desired handler
  --  * @param ud opaque data for the handler

   procedure ucl_parser_set_variables_handler
     (parser : System.Address;
      handler : ucl_variable_handler;
      ud : System.Address);  -- ucl.h:1014
   pragma Import (C, ucl_parser_set_variables_handler, "ucl_parser_set_variables_handler");

  --  * Load new chunk to a parser
  --  * @param parser parser structure
  --  * @param data the pointer to the beginning of a chunk
  --  * @param len the length of a chunk
  --  * @return true if chunk has been added and false in case of error

   function ucl_parser_add_chunk
     (parser : System.Address;
      data : access unsigned_char;
      len : size_t) return unsigned_char;  -- ucl.h:1024
   pragma Import (C, ucl_parser_add_chunk, "ucl_parser_add_chunk");

  --  * Load new chunk to a parser with the specified priority
  --  * @param parser parser structure
  --  * @param data the pointer to the beginning of a chunk
  --  * @param len the length of a chunk
  --  * @param priority the desired priority of a chunk (only 4 least significant bits
  --  * are considered for this parameter)
  --  * @return true if chunk has been added and false in case of error

   function ucl_parser_add_chunk_priority
     (parser : System.Address;
      data : access unsigned_char;
      len : size_t;
      priority : unsigned) return unsigned_char;  -- ucl.h:1036
   pragma Import (C, ucl_parser_add_chunk_priority, "ucl_parser_add_chunk_priority");

  --  * Insert new chunk to a parser (must have previously processed data with an existing top object)
  --  * @param parser parser structure
  --  * @param data the pointer to the beginning of a chunk
  --  * @param len the length of a chunk
  --  * @return true if chunk has been added and false in case of error

   function ucl_parser_insert_chunk
     (parser : System.Address;
      data : access unsigned_char;
      len : size_t) return unsigned_char;  -- ucl.h:1046
   pragma Import (C, ucl_parser_insert_chunk, "ucl_parser_insert_chunk");

  --  * Full version of ucl_add_chunk with priority and duplicate strategy
  --  * @param parser parser structure
  --  * @param data the pointer to the beginning of a chunk
  --  * @param len the length of a chunk
  --  * @param priority the desired priority of a chunk (only 4 least significant bits
  --  * are considered for this parameter)
  --  * @param strat duplicates merging strategy
  --  * @param parse_type input format
  --  * @return true if chunk has been added and false in case of error

   function ucl_parser_add_chunk_full
     (parser : System.Address;
      data : access unsigned_char;
      len : size_t;
      priority : unsigned;
      strat : ucl_duplicate_strategy;
      parse_type : ucl_parse_type) return unsigned_char;  -- ucl.h:1060
   pragma Import (C, ucl_parser_add_chunk_full, "ucl_parser_add_chunk_full");

  --  * Load ucl object from a string
  --  * @param parser parser structure
  --  * @param data the pointer to the string
  --  * @param len the length of the string, if `len` is 0 then `data` must be zero-terminated string
  --  * @return true if string has been added and false in case of error

   function ucl_parser_add_string
     (parser : System.Address;
      data : Interfaces.C.Strings.chars_ptr;
      len : size_t) return unsigned_char;  -- ucl.h:1071
   pragma Import (C, ucl_parser_add_string, "ucl_parser_add_string");

  --  * Load ucl object from a string
  --  * @param parser parser structure
  --  * @param data the pointer to the string
  --  * @param len the length of the string, if `len` is 0 then `data` must be zero-terminated string
  --  * @param priority the desired priority of a chunk (only 4 least significant bits
  --  * are considered for this parameter)
  --  * @return true if string has been added and false in case of error

   function ucl_parser_add_string_priority
     (parser : System.Address;
      data : Interfaces.C.Strings.chars_ptr;
      len : size_t;
      priority : unsigned) return unsigned_char;  -- ucl.h:1083
   pragma Import (C, ucl_parser_add_string_priority, "ucl_parser_add_string_priority");

  --  * Load and add data from a file
  --  * @param parser parser structure
  --  * @param filename the name of file
  --  * @param err if *err is NULL it is set to parser error
  --  * @return true if chunk has been added and false in case of error

   function ucl_parser_add_file
     (parser : System.Address;
      filename : Interfaces.C.Strings.chars_ptr) return unsigned_char;  -- ucl.h:1093
   pragma Import (C, ucl_parser_add_file, "ucl_parser_add_file");

  --  * Load and add data from a file
  --  * @param parser parser structure
  --  * @param filename the name of file
  --  * @param err if *err is NULL it is set to parser error
  --  * @param priority the desired priority of a chunk (only 4 least significant bits
  --  * are considered for this parameter)
  --  * @return true if chunk has been added and false in case of error

   function ucl_parser_add_file_priority
     (parser : System.Address;
      filename : Interfaces.C.Strings.chars_ptr;
      priority : unsigned) return unsigned_char;  -- ucl.h:1105
   pragma Import (C, ucl_parser_add_file_priority, "ucl_parser_add_file_priority");

  --  * Load and add data from a file
  --  * @param parser parser structure
  --  * @param filename the name of file
  --  * @param priority the desired priority of a chunk (only 4 least significant bits
  --  * are considered for this parameter)
  --  * @param strat Merge strategy to use while parsing this file
  --  * @param parse_type Parser type to use while parsing this file
  --  * @return true if chunk has been added and false in case of error

   function ucl_parser_add_file_full
     (parser : System.Address;
      filename : Interfaces.C.Strings.chars_ptr;
      priority : unsigned;
      strat : ucl_duplicate_strategy;
      parse_type : ucl_parse_type) return unsigned_char;  -- ucl.h:1118
   pragma Import (C, ucl_parser_add_file_full, "ucl_parser_add_file_full");

  --  * Load and add data from a file descriptor
  --  * @param parser parser structure
  --  * @param filename the name of file
  --  * @param err if *err is NULL it is set to parser error
  --  * @return true if chunk has been added and false in case of error

   function ucl_parser_add_fd
     (parser : System.Address; fd : int) return unsigned_char;  -- ucl.h:1129
   pragma Import (C, ucl_parser_add_fd, "ucl_parser_add_fd");

  --  * Load and add data from a file descriptor
  --  * @param parser parser structure
  --  * @param filename the name of file
  --  * @param err if *err is NULL it is set to parser error
  --  * @param priority the desired priority of a chunk (only 4 least significant bits
  --  * are considered for this parameter)
  --  * @return true if chunk has been added and false in case of error

   function ucl_parser_add_fd_priority
     (parser : System.Address;
      fd : int;
      priority : unsigned) return unsigned_char;  -- ucl.h:1141
   pragma Import (C, ucl_parser_add_fd_priority, "ucl_parser_add_fd_priority");

  --  * Load and add data from a file descriptor
  --  * @param parser parser structure
  --  * @param filename the name of file
  --  * @param err if *err is NULL it is set to parser error
  --  * @param priority the desired priority of a chunk (only 4 least significant bits
  --  * are considered for this parameter)
  --  * @param strat Merge strategy to use while parsing this file
  --  * @param parse_type Parser type to use while parsing this file
  --  * @return true if chunk has been added and false in case of error

   function ucl_parser_add_fd_full
     (parser : System.Address;
      fd : int;
      priority : unsigned;
      strat : ucl_duplicate_strategy;
      parse_type : ucl_parse_type) return unsigned_char;  -- ucl.h:1155
   pragma Import (C, ucl_parser_add_fd_full, "ucl_parser_add_fd_full");

  --  * Provide a UCL_ARRAY of paths to search for include files. The object is
  --  * copied so caller must unref the object.
  --  * @param parser parser structure
  --  * @param paths UCL_ARRAY of paths to search
  --  * @return true if the path search array was replaced in the parser

   function ucl_set_include_path
     (parser : System.Address; paths : access ucl_object_t) return unsigned_char;  -- ucl.h:1166
   pragma Import (C, ucl_set_include_path, "ucl_set_include_path");

  --  * Get a top object for a parser (refcount is increased)
  --  * @param parser parser structure
  --  * @param err if *err is NULL it is set to parser error
  --  * @return top parser object or NULL

   function ucl_parser_get_object
     (parser : System.Address) return access ucl_object_t;  -- ucl.h:1175
   pragma Import (C, ucl_parser_get_object, "ucl_parser_get_object");

  --  * Get the current stack object as stack accessor function for use in macro
  --  * functions (refcount is increased)
  --  * @param parser parser object
  --  * @param depth depth of stack to retrieve (top is 0)
  --  * @return current stack object or NULL

   function ucl_parser_get_current_stack_object
     (parser : System.Address; depth : unsigned) return access ucl_object_t;  -- ucl.h:1184
   pragma Import (C, ucl_parser_get_current_stack_object, "ucl_parser_get_current_stack_object");

  --  * Peek at the character at the current chunk position
  --  * @param parser parser structure
  --  * @return current chunk position character

   function ucl_parser_chunk_peek (parser : System.Address) return unsigned_char;  -- ucl.h:1191
   pragma Import (C, ucl_parser_chunk_peek, "ucl_parser_chunk_peek");

  --  * Skip the character at the current chunk position
  --  * @param parser parser structure
  --  * @return success boolean

   function ucl_parser_chunk_skip (parser : System.Address) return unsigned_char;  -- ucl.h:1198
   pragma Import (C, ucl_parser_chunk_skip, "ucl_parser_chunk_skip");

  --  * Get the error string if parsing has been failed
  --  * @param parser parser object
  --  * @return error description

   function ucl_parser_get_error (parser : System.Address) return Interfaces.C.Strings.chars_ptr;  -- ucl.h:1205
   pragma Import (C, ucl_parser_get_error, "ucl_parser_get_error");

  --  * Get the code of the last error
  --  * @param parser parser object
  --  * @return error code

   function ucl_parser_get_error_code (parser : System.Address) return int;  -- ucl.h:1212
   pragma Import (C, ucl_parser_get_error_code, "ucl_parser_get_error_code");

  --  * Get the current column number within parser
  --  * @param parser parser object
  --  * @return current column number

   function ucl_parser_get_column (parser : System.Address) return unsigned;  -- ucl.h:1219
   pragma Import (C, ucl_parser_get_column, "ucl_parser_get_column");

  --  * Get the current line number within parser
  --  * @param parser parser object
  --  * @return current line number

   function ucl_parser_get_linenum (parser : System.Address) return unsigned;  -- ucl.h:1226
   pragma Import (C, ucl_parser_get_linenum, "ucl_parser_get_linenum");

  --  * Clear the error in the parser
  --  * @param parser parser object

   procedure ucl_parser_clear_error (parser : System.Address);  -- ucl.h:1232
   pragma Import (C, ucl_parser_clear_error, "ucl_parser_clear_error");

  --  * Free ucl parser object
  --  * @param parser parser object

   procedure ucl_parser_free (parser : System.Address);  -- ucl.h:1238
   pragma Import (C, ucl_parser_free, "ucl_parser_free");

  --  * Get constant opaque pointer to comments structure for this parser. Increase
  --  * refcount to prevent this object to be destroyed on parser's destruction
  --  * @param parser parser structure
  --  * @return ucl comments pointer or NULL

   function ucl_parser_get_comments
     (parser : System.Address) return access constant ucl_object_t;  -- ucl.h:1246
   pragma Import (C, ucl_parser_get_comments, "ucl_parser_get_comments");

  --  * Utility function to find a comment object for the specified object in the input
  --  * @param comments comments object
  --  * @param srch search object
  --  * @return string comment enclosed in ucl_object_t

   function ucl_comments_find
     (comments : access constant ucl_object_t;
      srch : access constant ucl_object_t) return access constant ucl_object_t;  -- ucl.h:1254
   pragma Import (C, ucl_comments_find, "ucl_comments_find");


  --  * Move comment from `from` object to `to` object
  --  * @param comments comments object
  --  * @param what source object
  --  * @param with destination object
  --  * @return `true` if `from` has comment and it has been moved to `to`

   function ucl_comments_move
     (comments : access ucl_object_t;
      from : access constant ucl_object_t;
      to : access constant ucl_object_t) return unsigned_char;  -- ucl.h:1264
   pragma Import (C, ucl_comments_move, "ucl_comments_move");

  --  * Adds a new comment for an object
  --  * @param comments comments object
  --  * @param obj object to add comment to
  --  * @param comment string representation of a comment

   procedure ucl_comments_add
     (comments : access ucl_object_t;
      obj : access constant ucl_object_t;
      comment : Interfaces.C.Strings.chars_ptr);  -- ucl.h:1273
   pragma Import (C, ucl_comments_add, "ucl_comments_add");

  --  * Add new public key to parser for signatures check
  --  * @param parser parser object
  --  * @param key PEM representation of a key
  --  * @param len length of the key
  --  * @param err if *err is NULL it is set to parser error
  --  * @return true if a key has been successfully added

   function ucl_parser_pubkey_add
     (parser : System.Address;
      key : access unsigned_char;
      len : size_t) return unsigned_char;  -- ucl.h:1284
   pragma Import (C, ucl_parser_pubkey_add, "ucl_parser_pubkey_add");

  --  * Set FILENAME and CURDIR variables in parser
  --  * @param parser parser object
  --  * @param filename filename to set or NULL to set FILENAME to "undef" and CURDIR to getcwd()
  --  * @param need_expand perform realpath() if this variable is true and filename is not NULL
  --  * @return true if variables has been set

   function ucl_parser_set_filevars
     (parser : System.Address;
      filename : Interfaces.C.Strings.chars_ptr;
      need_expand : unsigned_char) return unsigned_char;  -- ucl.h:1294
   pragma Import (C, ucl_parser_set_filevars, "ucl_parser_set_filevars");

  --  * Returns current file for the parser
  --  * @param parser parser object
  --  * @return current file or NULL if parsing memory

   function ucl_parser_get_cur_file (parser : System.Address) return Interfaces.C.Strings.chars_ptr;  -- ucl.h:1302
   pragma Import (C, ucl_parser_get_cur_file, "ucl_parser_get_cur_file");

  --  * Defines special handler for certain types of data (identified by magic)

   type ucl_parser_special_handler_t is access function
        (arg1 : System.Address;
         arg2 : access unsigned_char;
         arg3 : size_t;
         arg4 : System.Address;
         arg5 : access size_t;
         arg6 : System.Address) return unsigned_char;
   pragma Convention (C, ucl_parser_special_handler_t);  -- ucl.h:1307

  --  * Special handler flags

   type ucl_special_handler_flags is
     (UCL_SPECIAL_HANDLER_DEFAULT,
      UCL_SPECIAL_HANDLER_PREPROCESS_ALL);
   pragma Convention (C, ucl_special_handler_flags);  -- ucl.h:1315

  --  * Special handler structure

   type ucl_parser_special_handler is record
      magic : access unsigned_char;  -- ucl.h:1324
      magic_len : aliased size_t;  -- ucl.h:1325
      flags : aliased ucl_special_handler_flags;  -- ucl.h:1326
      handler : ucl_parser_special_handler_t;  -- ucl.h:1327
      free_function : access procedure
           (arg1 : access unsigned_char;
            arg2 : size_t;
            arg3 : System.Address);  -- ucl.h:1328
      user_data : System.Address;  -- ucl.h:1329
      next : access ucl_parser_special_handler;  -- ucl.h:1330
   end record;
   pragma Convention (C_Pass_By_Copy, ucl_parser_special_handler);  -- ucl.h:1323

  -- Used internally
  --  * Add special handler for a parser, handles special sequences identified by magic
  --  * @param parser parser structure
  --  * @param handler handler structure

   procedure ucl_parser_add_special_handler (parser : System.Address; handler : access ucl_parser_special_handler);  -- ucl.h:1338
   pragma Import (C, ucl_parser_add_special_handler, "ucl_parser_add_special_handler");

  --  * Handler for include traces:
  --  * @param parser parser object
  --  * @param parent where include is done from
  --  * @param args arguments to an include
  --  * @param path path of the include
  --  * @param pathlen length of the path
  --  * @param user_data opaque userdata

   --  skipped function type ucl_include_trace_func_t

  --  * Register trace function for an include handler
  --  * @param parser parser object
  --  * @param func function to trace includes
  --  * @param user_data opaque data

   procedure ucl_parser_set_include_tracer
     (parser : System.Address;
      func : access procedure
        (arg1 : System.Address;
         arg2 : access constant ucl_object_t;
         arg3 : access constant ucl_object_t;
         arg4 : Interfaces.C.Strings.chars_ptr;
         arg5 : size_t;
         arg6 : System.Address);
      user_data : System.Address);  -- ucl.h:1363
   pragma Import (C, ucl_parser_set_include_tracer, "ucl_parser_set_include_tracer");

  --  * @defgroup emitter Emitting functions
  --  * These functions are used to serialise UCL objects to some string representation.
  --  *
  --  * Structure using for emitter callbacks

  --  * Append a single character
   type ucl_emitter_functions is record
      ucl_emitter_append_character : access function
           (arg1 : unsigned_char;
            arg2 : size_t;
            arg3 : System.Address) return int;  -- ucl.h:1382
      ucl_emitter_append_len : access function
           (arg1 : access unsigned_char;
            arg2 : size_t;
            arg3 : System.Address) return int;  -- ucl.h:1384
      ucl_emitter_append_int : access function (arg1 : int64_t; arg2 : System.Address) return int;  -- ucl.h:1386
      ucl_emitter_append_double : access function (arg1 : double; arg2 : System.Address) return int;  -- ucl.h:1388
      ucl_emitter_free_func : access procedure (arg1 : System.Address);  -- ucl.h:1390
      ud : System.Address;  -- ucl.h:1392
   end record;
   pragma Convention (C_Pass_By_Copy, ucl_emitter_functions);  -- ucl.h:1380

  --  * Append a string of a specified length
  --  * Append a 64 bit integer
  --  * Append floating point element
  --  * Free userdata
  --  * Opaque userdata pointer
  --  * Write a primitive element
   type ucl_emitter_operations is record
      ucl_emitter_write_elt : access procedure
           (arg1 : System.Address;
            arg2 : access constant ucl_object_t;
            arg3 : unsigned_char;
            arg4 : unsigned_char);  -- ucl.h:1398
      ucl_emitter_start_object : access procedure
           (arg1 : System.Address;
            arg2 : access constant ucl_object_t;
            arg3 : unsigned_char);  -- ucl.h:1401
      ucl_emitter_end_object : access procedure (arg1 : System.Address; arg2 : access constant ucl_object_t);  -- ucl.h:1404
      ucl_emitter_start_array : access procedure
           (arg1 : System.Address;
            arg2 : access constant ucl_object_t;
            arg3 : unsigned_char);  -- ucl.h:1407
      ucl_emitter_end_array : access procedure (arg1 : System.Address; arg2 : access constant ucl_object_t);  -- ucl.h:1409
   end record;
   pragma Convention (C_Pass_By_Copy, ucl_emitter_operations);  -- ucl.h:1395

  --  * Start ucl object
  --  * End ucl object
  --  * Start ucl array
  --  *
  --  * Structure that defines emitter functions

  --  * Name of emitter (e.g. json, compact_json)
   type ucl_emitter_context is record
      name : Interfaces.C.Strings.chars_ptr;  -- ucl.h:1417
      id : aliased int;  -- ucl.h:1419
      func : access constant ucl_emitter_functions;  -- ucl.h:1421
      ops : access constant ucl_emitter_operations;  -- ucl.h:1423
      indent : aliased unsigned;  -- ucl.h:1425
      top : access constant ucl_object_t;  -- ucl.h:1427
      comments : access constant ucl_object_t;  -- ucl.h:1429
   end record;
   pragma Convention (C_Pass_By_Copy, ucl_emitter_context);  -- ucl.h:1415

  --  * Unique id (e.g. UCL_EMIT_JSON for standard emitters
  --  * A set of output functions
  --  * A set of output operations
  --  * Current amount of indent tabs
  --  * Top level object
  --  * Optional comments
  --  *
  --  * Emit object to a string
  --  * @param obj object
  --  * @param emit_type if type is #UCL_EMIT_JSON then emit json, if type is
  --  * #UCL_EMIT_CONFIG then emit config like object
  --  * @return dump of an object (must be freed after using) or NULL in case of error

   function ucl_object_emit (obj : access constant ucl_object_t; emit_type : ucl_emitter)
                              return Interfaces.C.Strings.chars_ptr;   -- ucl.h:1439
   pragma Import (C, ucl_object_emit, "ucl_object_emit");

  --  * Emit object to a string that can contain `\0` inside
  --  * @param obj object
  --  * @param emit_type if type is #UCL_EMIT_JSON then emit json, if type is
  --  * #UCL_EMIT_CONFIG then emit config like object
  --  * @param len the resulting length
  --  * @return dump of an object (must be freed after using) or NULL in case of error

   function ucl_object_emit_len
     (obj : access constant ucl_object_t;
      emit_type : ucl_emitter;
      len : access size_t) return access unsigned_char;  -- ucl.h:1450
   pragma Import (C, ucl_object_emit_len, "ucl_object_emit_len");

  --  * Emit object to a string
  --  * @param obj object
  --  * @param emit_type if type is #UCL_EMIT_JSON then emit json, if type is
  --  * #UCL_EMIT_CONFIG then emit config like object
  --  * @param emitter a set of emitter functions
  --  * @param comments optional comments for the parser
  --  * @return dump of an object (must be freed after using) or NULL in case of error

   function ucl_object_emit_full
     (obj : access constant ucl_object_t;
      emit_type : ucl_emitter;
      emitter : access ucl_emitter_functions;
      comments : access constant ucl_object_t) return unsigned_char;  -- ucl.h:1462
   pragma Import (C, ucl_object_emit_full, "ucl_object_emit_full");

  --  * Start streamlined UCL object emitter
  --  * @param obj top UCL object
  --  * @param emit_type emit type
  --  * @param emitter a set of emitter functions
  --  * @return new streamlined context that should be freed by
  --  * `ucl_object_emit_streamline_finish`

   function ucl_object_emit_streamline_new
     (obj : access constant ucl_object_t;
      emit_type : ucl_emitter;
      emitter : access ucl_emitter_functions) return access ucl_emitter_context;  -- ucl.h:1475
   pragma Import (C, ucl_object_emit_streamline_new, "ucl_object_emit_streamline_new");

  --  * Start object or array container for the streamlined output
  --  * @param ctx streamlined context
  --  * @param obj container object

   procedure ucl_object_emit_streamline_start_container (ctx : access ucl_emitter_context; obj : access constant ucl_object_t);  -- ucl.h:1484
   pragma Import (C, ucl_object_emit_streamline_start_container, "ucl_object_emit_streamline_start_container");

  --  * Add a complete UCL object to streamlined output
  --  * @param ctx streamlined context
  --  * @param obj object to output

   procedure ucl_object_emit_streamline_add_object (ctx : access ucl_emitter_context; obj : access constant ucl_object_t);  -- ucl.h:1491
   pragma Import (C, ucl_object_emit_streamline_add_object, "ucl_object_emit_streamline_add_object");

  --  * End previously added container
  --  * @param ctx streamlined context

   procedure ucl_object_emit_streamline_end_container (ctx : access ucl_emitter_context);  -- ucl.h:1497
   pragma Import (C, ucl_object_emit_streamline_end_container, "ucl_object_emit_streamline_end_container");

  --  * Terminate streamlined container finishing all containers in it
  --  * @param ctx streamlined context

   procedure ucl_object_emit_streamline_finish (ctx : access ucl_emitter_context);  -- ucl.h:1503
   pragma Import (C, ucl_object_emit_streamline_finish, "ucl_object_emit_streamline_finish");

  --  * Returns functions to emit object to memory
  --  * @param pmem target pointer (should be freed by caller)
  --  * @return emitter functions structure

   function ucl_object_emit_memory_funcs (pmem : System.Address) return access ucl_emitter_functions;  -- ucl.h:1511
   pragma Import (C, ucl_object_emit_memory_funcs, "ucl_object_emit_memory_funcs");

  --  * Returns functions to emit object to FILE *
  --  * @param fp FILE * object
  --  * @return emitter functions structure

   function ucl_object_emit_file_funcs (fp : ICS.FILEs) return access ucl_emitter_functions;  -- ucl.h:1519
   pragma Import (C, ucl_object_emit_file_funcs, "ucl_object_emit_file_funcs");

  --  * Returns functions to emit object to a file descriptor
  --  * @param fd file descriptor
  --  * @return emitter functions structure

   function ucl_object_emit_fd_funcs (fd : int) return access ucl_emitter_functions;  -- ucl.h:1526
   pragma Import (C, ucl_object_emit_fd_funcs, "ucl_object_emit_fd_funcs");

  --  * Free emitter functions
  --  * @param f pointer to functions

   procedure ucl_object_emit_funcs_free (f : access ucl_emitter_functions);  -- ucl.h:1533
   pragma Import (C, ucl_object_emit_funcs_free, "ucl_object_emit_funcs_free");

  --  * @defgroup schema Schema functions
  --  * These functions are used to validate UCL objects using json schema format

  --  * Used to define UCL schema error

   type ucl_schema_error_code is
     (UCL_SCHEMA_OK,
      UCL_SCHEMA_TYPE_MISMATCH,
      UCL_SCHEMA_INVALID_SCHEMA,
      UCL_SCHEMA_MISSING_PROPERTY,
      UCL_SCHEMA_CONSTRAINT,
      UCL_SCHEMA_MISSING_DEPENDENCY,
      UCL_SCHEMA_EXTERNAL_REF_MISSING,
      UCL_SCHEMA_EXTERNAL_REF_INVALID,
      UCL_SCHEMA_INTERNAL_ERROR,
      UCL_SCHEMA_UNKNOWN);
   pragma Convention (C, ucl_schema_error_code);  -- ucl.h:1547

  --  *< no error
  --  *< type of object is incorrect
  --  *< schema is invalid
  --  *< one or more missing properties
  --  *< constraint found
  --  *< missing dependency
  --  *< cannot fetch external ref
  --  *< invalid external ref
  --  *< something bad happened
  --  *< generic error
  --  *
  --  * Generic ucl schema error

  --  *< error code
   subtype ucl_schema_error_msg_array is Interfaces.C.char_array (0 .. 127);
   type ucl_schema_error is record
      code : aliased ucl_schema_error_code;  -- ucl.h:1564
      msg : aliased ucl_schema_error_msg_array;  -- ucl.h:1565
      obj : access constant ucl_object_t;  -- ucl.h:1566
   end record;
   pragma Convention (C_Pass_By_Copy, ucl_schema_error);  -- ucl.h:1563

  --  *< error message
  --  *< object where error occurred
  --  *
  --  * Validate object `obj` using schema object `schema`.
  --  * @param schema schema object
  --  * @param obj object to validate
  --  * @param err error pointer, if this parameter is not NULL and error has been
  --  * occurred, then `err` is filled with the exact error definition.
  --  * @return true if `obj` is valid using `schema`

   function ucl_object_validate
     (schema : access constant ucl_object_t;
      obj : access constant ucl_object_t;
      err : access ucl_schema_error) return unsigned_char;  -- ucl.h:1577
   pragma Import (C, ucl_object_validate, "ucl_object_validate");

  --  * Validate object `obj` using schema object `schema` and root schema at `root`.
  --  * @param schema schema object
  --  * @param obj object to validate
  --  * @param root root schema object
  --  * @param err error pointer, if this parameter is not NULL and error has been
  --  * occurred, then `err` is filled with the exact error definition.
  --  * @return true if `obj` is valid using `schema`

   function ucl_object_validate_root
     (schema : access constant ucl_object_t;
      obj : access constant ucl_object_t;
      root : access constant ucl_object_t;
      err : access ucl_schema_error) return unsigned_char;  -- ucl.h:1589
   pragma Import (C, ucl_object_validate_root, "ucl_object_validate_root");


  --  * Validate object `obj` using schema object `schema` and root schema at `root`
  --  * using some external references provided.
  --  * @param schema schema object
  --  * @param obj object to validate
  --  * @param root root schema object
  --  * @param ext_refs external references (might be modified during validation)
  --  * @param err error pointer, if this parameter is not NULL and error has been
  --  * occurred, then `err` is filled with the exact error definition.
  --  * @return true if `obj` is valid using `schema`

   function ucl_object_validate_root_ext
     (schema : access constant ucl_object_t;
      obj : access constant ucl_object_t;
      root : access constant ucl_object_t;
      ext_refs : access ucl_object_t;
      err : access ucl_schema_error) return unsigned_char;  -- ucl.h:1605
   pragma Import (C, ucl_object_validate_root_ext, "ucl_object_validate_root_ext");

  --  * XXX: Poorly named API functions, need to replace them with the appropriate
  --  * named function. All API functions *must* use naming ucl_object_*. Usage of
  --  * ucl_obj* should be avoided.

end libucl;
