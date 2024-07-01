--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Real_Time;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ucl;

private with Ada.Containers.Hashed_Maps;

package ThickUCL is

   package RT renames Ada.Real_Time;
   package ASU renames Ada.Strings.Unbounded;
   package CON renames Ada.Containers;

   type UclTree is tagged private;

   type Leaf_type is
     (data_not_present,
      data_object,
      data_array,
      data_integer,
      data_float,
      data_string,
      data_boolean,
      data_time);

   subtype array_index  is Natural range Natural'First .. 499_999;
   subtype object_index is Natural range Natural'First .. 499_999;

   type DataString is
      record
         payload : ASU.Unbounded_String;
      end record;

   package jar_string is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => DataString);


   -----------------------------------------------------------
   --  Methods to build top level UCL Object (serial only)  --
   -----------------------------------------------------------
   --  Exceptions possible on insert procedures if name     --
   --  (key) already been used.                             --
   -----------------------------------------------------------

   procedure insert
     (tree : in out UclTree;
      name : String;
      value : Ucl.ucl_integer);

   procedure insert
     (tree : in out UclTree;
      name : String;
      value : Float);

   procedure insert
     (tree : in out UclTree;
      name : String;
      value : Boolean);

   procedure insert
     (tree : in out UclTree;
      name : String;
      value : RT.Time_Span);

   procedure insert
     (tree : in out UclTree;
      name : String;
      value : String);

   procedure start_array
     (tree : in out UclTree;
      name : String);

   procedure reopen_array
     (tree : in out UclTree;
      name : String;
      element_index : array_index := array_index'First);

   procedure close_array
     (tree : in out UclTree);

   procedure start_object
     (tree : in out UclTree;
      name : String);

   procedure reopen_object
     (tree : in out UclTree;
      name : String;
      element_index : array_index := array_index'First);

   procedure close_object
     (tree : in out UclTree);


   -----------------------------------
   --  Methods to query UCL object  --
   -----------------------------------

   ucl_type_mismatch  : exception;
   ucl_key_not_found  : exception;
   index_out_of_range : exception;

   --  Stump-level fields
   function get_data_type
     (tree : UclTree;
      key  : String) return Leaf_type;

   --  Check if key defined at the stump level
   function key_exists
     (tree : UclTree;
      key  : String) return Boolean;

   --  Returns true if stump-level key exists and holds a string
   function string_field_exists
     (tree : UclTree;
      key  : String) return Boolean;

   --  Returns true if stump-level key exists and holds an integer
   function integer_field_exists
     (tree : UclTree;
      key  : String) return Boolean;

   --  Returns true if stump-level key exists and holds a floating point number
   function float_field_exists
     (tree : UclTree;
      key  : String) return Boolean;

   --  Returns true if stump-level key exists and holds a boolean value
   function boolean_field_exists
     (tree : UclTree;
      key  : String) return Boolean;

   --  Returns true if stump-level key exists and holds a time value
   function time_field_exists
     (tree : UclTree;
      key  : String) return Boolean;

   --  Returns true if stump-level key exists and holds an array
   function array_field_exists
     (tree : UclTree;
      key  : String) return Boolean;

   --  Returns true if stump-level key exists and holds a ucl object
   function ucl_object_field_exists
     (tree : UclTree;
      key  : String) return Boolean;

   --  Get stump-level integer values from key (possible exception)
   function get_base_value
     (tree : UclTree;
      key  : String) return Ucl.ucl_integer;

   --  Get stump-level float values from key (possible exception)
   function get_base_value
     (tree : UclTree;
      key  : String) return Float;

   --  Get stump-level boolean values from key (possible exception)
   function get_base_value
     (tree : UclTree;
      key  : String) return Boolean;

   --  Get stump-level time values from key (possible exception)
   function get_base_value
     (tree : UclTree;
      key  : String) return RT.Time_Span;

   --  Get stump-level string values from key (possible exception)
   function get_base_value
     (tree : UclTree;
      key  : String) return String;

   --  Get index to a stump-level array (possible exceptions)
   function get_index_of_base_array
     (tree  : UclTree;
      key   : String) return array_index;

   --  Get number of elements in the array (possible exceptions)
   function get_number_of_array_elements
     (tree : UclTree;
      vndx : array_index) return Natural;

   --  Get data-type of array element #index (zero-indexed) (possible exceptions)
   function get_array_element_type
     (tree  : UclTree;
      vndx  : array_index;
      index : Natural) return Leaf_type;

   --  Get array element #index integer value (zero-indexed) (possible exceptions)
   function get_array_element_value
     (tree  : UclTree;
      vndx  : array_index;
      index : Natural) return Ucl.ucl_integer;

   --  Get array element #index float value (zero-indexed) (possible exceptions)
   function get_array_element_value
     (tree  : UclTree;
      vndx  : array_index;
      index : Natural) return Float;

   --  Get array element #index boolean value (zero-indexed) (possible exceptions)
   function get_array_element_value
     (tree  : UclTree;
      vndx  : array_index;
      index : Natural) return Boolean;

   --  Get array element #index time value (zero-indexed) (possible exceptions)
   function get_array_element_value
     (tree  : UclTree;
      vndx  : array_index;
      index : Natural) return RT.Time_Span;

   --  Get array element #index string value (zero-indexed) (possible exceptions)
   function get_array_element_value
     (tree  : UclTree;
      vndx  : array_index;
      index : Natural) return String;

   --  Get array element #index deeper array value (zero-indexed) (possible exceptions)
   function get_array_element_array
     (tree  : UclTree;
      vndx  : array_index;
      index : Natural) return array_index;

   --  Get array element #index object index (zero-indexed) (possible exceptions)
   function get_array_element_object
     (tree  : UclTree;
      vndx  : array_index;
      index : Natural) return object_index;

   --  Get index to a stump-level ucl object (possible exceptions)
   function get_index_of_base_ucl_object
     (tree  : UclTree;
      key   : String) return object_index;

   --  data type from deeper object
   function get_object_data_type
     (tree : UclTree;
      vndx : object_index;
      key  : String) return Leaf_type;

   --  Get integer values from key (possible exception)
   function get_object_value
     (tree : UclTree;
      vndx : object_index;
      key  : String) return Ucl.ucl_integer;

   --  Get float values from key (possible exception)
   function get_object_value
     (tree : UclTree;
      vndx : object_index;
      key  : String) return Float;

   --  Get boolean values from key (possible exception)
   function get_object_value
     (tree : UclTree;
      vndx : object_index;
      key  : String) return Boolean;

   --  Get time values from key (possible exception)
   function get_object_value
     (tree : UclTree;
      vndx : object_index;
      key  : String) return RT.Time_Span;

   --  Get string values from key (possible exception)
   function get_object_value
     (tree : UclTree;
      vndx : object_index;
      key  : String) return String;

   --  Get index of array from key (possible exception)
   function get_object_array
     (tree : UclTree;
      vndx : object_index;
      key  : String) return array_index;

   --  Get index of ucl object from key (possible exception)
   function get_object_object
     (tree : UclTree;
      vndx : object_index;
      key  : String) return object_index;

   --  Returns a container full of keys from a stump-level object
   --  The container is sorted alphabetically ascending
   procedure get_base_object_keys
     (tree : UclTree;
      object_keys : in out jar_string.Vector);

   --  Returns a container full of keys from nested object
   --  The container is sorted alphabetically ascending
   procedure get_object_object_keys
     (tree : UclTree;
      vndx : object_index;
      object_keys : in out jar_string.Vector);

   --  Returns the vector index of the indicated array element
   function get_array_element_vector_index
     (tree  : UclTree;
      vndx  : array_index;
      index : Natural) return Natural;

   --  Get the vector index of the entire object
   function get_object_vector_index
     (tree : UclTree;
      vndx : object_index;
      key  : String) return Natural;

   --  Returns true if the jar_string container contains the given key string
   function key_found (key_container : jar_string.Vector; key : String) return Boolean;


   ---------------------------------------
   --  Methods to prune the UCL object  --
   ---------------------------------------

   --  If key pair is defined at the stump level, the data is removed
   procedure drop_base_keypair
     (tree : in out UclTree;
      key  : String);


private

   use type Ucl.ucl_integer;
   use type RT.Time_Span;

   type DataType is
     (ucl_object,
      ucl_array,
      ucl_integer,
      ucl_float,
      ucl_string,
      ucl_boolean,
      ucl_time);

   package jar_integer is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => Ucl.ucl_integer);

   package jar_float is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => Float);

   package jar_boolean is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => Boolean);

   package jar_time is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => RT.Time_Span);

   type DataReference is tagged
      record
         data_type    : DataType;
         vector_index : Natural;
      end record;

   package jar_array is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => DataReference);

   function map_hash (key : ASU.Unbounded_String) return CON.Hash_Type;
   function equivalent (A, B : ASU.Unbounded_String) return Boolean;

   package jar_ucl_objects is new CON.Hashed_Maps
     (Key_Type => ASU.Unbounded_String,
      Element_Type => DataReference,
      Hash => map_hash,
      Equivalent_Keys => equivalent);

   package box_of_array_jars is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => jar_array.Vector,
      "=" => jar_array."=");

   package box_of_ucl_object_jars is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => jar_ucl_objects.Map,
      "=" => jar_ucl_objects."=");


   type UclTree is tagged
      record
         store_integers : jar_integer.Vector;
         store_floats   : jar_float.Vector;
         store_booleans : jar_boolean.Vector;
         store_times    : jar_time.Vector;
         store_strings  : jar_string.Vector;
         store_arrays   : box_of_array_jars.Vector;
         store_objects  : box_of_ucl_object_jars.Vector;
         open_structure : jar_array.Vector;

         tree_stump     : jar_ucl_objects.Map;
      end record;

   --  Returns true if name is an empty string
   function key_missing (name : String) return Boolean;

   --  Helper to get the currently open structure type (array or object)
   function last_open_structure (tree : UclTree) return DataType;

   --  Helper to get index where the last open structure is stored
   function last_reference_index (tree : UclTree) return Natural;

   --  Helper - generic field exists code to avoid duplication
   function generic_field_exists
     (tree  : UclTree;
      key   : String;
      ftype : DataType) return Boolean;

   --  Define comparision function for DataString for sorting purposes.
   function "<" (left, right : DataString) return Boolean;

   package string_sorting is new jar_string.Generic_Sorting;

   ERR_NEEDS_KEY  : constant String := "Error: key required but is missing.  Item skipped.";
   WARN_EXTRA_KEY : constant String := "Warning: key was found but not expected, ignoring.";
   ERR_ELE_INDEX  : constant String := "Error: Array element index is out of range.";


end ThickUCL;
