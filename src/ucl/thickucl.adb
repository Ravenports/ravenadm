--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Strings.Hash;
with Ada.Text_IO;

package body ThickUCL is

   package TIO renames Ada.Text_IO;

   ----------------
   --  map_hash  --
   ----------------
   function map_hash (key : ASU.Unbounded_String) return CON.Hash_Type is
   begin
      return Ada.Strings.Hash (ASU.To_String (key));
   end map_hash;


   ------------------
   --  equivalent  --
   ------------------
   function equivalent (A, B : ASU.Unbounded_String) return Boolean
   is
      use type ASU.Unbounded_String;
   begin
      return A = B;
   end equivalent;


   -------------------
   --  key_missing  --
   -------------------
   function key_missing (name : String) return Boolean
   is
   begin
      return name = "";
   end key_missing;


   ---------------------------
   --  last_open_structure  --
   ---------------------------
   function last_open_structure (tree : UclTree) return DataType is
   begin
      return tree.open_structure.Last_Element.data_type;
   end last_open_structure;


   ----------------------------
   --  last_reference_index  --
   ----------------------------
   function last_reference_index (tree : UclTree) return Natural is
   begin
      return tree.open_structure.Last_Element.vector_index;
   end last_reference_index;


   ---------------------------------------------------------------------------------
   --  For the following Insert procedures:
   --  With new open structures, the pointer is on the stump.  That means each
   --  value requires a key (name) to be non-empty.
   --
   --  When to top open structure is an array, each name is required to be empty.
   --  Names that are non-empty in this case will invoke error messages.
   --
   --  When the top open structure is an object, names will again be required.
   ---------------------------------------------------------------------------------

   -----------------
   --  insert #1  --
   -----------------
   procedure insert
     (tree : in out UclTree;
      name : String;
      value : Ucl.ucl_integer)
   is
      procedure wrap_payload;
      procedure append_into_array (Element : in out jar_array.Vector);
      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map);

      struct_index : Natural;
      global_index : Natural;
      dref : DataReference;
      name_us : constant ASU.Unbounded_String := ASU.To_Unbounded_String (name);

      procedure wrap_payload is
      begin
         tree.store_integers.Append (value);
         global_index := tree.store_integers.Last_Index;
         dref.data_type := ucl_integer;
         dref.vector_index := global_index;
      end wrap_payload;

      procedure append_into_array (Element : in out jar_array.Vector) is
      begin
         Element.Append (dref);
      end append_into_array;

      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map) is
      begin
         Element.Insert (name_us, dref);
      end append_into_uclobj;
   begin
      if tree.open_structure.Is_Empty then
         if key_missing (name) then
            TIO.Put_Line (ERR_NEEDS_KEY & ":" & value'Img);
            return;
         end if;
         wrap_payload;
         tree.tree_stump.Insert (name_us, dref);
         return;
      end if;

      struct_index := tree.last_reference_index;
      case tree.last_open_structure is
         when ucl_array =>
            if not key_missing (name) then
               TIO.Put_Line (WARN_EXTRA_KEY & " (" & name & ")");
            end if;
            wrap_payload;
            tree.store_arrays.Update_Element (Index => struct_index,
                                              Process => append_into_array'Access);
         when ucl_object =>
            if key_missing (name) then
               TIO.Put_Line (ERR_NEEDS_KEY & ":" & ucl_integer'Img);
               return;
            end if;
            wrap_payload;
            tree.store_objects.Update_Element (Index => struct_index,
                                               Process => append_into_uclobj'Access);
         when others =>
            --  It should be impossible to get here.
            null;
      end case;
   end insert;


   -----------------
   --  insert #2  --
   -----------------
   procedure insert
     (tree : in out UclTree;
      name : String;
      value : Float)
   is
      procedure wrap_payload;
      procedure append_into_array (Element : in out jar_array.Vector);
      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map);

      struct_index : Natural;
      global_index : Natural;
      dref : DataReference;
      name_us : constant ASU.Unbounded_String := ASU.To_Unbounded_String (name);

      procedure wrap_payload is
      begin
         tree.store_floats.Append (value);
         global_index := tree.store_floats.Last_Index;
         dref.data_type := ucl_float;
         dref.vector_index := global_index;
      end wrap_payload;

      procedure append_into_array (Element : in out jar_array.Vector) is
      begin
         Element.Append (dref);
      end append_into_array;

      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map) is
      begin
         Element.Insert (name_us, dref);
      end append_into_uclobj;
   begin
      if tree.open_structure.Is_Empty then
         if key_missing (name) then
            TIO.Put_Line (ERR_NEEDS_KEY & ":" & value'Img);
            return;
         end if;
         wrap_payload;
         tree.tree_stump.Insert (name_us, dref);
         return;
      end if;

      struct_index := tree.last_reference_index;
      case tree.last_open_structure is
         when ucl_array =>
            if not key_missing (name) then
               TIO.Put_Line (WARN_EXTRA_KEY & " (" & name & ")");
            end if;
            wrap_payload;
            tree.store_arrays.Update_Element (Index => struct_index,
                                              Process => append_into_array'Access);
         when ucl_object =>
            if key_missing (name) then
               TIO.Put_Line (ERR_NEEDS_KEY & ":" & ucl_integer'Img);
               return;
            end if;
            wrap_payload;
            tree.store_objects.Update_Element (Index => struct_index,
                                               Process => append_into_uclobj'Access);
         when others => null;
      end case;
   end insert;


   -----------------
   --  insert #3  --
   -----------------
   procedure insert
     (tree : in out UclTree;
      name : String;
      value : Boolean)
   is
      procedure wrap_payload;
      procedure append_into_array (Element : in out jar_array.Vector);
      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map);

      struct_index : Natural;
      global_index : Natural;
      dref : DataReference;
      name_us : constant ASU.Unbounded_String := ASU.To_Unbounded_String (name);

      procedure wrap_payload is
      begin
         tree.store_booleans.Append (value);
         global_index := tree.store_booleans.Last_Index;
         dref.data_type := ucl_boolean;
         dref.vector_index := global_index;
      end wrap_payload;

      procedure append_into_array (Element : in out jar_array.Vector) is
      begin
         Element.Append (dref);
      end append_into_array;

      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map) is
      begin
         Element.Insert (name_us, dref);
      end append_into_uclobj;
   begin
      if tree.open_structure.Is_Empty then
         if key_missing (name) then
            TIO.Put_Line (ERR_NEEDS_KEY & ":" & value'Img);
            return;
         end if;
         wrap_payload;
         tree.tree_stump.Insert (name_us, dref);
         return;
      end if;

      struct_index := tree.last_reference_index;
      case tree.last_open_structure is
         when ucl_array =>
            if not key_missing (name) then
               TIO.Put_Line (WARN_EXTRA_KEY & " (" & name & ")");
            end if;
            wrap_payload;
            tree.store_arrays.Update_Element (Index => struct_index,
                                              Process => append_into_array'Access);
         when ucl_object =>
            if key_missing (name) then
               TIO.Put_Line (ERR_NEEDS_KEY & ":" & ucl_integer'Img);
               return;
            end if;
            wrap_payload;
            tree.store_objects.Update_Element (Index => struct_index,
                                               Process => append_into_uclobj'Access);
         when others => null;
      end case;
   end insert;


   -----------------
   --  insert #4  --
   -----------------
   procedure insert
     (tree : in out UclTree;
      name : String;
      value : RT.Time_Span)
   is
      procedure wrap_payload;
      procedure append_into_array (Element : in out jar_array.Vector);
      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map);

      struct_index : Natural;
      global_index : Natural;
      dref : DataReference;
      name_us : constant ASU.Unbounded_String := ASU.To_Unbounded_String (name);

      procedure wrap_payload is
      begin
         tree.store_times.Append (value);
         global_index := tree.store_times.Last_Index;
         dref.data_type := ucl_time;
         dref.vector_index := global_index;
      end wrap_payload;

      procedure append_into_array (Element : in out jar_array.Vector) is
      begin
         Element.Append (dref);
      end append_into_array;

      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map) is
      begin
         Element.Insert (name_us, dref);
      end append_into_uclobj;
   begin
      if tree.open_structure.Is_Empty then
         if key_missing (name) then
            TIO.Put_Line (ERR_NEEDS_KEY & ": Time Type");
            return;
         end if;
         wrap_payload;
         tree.tree_stump.Insert (name_us, dref);
         return;
      end if;

      struct_index := tree.last_reference_index;
      case tree.last_open_structure is
         when ucl_array =>
            if not key_missing (name) then
               TIO.Put_Line (WARN_EXTRA_KEY & " (" & name & ")");
            end if;
            wrap_payload;
            tree.store_arrays.Update_Element (Index => struct_index,
                                              Process => append_into_array'Access);
         when ucl_object =>
            if key_missing (name) then
               TIO.Put_Line (ERR_NEEDS_KEY & ":" & ucl_integer'Img);
               return;
            end if;
            wrap_payload;
            tree.store_objects.Update_Element (Index => struct_index,
                                               Process => append_into_uclobj'Access);
         when others => null;
      end case;
   end insert;


   -----------------
   --  insert #5  --
   -----------------
   procedure insert
     (tree : in out UclTree;
      name : String;
      value : String)
    is
      procedure wrap_payload;
      procedure append_into_array (Element : in out jar_array.Vector);
      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map);

      struct_index : Natural;
      global_index : Natural;
      dref : DataReference;
      name_us : constant ASU.Unbounded_String := ASU.To_Unbounded_String (name);

      procedure wrap_payload
      is
         tray : DataString;
      begin
         tray.payload := ASU.To_Unbounded_String (value);
         tree.store_strings.Append (tray);
         global_index := tree.store_strings.Last_Index;
         dref.data_type := ucl_string;
         dref.vector_index := global_index;
      end wrap_payload;

      procedure append_into_array (Element : in out jar_array.Vector) is
      begin
         Element.Append (dref);
      end append_into_array;

      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map) is
      begin
         Element.Insert (name_us, dref);
      end append_into_uclobj;
   begin
      if tree.open_structure.Is_Empty then
         if key_missing (name) then
            TIO.Put_Line (ERR_NEEDS_KEY & ":" & value);
            return;
         end if;
         wrap_payload;
         tree.tree_stump.Insert (name_us, dref);
         return;
      end if;

      struct_index := tree.last_reference_index;
      case tree.last_open_structure is
         when ucl_array =>
            if not key_missing (name) then
               TIO.Put_Line (WARN_EXTRA_KEY & " (" & name & ")");
            end if;
            wrap_payload;
            tree.store_arrays.Update_Element (Index => struct_index,
                                              Process => append_into_array'Access);
         when ucl_object =>
            if key_missing (name) then
               TIO.Put_Line (ERR_NEEDS_KEY & ":" & ucl_integer'Img);
               return;
            end if;
            wrap_payload;
            tree.store_objects.Update_Element (Index => struct_index,
                                               Process => append_into_uclobj'Access);
         when others => null;
      end case;
   end insert;


   -------------------
   --  start_array  --
   -------------------
   procedure start_array
     (tree : in out UclTree;
      name : String)
   is
      procedure wrap_payload;
      procedure append_into_array (Element : in out jar_array.Vector);
      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map);

      struct_index : Natural;
      global_index : Natural;
      dref : DataReference;
      name_us : constant ASU.Unbounded_String := ASU.To_Unbounded_String (name);

      procedure wrap_payload
      is
         spanking_array : jar_array.Vector;
      begin
         tree.store_arrays.Append (spanking_array);
         global_index := tree.store_arrays.Last_Index;

         dref.data_type := ucl_array;
         dref.vector_index := global_index;

         tree.open_structure.Append (dref);
      end wrap_payload;

      procedure append_into_array (Element : in out jar_array.Vector) is
      begin
         Element.Append (dref);
      end append_into_array;

      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map) is
      begin
         Element.Insert (name_us, dref);
      end append_into_uclobj;
   begin
      if tree.open_structure.Is_Empty then
         if key_missing (name) then
            TIO.Put_Line (ERR_NEEDS_KEY & ": new array");
            return;
         end if;

         wrap_payload;
         tree.tree_stump.Insert (name_us, dref);
         return;
      end if;

      struct_index := tree.last_reference_index;
      case tree.last_open_structure is
         when ucl_array =>
            if not key_missing (name) then
               TIO.Put_Line (WARN_EXTRA_KEY & " (" & name & ")");
            end if;
            wrap_payload;
            tree.store_arrays.Update_Element (Index => struct_index,
                                              Process => append_into_array'Access);
         when ucl_object =>
            if key_missing (name) then
               TIO.Put_Line (ERR_NEEDS_KEY & ":" & ucl_integer'Img);
               return;
            end if;
            wrap_payload;
            tree.store_objects.Update_Element (Index => struct_index,
                                               Process => append_into_uclobj'Access);
         when others => null;
      end case;
   end start_array;


   --------------------
   --  reopen_array  --
   --------------------
   procedure reopen_array
     (tree : in out UclTree;
      name : String;
      element_index : array_index := array_index'First)
   is
      ERR_ARRAY_DNE : constant String := "Error: attempted to reopen array that does not exist.";
      ERR_NOT_ARRAY : constant String := "Reopen array error: key does not reference an array.";
      leaf : Leaf_type;
      dref : DataReference;
      LRI  : Natural;
   begin
      dref.data_type := ucl_array;
      if tree.open_structure.Is_Empty then
         if key_missing (name) then
            TIO.Put_Line (ERR_NEEDS_KEY & ": reopen array");
            return;
         end if;

         leaf := tree.get_data_type (name);
         case leaf is
            when data_not_present =>
               TIO.Put_Line (ERR_ARRAY_DNE & " (" & name & ")");
            when data_array =>
               dref.vector_index := tree.get_index_of_base_array (name);
               tree.open_structure.Append (dref);
            when others =>
               TIO.Put_Line (ERR_NOT_ARRAY & " (" & name & ")");
         end case;
         return;
      end if;

      LRI := tree.last_reference_index;
      case tree.last_open_structure is
         when ucl_array =>
            if not key_missing (name) then
               TIO.Put_Line (WARN_EXTRA_KEY & " (" & name & ")");
            end if;
            begin
               leaf := tree.get_array_element_type (LRI, element_index);
            exception
               when index_out_of_range =>
                  TIO.Put_Line (ERR_ELE_INDEX & "(" & element_index'Img & ")");
                  return;
            end;
            case leaf is
               when data_array =>
                  dref.vector_index := tree.get_array_element_vector_index (LRI, element_index);
                  tree.open_structure.Append (dref);
               when others =>
                  TIO.Put_Line (ERR_NOT_ARRAY & " (index" & element_index'Img & ")");
            end case;

         when ucl_object =>
            if key_missing (name) then
               TIO.Put_Line (ERR_NEEDS_KEY & ":" & ucl_integer'Img);
               return;
            end if;
            leaf := tree.get_object_data_type (LRI, name);
            case leaf is
               when data_not_present =>
                  TIO.Put_Line (ERR_ARRAY_DNE & " (" & name & ")");
               when data_array =>
                  dref.vector_index := tree.get_object_vector_index (LRI, name);
                  tree.open_structure.Append (dref);
               when others =>
                  TIO.Put_Line (ERR_NOT_ARRAY & " (index" & element_index'Img & ")");
            end case;

         when others => null;
      end case;
   end reopen_array;


   -------------------
   --  close_array  --
   -------------------
   procedure close_array (tree : in out UclTree)
   is
      ERR_NO_ARRAY_OPEN : constant String := "Error: directive to close array when none are open.";
      ERR_MISMATCH      : constant String := "Error: closure mismatch (array close but object open";
   begin
      if tree.open_structure.Is_Empty then
         TIO.Put_Line (ERR_NO_ARRAY_OPEN);
         return;
      end if;
      case tree.last_open_structure is
         when ucl_array =>
            tree.open_structure.Delete_Last;
         when ucl_object =>
            TIO.Put_Line (ERR_MISMATCH);
         when others =>
            null;
      end case;
   end close_array;


   --------------------
   --  start_object  --
   --------------------
   procedure start_object
     (tree : in out UclTree;
      name : String)
   is
      procedure wrap_payload;
      procedure append_into_array (Element : in out jar_array.Vector);
      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map);

      struct_index : Natural;
      global_index : Natural;
      dref : DataReference;
      name_us : constant ASU.Unbounded_String := ASU.To_Unbounded_String (name);

      procedure wrap_payload
      is
         spanking_map : jar_ucl_objects.Map;
      begin
         tree.store_objects.Append (spanking_map);
         global_index := tree.store_objects.Last_Index;

         dref.data_type := ucl_object;
         dref.vector_index := global_index;

         tree.open_structure.Append (dref);
      end wrap_payload;

      procedure append_into_array (Element : in out jar_array.Vector) is
      begin
         Element.Append (dref);
      end append_into_array;

      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map) is
      begin
         Element.Insert (name_us, dref);
      end append_into_uclobj;
   begin
      if tree.open_structure.Is_Empty then
         if key_missing (name) then
            TIO.Put_Line (ERR_NEEDS_KEY & ": new object map");
            return;
         end if;

         wrap_payload;
         tree.tree_stump.Insert (name_us, dref);
         return;
      end if;

      struct_index := tree.last_reference_index;
      case tree.last_open_structure is
         when ucl_array =>
            if not key_missing (name) then
               TIO.Put_Line (WARN_EXTRA_KEY & " (" & name & ")");
            end if;
            wrap_payload;
            tree.store_arrays.Update_Element (Index => struct_index,
                                              Process => append_into_array'Access);
         when ucl_object =>
            if key_missing (name) then
               TIO.Put_Line (ERR_NEEDS_KEY & ":" & ucl_integer'Img);
               return;
            end if;
            wrap_payload;
            tree.store_objects.Update_Element (Index => struct_index,
                                               Process => append_into_uclobj'Access);
         when others => null;
      end case;
   end start_object;


   ---------------------
   --  reopen_object  --
   ---------------------
   procedure reopen_object
     (tree : in out UclTree;
      name : String;
      element_index : array_index := array_index'First)
   is
      ERR_OBJECT_DNE : constant String := "Error: attempted to reopen object that does not exist.";
      ERR_NOT_OBJECT : constant String := "Reopen object error: key does not reference an object.";
      leaf : Leaf_type;
      dref : DataReference;
      LRI  : Natural;
   begin
      dref.data_type := ucl_object;
      if tree.open_structure.Is_Empty then
         if key_missing (name) then
            TIO.Put_Line (ERR_NEEDS_KEY & ": reopen object");
            return;
         end if;

         leaf := tree.get_data_type (name);
         case leaf is
            when data_not_present =>
               TIO.Put_Line (ERR_OBJECT_DNE & " (" & name & ")");
            when data_object =>
               dref.vector_index := tree.get_index_of_base_ucl_object (name);
               tree.open_structure.Append (dref);
            when others =>
               TIO.Put_Line (ERR_NOT_OBJECT & " (" & name & ")");
         end case;
         return;
      end if;

      LRI := tree.last_reference_index;
      case tree.last_open_structure is
         when ucl_array =>
            if not key_missing (name) then
               TIO.Put_Line (WARN_EXTRA_KEY & " (" & name & ")");
            end if;
            begin
               leaf := tree.get_array_element_type (LRI, element_index);
            exception
               when index_out_of_range =>
                  TIO.Put_Line (ERR_ELE_INDEX & "(" & element_index'Img & ")");
                  return;
            end;
            case leaf is
               when data_object =>
                  dref.vector_index := tree.get_array_element_vector_index (LRI, element_index);
                  tree.open_structure.Append (dref);
               when others =>
                  TIO.Put_Line (ERR_NOT_OBJECT & " (index" & element_index'Img & ")");
            end case;

         when ucl_object =>
            if key_missing (name) then
               TIO.Put_Line (ERR_NEEDS_KEY & ":" & ucl_integer'Img);
               return;
            end if;
            leaf := tree.get_object_data_type (LRI, name);
            case leaf is
               when data_not_present =>
                  TIO.Put_Line (ERR_OBJECT_DNE & " (" & name & ")");
               when data_object =>
                  dref.vector_index := tree.get_object_vector_index (LRI, name);
                  tree.open_structure.Append (dref);
               when others =>
                  TIO.Put_Line (ERR_NOT_OBJECT & " (index" & element_index'Img & ")");
            end case;

         when others => null;
      end case;
   end reopen_object;


   --------------------
   --  close_object  --
   --------------------
   procedure close_object
     (tree : in out UclTree)
   is
      ERR_NO_OBJ_OPEN : constant String := "Error: directive to close object when none are open.";
      ERR_MISMATCH    : constant String := "Error: closure mismatch (object close but array open";
   begin
      if tree.open_structure.Is_Empty then
         TIO.Put_Line (ERR_NO_OBJ_OPEN);
         return;
      end if;
      case tree.last_open_structure is
         when ucl_object =>
            tree.open_structure.Delete_Last;
         when ucl_array =>
            TIO.Put_Line (ERR_MISMATCH);
         when others =>
            null;
      end case;
   end close_object;


   ---------------------
   --  get_data_type  --
   ---------------------
   function get_data_type
     (tree : UclTree;
      key  : String) return Leaf_type
   is
      keystring : constant ASU.Unbounded_String := ASU.To_Unbounded_String (key);
   begin
      if key_missing (key) then
         return data_not_present;
      end if;

      if not tree.tree_stump.Contains (keystring) then
         return data_not_present;
      end if;

      case tree.tree_stump.Element (keystring).data_type is
         when ucl_object  => return data_object;
         when ucl_array   => return data_array;
         when ucl_integer => return data_integer;
         when ucl_float   => return data_float;
         when ucl_string  => return data_string;
         when ucl_boolean => return data_boolean;
         when ucl_time    => return data_time;
      end case;

   end get_data_type;


   ------------------
   --  key_exists  --
   ------------------
   function key_exists
     (tree : UclTree;
      key  : String) return Boolean
   is
      keystring : constant ASU.Unbounded_String := ASU.To_Unbounded_String (key);
   begin
      return tree.tree_stump.Contains (keystring);
   end key_exists;


   ----------------------------
   --  generic_field_exists  --
   ----------------------------
   function generic_field_exists
     (tree  : UclTree;
      key   : String;
      ftype : DataType) return Boolean
   is
      keystring : constant ASU.Unbounded_String := ASU.To_Unbounded_String (key);
   begin
      if not tree.tree_stump.Contains (keystring) then
         return False;
      end if;
      return tree.tree_stump.Element (keystring).data_type = ftype;
   end generic_field_exists;


   ----------------------------
   --  string_field_exists   --
   ----------------------------
   function string_field_exists
     (tree : UclTree;
      key  : String) return Boolean is
   begin
      return tree.generic_field_exists (key, ucl_string);
   end string_field_exists;


   -----------------------------
   --  integer_field_exists   --
   -----------------------------
   function integer_field_exists
     (tree : UclTree;
      key  : String) return Boolean is
   begin
      return tree.generic_field_exists (key, ucl_integer);
   end integer_field_exists;


   ---------------------------
   --  float_field_exists   --
   ---------------------------
   function float_field_exists
     (tree : UclTree;
      key  : String) return Boolean is
   begin
      return tree.generic_field_exists (key, ucl_float);
   end float_field_exists;


   -----------------------------
   --  boolean_field_exists   --
   -----------------------------
   function boolean_field_exists
     (tree : UclTree;
      key  : String) return Boolean is
   begin
      return tree.generic_field_exists (key, ucl_boolean);
   end boolean_field_exists;


   --------------------------
   --  time_field_exists   --
   --------------------------
   function time_field_exists
     (tree : UclTree;
      key  : String) return Boolean is
   begin
      return tree.generic_field_exists (key, ucl_time);
   end time_field_exists;


   ---------------------------
   --  array_field_exists   --
   ---------------------------
   function array_field_exists
     (tree : UclTree;
      key  : String) return Boolean is
   begin
      return tree.generic_field_exists (key, ucl_array);
   end array_field_exists;


   --------------------------------
   --  ucl_object_field_exists   --
   --------------------------------
   function ucl_object_field_exists
     (tree : UclTree;
      key  : String) return Boolean is
   begin
      return tree.generic_field_exists (key, ucl_object);
   end ucl_object_field_exists;


   --------------------------
   --  get_base_value #1   --
   --------------------------
   function get_base_value
     (tree : UclTree;
      key  : String) return Ucl.ucl_integer
   is
      field_type : constant Leaf_type := tree.get_data_type (key);
   begin
      case field_type is
         when data_not_present =>
            raise ucl_key_not_found with key;
         when data_integer =>
            declare
               keystring : constant ASU.Unbounded_String := ASU.To_Unbounded_String (key);
               index     : Natural;
            begin
               index := tree.tree_stump.Element (keystring).vector_index;
               return tree.store_integers.Element (index);
            end;
         when others =>
            raise ucl_type_mismatch with field_type'Img & " found instead of Integer";
      end case;
   end get_base_value;


   --------------------------
   --  get_base_value #2   --
   --------------------------
   function get_base_value
     (tree : UclTree;
      key  : String) return Float
   is
      field_type : constant Leaf_type := tree.get_data_type (key);
   begin
      case field_type is
         when data_not_present =>
            raise ucl_key_not_found with key;
         when data_float =>
            declare
               keystring : constant ASU.Unbounded_String := ASU.To_Unbounded_String (key);
               index     : Natural;
            begin
               index := tree.tree_stump.Element (keystring).vector_index;
               return tree.store_floats.Element (index);
            end;
         when others =>
            raise ucl_type_mismatch with field_type'Img & " found instead of Float";
      end case;
   end get_base_value;


   --------------------------
   --  get_base_value #3   --
   --------------------------
   function get_base_value
     (tree : UclTree;
      key  : String) return Boolean
   is
      field_type : constant Leaf_type := tree.get_data_type (key);
   begin
      case field_type is
         when data_not_present =>
            raise ucl_key_not_found with key;
         when data_boolean =>
            declare
               keystring : constant ASU.Unbounded_String := ASU.To_Unbounded_String (key);
               index     : Natural;
            begin
               index := tree.tree_stump.Element (keystring).vector_index;
               return tree.store_booleans.Element (index);
            end;
         when others =>
            raise ucl_type_mismatch with field_type'Img & " found instead of Boolean";
      end case;
   end get_base_value;


   --------------------------
   --  get_base_value #4   --
   --------------------------
   function get_base_value
     (tree : UclTree;
      key  : String) return RT.Time_Span
   is
      field_type : constant Leaf_type := tree.get_data_type (key);
   begin
      case field_type is
         when data_not_present =>
            raise ucl_key_not_found with key;
         when data_time =>
            declare
               keystring : constant ASU.Unbounded_String := ASU.To_Unbounded_String (key);
               index     : Natural;
            begin
               index := tree.tree_stump.Element (keystring).vector_index;
               return tree.store_times.Element (index);
            end;
         when others =>
            raise ucl_type_mismatch with field_type'Img & " found instead of Time";
      end case;
   end get_base_value;


   --------------------------
   --  get_base_value #5   --
   --------------------------
   function get_base_value
     (tree : UclTree;
      key  : String) return String
   is
      field_type : constant Leaf_type := tree.get_data_type (key);
   begin
      case field_type is
         when data_not_present =>
            raise ucl_key_not_found with key;
         when data_string =>
            declare
               keystring : constant ASU.Unbounded_String := ASU.To_Unbounded_String (key);
               index     : Natural;
            begin
               index := tree.tree_stump.Element (keystring).vector_index;
               return ASU.To_String (tree.store_strings.Element (index).payload);
            end;
         when others =>
            raise ucl_type_mismatch with field_type'Img & " found instead of String";
      end case;
   end get_base_value;


   -------------------------------------
   --  get_number_of_array_elements   --
   -------------------------------------
   function get_number_of_array_elements
     (tree : UclTree;
      vndx : array_index) return Natural
   is
      sa_count : constant Natural := Natural (tree.store_arrays.Length);
   begin
      if vndx >= sa_count then
         raise index_out_of_range with vndx'Img & " exceeds" & sa_count'Img;
      end if;
      return Natural (tree.store_arrays.Element (vndx).Length);
   end get_number_of_array_elements;


   -------------------------------
   --  get_array_element_type   --
   -------------------------------
   function get_array_element_type
     (tree  : UclTree;
      vndx  : array_index;
      index : Natural) return Leaf_type
   is
      array_length : constant Natural := tree.get_number_of_array_elements (vndx);
   begin
      if index >= array_length then
         raise index_out_of_range with "given" & index'Img & " but length is" & array_length'Img;
      end if;
      case tree.store_arrays.Element (vndx).Element (index).data_type is
         when ucl_object  => return data_object;
         when ucl_array   => return data_array;
         when ucl_integer => return data_integer;
         when ucl_float   => return data_float;
         when ucl_string  => return data_string;
         when ucl_boolean => return data_boolean;
         when ucl_time    => return data_time;
      end case;
   end get_array_element_type;


   ----------------------------------
   --  get_array_element_value #1  --
   ----------------------------------
   function get_array_element_value
     (tree  : UclTree;
      vndx  : array_index;
      index : Natural) return Ucl.ucl_integer
   is
      eltype : constant Leaf_type := tree.get_array_element_type (vndx, index);
      dnx    : Natural;
   begin
      case eltype is
         when data_integer =>
            dnx := tree.store_arrays.Element (vndx).Element (index).vector_index;
            return tree.store_integers.Element (dnx);
         when others =>
            raise ucl_type_mismatch with eltype'Img & " found instead of an integer";
      end case;
   end get_array_element_value;


   ----------------------------------
   --  get_array_element_value #2  --
   ----------------------------------
   function get_array_element_value
     (tree  : UclTree;
      vndx  : array_index;
      index : Natural) return Float
   is
      eltype : constant Leaf_type := tree.get_array_element_type (vndx, index);
      dnx    : Natural;
   begin
      case eltype is
         when data_float =>
            dnx := tree.store_arrays.Element (vndx).Element (index).vector_index;
            return tree.store_floats.Element (dnx);
         when others =>
            raise ucl_type_mismatch with eltype'Img & " found instead of a float";
      end case;
   end get_array_element_value;


   ----------------------------------
   --  get_array_element_value #3  --
   ----------------------------------
   function get_array_element_value
     (tree  : UclTree;
      vndx  : array_index;
      index : Natural) return Boolean
   is
      eltype : constant Leaf_type := tree.get_array_element_type (vndx, index);
      dnx    : Natural;
   begin
      case eltype is
         when data_boolean =>
            dnx := tree.store_arrays.Element (vndx).Element (index).vector_index;
            return tree.store_booleans.Element (dnx);
         when others =>
            raise ucl_type_mismatch with eltype'Img & " found instead of a boolean";
      end case;
   end get_array_element_value;


   ----------------------------------
   --  get_array_element_value #4  --
   ----------------------------------
   function get_array_element_value
     (tree  : UclTree;
      vndx  : array_index;
      index : Natural) return RT.Time_Span
   is
      eltype : constant Leaf_type := tree.get_array_element_type (vndx, index);
      dnx    : Natural;
   begin
      case eltype is
         when data_time =>
            dnx := tree.store_arrays.Element (vndx).Element (index).vector_index;
            return tree.store_times.Element (dnx);
         when others =>
            raise ucl_type_mismatch with eltype'Img & " found instead of time";
      end case;
   end get_array_element_value;


   ----------------------------------
   --  get_array_element_value #5  --
   ----------------------------------
   function get_array_element_value
     (tree  : UclTree;
      vndx  : array_index;
      index : Natural) return String
   is
      eltype : constant Leaf_type := tree.get_array_element_type (vndx, index);
      dnx    : Natural;
   begin
      case eltype is
         when data_string =>
            dnx := tree.store_arrays.Element (vndx).Element (index).vector_index;
            return ASU.To_String (tree.store_strings.Element (dnx).payload);
         when others =>
            raise ucl_type_mismatch with eltype'Img & " found instead of a string";
      end case;
   end get_array_element_value;


   ------------------------------------
   --  get_index_of_base_ucl_object  --
   ------------------------------------
   function get_index_of_base_ucl_object
     (tree  : UclTree;
      key   : String) return object_index
   is
      field_type : constant Leaf_type := tree.get_data_type (key);
   begin
      case field_type is
         when data_not_present =>
            raise ucl_key_not_found with key;
         when data_object =>
            declare
               keystring : constant ASU.Unbounded_String := ASU.To_Unbounded_String (key);
            begin
               return object_index (tree.tree_stump.Element (keystring).vector_index);
            end;
         when others =>
            raise ucl_type_mismatch with field_type'Img & " found instead of a ucl object";
      end case;
   end get_index_of_base_ucl_object;


   -------------------------------
   --  get_index_of_base_array  --
   -------------------------------
   function get_index_of_base_array
     (tree  : UclTree;
      key   : String) return array_index
   is
      field_type : constant Leaf_type := tree.get_data_type (key);
   begin
      case field_type is
         when data_not_present =>
            raise ucl_key_not_found with key;
         when data_array =>
            declare
               keystring : constant ASU.Unbounded_String := ASU.To_Unbounded_String (key);
            begin
               return array_index (tree.tree_stump.Element (keystring).vector_index);
            end;
         when others =>
            raise ucl_type_mismatch with field_type'Img & " found instead of an array";
      end case;
   end get_index_of_base_array;


   --------------------------------------
   --  get_array_element_vector_index  --
   --------------------------------------
   function get_array_element_vector_index
     (tree  : UclTree;
      vndx  : array_index;
      index : Natural) return Natural is
   begin
      --  this could thrown an exception with bade vndx and index values
      return tree.store_arrays.Element (vndx).Element (index).vector_index;
   end get_array_element_vector_index;


   -------------------------------
   --  get_array_element_array  --
   -------------------------------
   function get_array_element_array
     (tree  : UclTree;
      vndx  : array_index;
      index : Natural) return array_index
   is
      eltype : constant Leaf_type := tree.get_array_element_type (vndx, index);
   begin
      case eltype is
         when data_array =>
            return tree.get_array_element_vector_index (vndx, index);
         when others =>
            raise ucl_type_mismatch with eltype'Img & " found instead of an array";
      end case;
   end get_array_element_array;


   --------------------------------
   --  get_array_element_object  --
   --------------------------------
   function get_array_element_object
     (tree  : UclTree;
      vndx  : array_index;
      index : Natural) return object_index
   is
      eltype : constant Leaf_type := tree.get_array_element_type (vndx, index);
   begin
      case eltype is
         when data_object =>
            return tree.get_array_element_vector_index (vndx, index);
         when others =>
            raise ucl_type_mismatch with eltype'Img & " found instead of an object index";
      end case;
   end get_array_element_object;


   ----------------------------
   --  get_object_data_type  --
   ----------------------------
   function get_object_data_type
     (tree : UclTree;
      vndx : object_index;
      key  : String) return Leaf_type
   is
      keystring : constant ASU.Unbounded_String := ASU.To_Unbounded_String (key);
   begin
      if key_missing (key) then
         return data_not_present;
      end if;

      if not tree.store_objects.Element (vndx).Contains (keystring) then
         return data_not_present;
      end if;

      case tree.store_objects.Element (vndx).Element (keystring).data_type is
         when ucl_object  => return data_object;
         when ucl_array   => return data_array;
         when ucl_integer => return data_integer;
         when ucl_float   => return data_float;
         when ucl_string  => return data_string;
         when ucl_boolean => return data_boolean;
         when ucl_time    => return data_time;
      end case;

   end get_object_data_type;


   -------------------------------
   --  get_object_vector_index  --
   -------------------------------
   function get_object_vector_index
     (tree : UclTree;
      vndx : object_index;
      key  : String) return Natural
   is
      keystring : constant ASU.Unbounded_String := ASU.To_Unbounded_String (key);
   begin
      if not tree.store_objects.Element (vndx).Contains (keystring) then
         raise ucl_key_not_found with key;
      end if;
      return tree.store_objects.Element (vndx).Element (keystring).vector_index;
   end get_object_vector_index;


   ------------------------
   --  get_object_value  --
   ------------------------
   function get_object_value
     (tree : UclTree;
      vndx : object_index;
      key  : String) return Ucl.ucl_integer
   is
      field_type : constant Leaf_type := tree.get_object_data_type (vndx, key);
   begin
      case field_type is
         when data_not_present =>
            raise ucl_key_not_found with key;
         when data_integer =>
            declare
               keystring : constant ASU.Unbounded_String := ASU.To_Unbounded_String (key);
               index     : Natural;
            begin
               index := tree.store_objects (vndx).Element (keystring).vector_index;
               return tree.store_integers.Element (index);
            end;
         when others =>
            raise ucl_type_mismatch with field_type'Img & " found instead of Integer";
      end case;
   end get_object_value;


   ------------------------
   --  get_object_value  --
   ------------------------
   function get_object_value
     (tree : UclTree;
      vndx : object_index;
      key  : String) return Float
   is
      field_type : constant Leaf_type := tree.get_object_data_type (vndx, key);
   begin
      case field_type is
         when data_not_present =>
            raise ucl_key_not_found with key;
         when data_float =>
            declare
               keystring : constant ASU.Unbounded_String := ASU.To_Unbounded_String (key);
               index     : Natural;
            begin
               index := tree.store_objects (vndx).Element (keystring).vector_index;
               return tree.store_floats.Element (index);
            end;
         when others =>
            raise ucl_type_mismatch with field_type'Img & " found instead of Float";
      end case;
   end get_object_value;


   ------------------------
   --  get_object_value  --
   ------------------------
   function get_object_value
     (tree : UclTree;
      vndx : object_index;
      key  : String) return Boolean
   is
      field_type : constant Leaf_type := tree.get_object_data_type (vndx, key);
   begin
      case field_type is
         when data_not_present =>
            raise ucl_key_not_found with key;
         when data_boolean =>
            declare
               keystring : constant ASU.Unbounded_String := ASU.To_Unbounded_String (key);
               index     : Natural;
            begin
               index := tree.store_objects (vndx).Element (keystring).vector_index;
               return tree.store_booleans.Element (index);
            end;
         when others =>
            raise ucl_type_mismatch with field_type'Img & " found instead of Boolean";
      end case;
   end get_object_value;


   ------------------------
   --  get_object_value  --
   ------------------------
   function get_object_value
     (tree : UclTree;
      vndx : object_index;
      key  : String) return RT.Time_Span
   is
      field_type : constant Leaf_type := tree.get_object_data_type (vndx, key);
   begin
      case field_type is
         when data_not_present =>
            raise ucl_key_not_found with key;
         when data_time =>
            declare
               keystring : constant ASU.Unbounded_String := ASU.To_Unbounded_String (key);
               index     : Natural;
            begin
               index := tree.store_objects (vndx).Element (keystring).vector_index;
               return tree.store_times.Element (index);
            end;
         when others =>
            raise ucl_type_mismatch with field_type'Img & " found instead of Time";
      end case;
   end get_object_value;


   ------------------------
   --  get_object_value  --
   ------------------------
   function get_object_value
     (tree : UclTree;
      vndx : object_index;
      key  : String) return String
   is
      field_type : constant Leaf_type := tree.get_object_data_type (vndx, key);
   begin
      case field_type is
         when data_not_present =>
            raise ucl_key_not_found with key;
         when data_string =>
            declare
               keystring : constant ASU.Unbounded_String := ASU.To_Unbounded_String (key);
               index     : Natural;
            begin
               index := tree.store_objects (vndx).Element (keystring).vector_index;
               return ASU.To_String (tree.store_strings.Element (index).payload);
            end;
         when others =>
            raise ucl_type_mismatch with field_type'Img & " found instead of String";
      end case;
   end get_object_value;

   ------------------------
   --  get_object_array  --
   ------------------------
   function get_object_array
     (tree : UclTree;
      vndx : object_index;
      key  : String) return array_index
   is
      field_type : constant Leaf_type := tree.get_object_data_type (vndx, key);
   begin
      case field_type is
         when data_not_present =>
            raise ucl_key_not_found with key;
         when data_array =>
            return tree.get_object_vector_index (vndx, key);
         when others =>
            raise ucl_type_mismatch with field_type'Img & " found instead of an array";
      end case;
   end get_object_array;


   -------------------------
   --  get_object_object  --
   -------------------------
   function get_object_object
     (tree : UclTree;
      vndx : object_index;
      key  : String) return object_index
   is
      field_type : constant Leaf_type := tree.get_object_data_type (vndx, key);
   begin
      case field_type is
         when data_not_present =>
            raise ucl_key_not_found with key;
         when data_object =>
            return tree.get_object_vector_index (vndx, key);
         when others =>
            raise ucl_type_mismatch with field_type'Img & " found instead of an object";
      end case;
   end get_object_object;


   ----------------------------
   --  get_base_object_keys  --
   ----------------------------
   procedure get_base_object_keys
     (tree : UclTree;
      object_keys : in out jar_string.Vector)
   is
      procedure collect_key (Position : jar_ucl_objects.Cursor);

      tray : DataString;

      procedure collect_key (Position : jar_ucl_objects.Cursor)
      is
         stump_key : ASU.Unbounded_String renames jar_ucl_objects.Key (Position);
      begin
         tray.payload := stump_key;
         object_keys.Append (tray);
      end collect_key;
   begin
      object_keys.Clear;
      tree.tree_stump.Iterate (collect_key'Access);
      string_sorting.Sort (object_keys);
   end get_base_object_keys;


   ------------------------------
   --  get_object_object_keys  --
   ------------------------------
   procedure get_object_object_keys
     (tree : UclTree;
      vndx : object_index;
      object_keys : in out jar_string.Vector)
   is
      procedure collect_key (Position : jar_ucl_objects.Cursor);

      tray : DataString;

      procedure collect_key (Position : jar_ucl_objects.Cursor)
      is
         stump_key : ASU.Unbounded_String renames jar_ucl_objects.Key (Position);
      begin
         tray.payload := stump_key;
         object_keys.Append (tray);
      end collect_key;
   begin
      object_keys.Clear;
      tree.store_objects.Element (vndx).Iterate (collect_key'Access);
      string_sorting.Sort (object_keys);
   end get_object_object_keys;


   -----------------
   --  key_found  --
   -----------------
   function key_found (key_container : jar_string.Vector; key : String) return Boolean
   is
      procedure scan (cursor : jar_string.Cursor);

      found : Boolean := False;

      procedure scan (cursor : jar_string.Cursor) is
      begin
         if ASU.To_String (jar_string.Element (cursor).payload) = key then
            found := True;
         end if;
      end scan;
   begin
      key_container.Iterate (scan'Access);
      return found;
   end key_found;


   ---------
   --  <  --
   ---------
   function "<" (left, right : DataString) return Boolean is
   begin
            return ASU."<" (left.payload, right.payload);
   end "<";


   -------------------------
   --  drop_base_keypair  --
   -------------------------
   procedure drop_base_keypair
     (tree : in out UclTree;
      key  : String)
   is
      keystring : constant ASU.Unbounded_String := ASU.To_Unbounded_String (key);
   begin
      if key_exists (tree, key) then
         tree.tree_stump.Delete (keystring);
      end if;
   end drop_base_keypair;

end ThickUCL;
