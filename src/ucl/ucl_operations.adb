--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: /License.txt

with HelperText;

package body ucl_operations is

   package HT renames HelperText;

   -------------------------
   --  transfer_triggers  --
   -------------------------
   procedure transfer_triggers
     (trigger_metadata : ThickUCL.UclTree;
      metatree         : in out ThickUCL.UclTree)
   is
      --  The trigger_metadata needs to validated before calling this procedure
      procedure transfer_second_string (key : String; sondx : ThickUCL.object_index);
      procedure transfer_second_array (key : String; sondx : ThickUCL.object_index);

      object_keys   : ThickUCL.jar_string.Vector;
      second_keys   : ThickUCL.jar_string.Vector;
      ondx          : ThickUCL.object_index;

      procedure transfer_second_string (key : String; sondx : ThickUCL.object_index) is
      begin
         case trigger_metadata.get_object_data_type (sondx, key) is
            when ThickUCL.data_object =>
               declare
                  value : constant String := trigger_metadata.get_object_value (sondx, key);
               begin
                  metatree.insert (key, value);
               end;
            when others => null;
         end case;
      end transfer_second_string;

      procedure transfer_second_array (key : String; sondx : ThickUCL.object_index)
      is
         andx : ThickUCL.array_index;
         num_elements : Natural;
      begin
         case trigger_metadata.get_object_data_type (sondx, key) is
            when ThickUCL.data_array =>
               andx := trigger_metadata.get_object_array (sondx, key);
               num_elements := trigger_metadata.get_number_of_array_elements (andx);
               metatree.start_array (key);
               for x in 0 .. num_elements - 1 loop
                  declare
                     value : constant String :=
                       trigger_metadata.get_array_element_value (andx, x);
                  begin
                     metatree.insert ("", value);
                  end;
               end loop;
            when others => null;
         end case;
         metatree.close_array;
      end transfer_second_array;

   begin
      metatree.start_array ("triggers");

      trigger_metadata.get_base_object_keys (second_keys);
      for x in 0 .. Natural (second_keys.Length) - 1 loop
         declare
            key2 : constant String := HT.USS (second_keys.Element (x).payload);
         begin
            case trigger_metadata.get_data_type (key2) is
               when ThickUCL.data_object =>
                  ondx := trigger_metadata.get_index_of_base_ucl_object (key2);
                  trigger_metadata.get_object_object_keys (ondx, object_keys);
                  metatree.start_object ("");
                  transfer_second_array (KEY_DIR_PATH, ondx);
                  transfer_second_array (KEY_FILE_PATH, ondx);
                  transfer_second_array (KEY_FILE_GLOB, ondx);
                  transfer_second_array (KEY_FILE_REGX, ondx);
                  transfer_second_string (KEY_CLEANUP, ondx);
                  transfer_second_string (KEY_TRIGGER, ondx);
                  metatree.close_object;
               when others => null;
            end case;
         end;
      end loop;

      metatree.close_array;
   end transfer_triggers;


   ----------------------------
   --  valid_trigger_object  --
   ----------------------------
   function valid_trigger_object (trigger_metadata : ThickUCL.UclTree;
                                  ondx : ThickUCL.object_index) return Boolean
   is
      --  At least one between trigger and cleanup
      --  At least one element between the 4 paths
      procedure check_key (Position : ThickUCL.jar_string.Cursor);

      valid_trigger : Boolean := True;
      code_found    : Boolean := False;
      path_found    : Boolean := False;
      object_keys   : ThickUCL.jar_string.Vector;

      procedure check_key (Position : ThickUCL.jar_string.Cursor)
      is
         key  : constant String := HT.USS (ThickUCL.jar_string.Element (Position).payload);
         andx : ThickUCL.array_index;
         num_elements : Natural;
      begin
         if key = KEY_DIR_PATH or else
           key = KEY_FILE_PATH or else
           key = KEY_FILE_GLOB or else
           key = KEY_FILE_REGX or else
           key = KEY_TRIGGER or else
           key = KEY_CLEANUP
         then
            if key = KEY_TRIGGER or else key = KEY_CLEANUP then
               code_found := True;
            else
               if not path_found then
                  if key = KEY_DIR_PATH or else
                    key = KEY_FILE_PATH or else
                    key = KEY_FILE_GLOB or else
                    key = KEY_FILE_REGX
                  then
                     andx := trigger_metadata.get_object_array (ondx, key);
                     num_elements := trigger_metadata.get_number_of_array_elements (andx);
                     if num_elements > 0 then
                        path_found := True;
                     end if;
                  end if;
               end if;
            end if;
         else
            valid_trigger := False;
         end if;
      end check_key;

   begin
      trigger_metadata.get_object_object_keys (ondx, object_keys);
      object_keys.Iterate (check_key'Access);
      return valid_trigger and then code_found and then path_found;
   end valid_trigger_object;


   -----------------------------
   --  trigger_file_is_valid  --
   -----------------------------
   function trigger_file_is_valid (trigger_metadata : ThickUCL.UclTree) return Boolean
   is
      --  Entry: trigger_metadata is result of successful parse of UCL file
      --  Expected format: collection of 1 or more trigger objects

      object_keys   : ThickUCL.jar_string.Vector;
      second_keys   : ThickUCL.jar_string.Vector;
      ondx          : ThickUCL.object_index;
      global_valid  : Boolean := True;
   begin
      trigger_metadata.get_base_object_keys (second_keys);
      for x in 0 .. Natural (second_keys.Length) - 1 loop
         if global_valid then
            declare
               key2 : constant String := HT.USS (second_keys.Element (x).payload);
            begin
               case trigger_metadata.get_data_type (key2) is
                  when ThickUCL.data_object =>
                     ondx := trigger_metadata.get_index_of_base_ucl_object (key2);
                     if not valid_trigger_object (trigger_metadata, ondx) then
                        global_valid := False;
                     end if;
                  when others =>
                     global_valid := False;
               end case;
            end;
         end if;
      end loop;
      return global_valid;
   end trigger_file_is_valid;

end ucl_operations;
