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
            when ThickUCL.data_string =>
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
                  case trigger_metadata.get_array_element_type (andx, x) is
                     when ThickUCL.data_string =>
                        declare
                           value : constant String :=
                             trigger_metadata.get_array_element_value (andx, x);
                        begin
                           metatree.insert ("", value);
                        end;
                     when others => null;  --  Skipped by validation, just skip completely
                  end case;
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


   -------------------------
   --  transfer_messages  --
   -------------------------
   procedure transfer_messages
     (message_metadata : ThickUCL.UclTree;
      metatree         : in out ThickUCL.UclTree)
   is
      --  The message_metadata needs to validated before calling this procedure
      procedure transfer_second_string (key : String; sondx : ThickUCL.object_index);

      object_keys   : ThickUCL.jar_string.Vector;
      second_keys   : ThickUCL.jar_string.Vector;
      ondx          : ThickUCL.object_index;

      procedure transfer_second_string (key : String; sondx : ThickUCL.object_index) is
      begin
         case message_metadata.get_object_data_type (sondx, key) is
            when ThickUCL.data_string =>
               declare
                  value : constant String := message_metadata.get_object_value (sondx, key);
               begin
                  metatree.insert (key, value);
               end;
            when others => null;
         end case;
      end transfer_second_string;
   begin
      metatree.start_array ("messages");

      message_metadata.get_base_object_keys (second_keys);
      for x in 0 .. Natural (second_keys.Length) - 1 loop
         declare
            key2 : constant String := HT.USS (second_keys.Element (x).payload);
         begin
            case message_metadata.get_data_type (key2) is
               when ThickUCL.data_object =>
                  ondx := message_metadata.get_index_of_base_ucl_object (key2);
                  message_metadata.get_object_object_keys (ondx, object_keys);
                  metatree.start_object ("");
                  transfer_second_string (KEY_MESSAGE, ondx);
                  transfer_second_string (KEY_TYPE, ondx);
                  transfer_second_string (KEY_MINVER, ondx);
                  transfer_second_string (KEY_MAXVER, ondx);
                  metatree.close_object;
               when others => null;
            end case;
         end;
      end loop;

      metatree.close_array;
   end transfer_messages;


   ----------------------------
   --  valid_trigger_object  --
   ----------------------------
   function valid_trigger_object (trigger_metadata : ThickUCL.UclTree;
                                  ondx : ThickUCL.object_index) return Boolean
   is
      --  At least one between trigger and cleanup (verify string type)
      --  At least one element between the 4 paths (verify array type)
      --  parser prevents duplicates.
      procedure check_key (Position : ThickUCL.jar_string.Cursor);

      valid_trigger : Boolean := True;
      code_found    : Boolean := False;
      path_found    : Boolean := False;
      bad_data_type : Boolean := False;
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
               case trigger_metadata.get_object_data_type (ondx, key) is
                  when ThickUCL.data_string =>
                     code_found := True;
                  when others =>
                     bad_data_type := True;
               end case;
            else
               if not path_found then
                  if key = KEY_DIR_PATH or else
                    key = KEY_FILE_PATH or else
                    key = KEY_FILE_GLOB or else
                    key = KEY_FILE_REGX
                  then
                     case trigger_metadata.get_object_data_type (ondx, key) is
                        when ThickUCL.data_array =>
                           andx := trigger_metadata.get_object_array (ondx, key);
                           num_elements := trigger_metadata.get_number_of_array_elements (andx);
                           if num_elements > 0 then
                              path_found := True;
                           end if;
                        when others =>
                           bad_data_type := True;
                     end case;
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
      return valid_trigger and then code_found and then path_found and then not bad_data_type;
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


   ----------------------------
   --  valid_message_object  --
   ----------------------------
   function valid_message_object
     (message_metadata : ThickUCL.UclTree;
      ondx : ThickUCL.object_index) return Boolean
   is
      --  required: message
      --  required: type
      --  optional if type=upgrade: min_version
      --  optional if type=upgrade: max_version
      --  wrong if type != upgrade: min_version, max_version
      procedure check_key (Position : ThickUCL.jar_string.Cursor);

      object_keys   : ThickUCL.jar_string.Vector;
      valid_message : Boolean := True;
      bad_data_type : Boolean := False;
      bad_type_val  : Boolean := False;
      found_message : Boolean := False;
      found_upgrade : Boolean := False;
      found_install : Boolean := False;
      found_remove  : Boolean := False;
      found_minver  : Boolean := False;
      found_maxver  : Boolean := False;

      procedure check_key (Position : ThickUCL.jar_string.Cursor)
      is
         key : constant String := HT.USS (ThickUCL.jar_string.Element (Position).payload);
      begin
         if key = KEY_MESSAGE or else
           key = KEY_TYPE or else
           key = KEY_MINVER or else
           key = KEY_MAXVER
         then
            case message_metadata.get_object_data_type (ondx, key) is
               when ThickUCL.data_string =>
                  if key = KEY_MESSAGE then
                     found_message := True;
                  elsif key = KEY_TYPE then
                     declare
                        typeval : constant String :=
                          HT.lowercase (message_metadata.get_object_value (ondx, key));
                     begin
                        if typeval = "install" then
                           found_install := True;
                        elsif typeval = "remove" then
                           found_install := True;
                        elsif typeval = "upgrade" then
                           found_upgrade := True;
                        else
                           bad_type_val := True;
                        end if;
                     end;
                  elsif key = KEY_MINVER then
                     found_minver := True;
                  elsif key = KEY_MAXVER then
                     found_maxver := True;
                  end if;
               when others =>
                  bad_data_type := True;
            end case;
         else
            valid_message := False;
         end if;
      end check_key;
   begin
      message_metadata.get_object_object_keys (ondx, object_keys);
      object_keys.Iterate (check_key'Access);
      if bad_data_type or else bad_type_val then
         return False;
      end if;
      if (found_maxver or else found_minver) and then not found_upgrade then
         return False;
      end if;
      if not (found_install or else found_remove or else found_upgrade) then
         return False;
      end if;
      if not found_message then
         return False;
      end if;
      return valid_message;
   end valid_message_object;


   -----------------------------
   --  message_file_is_valid  --
   -----------------------------
   function message_file_is_valid
     (message_metadata : ThickUCL.UclTree) return Boolean
   is
      --  Entry: message_metadata is result of successful parse of UCL file
      --  Expected format: collection of 1 or more message objects

      object_keys   : ThickUCL.jar_string.Vector;
      second_keys   : ThickUCL.jar_string.Vector;
      ondx          : ThickUCL.object_index;
      global_valid  : Boolean := True;
   begin
      message_metadata.get_base_object_keys (second_keys);
      for x in 0 .. Natural (second_keys.Length) - 1 loop
         if global_valid then
            declare
               key2 : constant String := HT.USS (second_keys.Element (x).payload);
            begin
               case message_metadata.get_data_type (key2) is
                  when ThickUCL.data_object =>
                     ondx := message_metadata.get_index_of_base_ucl_object (key2);
                     if not valid_message_object (message_metadata, ondx) then
                        global_valid := False;
                     end if;
                  when others =>
                     global_valid := False;
               end case;
            end;
         end if;
      end loop;
      return global_valid;
   end message_file_is_valid;


end ucl_operations;
