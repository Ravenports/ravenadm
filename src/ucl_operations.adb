--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: /License.txt

with Ada.Text_IO;
with Ada.Directories;
with Ada.Characters.Latin_1;
with Ada.Containers.Vectors;
with ThickUCL.Files;
with HelperText;

package body UCL_Operations is

   package HT  renames HelperText;
   package LAT renames Ada.Characters.Latin_1;
   package DIR renames Ada.Directories;
   package CON renames Ada.Containers;
   package TIO renames Ada.Text_IO;

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
               metatree.close_array;
            when others => null;
         end case;
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


   ------------------------
   --  transfer_scripts  --
   ------------------------
   procedure transfer_scripts
     (script_metadata  : ThickUCL.UclTree;
      metatree         : in out ThickUCL.UclTree)
   is
      --  The script_metadata needs to validated before calling this procedure
      procedure transfer_second_string (key : String; sondx : ThickUCL.object_index);

      second_keys : ThickUCL.jar_string.Vector;

      procedure transfer_second_string (key : String; sondx : ThickUCL.object_index) is
      begin
         case script_metadata.get_object_data_type (sondx, key) is
            when ThickUCL.data_string =>
               declare
                  value : constant String := script_metadata.get_object_value (sondx, key);
               begin
                  metatree.insert (key, value);
               end;
            when others => null;
         end case;
      end transfer_second_string;
   begin
      metatree.start_object ("scripts");

      script_metadata.get_base_object_keys (second_keys);
      for x in 0 .. Natural (second_keys.Length) - 1 loop
         declare
            key2 : constant String := HT.USS (second_keys.Element (x).payload);
            andx : ThickUCL.array_index;
            ondx : ThickUCL.object_index;
            num_scripts : Natural;
         begin
            metatree.start_array (key2);
            case script_metadata.get_data_type (key2) is
               when ThickUCL.data_array =>
                  andx := script_metadata.get_index_of_base_array (key2);
                  num_scripts := script_metadata.get_number_of_array_elements (andx);
                  for y in 0 .. num_scripts - 1 loop
                     ondx := script_metadata.get_array_element_object (andx, y);
                     metatree.start_object ("");
                     transfer_second_string (KEY_ARGS, ondx);
                     transfer_second_string (KEY_CODE, ondx);
                     metatree.close_object;
                  end loop;
               when others => null;
            end case;
            metatree.close_array;
         end;
      end loop;
      metatree.close_object;
   end transfer_scripts;


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
   function message_file_is_valid (message_metadata : ThickUCL.UclTree) return Boolean
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


   ----------------------------
   --  script_file_is_valid  --
   ----------------------------
   function script_file_is_valid (script_metadata : ThickUCL.UclTree) return Boolean
   is
      --  Entry: script_metadata is result of successful parse of UCL file
      --  Expected format: object of 1 or more named arrays of objects
      --  The arrays cannot be empty

      function phase_key_is_valid (phase_key : String) return Boolean;

      second_keys   : ThickUCL.jar_string.Vector;
      global_valid  : Boolean := True;

      function phase_key_is_valid (phase_key : String) return Boolean
      is
         found : Boolean := False;
      begin
         if phase_key = "pre-install" or else
           phase_key = "post-install" or else
           phase_key = "pre-deinstall" or else
           phase_key = "post-deinstall" or else
           phase_key = "pre-install-lua" or else
           phase_key = "post-install-lua" or else
           phase_key = "pre-deinstall-lua" or else
           phase_key = "post-deinstall-lua"
         then
            found := True;
         end if;
         return found;
      end phase_key_is_valid;
   begin
      script_metadata.get_base_object_keys (second_keys);
      for x in 0 .. Natural (second_keys.Length) - 1 loop
         if global_valid then
            declare
               key2 : constant String := HT.USS (second_keys.Element (x).payload);
               andx : ThickUCL.array_index;
               ondx : ThickUCL.object_index;
               num_scripts : Natural;
            begin
               if phase_key_is_valid (key2) then
                  case script_metadata.get_data_type (key2) is
                     when ThickUCL.data_array =>
                        andx := script_metadata.get_index_of_base_array (key2);
                        num_scripts := script_metadata.get_number_of_array_elements (andx);
                        if num_scripts > 0 then
                           for y in 0 .. num_scripts - 1 loop
                              case script_metadata.get_array_element_type (andx, y) is
                                 when ThickUCL.data_object =>
                                    ondx := script_metadata.get_array_element_object (andx, y);
                                    if not valid_script_object (script_metadata, ondx) then
                                       global_valid := False;
                                    end if;
                                 when others =>
                                    global_valid := False;
                              end case;
                           end loop;
                        else
                           global_valid := False;
                        end if;
                     when others =>
                        global_valid := False;
                  end case;
               else
                  global_valid := False;
               end if;
            end;
         end if;
      end loop;
      return global_valid;
   end script_file_is_valid;


   ---------------------------
   --  valid_script_object  --
   ---------------------------
   function valid_script_object
     (script_metadata : ThickUCL.UclTree;
      ondx : ThickUCL.object_index) return Boolean
   is
      procedure check_key (Position : ThickUCL.jar_string.Cursor);

      --  required: code
      --  required: args

      object_keys   : ThickUCL.jar_string.Vector;
      valid_script  : Boolean := True;
      found_code    : Boolean := False;
      found_args    : Boolean := False;
      bad_data_type : Boolean := False;

      procedure check_key (Position : ThickUCL.jar_string.Cursor)
      is
         key : constant String := HT.USS (ThickUCL.jar_string.Element (Position).payload);
      begin
         if key = KEY_CODE or else
           key = KEY_ARGS
         then
            case script_metadata.get_object_data_type (ondx, key) is
               when ThickUCL.data_string =>
                  if key = KEY_CODE then
                     found_code := True;
                  elsif key = KEY_ARGS then
                     found_args := True;
                  end if;
               when others =>
                  bad_data_type := True;
            end case;
         else
            valid_script := False;
         end if;
      end check_key;
   begin
      script_metadata.get_object_object_keys (ondx, object_keys);
      object_keys.Iterate (check_key'Access);
      if bad_data_type or else not found_code or else not found_args then
         return False;
      end if;
      return valid_script;
   end valid_script_object;


   ----------------------------
   --  port_ucl_files_valid  --
   ----------------------------
   function port_ucl_files_valid (ravensrcdir : String) return Boolean
   is
      package file_set is new CON.Vectors
        (Element_Type => HT.Text,
         Index_Type   => Natural,
         "="          => HT.SU."=");

      procedure gather_ucl_files (pattern : String);
      procedure check_triggers (Position : file_set.Cursor);
      procedure check_messages (Position : file_set.Cursor);
      procedure check_scripts  (Position : file_set.Cursor);

      ucl_files : file_set.Vector;
      all_valid : Boolean := True;
      baducl    : constant String := "Invalid UCL format: files/";
      filesdir  : constant String := ravensrcdir & "/files";
      filter    : constant DIR.Filter_Type := (DIR.Directory     => False,
                                               DIR.Ordinary_File => True,
                                               DIR.Special_File  => False);

      procedure gather_ucl_files (pattern : String)
      is
         Search  : DIR.Search_Type;
         Dir_Ent : DIR.Directory_Entry_Type;
      begin
         ucl_files.Clear;
         DIR.Start_Search (Search    => Search,
                           Directory => filesdir,
                           Pattern   => pattern,
                           Filter    => filter);
         while DIR.More_Entries (Search => Search) loop
            DIR.Get_Next_Entry (Search => Search, Directory_Entry => Dir_Ent);
            ucl_files.Append (HT.SUS (DIR.Simple_Name (Dir_Ent)));
         end loop;
         DIR.End_Search (Search);
      end gather_ucl_files;

      procedure check_triggers (Position : file_set.Cursor)
      is
         full_path : constant String :=
           ravensrcdir & "/files/" & HT.USS (file_set.Element (Position));
         trigger_metadata : ThickUCL.UclTree;
      begin
         if HT.trails (full_path, ".ucl") or else HT.trails (full_path, ".ucl.in") then
            begin
               ThickUCL.Files.parse_ucl_file (trigger_metadata, full_path, "");
               if not trigger_file_is_valid (trigger_metadata) then
                  TIO.Put_Line ("Invalid scheme for trigger: files/" &
                                  HT.USS (file_set.Element (Position)));
                  all_valid := False;
               end if;
            exception
               when ThickUCL.Files.ucl_file_unparseable =>
                  TIO.Put_Line (baducl &  HT.USS (file_set.Element (Position)));
                  all_valid := False;
            end;
         end if;
      end check_triggers;

      procedure check_messages (Position : file_set.Cursor)
      is
         full_path : constant String :=
           ravensrcdir & "/files/" & HT.USS (file_set.Element (Position));
         message_metadata : ThickUCL.UclTree;
      begin
         if HT.trails (full_path, ".ucl") or else HT.trails (full_path, ".ucl.in") then
            begin
               ThickUCL.Files.parse_ucl_file (message_metadata, full_path, "");
               if not message_file_is_valid (message_metadata) then
                  TIO.Put_Line ("Invalid scheme for message: files/" &
                                  HT.USS (file_set.Element (Position)));
                  all_valid := False;
               end if;
            exception
               when ThickUCL.Files.ucl_file_unparseable =>
                  TIO.Put_Line (baducl &  HT.USS (file_set.Element (Position)));
                  all_valid := False;
            end;
         end if;
      end check_messages;

      procedure check_scripts  (Position : file_set.Cursor)
      is
         full_path : constant String :=
           ravensrcdir & "/files/" & HT.USS (file_set.Element (Position));
         script_metadata : ThickUCL.UclTree;
      begin
         if HT.trails (full_path, ".ucl") or else HT.trails (full_path, ".ucl.in") then
            begin
               ThickUCL.Files.parse_ucl_file (script_metadata, full_path, "");
               if not script_file_is_valid (script_metadata) then
                  TIO.Put_Line ("Invalid scheme for script: files/" &
                                  HT.USS (file_set.Element (Position)));
                  all_valid := False;
               end if;
            exception
               when ThickUCL.Files.ucl_file_unparseable =>
                  TIO.Put_Line (baducl &  HT.USS (file_set.Element (Position)));
                  all_valid := False;
            end;
         end if;
      end check_scripts;
   begin
      if not DIR.Exists (filesdir) then
         return True;
      end if;
      case DIR.Kind (filesdir) is
         when DIR.Directory => null;
         when others =>
            TIO.Put_Line ("Fatal: " & ravensrcdir & " is not a directory.");
            return False;
      end case;

      gather_ucl_files ("triggers-*");
      ucl_files.Iterate (check_triggers'Access);

      gather_ucl_files ("messages-*");
      ucl_files.Iterate (check_messages'Access);

      gather_ucl_files ("scripts-*");
      ucl_files.Iterate (check_scripts'Access);

      return all_valid;

   end port_ucl_files_valid;


   -------------------
   --  extract_ADO  --
   -------------------
   procedure extract_ADO
     (metadata_string : String;
      metadata        : in out ADO_Data)
   is
      tree : ThickUCL.UclTree;
      KEY_ABI : constant String := "abi";
      KEY_DEP : constant String := "deps";
      KEY_OPT : constant String := "options";
      ondx    : ThickUCL.object_index;
   begin
      metadata.abi := HelperText.blank;
      metadata.dependencies.Clear;
      metadata.options.Clear;
      ThickUCL.Files.parse_ucl_string (tree, metadata_string, "");

      if tree.key_exists (KEY_ABI) then
         declare
            abi : constant String := tree.get_base_value (KEY_ABI);
         begin
            metadata.abi := HT.SUS (abi);
         end;
      end if;

      if tree.key_exists (KEY_DEP) then
         case tree.get_data_type (KEY_DEP) is
            when ThickUCL.data_object =>
               declare
                  depends : ThickUCL.jar_string.Vector;

                  procedure add_dep (Position : ThickUCL.jar_string.Cursor)
                  is
                     nsv : HT.Text renames ThickUCL.jar_string.Element (Position).payload;
                     vsn : constant String := tree.get_object_value (ondx, HT.USS (nsv));
                  begin
                     metadata.dependencies.Append (HT.SUS (HT.USS (nsv) & LAT.Tilde & vsn));
                  end add_dep;
               begin
                  ondx := tree.get_index_of_base_ucl_object (KEY_DEP);
                  tree.get_object_object_keys (ondx, depends);
                  depends.Iterate (add_dep'Access);
               end;
            when others => null;
         end case;
      end if;

      if tree.key_exists (KEY_OPT) then
         case tree.get_data_type (KEY_OPT) is
            when ThickUCL.data_object =>
               declare
                  options : ThickUCL.jar_string.Vector;

                  procedure add_option (Position : ThickUCL.jar_string.Cursor)
                  is
                     name : HT.Text renames ThickUCL.jar_string.Element (Position).payload;
                     setting : constant Boolean := tree.get_object_value (ondx, HT.USS (name));
                  begin
                     if setting then
                        metadata.options.Append (HT.SUS (HT.USS (name) & " => true"));
                     else
                        metadata.options.Append (HT.SUS (HT.USS (name) & " => false"));
                     end if;
                  end add_option;
               begin
                  ondx := tree.get_index_of_base_ucl_object (KEY_OPT);
                  tree.get_object_object_keys (ondx, options);
                  options.Iterate (add_option'Access);
               end;
            when others => null;
         end case;
      end if;

   exception
      when ThickUCL.Files.ucl_data_unparseable =>
         null;  --  silently fail
   end extract_ADO;

end UCL_Operations;
