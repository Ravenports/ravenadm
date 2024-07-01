--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;
with Ada.Strings.Fixed;
with System;

package body ThickUCL.Files is

   package TIO renames Ada.Text_IO;

   ----------------------
   --  parse_ucl_file  --
   ----------------------
   procedure parse_ucl_file
     (tree : in out UclTree;
      path : String;
      nvpairs : String) is
   begin
      parse_ucl_guts (tree    => tree,
                      isfile  => True,
                      ucldata => path,
                      nvpairs => nvpairs);
   exception
      when ucl_data_unparseable =>
         TIO.Put_Line ("Failed to parse UCL file " & path);
   end parse_ucl_file;


   ------------------------
   --  parse_ucl_string  --
   ------------------------
   procedure parse_ucl_string
     (tree    : in out UclTree;
      ucldata : String;
      nvpairs : String) is
   begin
      parse_ucl_guts (tree    => tree,
                      isfile  => False,
                      ucldata => ucldata,
                      nvpairs => nvpairs);
   exception
      when ucl_data_unparseable =>
         TIO.Put_Line ("Failed to parse UCL data");
   end parse_ucl_string;


   ----------------------
   --  parse_ucl_guts  --
   ----------------------
   procedure parse_ucl_guts
     (tree    : in out UclTree;
      isfile  : Boolean;
      ucldata : String;
      nvpairs : String)
   is
      procedure register_pair (pair : String);

      parser  : Ucl.T_parser;
      obj     : access libucl.ucl_object_t;

      procedure register_pair (pair : String)
      is
         slash : constant Integer := Ada.Strings.Fixed.Index (pair, "=");
      begin
         if slash = 0 then
            TIO.Put_Line ("Will not register '" & pair & "' because '=' is missing");
         else
            declare
               key : constant String := pair (pair'First .. slash - 1);
               val : constant String := pair (slash + 1 .. pair'Last);
            begin
               Ucl.ucl_parser_register_variable (parser, key, val);
            end;
         end if;
      end register_pair;
   begin
      parser := Ucl.ucl_parser_new_nofilevars;
      if nvpairs /= "" then
         declare
            back : Integer;
            front : Integer := nvpairs'First;
         begin
            loop
               back := Ada.Strings.Fixed.Index (Source => nvpairs, Pattern => "|", From => front);
               if back > 0 then
                  register_pair (nvpairs (front .. back - 1));
               else
                  register_pair (nvpairs (front .. nvpairs'Last));
                  exit;
               end if;
               front := back + 1;
            end loop;
         end;
      end if;
      if isfile then
         if not Ucl.ucl_parser_add_file (parser, ucldata) then
            libucl.ucl_parser_free (parser);
            raise ucl_file_unparseable with ucldata;
         end if;
      else
         if not Ucl.ucl_parser_add_chunk (parser, ucldata) then
            libucl.ucl_parser_free (parser);
            raise ucl_data_unparseable;
         end if;
      end if;
      obj := Ucl.ucl_parser_get_object (parser);
      libucl.ucl_parser_free (parser);

      if obj = null then
         --  This shouldn't happen, but add check to avoid possible free-on-null
         return;
      end if;

      if Ucl.type_is_object (obj) then
         populate_the_tree (tree, obj);
      end if;

      libucl.ucl_object_unref (obj);
   end parse_ucl_guts;


   ------------------
   --  extract_key --
   ------------------
   function extract_key (item : access constant libucl.ucl_object_t) return String is
   begin
      return Ucl.ucl_object_key (item);
   end extract_key;


   -------------------------
   --  populate_the_tree  --
   -------------------------
   procedure populate_the_tree
     (tree : in out UclTree;
      rootobj : access constant libucl.ucl_object_t)
   is
      iter : aliased libucl.ucl_object_iter_t := libucl.ucl_object_iter_t (System.Null_Address);
      item : access constant libucl.ucl_object_t;
      item_type : Ucl.intermediate_ucl_type;
   begin
      loop
         item := Ucl.ucl_object_iterate (rootobj, iter'Access, True);
         exit when item = null;

         item_type := Ucl.intermediate_type (item);
         case item_type is
            when Ucl.med_null =>
               TIO.Put_Line ("Unexpected ucl parse error: object type is null, key="
                             & extract_key (item));

            when Ucl.med_userdata =>
               TIO.Put_Line ("Data of type UCL_USERDATA found.  Skipping unsupported type, key="
                             & extract_key (item));

            when Ucl.med_boolean =>
               tree.insert (extract_key (item), Ucl.ucl_object_toboolean (item));
            when Ucl.med_int =>
               tree.insert (extract_key (item), Ucl.ucl_object_toint (item));
            when Ucl.med_float =>
               tree.insert (extract_key (item), Ucl.ucl_object_tofloat (item));
            when Ucl.med_string =>
               tree.insert (extract_key (item), Ucl.ucl_object_tostring_forced (item));
            when Ucl.med_time =>
               tree.insert (extract_key (item), Ucl.ucl_object_totime (item));

            when Ucl.med_object =>
               tree.start_object (extract_key (item));
               populate_the_tree (tree, item);
               tree.close_object;

            when Ucl.med_array =>
               tree.start_array (extract_key (item));
               populate_array (tree, item);
               tree.close_array;
         end case;
      end loop;
   end populate_the_tree;


   -------------------------
   --  populate_the_tree  --
   -------------------------
   procedure populate_array
     (tree : in out UclTree;
      arrayobj :  access constant libucl.ucl_object_t)
   is
      iter : aliased libucl.ucl_object_iter_t := libucl.ucl_object_iter_t (System.Null_Address);
      item : access constant libucl.ucl_object_t;
      item_type : Ucl.intermediate_ucl_type;
   begin
      loop
         item := Ucl.ucl_object_iterate (arrayobj, iter'Access, True);
         exit when item = null;

         item_type := Ucl.intermediate_type (item);
         case item_type is
            when Ucl.med_null =>
               TIO.Put_Line ("Unexpected ucl parse error: array type is null");

            when Ucl.med_userdata =>
               TIO.Put_Line ("Data of type UCL_USERDATA found.  Skipping unsupported type");

            when Ucl.med_boolean =>
               tree.insert ("", Ucl.ucl_object_toboolean (item));
            when Ucl.med_int =>
               tree.insert ("", Ucl.ucl_object_toint (item));
            when Ucl.med_float =>
               tree.insert ("", Ucl.ucl_object_tofloat (item));
            when Ucl.med_string =>
               tree.insert ("", Ucl.ucl_object_tostring_forced (item));
            when Ucl.med_time =>
               tree.insert ("", Ucl.ucl_object_totime (item));

            when Ucl.med_object =>
               tree.start_object ("");
               populate_the_tree (tree, item);
               tree.close_object;

            when Ucl.med_array =>
               tree.start_array ("");
               populate_array (tree, item);
               tree.close_array;
         end case;
      end loop;
   end populate_array;

end ThickUCL.Files;
