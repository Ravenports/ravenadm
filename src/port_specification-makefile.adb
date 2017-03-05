--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Text_IO;
with Ada.Characters.Latin_1;

package body Port_Specification.Makefile is

   package TIO renames Ada.Text_IO;
   package LAT renames Ada.Characters.Latin_1;

   --------------------------------------------------------------------------------------------
   --  generator
   --------------------------------------------------------------------------------------------
   procedure generator
     (specs         : Portspecs;
      variant       : String;
      opsys         : supported_opsys;
      arch_standard : supported_arch;
      osrelease     : String;
      osmajor       : String;
      osversion     : String;
      option_string : String;
      output_file   : String
     )
   is
      procedure send (data : String; use_put : Boolean := False);
      procedure send (varname, value : String);
      procedure send (varname : String; value : HT.Text);
      procedure send (varname : String; crate : string_crate.Vector; flavor : Positive := 1);
      procedure send (varname : String; crate : list_crate.Map; flavor : Positive := 1);
      procedure send (varname : String; value : Boolean; dummy : Boolean);
      procedure send (varname : String; value, default : Integer);
      procedure print_item (position : string_crate.Cursor);
      procedure dump_list (position : list_crate.Cursor);
      procedure dump_distfiles (position : string_crate.Cursor);
      procedure dump_ext_zip   (position : string_crate.Cursor);
      procedure dump_ext_7z    (position : string_crate.Cursor);
      procedure dump_ext_lha   (position : string_crate.Cursor);
      procedure dump_extract_head_tail (position : list_crate.Cursor);

      write_to_file   : constant Boolean := (output_file /= "");
      makefile_handle : TIO.File_Type;
      varname_prefix  : HT.Text;

      procedure send (data : String;  use_put : Boolean := False) is
      begin
         if write_to_file then
            if use_put then
               TIO.Put (makefile_handle, data);
            else
               TIO.Put_Line (makefile_handle, data);
            end if;
         else
            if use_put then
               TIO.Put (data);
            else
               TIO.Put_Line (data);
            end if;
         end if;
      end send;

      procedure send (varname, value : String) is
      begin
         send (varname & LAT.Equals_Sign & value);
      end send;

      procedure send (varname : String; value : HT.Text) is
      begin
         if not HT.IsBlank (value) then
            send (varname & LAT.Equals_Sign & HT.USS (value));
         end if;
      end send;

      procedure send (varname : String; value : Boolean; dummy : Boolean) is
      begin
         if value then
            send (varname & LAT.Equals_Sign & "yes");
         end if;
      end send;

      procedure send (varname : String; value, default : Integer) is
      begin
         if value /= default then
            send (varname & LAT.Equals_Sign & HT.int2str (value));
         end if;
      end send;

      procedure send (varname : String; crate : string_crate.Vector; flavor : Positive := 1) is
      begin
         if crate.Is_Empty then
            return;
         end if;
         case flavor is
            when 1 =>
               send (varname & "=", True);
               crate.Iterate (Process => print_item'Access);
               send ("");
            when 2 =>
               varname_prefix := HT.SUS (varname);
               crate.Iterate (Process => dump_distfiles'Access);
            when 3 =>
               crate.Iterate (Process => dump_ext_zip'Access);
            when 4 =>
               crate.Iterate (Process => dump_ext_7z'Access);
            when 5 =>
               crate.Iterate (Process => dump_ext_lha'Access);
            when others =>
               null;
         end case;
      end send;

      procedure send (varname : String; crate : list_crate.Map; flavor : Positive := 1) is
      begin
         varname_prefix := HT.SUS (varname);
         case flavor is
            when 1 => crate.Iterate (Process => dump_list'Access);
            when 6 => crate.Iterate (Process => dump_extract_head_tail'Access);
            when others => null;
         end case;
      end send;

      procedure dump_list (position : list_crate.Cursor)
      is
         NDX : String := HT.USS (varname_prefix)  & "_" &
                         HT.USS (list_crate.Element (position).group) & LAT.Equals_Sign;
      begin
         send (NDX, True);
         list_crate.Element (position).list.Iterate (Process => print_item'Access);
         send ("");
      end dump_list;

      procedure dump_extract_head_tail (position : list_crate.Cursor)
      is
         NDX : String := HT.USS (varname_prefix)  & "_" &
                         HT.USS (list_crate.Element (position).group) & LAT.Equals_Sign;
      begin
         if not list_crate.Element (position).list.Is_Empty then
            send (NDX, True);
            list_crate.Element (position).list.Iterate (Process => print_item'Access);
            send ("");
         end if;
      end dump_extract_head_tail;

      procedure print_item (position : string_crate.Cursor)
      is
         index : Natural := string_crate.To_Index (position);
      begin
         if index > 1 then
            send (" ", True);
         end if;
         send (HT.USS (string_crate.Element (position)), True);
      end print_item;

      procedure dump_distfiles (position : string_crate.Cursor)
      is
         index : Natural := string_crate.To_Index (position);
         NDX   : String  := HT.USS (varname_prefix)  & "_" & HT.int2str (index) & LAT.Equals_Sign;
      begin
         send (NDX & HT.USS (string_crate.Element (position)));
      end dump_distfiles;

      procedure dump_ext_zip (position : string_crate.Cursor)
      is
         N : String := HT.USS (string_crate.Element (position));
      begin
         send ("EXTRACT_HEAD_" & N & "=/usr/bin/unzip -qo");
         send ("EXTRACT_TAIL_" & N & "=-d ${EXTRACT_WRKDIR_" & N & "}");
      end dump_ext_zip;

      procedure dump_ext_7z (position : string_crate.Cursor)
      is
         N : String := HT.USS (string_crate.Element (position));
      begin
         send ("EXTRACT_HEAD_" & N & "=7z x -bd -y -o${EXTRACT_WRKDIR_" & N & "} >/dev/null");
         send ("EXTRACT_TAIL_" & N & "=# empty");
      end dump_ext_7z;

      procedure dump_ext_lha (position : string_crate.Cursor)
      is
         N : String := HT.USS (string_crate.Element (position));
      begin
         send ("EXTRACT_HEAD_" & N & "=lha xfpw=${EXTRACT_WRKDIR_" & N & "}");
         send ("EXTRACT_TAIL_" & N & "=# empty");
      end dump_ext_lha;

   begin
      if not specs.variant_exists (variant) then
         TIO.Put_Line ("Error : Variant '" & variant & "' does not exist!");
         return;
      end if;

      if write_to_file then
        TIO.Create (File => makefile_handle,
                    Mode => TIO.Out_File,
                    Name => output_file);
      end if;

      --  TODO: Check these to see if they are actually used.
      --  The pkg manifests did use many of these, but they will be generated by ravenadm

      send ("# Makefile has been autogenerated by ravenadm tool" & LAT.LF);

      send ("NAMEBASE",         HT.USS (specs.namebase));
      send ("VERSION",          HT.USS (specs.version));
      send ("REVISION",         specs.revision, 0);
      send ("EPOCH",            specs.epoch, 0);
      send ("KEYWORDS",         specs.keywords);    --  probably remove
      send ("VARIANT",          variant);
      send ("DL_SITES",         specs.dl_sites);
      send ("DISTFILE",         specs.distfiles, 2);
      send ("DIST_SUBDIR",      specs.dist_subdir);
      send ("DISTNAME",         specs.distname);
      send ("DF_INDEX",         specs.df_index);
      send ("EXTRACT_ONLY",     specs.extract_only);
      send ("ZIP-EXTRACT",      specs.extract_zip, 3);
      send ("7Z-EXTRACT",       specs.extract_7z, 4);
      send ("LHA-EXTRACT",      specs.extract_lha, 5);
      send ("EXTRACT_HEAD",     specs.extract_head, 6);
      send ("EXTRACT_TAIL",     specs.extract_tail, 6);
      send ("NO_BUILD",         specs.skip_build, True);
      send ("SINGLE_JOB",       specs.single_job, True);
      send ("DESTDIR_VIA_ENV",  specs.destdir_env, True);
      send ("BUILD_WRKSRC",     specs.build_wrksrc);
      send ("MAKEFILE",         specs.makefile);
      send ("MAKE_ENV",         specs.make_env);
      send ("MAKE_ARGS",        specs.make_args);
      send ("CFLAGS",           specs.cflags);

      --  TODO: This is not correct, placeholder.  rethink.
      send (".include " & LAT.Quotation & "/usr/raven/share/mk/raven.mk" & LAT.Quotation);

      if write_to_file then
         TIO.Close (makefile_handle);
      end if;
   exception
      when others =>
         if TIO.Is_Open (makefile_handle) then
            TIO.Close (makefile_handle);
         end if;
   end generator;


end Port_Specification.Makefile;
