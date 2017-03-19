--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with File_Operations;
with Parameters;
with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Text_IO;

package body PortScan.Packager is

   package FOP renames File_Operations;
   package LAT renames Ada.Characters.Latin_1;
   package DIR renames Ada.Directories;
   package TIO renames Ada.Text_IO;
   package PM  renames Parameters;

   --------------------------------------------------------------------------------------------
   --  exec_phase_package
   --------------------------------------------------------------------------------------------
   function exec_phase_package
     (specification : PSP.Portspecs;
      log_handle    : TIO.File_Type;
      seq_id        : port_id;
      rootdir       : String) return Boolean
   is
      procedure metadata (position : string_crate.Cursor);

      namebase : String := HT.USS (all_ports (seq_id).port_namebase);
      spkgdir : String := rootdir & "/construction/subpackages/";
      wrkdir  : String := rootdir & "/construction/" & namebase;

      procedure metadata (position : string_crate.Cursor)
      is
         type three is range 1 .. 3;
         function convert_prepost (prepost : three) return String;
         function convert_stage   (stage   : three) return String;
         subpackage    : constant String := HT.USS (string_crate.Element (position));
         message_file  : constant String := wrkdir & "/PKG_DISPLAY." & subpackage;
         descript_file : constant String := wrkdir & "/PKG_DESC." & subpackage;
         display  : String := "/+DISPLAY";
         descript : String := "/+DESC";
         manifest : String := "/+MANIFEST";

         function convert_prepost (prepost : three) return String is
         begin
            case prepost is
               when 1 => return "pre-";
               when 2 => return "";
               when 3 => return "post-";
            end case;
         end convert_prepost;

         function convert_stage (stage   : three) return String is
         begin
            case stage is
               when 1 => return "install";
               when 2 => return "upgrade";
               when 3 => return "desinstall";
            end case;
         end convert_stage;
      begin
         DIR.Create_Path (spkgdir & subpackage);
         if DIR.Exists (message_file) then
            DIR.Copy_File (Source_Name => message_file,
                           Target_Name => spkgdir & subpackage & display);
         end if;
         if DIR.Exists (descript_file) then
            DIR.Copy_File (Source_Name => descript_file,
                           Target_Name => spkgdir & subpackage & descript);
         end if;

         for action in three'Range loop
            for psp in three'Range loop
               declare
                  script_file : String := wrkdir & "/pkg-" & convert_prepost (psp) &
                                          convert_stage (action) & "." & subpackage;
                  pkg_script  : String := spkgdir & subpackage & "/pkg-" & convert_prepost (psp) &
                                          convert_stage (action);
               begin
                  if DIR.Exists (script_file) then
                     DIR.Copy_File (Source_Name => script_file,
                                    Target_Name => pkg_script);
                  end if;
               end;
            end loop;
         end loop;

         write_package_manifest (spec       => specification,
                                 subpackage => subpackage,
                                 variant    => HT.USS (all_ports (seq_id).port_variant),
                                 filename   => spkgdir & subpackage & manifest);
      end metadata;
   begin

      all_ports (seq_id).subpackages.Iterate (metadata'Access);

      return False;

   end exec_phase_package;


   --------------------------------------------------------------------------------------------
   --  write_package_manifest
   --------------------------------------------------------------------------------------------
   procedure write_package_manifest
     (spec       : PSP.Portspecs;
      subpackage : String;
      variant    : String;
      filename   : String)
   is
      function quote (thetext : String) return String;
      function get_prefix return String;
      procedure single_if_defined (name, value : String);
      procedure array_if_defined (name, value : String);

      file_handle : TIO.File_Type;

      function quote (thetext : String) return String is
      begin
         return LAT.Quotation & thetext & LAT.Quotation;
      end quote;

      function get_prefix return String
      is
         prefix : String := spec.get_field_value (PSP.sp_prefix);
      begin
         if prefix = "" then
            return HT.USS (PM.configuration.dir_localbase);
         else
            return prefix;
         end if;
      end get_prefix;

      procedure single_if_defined (name, value : String) is
      begin
         if value /= "" then
            TIO.Put_Line (file_handle, name & ": " & quote (value));
         end if;
      end single_if_defined;

      procedure array_if_defined (name, value : String)
      is
         --  No identified need for quoting
      begin
         if value /= "" then
            TIO.Put_Line (file_handle, name & ": [ " & value & " ]");
         end if;
      end array_if_defined;

      name    : String := quote (spec.get_namebase) & "-" & subpackage & "-" & variant;
      origin  : String := quote (spec.get_namebase) & ":" & subpackage & ":" & variant;

   begin
      TIO.Create (File => file_handle,
                  Mode => TIO.Out_File,
                  Name => filename);
      TIO.Put_Line
        (file_handle,
         LAT.Left_Curly_Bracket & LAT.LF &
           "name: " & name & LAT.LF &
           "version: " & origin & LAT.LF &
           "comment: <<EOD" & LAT.LF &
           spec.get_tagline (variant) & LAT.LF &
           "EOD" & LAT.LF &
           "maintainer: " & quote (spec.get_field_value (PSP.sp_contacts)) & LAT.LF &
           "prefix: " & quote (get_prefix) & LAT.LF &
           "categories: [ " & spec.get_field_value (PSP.sp_keywords) & " ]" & LAT.LF &
           "licenselogic: " & quote ("single")  -- TODO: IMPLEMENT
        );
      single_if_defined ("www",      spec.get_field_value (PSP.sp_homepage));
      array_if_defined  ("licenses", spec.get_field_value (PSP.sp_licenses));
      array_if_defined  ("users",    spec.get_field_value (PSP.sp_users));
      array_if_defined  ("groups",   spec.get_field_value (PSP.sp_groups));
      TIO.Put_Line (file_handle, "deps: {");
      --  TODO: dependencies
      TIO.Put_Line (file_handle, "}");
      TIO.Put_Line (file_handle, "options: {"  & spec.get_field_value (PSP.sp_opt_helper) & " }");
      TIO.Close (file_handle);

   exception
      when others =>
         if TIO.Is_Open (file_handle) then
            TIO.Close (file_handle);
         end if;
   end write_package_manifest;


   --------------------------------------------------------------------------------------------
   --  exec_phase_check_plist
   --------------------------------------------------------------------------------------------
   function exec_check_plist
     (log_handle : TIO.File_Type;
      seq_id     : port_id) return Boolean is
   begin
      return True;
   end exec_check_plist;

end PortScan.Packager;
