--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Archive.Dirent.Scan;
with Archive.Unix;
with Parameters;

package body hierarchy is

   package SCN renames Archive.Dirent.Scan;
   package PM  renames Parameters;


   ---------------------
   --  take_snapshot  --
   ---------------------
   procedure take_snapshot
     (DC        : in out Dirent_Collection.Map;
      rootdir   : String;
      skip_dirs : admtypes.string_crate.Vector)
   is
      procedure dive (this_directory : String);
      procedure dive (this_directory : String)
      is
         procedure analyze_entity (Position : SCN.dscan_crate.Cursor);

         this_level : SCN.dscan_crate.Vector;

         procedure analyze_entity (Position : SCN.dscan_crate.Cursor)
         is
            entname  : constant String := SCN.dscan_crate.Element (Position).simple_name;
            relpath  : constant String := this_directory & entname;
            features : Archive.Unix.File_Characteristics;
            myrec    : direntrec;
         begin
            if skip_dirs.Contains (HT.SUS (relpath)) then
               return;
            end if;
            features := Archive.Unix.get_charactistics (rootdir & relpath);
            myrec.gid    := features.gid;
            myrec.uid    := features.uid;
            myrec.perms  := features.perms;
            myrec.ftype  := features.ftype;
            myrec.digest := Blake_3.file_digest (rootdir & relpath);
            DC.Insert (HT.SUS (relpath), myrec);
            case features.ftype is
               when Archive.directory =>
                  dive (relpath & "/");
               when others => null;
            end case;
         end analyze_entity;
      begin
         SCN.scan_directory (rootdir & this_directory, this_level);
         this_level.Iterate (analyze_entity'Access);
      end dive;
   begin
      dive ("/");
   end take_snapshot;


   -------------------
   --  check_again  --
   -------------------
   procedure check_again
     (DC        : in out Dirent_Collection.Map;
      rootdir   : String;
      skip_dirs : admtypes.string_crate.Vector;
      extras    : in out admtypes.string_crate.Vector;
      modified  : in out admtypes.string_crate.Vector)
   is
      procedure set_second (Key : HT.Text; Element : in out direntrec);
      procedure dive (this_directory : String);

      procedure set_second (Key : HT.Text; Element : in out direntrec) is
      begin
         Element.second := True;
      end set_second;

      procedure dive (this_directory : String)
      is
         procedure analyze_entity (Position : SCN.dscan_crate.Cursor);

         this_level : SCN.dscan_crate.Vector;

         procedure analyze_entity (Position : SCN.dscan_crate.Cursor)
         is
            entname  : constant String := SCN.dscan_crate.Element (Position).simple_name;
            relpath  : constant String := this_directory & entname;
            entkey   : constant HT.Text := HT.SUS (relpath);
            features : Archive.Unix.File_Characteristics;
            myrec    : direntrec;
            digest   : Blake_3.blake3_hash;

            use type Archive.owngrp_id;
            use type Archive.file_type;
            use type Archive.permissions;
         begin
            if skip_dirs.Contains (HT.SUS (relpath)) then
               return;
            end if;
            if DC.Contains (entkey) then
               digest := Blake_3.file_digest (rootdir & relpath);
               features := Archive.Unix.get_charactistics (rootdir & relpath);
               if myrec.gid = features.gid and then
                 myrec.uid = features.uid and then
                 myrec.ftype = features.ftype and then
                 myrec.perms = features.perms and then
                 myrec.digest = digest
               then
                  null;
               else
                  modified.Append (entkey);
               end if;
               DC.Update_Element (DC.Find (entkey), set_second'Access);
            else
               extras.Append (entkey);
            end if;

            case features.ftype is
               when Archive.directory =>
                  dive (relpath & "/");
               when others => null;
            end case;
         end analyze_entity;
      begin
         SCN.scan_directory (rootdir & this_directory, this_level);
         this_level.Iterate (analyze_entity'Access);
      end dive;
   begin
      dive ("/");
   end check_again;


   -----------------------
   --  set_file_filter  --
   -----------------------
   procedure set_file_filter (skip_dirs : in out admtypes.string_crate.Vector)
   is
      procedure push (file_or_directory : String);

      localbase  : constant String := HT.USS (PM.configuration.dir_localbase);

      procedure push (file_or_directory : String) is
      begin
         skip_dirs.Append (HT.SUS (file_or_directory));
      end push;
   begin
      --  share/xml/catalog.ports
      --  # xmlcatmgr is constantly updating catalog.ports, ignore
      push (localbase & "/share/xml/catalog.ports");

      --  lib/gio/modules/giomodule.cache
      --  # gio modules cache could be modified for any gio modules
      push (localbase & "/lib/gio/modules/giomodule.cache");

      push ("/bin");
      push ("/ccache");
      push ("/construction");
      push ("/dev");
      push ("/distfiles");
      push ("/packages");    --  do we still need this?
      push ("/tmp");
      push ("/repo");
      push ("/proc");
      push ("/home");
      push ("/root");

   end set_file_filter;


   ------------------------
   --  ignore_this_file  --
   ------------------------
   function ignore_this_file (filename : HT.Text) return Boolean
   is
      line      : constant String := HT.USS (filename);
      localbase : constant String := HT.USS (PM.configuration.dir_localbase);
   begin
      --  */ls-R
      --  # ls-R files from texmf are often regenerated
      if HT.leads (line, localbase & "/") then
         if HT.trails (line, "/ls-R") then
            return True;
         end if;
      end if;

      --  etc/gconf/gconf.xml.defaults/%gconf-tree*.xml
      --  # gconftool-2 --makefile-uninstall-rule is unpredictable
      if HT.leads (line, localbase & "/etc/gconf/gconf.xml.defaults/") and then
        HT.trails (line, ".xml")
      then
         if HT.contains (line, "/%gconf-tree") then
            return True;
         end if;
      end if;

      --  */__pycache__/*
      --  # we need to stop packaging python cache files.  until then, ignore
      if HT.leads (line, localbase & "/lib/python") and then
           HT.contains (line, "/__pycache__/")
      then
         return True;
      end if;

      return False;

   end ignore_this_file;


end hierarchy;
