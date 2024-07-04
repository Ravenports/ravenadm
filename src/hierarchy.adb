--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Characters.Latin_1;
with Archive.Dirent.Scan;
with Archive.Unix;
with Parameters;

package body Hierarchy is

   package LAT renames Ada.Characters.Latin_1;
   package SCN renames Archive.Dirent.Scan;
   package PM  renames Parameters;


   ---------------------
   --  take_snapshot  --
   ---------------------
   procedure take_snapshot
     (DC        : in out Dirent_Collection.Map;
      rootdir   : String)
   is
      procedure dive (this_directory : String);

      skip_dirs : admtypes.string_crate.Vector;

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
            case features.ftype is
               when Archive.directory |
                    Archive.symlink |
                    Archive.fifo |
                    Archive.unsupported =>
                  myrec.digest := (others => '0');
               when Archive.regular |
                    Archive.hardlink =>
                  myrec.digest := Blake_3.file_digest (rootdir & relpath);
            end case;
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
      set_file_filter (skip_dirs);
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
               features := Archive.Unix.get_charactistics (rootdir & relpath);
               case features.ftype is
                  when Archive.directory |
                       Archive.symlink |
                       Archive.fifo |
                       Archive.unsupported =>
                     digest := (others => '0');
                  when Archive.regular |
                       Archive.hardlink =>
                     digest := Blake_3.file_digest (rootdir & relpath);
               end case;
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
      extras.Clear;
      modified.Clear;
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
      --  # xmlcatmgr is constantly updating catalog.ports, ignore
      if line = localbase & "/share/xml/catalog.ports" then
         return True;
      end if;

      --  # gio modules cache could be modified for any gio modules
      if line = localbase & "/lib/gio/modules/giomodule.cache" then
         return True;
      end if;

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


   --------------------------------
   --  detect_leftovers_and_MIA  --
   --------------------------------
   function detect_leftovers_and_MIA
     (log_handle  : TIO.File_Type;
      DC          : in out Dirent_Collection.Map;
      rootdir     : String;
      description : String;
      fatal       : Boolean) return Boolean
   is
      procedure filter_extras (Position : admtypes.string_crate.Cursor);
      procedure filter_modify (Position : admtypes.string_crate.Cursor);
      procedure prune_leftovers (Position : admtypes.string_crate.Cursor);
      procedure print (cursor : admtypes.string_crate.Cursor);

      skip_dirs : admtypes.string_crate.Vector;
      extras    : admtypes.string_crate.Vector;
      modified  : admtypes.string_crate.Vector;
      leftover  : admtypes.string_crate.Vector;
      changed   : admtypes.string_crate.Vector;
      missing   : admtypes.string_crate.Vector;
      parent_lo : admtypes.string_crate.Vector;
      passed    : Boolean := True;

      procedure filter_extras (Position : admtypes.string_crate.Cursor)
      is
         filepath : HT.Text renames admtypes.string_crate.Element (Position);
      begin
         if ignore_this_file (filepath) then
            add_exception_of_leftover_ancestors (parent_lo, filepath);
         else
            leftover.Append (filepath);
         end if;
      end filter_extras;

      procedure filter_modify (Position : admtypes.string_crate.Cursor)
      is
         filepath : HT.Text renames admtypes.string_crate.Element (Position);
      begin
         if not ignore_this_file (filepath) then
           changed.Append (filepath);
         end if;
      end filter_modify;

      procedure print (cursor : admtypes.string_crate.Cursor)
      is
         dossier : constant String := HT.USS (admtypes.string_crate.Element (cursor));
      begin
         TIO.Put_Line (log_handle, LAT.HT & dossier);
      end print;

      procedure prune_leftovers (Position : admtypes.string_crate.Cursor)
      is
         filepath : HT.Text renames admtypes.string_crate.Element (Position);
         cursor   : admtypes.string_crate.Cursor;
      begin
         if leftover.Contains (filepath) then
            cursor := leftover.Find (filepath);
            leftover.Delete (cursor);
         end if;
      end prune_leftovers;

   begin
      set_file_filter (skip_dirs);
      check_again (DC        => DC,
                   rootdir   => rootdir,
                   skip_dirs => skip_dirs,
                   extras    => extras,
                   modified  => modified);
      leftover.Clear;
      changed.Clear;
      extras.Iterate (filter_extras'Access);
      modified.Iterate (filter_modify'Access);

      set_missing_files (DC, missing);
      parent_lo.Iterate (prune_leftovers'Access);

      admtypes.sorter.Sort (Container => changed);
      admtypes.sorter.Sort (Container => missing);
      admtypes.sorter.Sort (Container => leftover);

      TIO.Put_Line (log_handle, LAT.LF & "=> Checking for system changes " & description);
      if not leftover.Is_Empty then
         passed := False;
         TIO.Put_Line (log_handle, LAT.LF & "   Left over files/directories:");
         leftover.Iterate (Process => print'Access);
      end if;
      if not missing.Is_Empty then
         passed := False;
         TIO.Put_Line (log_handle, LAT.LF & "   Missing files/directories:");
         missing.Iterate (Process => print'Access);
      end if;
      if not changed.Is_Empty then
         passed := False;
         TIO.Put_Line (log_handle, LAT.LF & "   Modified files/directories:");
         changed.Iterate (Process => print'Access);
      end if;
      if passed then
         TIO.Put_Line (log_handle, "Everything is fine.");
      end if;
      if not fatal then
         return True;
      end if;
      return passed;

   end detect_leftovers_and_MIA;


   -------------------------
   --  set_missing_files  --
   -------------------------
   procedure set_missing_files
     (DC        : Dirent_Collection.Map;
      missing   : in out admtypes.string_crate.Vector)
   is
      procedure check_record (Position : Dirent_Collection.Cursor);
      procedure check_record (Position : Dirent_Collection.Cursor)
      is
         myrec : direntrec renames Dirent_Collection.Element (Position);
         key   : HT.Text renames Dirent_Collection.Key (Position);
      begin
         if not myrec.second then
            missing.Append (key);
         end if;
      end check_record;
   begin
      missing.Clear;
      DC.Iterate (check_record'Access);
   end set_missing_files;


   -------------------------------------------
   --  add_exception_of_leftover_ancestors  --
   -------------------------------------------
   procedure add_exception_of_leftover_ancestors
     (also_skip : in out admtypes.string_crate.Vector;
      leftover  : HT.Text)
   is
      procedure dive (parent_dir : String);

      delimiter : constant String := "/";

      procedure dive (parent_dir : String)
      is
         parend_dir_text : constant HT.Text := HT.SUS (parent_dir);
      begin
         if parent_dir = "" then
            return;
         end if;
         if also_skip.Contains (parend_dir_text) then
            return;  --  All parents of this directory are already recorded, exit now
         end if;
         also_skip.Append (parend_dir_text);
         dive (HT.head (parent_dir, delimiter));
      end dive;
   begin
      dive (HT.head (HT.USS (leftover), delimiter));
   end add_exception_of_leftover_ancestors;


end Hierarchy;
