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
      rootdir   : String;
      builder   : Positive)
   is
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
               when Archive.directory | Archive.symlink | Archive.fifo =>
                  myrec.digest := (others => '0');
               when Archive.unsupported =>
                  myrec.digest := (others => '0');  --  should be impossible.
               when Archive.regular | Archive.hardlink =>
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
         SCN.scan_directory (rootdir & this_directory, this_level, builder);
         this_level.Iterate (analyze_entity'Access);
      end dive;
   begin
      set_directory_filter (skip_dirs);
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
      modified  : in out admtypes.string_crate.Vector;
      builder   : Positive)
   is
      procedure set_second (Key : HT.Text; Element : in out direntrec);
      procedure dive (this_directory : String);

      M, U, G, D, T : Boolean;

      procedure set_second (Key : HT.Text; Element : in out direntrec) is
      begin
         Element.second := True;
         Element.dM := not M;
         Element.dU := not U;
         Element.dG := not G;
         Element.dD := not D;
         Element.dT := not T;
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
            features := Archive.Unix.get_charactistics (rootdir & relpath);
            if DC.Contains (entkey) then
               myrec := DC.Element (entkey);
               case features.ftype is
                  when Archive.directory | Archive.symlink | Archive.fifo =>
                     digest := (others => '0');
                  when Archive.unsupported =>
                     digest := (others => '0');
                  when Archive.regular | Archive.hardlink =>
                     digest := Blake_3.file_digest (rootdir & relpath);
               end case;

               M := myrec.perms = features.perms;
               U := myrec.uid = features.uid;
               G := myrec.gid = features.gid;
               D := myrec.digest = digest;
               T := myrec.ftype = features.ftype;

               if not (M and then U and then G and then D and then T) then
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
         SCN.scan_directory (rootdir & this_directory, this_level, builder);
         this_level.Iterate (analyze_entity'Access);
      end dive;
   begin
      extras.Clear;
      modified.Clear;
      dive ("/");
   end check_again;


   ----------------------------
   --  set_directory_filter  --
   ----------------------------
   procedure set_directory_filter (skip_dirs : in out admtypes.string_crate.Vector)
   is
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
      push ("/tmp");
      push ("/repo");
      push ("/proc");
      push ("/home");
      push ("/root");
      push ("/usr");
      push ("/var/cache");
      push ("/var/db");
      push ("/var/log");
      push ("/var/mail");
      push ("/var/run");
      push ("/var/spool");
      push ("/var/tmp");
      push ("/port");
      push ("/xports");

   end set_directory_filter;


   ------------------------------
   --  set_single_file_filter  --
   ------------------------------
   procedure set_single_file_filter (skip_files : in out admtypes.string_crate.Vector)
   is
      localbase  : constant String := HT.USS (PM.configuration.dir_localbase);

      procedure push (file_or_directory : String) is
      begin
         skip_files.Append (HT.SUS (file_or_directory));
      end push;
   begin
      --  # xmlcatmgr is constantly updating catalog.ports, ignore
      push (localbase & "/share/xml/catalog.ports");

      --  # gio modules cache could be modified for any gio modules
      push (localbase & "/lib/gio/modules/giomodule.cache");

      push ("/etc/group");
      push ("/etc/passwd");
      push ("/etc/pwd.db");
      push ("/etc/spwd.db");
      push ("/etc/master.passwd");
      push ("/etc/.pwd.lock");   --  Linux
      push ("/etc/group-");      --  Linux
      push ("/etc/passwd-");     --  Linux
      push ("/etc/ld.so.cache"); --  Linux

   end set_single_file_filter;


   ------------------------
   --  ignore_this_file  --
   ------------------------
   function ignore_this_file (filename : HT.Text;
                              singles : admtypes.string_crate.Vector) return Boolean
   is
      line      : constant String := HT.USS (filename);
      localbase : constant String := HT.USS (PM.configuration.dir_localbase);
   begin
      if singles.Contains (filename) then
         return True;
      end if;

      --  # gio modules cache could be modified for any gio modules
      if line = localbase & "/lib/gio/modules/giomodule.cache" then
         return True;
      end if;

      --  # /etc/resolv.conf is manipulated by the framework.  it's normal
      --  ignores resolv.conf and resolv.conf.orig
      if HT.leads (line, "/etc/resolv.conf") then
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
      fatal       : Boolean;
      builder     : Positive) return Boolean
   is
      procedure filter_extras (Position : admtypes.string_crate.Cursor);
      procedure filter_modify (Position : admtypes.string_crate.Cursor);
      procedure prune_leftovers (Position : admtypes.string_crate.Cursor);
      procedure print_modified (cursor : admtypes.string_crate.Cursor);
      procedure print (cursor : admtypes.string_crate.Cursor);

      skip_dirs : admtypes.string_crate.Vector;
      singles   : admtypes.string_crate.Vector;
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
         if ignore_this_file (filepath, singles) then
            add_exception_of_leftover_ancestors (parent_lo, filepath);
         else
            leftover.Append (filepath);
         end if;
      end filter_extras;

      procedure filter_modify (Position : admtypes.string_crate.Cursor)
      is
         filepath : HT.Text renames admtypes.string_crate.Element (Position);
      begin
         if not ignore_this_file (filepath, singles) then
           changed.Append (filepath);
         end if;
      end filter_modify;

      procedure print (cursor : admtypes.string_crate.Cursor)
      is
         dossier : constant String := HT.USS (admtypes.string_crate.Element (cursor));
      begin
         TIO.Put_Line (log_handle, LAT.HT & dossier);
      end print;

      procedure print_modified (cursor : admtypes.string_crate.Cursor)
      is
         filename : HT.Text renames admtypes.string_crate.Element (cursor);
         myrec    : direntrec renames DC.Element (filename);
         status   : String (1 .. 8) := (others => ' ');
      begin
         if myrec.dD then
            status (1) := 'D';
         end if;
         if myrec.dU then
            status (2) := 'U';
         end if;
         if myrec.dG then
            status (3) := 'G';
         end if;
         if myrec.dM then
            status (4) := 'M';
         end if;
         if myrec.dT then
            status (5) := 'T';
         end if;
         TIO.Put_Line (log_handle, LAT.HT & status & HT.USS (filename));
      end print_modified;

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
      load_custom_single_exceptions (rootdir, singles);
      set_single_file_filter (singles);
      set_directory_filter (skip_dirs);
      check_again (DC        => DC,
                   rootdir   => rootdir,
                   skip_dirs => skip_dirs,
                   extras    => extras,
                   modified  => modified,
                   builder   => builder);
      leftover.Clear;
      changed.Clear;
      extras.Iterate (filter_extras'Access);
      modified.Iterate (filter_modify'Access);

      set_missing_files (DC, missing, singles);
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
         changed.Iterate (Process => print_modified'Access);
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
      missing   : in out admtypes.string_crate.Vector;
      singles   : admtypes.string_crate.Vector)
   is
      procedure check_record (Position : Dirent_Collection.Cursor);
      procedure check_record (Position : Dirent_Collection.Cursor)
      is
         myrec : direntrec renames Dirent_Collection.Element (Position);
         key   : HT.Text renames Dirent_Collection.Key (Position);
      begin
         if not myrec.second then
            if not singles.Contains (key) then
               missing.Append (key);
            end if;
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


   -------------------------------------
   --  load_custom_single_exceptions  --
   -------------------------------------
   procedure load_custom_single_exceptions (rootdir : String;
                                            singles : in out admtypes.string_crate.Vector)
   is
      --  optional file located at /tmp/skip_file_check
      --  enter one file per line
      input_file : constant String := rootdir & "/tmp/skip_file_check";
      features : Archive.Unix.File_Characteristics;
      handle   : TIO.File_Type;
   begin
      features := Archive.Unix.get_charactistics (input_file);
      case features.ftype is
         when Archive.regular => null;
         when others => return;
      end case;

      TIO.Open (handle, TIO.In_File, input_file);
      while not TIO.End_Of_File (handle) loop
         declare
            line : constant String := TIO.Get_Line (handle);
         begin
            singles.Append (HT.SUS (line));
         end;
      end loop;
      TIO.Close (handle);
   exception
      when others =>
         if TIO.Is_Open (handle) then
            TIO.Close (handle);
         end if;
   end load_custom_single_exceptions;

end Hierarchy;
