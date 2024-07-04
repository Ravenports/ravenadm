--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;
with Ada.Containers.Hashed_Maps;
with Archive;
with Blake_3;
with HelperText;
with admtypes;

package Hierarchy is

   package CON renames Ada.Containers;
   package TIO renames Ada.Text_IO;
   package HT  renames HelperText;

   type direntrec is
      record
         uid    : Archive.owngrp_id;
         gid    : Archive.owngrp_id;
         perms  : Archive.permissions;
         ftype  : Archive.file_type;
         digest : Blake_3.blake3_hash;
         second : Boolean := False;
      end record;

   package Dirent_Collection is new CON.Hashed_Maps
     (Key_Type        => HT.Text,
      Element_Type    => direntrec,
      Hash            => HT.hash,
      Equivalent_Keys => HT.equivalent);

   --  recursively scans rootdir, skipping directories that are contained in skip_dirs
   procedure take_snapshot
     (DC        : in out Dirent_Collection.Map;
      rootdir   : String);

   --  Given the results of the snapshot, this function displays any missing, extra or
   --  modified files and returns False if any of those occur.  (True means check passed)
   function detect_leftovers_and_MIA
     (log_handle  : TIO.File_Type;
      DC          : in out Dirent_Collection.Map;
      rootdir     : String;
      description : String;
      fatal       : Boolean) return Boolean;

private

   --  Initialize filter set (directories to avoid checking)
   procedure set_file_filter (skip_dirs : in out admtypes.string_crate.Vector);

   function ignore_this_file (filename : HT.Text) return Boolean;

   procedure check_again
     (DC        : in out Dirent_Collection.Map;
      rootdir   : String;
      skip_dirs : admtypes.string_crate.Vector;
      extras    : in out admtypes.string_crate.Vector;
      modified  : in out admtypes.string_crate.Vector);

   procedure set_missing_files
     (DC        : Dirent_Collection.Map;
      missing   : in out admtypes.string_crate.Vector);

   procedure add_exception_of_leftover_ancestors
     (also_skip : in out admtypes.string_crate.Vector;
      leftover  : HT.Text);

end Hierarchy;