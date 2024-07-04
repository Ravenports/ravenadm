--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Containers.Hashed_Maps;
with Archive;
with Blake_3;
with HelperText;
with admtypes;

package hierarchy is

   package CON renames Ada.Containers;
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
      rootdir   : String;
      skip_dirs : admtypes.string_crate.Vector);

   procedure check_again
     (DC        : in out Dirent_Collection.Map;
      rootdir   : String;
      skip_dirs : admtypes.string_crate.Vector;
      extras    : in out admtypes.string_crate.Vector;
      modified  : in out admtypes.string_crate.Vector);

private

   procedure set_file_filter (skip_dirs : in out admtypes.string_crate.Vector);

   function ignore_this_file (filename : HT.Text) return Boolean;

end hierarchy;
