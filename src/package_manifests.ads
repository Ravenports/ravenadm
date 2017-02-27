--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Package_Manifests is

   compress_issue   : exception;
   decompress_issue : exception;

   type Filename is new String;

   --  Given the path to the manifest, compress it and return it
   function compress_manifest (manifest : Filename) return String;

   --  Given the path to the manifest, compress and save it as a new file
   procedure compress_manifest
     (old_manifest : Filename;
      new_manifest : Filename);

   --  Given a string representing a compressed manifest, decompress and save to file
   procedure decompress_manifest
     (compressed_string : String;
      save_to_file      : Filename);

   --  Given the path to a compressed manifest, decompress and save to new file
   procedure decompress_manifest_file
     (compressed_file : Filename;
      save_to_file    : Filename);

private

   file_handling : exception;

end Package_Manifests;
