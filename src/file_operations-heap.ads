--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with System.Pool_Local;

generic
   File_Size : Natural;    -- handle empty file

package File_Operations.Heap is

   localPool: System.Pool_Local.Unbounded_Reclaim_Pool;
   subtype HM_File_String is String (1 .. File_Size);
   type contents_ref is access HM_File_String;

   for contents_ref'Storage_Pool use localPool;

   file_contents : contents_ref;

   procedure slurp_file (dossier : String);

end File_Operations.Heap;
