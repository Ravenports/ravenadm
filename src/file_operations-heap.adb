--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Direct_IO;

package body File_Operations.Heap is

   procedure slurp_file (dossier : String) is
   begin
      file_contents := new HM_File_String;

      if File_Size = 0 then
         return;
      end if;

      declare
         package File_String_IO is new Ada.Direct_IO (HM_File_String);

         file_handle : File_String_IO.File_Type;
         attempts    : Natural := 0;
      begin
         --  The introduction of variants causes a buildsheet to be scanned once per variant.
         --  It's possible (even common) for simultaneous requests to scan the same buildsheet to
         --  occur.  Thus, if the file is already open, wait and try again (up to 5 times)
         loop
            begin
               File_String_IO.Open  (File => file_handle,
                                     Mode => File_String_IO.In_File,
                                     Name => dossier);
               exit;
            exception
               when File_String_IO.Status_Error | File_String_IO.Use_Error =>
                  if attempts = 5 then
                     raise file_handling with "slurp_file: failed open: " & dossier;
                  end if;
                  attempts := attempts + 1;
                  delay 0.1;
            end;
         end loop;
         File_String_IO.Read  (File => file_handle, Item => file_contents.all);
         File_String_IO.Close (file_handle);
      end;
   exception
      when Storage_Error =>
         raise file_handling with "failed to allocate memory on heap";
   end slurp_file;

end File_Operations.Heap;
