--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Text_IO;

package body File_Operations.Heap is

   package TIO renames Ada.Text_IO;

   procedure slurp_file (dossier : String) is
   begin
      file_contents := new HM_File_String;

      if File_Size = 0 then
         return;
      end if;

      declare
         handle   : TIO.File_Type;
         attempts : Natural := 0;
         arrow    : Natural := file_contents'First;
      begin
         --  The introduction of variants causes a buildsheet to be scanned once per variant.
         --  It's possible (even common) for simultaneous requests to scan the same buildsheet to
         --  occur.  Thus, if the file is already open, wait and try again (up to 5 times)
         loop
            begin
               TIO.Open (handle, TIO.In_File, dossier);
               exit;
            exception
               when TIO.Use_Error | TIO.Status_Error =>
                  if attempts = 5 then
                     raise file_handling with "slurp_file: failed open: " & dossier;
                  end if;
                  attempts := attempts + 1;
                  delay 0.1;
            end;
         end loop;
         loop
            exit when TIO.End_Of_File (handle);
            declare
               line : constant String := TIO.Get_Line (handle);
               feed : constant Natural := arrow + line'Length;
            begin
               file_contents.all (arrow .. feed - 1) := line;
               file_contents.all (feed) := ASCII.LF;
               arrow := feed + 1;
            end;
         end loop;
         TIO.Close (handle);
      exception
         when others =>
            if TIO.Is_Open (handle) then
               TIO.Close (handle);
            end if;
            raise file_handling with "slurp_file(" & dossier & ") failed";
      end;
   exception
      when Storage_Error =>
         raise file_handling with "slurp_file: failed to allocate memory on heap";
   end slurp_file;

end File_Operations.Heap;
