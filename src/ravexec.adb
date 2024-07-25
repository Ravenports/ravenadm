--  SPDX-License-Identifier: ISC
--  Reference: /License.txt


package body Ravexec is


   ---------------------
   --  start_new_log  --
   ---------------------
   function start_new_log (filename : String) return File_Descriptor
   is
      path : IC.Strings.chars_ptr;
      cfd : IC.int;
   begin
      path := IC.Strings.New_String (filename);
      cfd := C_Start_Log (path);
      IC.Strings.Free (path);
      return File_Descriptor (cfd);
   end start_new_log;


   ---------------------
   --  scribe_to_log  --
   ---------------------
   procedure scribe_to_log (fd : File_Descriptor; line : String)
   is
      cfd : constant IC.int := IC.int (fd);
      msg : IC.Strings.chars_ptr;
   begin
      msg := IC.Strings.New_String (line);
      C_Direct_Scribe (cfd, msg);
      IC.Strings.Free (msg);
   end scribe_to_log;


   ------------------------
   --  close_file_blind  --
   ------------------------
   procedure close_file_blind (fd : File_Descriptor)
   is
      result : IC.int;
      cfd : constant IC.int := IC.int (fd);
      pragma Unreferenced (result);
   begin
      if fd = not_connected then
         return;
      end if;
      result := C_Close (cfd);
   end close_file_blind;

end Ravexec;
