--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with HelperText;

package body Display is

   package  HT renames HelperText;

   --------------------------------------------------------------------------------------------
   --  insert_history
   --------------------------------------------------------------------------------------------
   procedure insert_history (HR : history_rec) is
   begin
      if history_arrow = cyclic_range'Last then
         history_arrow := cyclic_range'First;
      else
         history_arrow := history_arrow + 1;
      end if;
      history (history_arrow) := HR;
   end insert_history;


   ------------------------------------------------------------------------
   --  fmtpc
   ------------------------------------------------------------------------
   function fmtpc (f : Float; percent : Boolean) return fivelong
   is
      type loadtype is delta 0.01 digits 4;
      result : fivelong := (others => ' ');
      raw1   : constant loadtype := loadtype (f);
      raw2   : constant String := raw1'Img;
      raw3   : constant String := raw2 (2 .. raw2'Last);
      rlen   : constant Natural := raw3'Length;
      start  : constant Natural := 6 - rlen;
   begin
      result (start .. 5) := raw3;
      if percent then
         result (5) := '%';
      end if;
      return result;
   end fmtpc;


   ------------------------------------------------------------------------
   --  fmtload
   ------------------------------------------------------------------------
   function fmtload (f : Float) return fivelong
   is
      type loadtype is delta 0.01 digits 4;
      result : fivelong := (others => ' ');
   begin
      if f < 100.0 then
         return fmtpc (f, False);
      elsif f < 1000.0 then
         declare
            type loadtype is delta 0.1 digits 4;
            raw1 : constant loadtype := loadtype (f);
         begin
            return HT.trim (raw1'Img);
         end;
      elsif f < 10000.0 then
         declare
            raw1 : constant Integer := Integer (f);
         begin
            --  preceded by space, 1000.0 .. 9999.99, should be 5 chars
            return raw1'Img;
         end;
      elsif f < 100000.0 then
         declare
            raw1 : constant Integer := Integer (f);
         begin
            --  100000.0 .. 99999.9
            return HT.trim (raw1'Img);
         end;
      else
         return "100k+";
      end if;
   exception
      when others =>
         return "ERROR";
   end fmtload;

end Display;
