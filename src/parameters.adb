--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Directories;
with Ada.Characters.Latin_1;
with Ada.Text_IO;

package body Parameters is

   package DIR renames Ada.Directories;
   package LAT renames Ada.Characters.Latin_1;
   package TIO renames Ada.Text_IO;

   --------------------------------------------------------------------------------------------
   --  all_paths_valid
   --------------------------------------------------------------------------------------------
   function all_paths_valid return Boolean
   is
      function invalid_directory (folder : HT.Text; desc : String) return Boolean;

      use type DIR.File_Kind;

      function invalid_directory (folder : HT.Text; desc : String) return Boolean
      is
         dossier : constant String := HT.USS (folder);
         errmsg : constant String := "Configuration invalid: ";
      begin
         if DIR.Exists (dossier) and then DIR.Kind (dossier) = DIR.Directory then
            return False;
         else
            TIO.Put_Line (errmsg & desc & " directory: " & dossier);
            return True;
         end if;
      end invalid_directory;
   begin
      if HT.USS (configuration.dir_system) = "/" then
         TIO.Put_Line ("[G] System root cannot be " & LAT.Quotation & "/" & LAT.Quotation);
         return False;
      elsif invalid_directory (configuration.dir_system, "[G] System root") then
         return False;
      elsif invalid_directory (configuration.dir_packages, "[B] Packages") then
         return False;
      elsif invalid_directory (configuration.dir_conspiracy, "[A] Conspiracy") then
         return False;
      elsif invalid_directory (configuration.dir_distfiles, "[C] Distfiles") then
         return False;
      elsif invalid_directory (configuration.dir_logs, "[E] Build logs") then
         return False;
      end if;

      if HT.USS (configuration.dir_ccache) = no_ccache then
         return True;
      else
         return not invalid_directory (configuration.dir_ccache, "[H] Compiler cache");
      end if;
   end all_paths_valid;

end Parameters;
