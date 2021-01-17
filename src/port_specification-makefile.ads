--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions; use Definitions;

private with Ada.Text_IO;

package Port_Specification.Makefile is

   --  This generates a makefile from the specification based on given parameters
   --  The variant must be "standard" or one of the specified variants
   --  The output_file must be blank (stdout) or a file to (over)write a file with output.

   procedure generator
     (specs         : Portspecs;
      variant       : String;
      opsys         : supported_opsys;
      arch          : supported_arch;
      output_file   : String
     );

private

   package TIO renames Ada.Text_IO;

   dev_error : exception;

   --  Given a string GITHUB/account:project:tag(:directory) return a standard
   --  distname per github rules.  Also works for GITHUB_PRIVATE and GHPRIV
   function generate_github_distname (download_site : String) return String;

   --  Given a string GITLAB/account:project:tag return a standard distname.
   --  Rare characters in account/project may have to be URL encoded
   function generate_gitlab_distname (download_site : String) return String;

   --  Given a string CRATES/project:version return a standard distname.
   function generate_crates_distname (download_site : String) return String;

   --  Used for non-custom (and not invalid) licenses, returns the full license name
   function standard_license_names (license : license_type) return String;

   procedure handle_github_relocations (specs : Portspecs; makefile : TIO.File_Type);

   --  Returns true if USES module should be omitted from Makefile USES definition
   function passive_uses_module (value : String) return Boolean;

end Port_Specification.Makefile;
