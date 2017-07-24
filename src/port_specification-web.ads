--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions; use Definitions;
with Ada.Text_IO;

package Port_Specification.Web is

   package TIO renames Ada.Text_IO;


   --  Convert specification to a static web page written to dossier
   procedure produce_page
     (specs   : Portspecs;
      variant : String;
      dossier : TIO.File_Type;
      portdir : String);

private

   --  standard page header used on all pages
   function page_header (title : String) return String;

   --  standard page footer used on all pages
   function page_footer return String;

   --  Escape certain characters that are quoted in html (&, <, >, ")
   function escape_value (raw : String) return String;

   --  formats to:
   --  <name>="<escaped value>"
   function nvpair (name, value : String) return String;

   --  return populated div enclosure
   function div (id, value : String) return String;

   --  returns the pre-populated web page template for the entire body
   function body_template return String;

   --  returns a row template for the subpackage descriptions
   function two_cell_row_template return String;

   --  populate body template
   function generate_body
     (specs   : Portspecs;
      variant : String;
      portdir : String) return String;

   --  html format WWW reference
   function format_homepage (homepage : String) return String;

   --  return "(dual)" or "(multi)" if appropriate
   function list_scheme (licenses, scheme : String) return String;

   --  Make hyperlink
   function link (href, link_class, value : String) return String;

   --  Dynamic "other variants" contents
   function other_variants (specs : Portspecs; variant : String) return String;

   --  Constructs the rows of the subpackage description body
   function subpackage_description_block
     (specs    : Portspecs;
      namebase : String;
      variant  : String;
      portdir  : String) return String;

   --  Return contents of distinfo file
   function retrieve_distinfo (specs : Portspecs; portdir : String) return String;

   --  Constructs the rows of the dependencies-by-type table body
   function dependency_block (specs : Portspecs) return String;

   --  Constructs the rows of the master site groups and the sites it contains
   function master_sites_block (specs : Portspecs) return String;

   --  Constructs optional deprecated message
   function deprecated_message (specs : Portspecs) return String;

   --  Construct "broken message" blocks (does not transform opsys/arch)
   function broken_attributes (specs : Portspecs) return String;

   --  Construct "Only for platform" block
   function inclusive_platform (specs : Portspecs) return String;

   --  Construct "Exclude platform" block
   function exclusive_platform (specs : Portspecs) return String;

   --  Construct "exclude architecture" block
   function exclusive_arch (specs : Portspecs) return String;

end Port_Specification.Web;
