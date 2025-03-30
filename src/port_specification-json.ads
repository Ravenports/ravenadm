--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions; use Definitions;
with Ada.Text_IO;

package Port_Specification.Json is

   package TIO renames Ada.Text_IO;

   --  Describes the port using standard parameters formatted in JSON
   --  Used for Repology index
   procedure describe_port
     (specs   : Portspecs;
      dossier : TIO.File_Type;
      bucket  : String;
      index   : Positive);

private

   pad : constant Natural := 4;

   --  returns "generated" instead of raw value when generated is true
   function fpc_value (is_generated : Boolean; raw_value : String) return String;

   --  returns Keyword array
   function describe_keywords (specs : Portspecs) return String;

   --  returns array of distfiles
   function describe_distfiles (specs : Portspecs) return String;

   --  return array of subpackages
   function describe_subpackages (specs : Portspecs; variant : String) return String;

   --  return array of patched vulnerabilities
   function describe_cve (specs : Portspecs) return String;

   --  When the homepage is set to "none", return an empty string, otherwise return a json string
   function homepage_line (specs : Portspecs) return String;

   --  If USES=cpe is set, push CPE_VENDOR and CPE_PRODUCT values
   function describe_Common_Platform_Enumeration (specs : Portspecs) return String;

   --  If USES=cpe is set and CVE_FIXED list is not empty
   function describe_patched_vulnerabilities (specs : Portspecs) return String;

end Port_Specification.Json;
