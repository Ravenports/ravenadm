--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions; use Definitions;

package Utilities is

   --  Return lower case version of supported operating systems
   function lower_opsys (opsys : supported_opsys) return String;

   --  Return mixed case version of supported operating systems
   function mixed_opsys (opsys : supported_opsys) return String;

end Utilities;
