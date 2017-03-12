--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions; use Definitions;

package Utilities is

   --  Return lower case version of supported operating systems
   function lower_opsys (opsys : supported_opsys) return String;

   --  Return mixed case version of supported operating systems
   function mixed_opsys (opsys : supported_opsys) return String;

   --  Return text equivalent of supported_arch item
   function cpu_arch (arch : supported_arch) return String;

   --  Return True if candidate matches any supported lower opsys
   function valid_lower_opsys (candidate : String) return Boolean;

   --  Return True if candidate matches any supported cpu arch
   function valid_cpu_arch (candidate : String) return Boolean;

   --  Returns first two hexidecimal digits (uppercase) from sha1 digest
   function bucket (palabra : String) return String;

end Utilities;
