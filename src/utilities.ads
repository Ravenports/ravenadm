--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions; use Definitions;
with HelperText;

package Utilities is

   bad_input : exception;

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

   --  Converted validated string to supported arch
   function convert_cpu_arch (archstr : String) return supported_arch;

   --  Returns first two hexidecimal digits (uppercase) from sha1 digest
   function bucket (palabra : String) return String;

   --  Replaces {{x}} with ${x}
   procedure apply_cbc_string (value : in out HelperText.Text);

   --  Quotation mask.  Returns the given string with all data between
   --  double quotation marks replaced with an "X".  It will replace
   --  escaped double-quotes inside quotes with an "X" as well.
   function mask_quoted_string (raw : String) return String;

end Utilities;
