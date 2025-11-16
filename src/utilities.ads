--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions; use Definitions;
with HelperText;
with Ada.Calendar;

package Utilities is

   package CAL renames Ada.Calendar;

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

   --  Convert validated string to supported arch
   function convert_cpu_arch (archstr : String) return supported_arch;

   --  Convert validated lower cased string to supported operating system
   function convert_opsys (opsysstr : String) return supported_opsys;

   --  Returns first two hexidecimal digits (uppercase) from sha1 digest
   function bucket (palabra : String) return String;

   --  Replaces {{x}} with ${x}
   procedure apply_cbc_string (value : in out HelperText.Text);

   --  Quotation mask.  Returns the given string with all data between
   --  double quotation marks replaced with an "X".  It will replace
   --  escaped double-quotes inside quotes with an "X" as well.
   function mask_quoted_string (raw : String) return String;

      --  Quick formatting of json value
   function json_nvpair_integer
     (name    : String;
      value   : Integer;
      index   : Positive;
      padding : Natural) return String;

   --  Quick formatting of json value
   function json_nvpair_string
     (name    : String;
      value   : String;
      index   : Positive;
      padding : Natural) return String;

   --  Quick formatting of json value
   function json_nvpair_complex
     (name    : String;
      value   : String;
      index   : Positive;
      padding : Natural) return String;

   --  Quick formatting of json line containing just the name.
   --  Used when value is complex such as object or array which appear on separate lines.
   function json_name_complex (name : String; index : Positive; padding : Natural) return String;

   --  Quickly formats open curly or closed curly bracket for json object definition
   function json_object (initiating : Boolean; padding, index : Natural) return String;

   --  Quickly formats open square or closed square brack for json array definition
   function json_array (initiating : Boolean; padding : Natural) return String;

   --  Convert a string of digits representing unix epoch into Ada Time type
   function convert_unixtime (unix_string : String) return CAL.Time;

   --  Return true if name length is less than 9 characters.
   --  if name string contains "{{" or "${", the length can't be accurately evaluates so
   --  true is returned in that case as well.
   function valid_user_group (name : String) return Boolean;

end Utilities;
