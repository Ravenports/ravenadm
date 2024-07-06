--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: License.txt

with Ada.Containers.Vectors;
with HelperText;

package admtypes is

   package string_crate is new Ada.Containers.Vectors
     (Element_Type => HelperText.Text,
      Index_Type   => Positive,
      "="          => HelperText.SU."=");

   package sorter is new string_crate.Generic_Sorting ("<" => HelperText.SU."<");

   type ADO_Data is
      record
         abi          : HelperText.Text;
         dependencies : string_crate.Vector;
         options      : string_crate.Vector;
      end record;

end admtypes;
