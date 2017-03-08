--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions; use Definitions;

package Port_Specification.Makefile is

   --  This generates a makefile from the specification based on given parameters
   --  The variant must be "standard" or one of the specified variants
   --  The osrelease is 2-part release version of opsys
   --  The osversion is opsys-specific kernel number
   --  The osmajor is the opsys-specific major release (may be different from osrelease)
   --  The option_string only applies to the "standard" variant (leave blank for others)
   --      and it formats option configuration listed the names of ON options sep. by spaces
   --      e.g. "<NAME1> <NAME2>"
   --  The output_file must be blank (stdout) or a file to (over)write a file with output.

   procedure generator
     (specs         : Portspecs;
      variant       : String;
      opsys         : supported_opsys;
      arch_standard : supported_arch;
      osrelease     : String;
      osmajor       : String;
      osversion     : String;
      option_string : String;
      output_file   : String
     );

end Port_Specification.Makefile;
