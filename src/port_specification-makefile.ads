--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions; use Definitions;

package Port_Specification.Makefile is

   --  This generates a makefile from the specification based on given parameters
   --  The variant must be "standard" or one of the specified variants
   --  The output_file must be blank (stdout) or a file to (over)write a file with output.

   procedure generator
     (specs         : Portspecs;
      variant       : String;
      opsys         : supported_opsys;
      output_file   : String
     );

private

   dev_error : exception;

end Port_Specification.Makefile;
