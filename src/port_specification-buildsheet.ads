--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Port_Specification.Buildsheet is

   --  This generates the port buildsheet from the specification based on given parameters
   --  The output_file must be blank (stdout) or a file to (over)write a file with output.

   procedure generator
     (specs       : Portspecs;
      ravensrcdir : String;
      output_file : String);

private

   --  Takes a string and appends enough tabs to align to column 24
   --  If payload is empty, it returns 3 tabs
   --  If payload > 24 columns, return payload without modification
   function align24 (payload : String) return String;

   --  Takes a string and appends enough tabs to align to column 40
   --  If payload is empty, it returns 5 tabs
   --  If payload > 40 columns, return payload without modification
   function align40 (payload : String) return String;

end Port_Specification.Buildsheet;
