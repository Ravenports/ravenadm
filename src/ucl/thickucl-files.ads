--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with libucl;

package ThickUCL.Files is

   ucl_file_unparseable : exception;
   ucl_data_unparseable : exception;

   --  Opens UCL file for reading and attempts to parse the contents.
   --  If it fails, the ucl_file_unparseable exception is thrown.
   --  The nvpairs argument is a concatenated list of name-value pairs delimited
   --  with a vertical pipe (e.g. "key1=val1|key2=val2|key3=val3") where the
   --  keys are variables registers in the parser.
   procedure parse_ucl_file
     (tree    : in out UclTree;
      path    : String;
      nvpairs : String);

   --  Version of parse_ucl_file, but the file has already been read into in string.
   --  Rather than force a write and read, just parse the string directly.
   procedure parse_ucl_string
     (tree    : in out UclTree;
      ucldata : String;
      nvpairs : String);

private

   procedure populate_the_tree
     (tree : in out UclTree;
      rootobj : access constant libucl.ucl_object_t);

   procedure populate_array
     (tree : in out UclTree;
      arrayobj :  access constant libucl.ucl_object_t);

   function extract_key
     (item : access constant libucl.ucl_object_t) return String;

   --  isfile determines if ucldata is a path name or the file contents
   procedure parse_ucl_guts
     (tree    : in out UclTree;
      isfile  : Boolean;
      ucldata : String;
      nvpairs : String);

end ThickUCL.Files;
