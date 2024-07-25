--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Characters.Latin_1;
with GNAT.Traceback.Symbolic;

package body Unix.Ravexec is

   package LAT renames Ada.Characters.Latin_1;
   package TRC renames GNAT.Traceback;


   ---------------------
   --  start_new_log  --
   ---------------------
   function start_new_log (filename : String) return File_Descriptor
   is
      path : IC.Strings.chars_ptr;
      cfd : IC.int;
   begin
      path := IC.Strings.New_String (filename);
      cfd := C_Start_Log (path);
      IC.Strings.Free (path);
      return File_Descriptor (cfd);
   end start_new_log;


   -------------
   --  write  --
   -------------
   procedure write (fd : File_Descriptor; line : String)
   is
      cfd : constant IC.int := IC.int (fd);
      msg : IC.Strings.chars_ptr;
   begin
      msg := IC.Strings.New_String (line);
      C_Direct_Scribe (cfd, msg);
      IC.Strings.Free (msg);
   end write;


   ---------------
   --  writeln  --
   ---------------
   procedure writeln (fd : File_Descriptor; line : String)
   is
      cfd : constant IC.int := IC.int (fd);
      msg : IC.Strings.chars_ptr;
   begin
      msg := IC.Strings.New_String (line & LAT.LF);
      C_Direct_Scribe (cfd, msg);
      IC.Strings.Free (msg);
   end writeln;


   ------------------------
   --  close_file_blind  --
   ------------------------
   procedure close_file_blind (fd : File_Descriptor)
   is
      result : IC.int;
      cfd : constant IC.int := IC.int (fd);
      pragma Unreferenced (result);
   begin
      if fd = not_connected then
         return;
      end if;
      result := C_Close (cfd);
   end close_file_blind;


   ---------------------------
   --  set_argument_vector  --
   ---------------------------
   procedure set_argument_vector
     (Arg_String : String;
      argvector  : in out CSVector;
      num_args   : out IC.int)
   is
      Idx : Integer;

      use type IC.int;
   begin
      num_args := 0;
      Idx := Arg_String'First;

      loop
         declare
            Quoted   : Boolean := False;
            Backqd   : Boolean := False;
            Old_Idx  : Integer;

         begin
            Old_Idx := Idx;

            loop
               --  A vanilla space is the end of an argument

               if not Backqd and then not Quoted
                 and then Arg_String (Idx) = ' '
               then
                  exit;

               --  Start of a quoted string

               elsif not Backqd and then not Quoted
                 and then Arg_String (Idx) = '"'
               then
                  Quoted := True;

               --  End of a quoted string and end of an argument

               elsif not Backqd and then Quoted
                 and then Arg_String (Idx) = '"'
               then
                  Idx := Idx + 1;
                  exit;

               --  Following character is backquoted

               elsif Arg_String (Idx) = '\' then
                  Backqd := True;

               --  Turn off backquoting after advancing one character

               elsif Backqd then
                  Backqd := False;

               end if;

               Idx := Idx + 1;
               exit when Idx > Arg_String'Last;
            end loop;

            --  Found an argument

            argvector (num_args) := ICS.New_String (Arg_String (Old_Idx .. Idx - 1));
            num_args := num_args + 1;
            exit when num_args = MAX_ARGS;

            --  Skip extraneous spaces

            while Idx <= Arg_String'Last and then Arg_String (Idx) = ' '
            loop
               Idx := Idx + 1;
            end loop;
         end;

         exit when Idx > Arg_String'Last;
      end loop;

      argvector (num_args) := ICS.Null_Ptr;

   end set_argument_vector;


   -------------------------------
   --  launch_separate_process  --
   -------------------------------
   function launch_separate_process
     (builder   : builders;
      log_fd    : File_Descriptor;
      program   : String;
      arguments : String) return pid_t
   is
      cbuilder  : constant IC.int := IC.int (builder);
      cfd       : constant IC.int := IC.int (log_fd);
      cprogram  : ICS.chars_ptr;
      argvector : CSVector (0 .. MAX_ARGS);
      num_args  : IC.int;
      pid_res   : IC.int;
      argv      : Chars_Ptr_Ptr;

      use type IC.int;
   begin
      cprogram := ICS.New_String (program);
      set_argument_vector (arguments, argvector, num_args);
      argv := argvector (argvector'First)'Unchecked_Access;

      pid_res := C_Phase_Execution (cbuilder, cfd, cprogram, argv);

      ICS.Free (cprogram);
      if num_args > 0 then
         for x in 0 .. num_args - 1 loop
            ICS.Free (argvector (x));
         end loop;
      end if;

      return pid_t (pid_res);
   end launch_separate_process;


   ------------------
   --  dump_stack  --
   ------------------
   procedure dump_stack (fd : File_Descriptor)
   is
      trace : TRC.Tracebacks_Array (1 .. 2_000);
      trlen : Natural;
   begin
      writeln (fd, "Dump of stack:");
      TRC.Call_Chain (trace, trlen);
       writeln (fd, TRC.Symbolic.Symbolic_Traceback (trace (1 .. trlen)));
   end dump_stack;


end Unix.Ravexec;
