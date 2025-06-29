--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;
with Ada.Characters.Latin_1;
with GNAT.Traceback.Symbolic;
with GNAT.OS_Lib;

package body Unix.Ravexec is

   package LAT renames Ada.Characters.Latin_1;
   package TRC renames GNAT.Traceback;
   package OSL renames GNAT.OS_Lib;


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
      if fd = not_connected then
         Ada.Text_IO.Put_Line ("FD closed: " & line);
      else
         msg := IC.Strings.New_String (line);
         C_Direct_Scribe (cfd, msg);
         IC.Strings.Free (msg);
      end if;
   end write;


   ---------------
   --  writeln  --
   ---------------
   procedure writeln (fd : File_Descriptor; line : String)
   is
      cfd : constant IC.int := IC.int (fd);
      msg : IC.Strings.chars_ptr;
   begin
      if fd = not_connected then
         Ada.Text_IO.Put_Line ("FD closed: " & line);
      else
         msg := IC.Strings.New_String (line & LAT.LF);
         C_Direct_Scribe (cfd, msg);
         IC.Strings.Free (msg);
      end if;
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
      argvector  : in out struct_argv;
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

            argvector.args (num_args) := ICS.New_String (Arg_String (Old_Idx .. Idx - 1));
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

      argvector.args (num_args) := ICS.Null_Ptr;

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
   begin
      case platform_type is
         when freebsd =>
            declare
               procid  : OSL.Process_Id;
               Args    : OSL.Argument_List_Access;
               command : constant String := ravenexec & " " & HT.int2str (Integer (log_fd)) & " "
                         & program & " " & arguments;
            begin
               Args   := OSL.Argument_String_To_List (command);
               procid := OSL.Non_Blocking_Spawn
                 (Program_Name => Args (Args'First).all,
                  Args => Args (Args'First + 1 .. Args'Last));
               OSL.Free (Args);
               return pid_t (OSL.Pid_To_Integer (procid));
            end;
         when others =>
            declare
               cbuilder  : constant IC.int := IC.int (builder);
               cfd       : constant IC.int := IC.int (log_fd);
               argvector : aliased struct_argv;
               cprogram  : ICS.chars_ptr;
               num_args  : IC.int;
               pid_res   : IC.int;
               use type IC.int;
            begin
               cprogram := ICS.New_String (program);
               set_argument_vector (program & " " & arguments, argvector, num_args);
               pid_res := C_Phase_Execution (cbuilder, cfd, cprogram, num_args,
                                             argvector'Unchecked_Access);
               ICS.Free (cprogram);
               if num_args > 0 then
                  for x in 0 .. num_args - 1 loop
                     ICS.Free (argvector.args (x));
                  end loop;
               end if;
               return pid_t (pid_res);
            end;
      end case;

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


   -------------------------
   --  kill_process_tree  --
   -------------------------
   procedure kill_process_tree (builder : builders; log_fd : File_Descriptor)
   is
      result : IC.int;
      cbuilder : constant IC.int := IC.int (builder);
   begin
      result := kill_runaway (cbuilder);
      case result is
         when 0 => null;
         when others =>
            writeln (log_fd, "Warning: failed to kill process of builder" & builder'Img);
      end case;
   end kill_process_tree;


end Unix.Ravexec;
