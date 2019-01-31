--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions;  use Definitions;

package Display is

   subtype history_origin  is String (1 .. 45);
   subtype history_elapsed is String (1 .. 8);
   subtype history_action  is String (1 .. 8);
   subtype fivelong        is String (1 .. 5);
   subtype fld_phase       is String (1 .. 12);
   subtype fld_origin      is String (1 .. 40);
   subtype fld_lines       is String (1 .. 7);
   subtype fld_slavid      is String (1 .. 2);
   type history_rec is
      record
         id          : builders;
         slavid      : String (1 .. 2);
         run_elapsed : history_elapsed;
         action      : history_action;
         pkg_elapsed : history_elapsed;
         origin      : history_origin;
         established : Boolean := False;
      end record;

   type summary_rec is
      record
         Initially : Natural;
         Built     : Natural;
         Failed    : Natural;
         Ignored   : Natural;
         Skipped   : Natural;
         elapsed   : history_elapsed;
         impulse   : Natural;
         pkg_hour  : Natural;
         load      : Float;
         swap      : Float;
      end record;

   type builder_rec is
      record
         id        : builders;
         shutdown  : Boolean;
         idle      : Boolean;
         slavid    : fld_slavid;
         Elapsed   : history_elapsed;
         LLines    : fld_lines;
         phase     : fld_phase;
         origin    : fld_origin;
      end record;

   action_shutdown : constant history_action := "shutdown";
   action_skipped  : constant history_action := "skipped ";
   action_ignored  : constant history_action := "ignored ";
   action_success  : constant history_action := "success ";
   action_failure  : constant history_action := "failure ";

   --  Insert history as builder finishes (shutdown, success, failure);
   procedure insert_history (HR : history_rec);

   --  Expose helper function that formats float values for www report
   function fmtpc (f : Float; percent : Boolean) return fivelong;

   --  Expose helper function that formats load values for www report
   function fmtload (f : Float) return fivelong;

private

   type cyclic_range is range 1 .. 50;
   type dim_history is array (cyclic_range) of history_rec;
   history       : dim_history;
   history_arrow : cyclic_range := cyclic_range'Last;

end Display;
