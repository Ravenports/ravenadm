--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Unix;
with PortScan.Log;

package body PortScan.Buildcycle is

   package LOG renames PortScan.Log;

   --------------------------------------------------------------------------------------------
   --  build_package
   --------------------------------------------------------------------------------------------
   function build_package (id          : builders;
                           sequence_id : port_id;
                           interactive : Boolean := False;
                           interphase  : String  := "") return Boolean
   is
      R : Boolean;
      break_phase  : constant phases := valid_test_phase (interphase);
      run_selftest : constant Boolean := Unix.env_variable_defined (selftest);
   begin
      trackers (id).seq_id := sequence_id;
      trackers (id).loglines := 0;
      if uselog then
         if not initialize_log (id) then
            finalize_log (id);
            return False;
         end if;
      end if;
      for phase in phases'Range loop
         phase_trackers (id) := phase;
         case phase is
            when blr_depends =>
               R := exec_phase_depends (id, phase);

            when  fetch | checksum | extract | patch | pkg_package =>
               R := exec_phase_generic (id, phase);

            when configure =>
               if testing then
                  mark_file_system (id, "preconfig");
               end if;
               R := exec_phase_generic (id, phase);

            when build =>
               R := exec_phase_build (id);

            when test =>
               if testing and run_selftest then
                  R := exec_phase_generic (id, phase);
               end if;

            when stage =>
               if testing then
                  mark_file_system (id, "prestage");
               end if;
               R := exec_phase_generic (id, phase);

            when install_mtree | install | check_plist =>
               if testing then
                  R := exec_phase_generic (id, phase);
               end if;

            when deinstall =>
               if testing then
                  R := exec_phase_deinstall (id);
               end if;
         end case;
         exit when R = False;
         exit when interactive and then phase = break_phase;
      end loop;
      if uselog then
         finalize_log (id);
      end if;
      if interactive then
         interact_with_builder (id);
      end if;
      return R;
   end build_package;


   --------------------------------------------------------------------------------------------
   --  last_build_phase
   --------------------------------------------------------------------------------------------
   function last_build_phase (id : builders) return String is
   begin
      return phase2str (phase => phase_trackers (id));
   end last_build_phase;


   --------------------------------------------------------------------------------------------
   --  max_time_without_output
   --------------------------------------------------------------------------------------------
   function max_time_without_output (phase : phases) return execution_limit
   is
      base : Integer;
   begin
      case phase is
         when blr_depends      => base := 15;  --  octave forge extraction is driver
         when fetch | checksum => return 480;  --  8 hours
         when extract          => base := 20;
         when patch            => base := 3;
         when configure        => base := 15;
         when build            => base := 25;   --  for gcc linking, tex
         when stage            => base := 20;   --  desire 15 but too many rogue builders-in-stage
         when test             => base := 25;
         when check_plist      => base := 10;   --  For packages with thousands of files
         when pkg_package      => base := 80;
         when install_mtree    => base := 3;
         when install          => base := 10;
         when deinstall        => base := 10;
      end case;
      declare
         multiplier_x10 : constant Positive := timeout_multiplier_x10;
      begin
         return execution_limit (base * multiplier_x10 / 10);
      end;
   end max_time_without_output;


   --------------------------------------------------------------------------------------------
   --  phase2str
   --------------------------------------------------------------------------------------------
   function phase2str (phase : phases) return String is
   begin
      case phase is
         when blr_depends     => return "dependencies";
         when fetch           => return "fetch";
         when checksum        => return "checksum";
         when extract         => return "extract";
         when patch           => return "patch";
         when configure       => return "configure";
         when build           => return "build";
         when stage           => return "stage";
         when test            => return "test";
         when pkg_package     => return "package";
         when install_mtree   => return "install-mtree";
         when install         => return "install";
         when deinstall       => return "deinstall";
         when check_plist     => return "check-plist";
      end case;
   end phase2str;


   --------------------------------------------------------------------------------------------
   --  valid_test_phase #1
   --------------------------------------------------------------------------------------------
   function valid_test_phase (afterphase : String) return phases is
   begin
      if afterphase = "extract" then
         return extract;
      elsif afterphase = "patch" then
         return patch;
      elsif afterphase = "configure" then
         return configure;
      elsif afterphase = "build" then
         return build;
      elsif afterphase = "stage" then
         return check_plist;
      elsif afterphase = "install" then
         return install;
      elsif afterphase = "deinstall" then
         return deinstall;
      else
         return phases'First;
      end if;
   end valid_test_phase;


   --------------------------------------------------------------------------------------------
   --  valid_test_phase #2
   --------------------------------------------------------------------------------------------
   function valid_test_phase (afterphase : String) return Boolean is
   begin
      return
        afterphase = "extract"   or else
        afterphase = "patch"     or else
        afterphase = "configure" or else
        afterphase = "build"     or else
        afterphase = "stage"     or else
        afterphase = "install"   or else
        afterphase = "deinstall";
   end valid_test_phase;


   --------------------------------------------------------------------------------------------
   --  exec_phase_generic
   --------------------------------------------------------------------------------------------
   function exec_phase_generic (id : builders; phase : phases) return Boolean
   is
      time_limit : execution_limit := max_time_without_output (phase);
   begin
      return exec_phase (id => id, phase => phase, time_limit => time_limit);
   end exec_phase_generic;


   --------------------------------------------------------------------------------------------
   --  exec_phase_build
   --------------------------------------------------------------------------------------------
   function exec_phase_build (id : builders) return Boolean
   is
      time_limit : execution_limit := max_time_without_output (build);
      passed : Boolean;
   begin
      passed := exec_phase (id          => id,
                            phase       => build,
                            time_limit  => time_limit,
                            skip_header => False,
                            skip_footer => True);
      if testing and then passed then
         passed := detect_leftovers_and_MIA (id, "preconfig", "between port configure and build");
      end if;
      if uselog then
         log_phase_end (id);
      end if;
      return passed;
   end exec_phase_build;


end PortScan.Buildcycle;
