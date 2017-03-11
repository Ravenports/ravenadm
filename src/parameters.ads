--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions;  use Definitions;

with HelperText;

package Parameters is

   package HT renames HelperText;

   no_ccache     : constant String := "disabled";
   raven_confdir : constant String := host_localbase & "/etc/ravenadm";

   type configuration_record is
      record
         profile         : HT.Text;
         dir_system      : HT.Text;
         dir_repository  : HT.Text;
         dir_packages    : HT.Text;
         dir_conspiracy  : HT.Text;
         dir_distfiles   : HT.Text;
         dir_buildbase   : HT.Text;
         dir_logs        : HT.Text;
         dir_ccache      : HT.Text;
         dir_localbase   : HT.Text;
         num_builders    : builders;
         jobs_limit      : builders;
         avoid_tmpfs     : Boolean;
         avec_ncurses    : Boolean;
         defer_prebuilt  : Boolean;
      end record;

   configuration  : configuration_record;
   active_profile : HT.Text;

   --  Maybe a previously valid directory path has been removed.  This
   --  function returns true when all the paths still work.
   --  The configuration must be loaded before it's run, of course.
   function all_paths_valid return Boolean;

end Parameters;
