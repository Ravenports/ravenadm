--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions;  use Definitions;

with HelperText;

package Parameters is

   package HT renames HelperText;

   type configuration_record is
      record
         profile         : HT.Text;
         dir_system      : HT.Text;
         dir_repository  : HT.Text;
         dir_packages    : HT.Text;
         dir_portsdir    : HT.Text;
         dir_distfiles   : HT.Text;
         dir_buildbase   : HT.Text;
         dir_logs        : HT.Text;
         dir_ccache      : HT.Text;
         dir_options     : HT.Text;
         num_builders    : builders;
         jobs_limit      : builders;
         tmpfs_workdir   : Boolean;
         tmpfs_localbase : Boolean;
         avec_ncurses    : Boolean;
         defer_prebuilt  : Boolean;
      end record;

   configuration  : configuration_record;
   active_profile : HT.Text;

end Parameters;
