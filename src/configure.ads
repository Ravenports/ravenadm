--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

private with Ada.Characters.Latin_1;
private with Parameters;

package Configure is

   menu_error : exception;

   --  Interactive configuration menu
   procedure launch_configure_menu;

private

   package LAT renames Ada.Characters.Latin_1;
   package PM  renames Parameters;

   indent : constant String (1 ..  3) := (others => LAT.Space);

   type option is range 1 .. 16;
   type default is range 1 .. 10;
   subtype ofield is String (1 .. 30);
   type desc_type is array (option) of ofield;
   type default_type is array (default) of ofield;

   descriptions : constant desc_type :=
     (
      "[A] System root directory     ",
      "[B] Toolchain directory       ",
      "[C] Localbase directory       ",
      "[D] Conspiracy directory      ",
      "[E] Custom ports directory    ",
      "[F] Distfiles directory       ",
      "[G] Packages directory        ",
      "[H] Compiler cache directory  ",
      "[I] Build base directory      ",
      "[J] Build logs directory      ",
      "[K] Num. concurrent builders  ",
      "[L] Max. jobs per builder     ",
      "[M] Avoid use of tmpfs        ",
      "[N] Always record options     ",
      "[O] Display using ncurses     ",
      "[P] Fetch prebuilt packages   "
     );

   version_desc : constant default_type :=
     (
      "[A] Firebird SQL server       ",
      "[B] Lua (language)            ",
      "[C] MySQL-workalike server    ",
      "[D] Perl (language)           ",
      "[E] PHP (language)            ",
      "[F] PostgreSQL server         ",
      "[G] Python 3 (language)       ",
      "[H] Ruby (language)           ",
      "[I] SSL/TLS library           ",
      "[J] TCL/TK toolkit            "
     );

   optX5A : constant String := "[V]   Set version defaults (e.g. perl, ruby, mysql ...)";
   optX1A : constant String := "[>]   Switch/create profiles (changes discarded)";
   optX1B : constant String := "[>]   Switch/create profiles";
   optX4B : constant String := "[<]   Delete alternative profile";
   optX2A : constant String := "[ESC] Exit without saving changes";
   optX3A : constant String := "[RET] Save changes (starred)";
   optX3B : constant String := "[RET] Exit";

   dupe   : PM.configuration_record;

   version_A : constant String := "2.5:3.0";  --  Firebird SQL
   version_B : constant String := "5.2:5.3";  --  Lua
   version_C : constant String := "oracle-5.5:oracle-5.6:oracle-5.7:" &
                                  "mariadb-10.1:mariadb-10.2:" &
                                  "percona-5.5:percona-5.6:percona-5.7:" &
                                  "galera-5.5:galera-5.6:galera-5.7";
   version_D : constant String := "5.22:5.24";  --  Perl
   version_E : constant String := "5.6:7.0:7.1";  --  php
   version_F : constant String := "9.2:9.3:9.4:9.5:9.6";  --  postgresql
   version_G : constant String := "3.4:3.5:3.6";  --  python3
   version_H : constant String := "2.3:2.4";      --  ruby
   version_I : constant String := "openssl:openssl-devel:libressl:libressl-devel";
   version_J : constant String := "8.5:8.6";  -- TCL/TK

   procedure clear_screen;
   procedure print_header;
   procedure print_menu (pristine : in out Boolean; extra_profiles : Boolean);
   procedure print_opt (opt : option; pristine : in out Boolean);
   procedure change_directory_option (opt : option; pristine : in out Boolean);
   procedure change_boolean_option   (opt : option; pristine : in out Boolean);
   procedure change_positive_option  (opt : option; pristine : in out Boolean);
   procedure delete_profile;
   procedure switch_profile;
   procedure move_to_defaults_menu (pristine : in out Boolean);
   procedure print_default (def : default; pristine : in out Boolean);
   procedure update_version
     (def      : default;
      choices  : String;
      pristine : in out Boolean;
      label    : String);

end Configure;
