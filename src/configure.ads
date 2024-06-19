--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions; use Definitions;
private with Ada.Characters.Latin_1;
private with Parameters;

package Configure is

   menu_error : exception;

   --  Interactive configuration menu
   procedure launch_configure_menu;

   --  Print out configuration value
   --  If input not 'A' - 'Q', return "Error: Input must be character 'A'...'Q'"
   procedure print_configuration_value (option : Character);

private

   package LAT renames Ada.Characters.Latin_1;
   package PM  renames Parameters;

   indent : constant String (1 ..  3) := (others => LAT.Space);

   type option is range 1 .. 17;
   type default is range 1 .. 9;
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
      "[G] Profile directory (logs+) ",
      "[H] Packages directory        ",
      "[I] Compiler cache directory  ",
      "[J] Build base directory      ",
      "[K] Num. concurrent builders  ",
      "[L] Max. jobs per builder     ",
      "[M] Avoid use of tmpfs        ",
      "[N] Fetch prebuilt packages   ",
      "[O] Display using ncurses     ",
      "[P] Always record options     ",
      "[Q] Assume default options    "
     );

   version_desc : constant default_type :=
     (
      "[A] Lua (language)            ",
      "[B] MySQL-workalike server    ",
      "[C] Perl (language)           ",
      "[D] PHP (language)            ",
      "[E] PostgreSQL server         ",
      "[F] Python 3 (language)       ",
      "[G] Ruby (language)           ",
      "[H] SSL/TLS library           ",
      "[I] TCL/TK toolkit            "
     );

   optX5A : constant String := "[V]   Set version defaults (e.g. perl, ruby, mysql ...)";
   optX1A : constant String := "[>]   Switch/create profiles (changes discarded)";
   optX1B : constant String := "[>]   Switch/create profiles";
   optX4B : constant String := "[<]   Delete alternative profile";
   optX2A : constant String := "[ESC] Exit without saving changes";
   optX3A : constant String := "[RET] Save changes (starred)  ";
   optX3B : constant String := "[RET] Exit  ";

   dupe   : PM.configuration_record;

   version_A : constant String := "5.2:5.3:" & default_lua;
   version_B : constant String := "oracle-5.7:" & default_mysql & ":oracle-8.4:mariadb-10.4:" &
                                  "mariadb-10.5:mariadb-10.6:mariadb-10.11:mariadb-11";
   version_C : constant String := default_perl & ":5.38";
   version_D : constant String := "8.1:" & default_php & ":8.3";
   version_E : constant String := "12:13:14:" & default_pgsql & ":16";
   version_F : constant String := "3.11:" & default_python3;
   version_G : constant String := "3.1:" & default_ruby & ":3.3";
   version_H : constant String := "openssl10:openssl11:openssl30:" &
                                   default_ssl & ":libressl-devel";
   version_I : constant String := "8.5:" & default_tcltk;

   procedure clear_screen;
   procedure print_header;
   procedure print_opt (opt : option; pristine : in out Boolean);
   procedure change_directory_option (opt : option; pristine : in out Boolean);
   procedure change_boolean_option   (opt : option; pristine : in out Boolean);
   procedure change_positive_option  (opt : option; pristine : in out Boolean);
   procedure delete_profile;
   procedure switch_profile;
   procedure move_to_defaults_menu (pristine_def : in out Boolean);
   procedure print_default (def : default; pristine_def : in out Boolean);
   procedure update_version
     (def     : default;
      choices : String;
      label   : String);
   procedure print_menu
     (pristine       : in out Boolean;
      extra_profiles : Boolean;
      pristine_def   : Boolean);

end Configure;
