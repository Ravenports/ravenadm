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
   subtype ofield is String (1 .. 30);
   type desc_type is array (option) of ofield;

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

   optX1A : constant String := "[>]   Switch/create profiles (changes discarded)";
   optX1B : constant String := "[>]   Switch/create profiles";
   optX4B : constant String := "[<]   Delete alternative profile";
   optX2A : constant String := "[ESC] Exit without saving changes";
   optX3A : constant String := "[RET] Save changes (starred)";
   optX3B : constant String := "[RET] Exit";

   dupe   : PM.configuration_record;

   procedure clear_screen;
   procedure print_header;
   procedure print_menu (pristine : in out Boolean; extra_profiles : Boolean);
   procedure print_opt (opt : option; pristine : in out Boolean);
   procedure change_directory_option (opt : option; pristine : in out Boolean);
   procedure change_boolean_option   (opt : option; pristine : in out Boolean);
   procedure change_positive_option  (opt : option; pristine : in out Boolean);
   procedure delete_profile;
   procedure switch_profile;

end Configure;
