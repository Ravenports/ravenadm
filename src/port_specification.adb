--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions; use Definitions;
with Ada.Text_IO;
with Ada.Characters.Latin_1;

package body Port_Specification is

   package TIO renames Ada.Text_IO;
   package LAT renames Ada.Characters.Latin_1;

   --------------------------------------------------------------------------------------------
   --  initialize
   --------------------------------------------------------------------------------------------
   procedure initialize (specs : out Portspecs) is
   begin
      specs.namebase := HT.blank;
      specs.version  := HT.blank;
      specs.revision := 0;
      specs.epoch    := 0;
      specs.keywords.Clear;
      specs.variants.Clear;
      specs.taglines.Clear;
      specs.contacts.Clear;
      specs.dl_sites.Clear;
      specs.distfiles.Clear;
      specs.dist_subdir := HT.blank;
      specs.df_index.Clear;
      specs.subpackages.Clear;
      specs.ops_avail.Clear;
      specs.variantopts.Clear;
      specs.exc_opsys.Clear;
      specs.inc_opsys.Clear;
      specs.exc_arch.Clear;

      specs.last_set := so_initialized;
   end initialize;


   --------------------------------------------------------------------------------------------
   --  set_single_string
   --------------------------------------------------------------------------------------------
   procedure set_single_string
     (specs : in out Portspecs;
      field : spec_field;
      value : String) is
   begin
      if HT.contains (S => value, fragment => " ") then
         raise contains_spaces;
      end if;
      case field is
         when sp_namebase =>
            if specs.last_set /= so_initialized then
               raise misordered with field'Img;
            end if;
            specs.namebase := HT.SUS (value);
            specs.last_set := so_namebase;
         when sp_version =>
            if specs.last_set /= so_namebase then
               raise misordered with field'Img;
            end if;
            specs.version := HT.SUS (value);
            specs.last_set := so_version;
         when sp_distsubdir =>
            if specs.last_set /= so_distfiles and then
              specs.last_set /= so_contacts
            then
               raise misordered with field'Img;
            end if;
            specs.dist_subdir := HT.SUS (value);
            specs.last_set := so_distsubdir;
         when others =>
            raise wrong_type with field'Img;
      end case;

   end set_single_string;


   --------------------------------------------------------------------------------------------
   --  append_list
   --------------------------------------------------------------------------------------------
   procedure append_list
     (specs : in out Portspecs;
      field : spec_field;
      value : String)
   is
      procedure verify_entry_is_post_options;

      text_value : HT.Text := HT.SUS (value);

      procedure verify_entry_is_post_options is
      begin
         if spec_order'Pos (specs.last_set) < spec_order'Pos (so_opts_avail) then
            raise misordered with field'Img;
         end if;
      end verify_entry_is_post_options;
   begin
      if HT.contains (S => value, fragment => " ") then
         raise contains_spaces;
      end if;
      case field is
         when sp_keywords =>
            if specs.last_set /= so_keywords and then
              specs.last_set /= so_epoch and then
              specs.last_set /= so_revision and then
              specs.last_set /= so_version
            then
               raise misordered with field'Img;
            end if;
            if not keyword_is_valid (value) then
               raise wrong_value with "Keyword '" & value & "' is not recognized";
            end if;
            if specs.keywords.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            specs.keywords.Append (text_value);
            specs.last_set := so_keywords;
         when sp_variants =>
            if specs.last_set /= so_variants and then
              specs.last_set /= so_keywords
            then
               raise misordered with field'Img;
            end if;
            if specs.variants.Is_Empty and then
              value /= variant_standard
            then
               raise wrong_value with "First variant must be '" & variant_standard & "'";
            end if;
            if value'Length > 15 then
               raise wrong_value with "'" & value & "' value is too long (15-char limit)";
            end if;
            if specs.variants.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            specs.variants.Append (text_value);
            specs.last_set := so_variants;
         when sp_contacts =>
            if specs.last_set /= so_contacts and then
              specs.last_set /= so_taglines
            then
               raise misordered with field'Img;
            end if;
            if not specs.all_taglines_defined then
               raise wrong_value with "Every variant must have SDESC definition.";
            end if;
            if not specs.contacts.Is_Empty then
               if specs.contacts.Contains (HT.SUS (contact_nobody)) then
                  raise wrong_value with "contact '" & contact_nobody & "' must be solitary";
               end if;
               if specs.contacts.Contains (HT.SUS (contact_automaton)) then
                  raise wrong_value with "contact '" & contact_automaton & "' must be solitary";
               end if;
               if value = contact_nobody or else value = contact_automaton then
                  raise wrong_value with "contact '" & value & "' must be solitary";
               end if;
            end if;
            if value /= contact_nobody and then
              value /= contact_automaton and then
              not (HT.contains (value, "_") and then
                   HT.contains (value, "[") and then
                   HT.contains (value, "@") and then
                   HT.contains (value, "]") and then
                  value (value'Last) = LAT.Right_Square_Bracket)
            then
               raise wrong_value with "incorrect contact format of '" & value & "'";
            end if;
            if specs.contacts.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            specs.contacts.Append (text_value);
            specs.last_set := so_contacts;
         when sp_distfiles =>
            if specs.last_set /= so_distfiles and then
              specs.last_set /= so_dl_sites
            then
               raise misordered with field'Img;
            end if;
            if specs.distfiles.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            if not HT.contains (value, ":") then
               raise wrong_value with "No download group prefix present in distfile";
            end if;
            specs.distfiles.Append (text_value);
            specs.last_set := so_distfiles;
         when sp_df_index =>
            if specs.last_set /= so_df_index and then
              specs.last_set /= so_distsubdir and then
              specs.last_set /= so_distfiles
            then
               raise misordered with field'Img;
            end if;
            if specs.df_index.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            declare
               mynum : Integer := Integer'Value (value);
            begin
               if mynum < 1 or else
                 mynum > Integer (specs.distfiles.Length)
               then
                  raise wrong_value with "df_index value '" & value & "' does not match " &
                    "distfile indices";
               end if;
            exception
               when Constraint_Error =>
                  raise wrong_value with "df_index value '" & value & "' is not an integer";
            end;
            specs.df_index.Append (text_value);
            specs.last_set := so_df_index;
         when sp_opts_avail =>
            if specs.last_set /= so_opts_avail and then
              specs.last_set /= so_subpackages
            then
               raise misordered with field'Img;
            end if;
            if specs.ops_avail.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            if value = options_none then
               if not specs.ops_avail.Is_Empty then
                  raise wrong_value with "'" & options_none & "' must be set first and alone";
               end if;
            else
               if HT.uppercase (value) /= value then
                  raise wrong_value with "option value '" & value & "' is not capitalized";
               end if;
            end if;
            if value'Length > 14 then
               raise wrong_value with "'" & value & "' name is too long (14-char limit)";
            end if;
            specs.ops_avail.Append (text_value);
            specs.last_set := so_opts_avail;
         when sp_exc_opsys =>
            verify_entry_is_post_options;
            if not specs.inc_opsys.Is_Empty then
               raise wrong_value with "NOT_FOR_OPSYS can't be used after ONLY_FOR_OPSYS";
            end if;
            if specs.exc_opsys.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            if not lower_opsys_is_valid (value) then
               raise wrong_value with "opsys '" & value & "' is not valid.";
            end if;
            specs.exc_opsys.Append (text_value);
         when sp_inc_opsys =>
            verify_entry_is_post_options;
            if not specs.exc_opsys.Is_Empty then
               raise wrong_value with "ONLY_FOR_OPSYS can't be used after NOT_FOR_OPSYS";
            end if;
            if specs.inc_opsys.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            if not lower_opsys_is_valid (value) then
               raise wrong_value with "opsys '" & value & "' is not valid.";
            end if;
            specs.inc_opsys.Append (text_value);
         when sp_exc_arch =>
            verify_entry_is_post_options;
            if specs.exc_arch.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            if not arch_is_valid (value) then
               raise wrong_value with "'" & value & "' is not a valid architecture.";
            end if;
            specs.exc_arch.Append (text_value);
         when others =>
            raise wrong_type with field'Img;
      end case;

   end append_list;


   --------------------------------------------------------------------------------------------
   --  append_array
   --------------------------------------------------------------------------------------------
   procedure establish_group
     (specs : in out Portspecs;
      field : spec_field;
      group : String)
   is
      text_group : HT.Text := HT.SUS (group);
      initial_rec : group_list;
   begin
      if HT.contains (S => group, fragment => " ") then
         raise contains_spaces;
      end if;
      initial_rec.group := text_group;
      case field is
         when sp_dl_groups =>
            if specs.last_set /= so_dl_groups and then
              specs.last_set /= so_contacts
            then
               raise misordered with field'Img;
            end if;
            if specs.dl_sites.Is_Empty then
               if group /= dlgroup_main and then
                 group /= dlgroup_none
               then
                  raise wrong_value with "First download group must be '" & dlgroup_main &
                    "' or '" & dlgroup_none & "'";
               end if;
            else
               if group = dlgroup_none then
                  raise wrong_value with "download group '" & group &
                    "' follows group definition";
               end if;
               if group = dlgroup_main then
                  raise wrong_value with "'" & group & "' download group must be " &
                    "defined earlier";
               end if;
            end if;
            if group'Length > 15 then
               raise wrong_value with "'" & group & "' value is too long (15-char limit)";
            end if;
            if specs.dl_sites.Contains (text_group) then
               raise dupe_list_value with group;
            end if;
            specs.dl_sites.Insert (Key      => text_group,
                                   New_Item => initial_rec);
            specs.last_set := so_dl_groups;
         when sp_subpackages =>
            --  variant, order, length and uniqueness already checked
            --  don't updatee last_set either
            specs.subpackages.Insert (Key      => text_group,
                                      New_Item => initial_rec);
         when sp_vopts =>
            --  variant, order, length and uniqueness already checked
            --  don't updatee last_set either
            specs.variantopts.Insert (Key      => text_group,
                                      New_Item => initial_rec);
         when others =>
            raise wrong_type with field'Img;
      end case;
   end establish_group;


   --------------------------------------------------------------------------------------------
   --  append_array
   --------------------------------------------------------------------------------------------
   procedure append_array
     (specs : in out Portspecs;
      field : spec_field;
      key   : String;
      value : String;
      allow_spaces : Boolean)
   is
      procedure grow (Key : HT.Text; Element : in out group_list);

      text_key   : HT.Text := HT.SUS (key);
      text_value : HT.Text := HT.SUS (value);

      procedure grow (Key : HT.Text; Element : in out group_list) is
      begin
         Element.list.Append (text_value);
      end grow;
   begin
      if not allow_spaces and then
        HT.contains (S => value, fragment => " ")
      then
         raise contains_spaces;
      end if;
      case field is
         when sp_taglines =>
            if specs.last_set /= so_taglines and then
              specs.last_set /= so_variants
            then
               raise misordered with field'Img;
            end if;
            if specs.taglines.Contains (text_key) then
               raise dupe_spec_key with key & " (SDESC)";
            end if;
            --  SDESC requirements checked by caller.  Assume string is legal.
            specs.taglines.Insert (Key      => text_key,
                                   New_Item => text_value);
            specs.last_set := so_taglines;
         when sp_dl_sites =>
            if specs.last_set /= so_dl_sites and then
              specs.last_set /= so_dl_groups
            then
               raise misordered with field'Img;
            end if;
            if not specs.dl_sites.Contains (text_key) then
               raise missing_group with key;
            end if;
            if specs.dl_sites.Element (text_key).list.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            specs.dl_sites.Update_Element (Position => specs.dl_sites.Find (text_key),
                                           Process  => grow'Access);
            specs.last_set := so_dl_sites;
         when sp_subpackages =>
            if spec_order'Pos (specs.last_set) > spec_order'Pos (so_subpackages) or else
              spec_order'Pos (specs.last_set) < spec_order'Pos (so_dl_groups)
            then
               raise misordered with field'Img;
            end if;
            if not specs.subpackages.Contains (text_key) then
               raise missing_group with key;
            end if;
            if specs.subpackages.Element (text_key).list.Contains (text_value) then
               raise dupe_list_value with value;
            end if;
            specs.subpackages.Update_Element (Position => specs.subpackages.Find (text_key),
                                              Process  => grow'Access);
            specs.last_set := so_subpackages;
         when sp_vopts =>
            if specs.last_set /= so_vopts and then
              specs.last_set /= so_opts_avail
            then
               raise misordered with field'Img;
            end if;
            if not specs.variantopts.Contains (text_key) then
               raise missing_group with key;
            end if;
            declare
               strlast : Natural;
               WON     : HT.Text;
               WOFF    : HT.Text;
            begin
               if HT.trails (value, "=ON") then
                  strlast := value'Last - 3;
                  WON     := text_value;
                  WOFF    := HT.SUS (value (value'First .. strlast) & "=OFF");
               elsif HT.trails (value, "=OFF") then
                  strlast := value'Last - 4;
                  WOFF    := text_value;
                  WON     := HT.SUS (value (value'First .. strlast) & "=ON");
               else
                  raise wrong_value with "'" & value & "' doesn't end in '=ON' or '=OFF'";
               end if;
               if specs.variantopts.Element (text_key).list.Contains (WON) or else
                 specs.variantopts.Element (text_key).list.Contains (WOFF)
               then
                  raise dupe_list_value with value;
               end if;
               if not specs.ops_avail.Contains (HT.SUS (value (value'First .. strlast))) then
                  raise wrong_value with "'" & value (value'First .. strlast)
                    & "' was not present in OPTIONS_AVAILABLE";
               end if;
            end;
            specs.variantopts.Update_Element (Position => specs.variantopts.Find (text_key),
                                              Process  => grow'Access);
            specs.last_set := so_vopts;
         when others =>
            raise wrong_type with field'Img;
      end case;
   end append_array;


   --------------------------------------------------------------------------------------------
   --  set_natural_integer
   --------------------------------------------------------------------------------------------
   procedure set_natural_integer
     (specs : in out Portspecs;
      field : spec_field;
      value : Natural) is
   begin
      case field is
         when sp_revision =>
            if specs.last_set /= so_version then
               raise misordered with field'Img;
            end if;
            specs.revision := value;
            specs.last_set := so_revision;
         when sp_epoch =>
            if specs.last_set /= so_revision and then
              specs.last_set /= so_version
            then
               raise misordered with field'Img;
            end if;
            specs.epoch := value;
            specs.last_set := so_epoch;
         when others =>
            raise wrong_type with field'Img;
      end case;
   end set_natural_integer;


   --------------------------------------------------------------------------------------------
   --  variant_exists
   --------------------------------------------------------------------------------------------
   function variant_exists (specs : Portspecs; variant : String) return Boolean is
   begin
      return specs.variants.Contains (Item => HT.SUS (variant));
   end variant_exists;


   --------------------------------------------------------------------------------------------
   --  group_exists
   --------------------------------------------------------------------------------------------
   function group_exists
     (specs : Portspecs;
      field : spec_field;
      group : String) return Boolean
   is
      text_group : HT.Text := HT.SUS (group);
   begin
      case field is
         when sp_dl_sites =>
            return specs.dl_sites.Contains (text_group);
         when others => return False;
      end case;
   end group_exists;


   --------------------------------------------------------------------------------------------
   --  all_taglines_defined
   --------------------------------------------------------------------------------------------
   function all_taglines_defined (specs : Portspecs) return Boolean
   is
      procedure check (position : string_crate.Cursor);

      all_present : Boolean := True;

      procedure check (position : string_crate.Cursor)
      is
         variant : HT.Text := string_crate.Element (position);
      begin
         if not specs.taglines.Contains (variant) then
            all_present := False;
         end if;
      end check;
   begin
      specs.variants.Iterate (Process => check'Access);
      return all_present;
   end all_taglines_defined;


   --------------------------------------------------------------------------------------------
   --  check_variants
   --------------------------------------------------------------------------------------------
   function check_variants (specs : Portspecs) return String
   is
      procedure check (position : string_crate.Cursor);
      procedure check_option (position : string_crate.Cursor);

      result  : HT.Text := HT.blank;
      variant : HT.Text;

      --  OPTIONS_AVAILABLE process
      procedure check_option (position : string_crate.Cursor)
      is
         option : HT.Text := string_crate.Element (position);
      begin
         if HT.IsBlank (result) then
            declare
               step  : String  := HT.USS (option);
               WON   : HT.Text := HT.SUS (step & "=ON");
               WOFF  : HT.Text := HT.SUS (step & "=OFF");
            begin
               TIO.Put_Line (step & " " & HT.USS (WON));
               if not specs.variantopts.Element (variant).list.Contains (WON) and then
                 not specs.variantopts.Element (variant).list.Contains (WOFF)
               then
                  result := HT.SUS (HT.USS (variant) & ":" & step);
               end if;
            end;
         end if;
      end check_option;

      --  variant process
      procedure check (position : string_crate.Cursor) is
      begin
         variant := string_crate.Element (position);
         if HT.IsBlank (result) then
            if HT.USS (variant) /= variant_standard then

               --  It's impossible that variantopts doesn't have variant, so don't test
               specs.ops_avail.Iterate (Process => check_option'Access);
            end if;
         end if;
      end check;
   begin
      specs.variants.Iterate (Process => check'Access);
      return HT.USS (result);
   end check_variants;


   --------------------------------------------------------------------------------------------
   --  lower_opsys_is_valid
   --------------------------------------------------------------------------------------------
   function lower_opsys_is_valid (test_opsys : String) return Boolean is
   begin
      return (test_opsys = "dragonfly" or else
              test_opsys = "freebsd" or else
              test_opsys = "netbsd" or else
              test_opsys = "openbsd" or else
              test_opsys = "linux" or else
              test_opsys = "sunos" or else
              test_opsys = "macos");
   end lower_opsys_is_valid;


   --------------------------------------------------------------------------------------------
   --  arch_is_valid
   --------------------------------------------------------------------------------------------
   function arch_is_valid (test_arch : String) return Boolean is
   begin
      return (test_arch = "x86_64" or else
              test_arch = "aarch64" or else
              test_arch = "i386");
   end arch_is_valid;


   --------------------------------------------------------------------------------------------
   --  keyword_is_valid
   --------------------------------------------------------------------------------------------
   function keyword_is_valid (keyword : String) return Boolean is
   begin
      return (keyword = "accessibility" or else
              keyword = "archivers" or else
              keyword = "astro" or else
              keyword = "audio" or else
              keyword = "benchmarks" or else
              keyword = "biology" or else
              keyword = "cad" or else
              keyword = "comms" or else
              keyword = "converters" or else
              keyword = "databases" or else
              keyword = "deskutils" or else
              keyword = "devel" or else
              keyword = "dns" or else
              keyword = "editors" or else
              keyword = "emulators" or else
              keyword = "finance" or else
              keyword = "ftp" or else
              keyword = "games" or else
              keyword = "graphics" or else
              keyword = "irc" or else
              keyword = "lang" or else
              keyword = "mail" or else
              keyword = "math" or else
              keyword = "misc" or else
              keyword = "multimedia" or else
              keyword = "net" or else
              keyword = "net_im" or else
              keyword = "net_mgmt" or else
              keyword = "net_p2p" or else
              keyword = "news" or else
              keyword = "print" or else
              keyword = "raven" or else
              keyword = "science" or else
              keyword = "security" or else
              keyword = "shells" or else
              keyword = "sysutils" or else
              keyword = "textproc" or else
              keyword = "www" or else
              keyword = "x11" or else
              keyword = "x11_clocks" or else
              keyword = "x11_drivers" or else
              keyword = "x11_fm" or else
              keyword = "x11_fonts" or else
              keyword = "x11_servers" or else
              keyword = "x11_toolkits" or else
              keyword = "x11_wm" or else
              keyword = "ada" or else
              keyword = "c++" or else
              keyword = "csharp" or else
              keyword = "java" or else
              keyword = "javascript" or else
              keyword = "lisp" or else
              keyword = "perl" or else
              keyword = "php" or else
              keyword = "python" or else
              keyword = "ruby" or else
              keyword = "scheme" or else
              keyword = "Arabic" or else
              keyword = "Chinese" or else
              keyword = "French" or else
              keyword = "German" or else
              keyword = "Italian" or else
              keyword = "Japanese" or else
              keyword = "Russian" or else
              keyword = "Spanish" or else
              keyword = "Vietnamese");
   end keyword_is_valid;


   --------------------------------------------------------------------------------------------
   --  dump_specification
   --------------------------------------------------------------------------------------------
   procedure dump_specification (specs : Portspecs)
   is
      procedure print_item (position : string_crate.Cursor);
      procedure print_item (position : def_crate.Cursor);
      procedure dump (position : list_crate.Cursor);
      procedure print_vector_list (thelabel : String; thelist : spec_field);
      procedure print_group_list  (thelabel : String; thelist : spec_field);
      procedure print_single (thelabel : String; thelist : spec_field);

      array_label : Positive;

      procedure print_item (position : string_crate.Cursor)
      is
         index : Natural := string_crate.To_Index (position);
      begin
         if index > 1 then
            TIO.Put (" ");
         end if;
         TIO.Put (HT.USS (string_crate.Element (position)));
      end print_item;

      procedure print_item (position : def_crate.Cursor) is
      begin
         case array_label is
            when 1 => TIO.Put ("SDESC[");
            when others => null;
         end case;
         TIO.Put_Line (HT.USS (def_crate.Key (position)) & LAT.Right_Square_Bracket &
                         LAT.HT & LAT.HT & HT.USS (def_crate.Element (position)));
      end print_item;

      procedure dump (position : list_crate.Cursor) is
      begin
         TIO.Put ("   " & HT.USS (list_crate.Element (position).group) & "  " & LAT.HT & LAT.HT);
         list_crate.Element (position).list.Iterate (Process => print_item'Access);
         TIO.Put (LAT.LF);
      end dump;

      procedure print_vector_list (thelabel : String; thelist : spec_field)
      is
         labellen : Natural := thelabel'Length;
      begin
         TIO.Put (thelabel & LAT.Equals_Sign & LAT.HT);
         if labellen < 7 then
            TIO.Put (LAT.HT & LAT.HT);
         elsif labellen < 15 then
            TIO.Put (LAT.HT);
         end if;
         case thelist is
            when sp_exc_opsys  => specs.exc_opsys.Iterate (Process => print_item'Access);
            when sp_inc_opsys  => specs.inc_opsys.Iterate (Process => print_item'Access);
            when sp_exc_arch   => specs.exc_arch.Iterate (Process => print_item'Access);
            when sp_opts_avail => specs.ops_avail.Iterate (Process => print_item'Access);
            when sp_df_index   => specs.df_index.Iterate (Process => print_item'Access);
            when sp_distfiles  => specs.distfiles.Iterate (Process => print_item'Access);
            when sp_contacts   => specs.contacts.Iterate (Process => print_item'Access);
            when sp_variants   => specs.variants.Iterate (Process => print_item'Access);
            when sp_keywords   => specs.keywords.Iterate (Process => print_item'Access);
            when others => null;
         end case;
         TIO.Put (LAT.LF);
      end print_vector_list;

      procedure print_group_list (thelabel : String; thelist : spec_field) is
      begin
         TIO.Put_Line (thelabel & LAT.Colon);
         case thelist is
            when sp_vopts       => specs.variantopts.Iterate (Process => dump'Access);
            when sp_subpackages => specs.subpackages.Iterate (Process => dump'Access);
            when sp_dl_sites    => specs.dl_sites.Iterate (Process => dump'Access);
            when others => null;
         end case;
      end print_group_list;

      procedure print_single (thelabel : String; thelist : spec_field)
      is
         labellen : Natural := thelabel'Length;
      begin
         TIO.Put (thelabel & LAT.Equals_Sign & LAT.HT);
         if labellen < 7 then
            TIO.Put (LAT.HT & LAT.HT);
         elsif labellen < 15 then
            TIO.Put (LAT.HT);
         end if;
         case thelist is
            when sp_namebase   => TIO.Put_Line (HT.USS (specs.namebase));
            when sp_version    => TIO.Put_Line (HT.USS (specs.version));
            when sp_revision   => TIO.Put_Line (HT.int2str (specs.revision));
            when sp_epoch      => TIO.Put_Line (HT.int2str (specs.epoch));
            when sp_distsubdir => TIO.Put_Line (HT.USS (specs.dist_subdir));
            when others => null;
         end case;
      end print_single;
   begin
      print_single      ("NAMEBASE", sp_namebase);
      print_single      ("VERSION",  sp_version);
      print_single      ("REVISION", sp_revision);
      print_single      ("EPOCH",    sp_epoch);
      print_vector_list ("KEYWORDS", sp_keywords);
      print_vector_list ("VARIANTS", sp_variants);
      array_label := 1;
      specs.taglines.Iterate (Process => print_item'Access);
      print_vector_list ("CONTACTS", sp_contacts);
      print_group_list  ("SITES", sp_dl_sites);
      print_vector_list ("DISTFILE", sp_distfiles);
      print_single      ("DIST_SUBDIR=", sp_distsubdir);
      print_vector_list ("DF_INDEX", sp_df_index);
      print_group_list  ("SPKGS", sp_subpackages);
      print_vector_list ("OPTIONS_AVAILABLE", sp_opts_avail);
      print_group_list  ("VOPTS", sp_subpackages);
      print_vector_list ("ONLY_FOR_OPSYS", sp_inc_opsys);
      print_vector_list ("NOT_FOR_OPSYS", sp_exc_opsys);
      print_vector_list ("NOT_FOR_ARCH", sp_exc_arch);

   end dump_specification;

end Port_Specification;
