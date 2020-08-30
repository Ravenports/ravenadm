--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

private with Ada.Calendar;

package PortScan.Scan is

   missing_index  : exception;
   bad_index_data : exception;
   bsheet_parsing : exception;
   populate_error : exception;

   --  Scan the entire conspiracy and unkindness directories in order with a single,
   --  non-recursive pass using up to 32 parallel scanners.  Return True on success
   function scan_entire_ports_tree (sysrootver : sysroot_characteristics) return Boolean;

   --  Scan entire conspiracy and unkindness directories using parallel scanners in order
   --  to compile a complete unique set of distribution files.  Used for distfile purging.
   --  Return True on success.
   function gather_distfile_set (sysrootver : sysroot_characteristics) return Boolean;

   --  Starting with a single port, recurse to determine a limited but complete
   --  dependency tree.  Repeated calls will augment already existing data.
   --  Return True on success
   function scan_single_port
     (namebase     : String;
      variant      : String;
      always_build : Boolean;
      sysrootver   : sysroot_characteristics;
      fatal        : out Boolean) return Boolean;

   --  This procedure causes the reverse dependencies to be calculated, and
   --  then the extended (recursive) reverse dependencies.  The former is
   --  used progressively to determine when a port is free to build and the
   --  latter sets the build priority.
   procedure set_build_priority (eliminate_orphan_depends : Boolean);

   --  Iterate through portlist and scan each individual port recursively.
   --  May be interrupted with a a singal.  Returns False if any port fails scan or if
   --  the process was interrupted by a signal.
   function scan_provided_list_of_ports
     (always_build : Boolean;
      sysrootver   : sysroot_characteristics) return Boolean;

   --  Linearly scan through entire conspiracy directory and generate a new index
   procedure generate_conspiracy_index (sysrootver : sysroot_characteristics);

   --  Using parallel scanners, go through entire source directory and generate
   --  all possible buildsheets.  Stop on first error.
   procedure generate_all_buildsheets (ravensource  : String);

   --  List every port to be built and the final tally.
   procedure display_results_of_dry_run;

   --  Scan distfiles directory, then purge all obsolete distfiles.
   procedure purge_obsolete_distfiles;

   --  Scan directory that contains the packages (*.tzst) and stores the
   --  file names in the container.  Returns False if no packages are found.
   function scan_repository (repository : String) return Boolean;

   --  Scans conspiracy + unkindness and generates a website at the given directory
   --  Generate a web page for every single active port
   --  Obsolete web pages are not removed, but no links will reference them
   function generate_entire_website
     (www_site   : String;
      sysrootver : sysroot_characteristics) return Boolean;

   --  Regenerate buildsheets for unkindness as necessary, and return True if index needs
   --  to be regenerated
   function unkindness_index_required return Boolean;

   --  Linearly scan through generated unkindness directory and generate a new index
   --  Returns false if exception hit
   function generate_unkindness_index (sysrootver : sysroot_characteristics) return Boolean;

   --  Walk through log directory and gather list of existing build logs
   procedure gather_list_of_build_logs;

   --  Calculate all possible build log names and remove from the actual list
   --  of build logs if found.  Use either conspiracy_variants or unkindness_variants
   --  based on value of main_tree boolean.
   procedure eliminate_current_logs (main_tree : Boolean);

   --  Removes the logs represented by the contents of log_list container
   procedure remove_obsolete_logs;

private

   package CAL renames Ada.Calendar;

   type dependency_type is (build, buildrun, runtime, extra_runtime);
   subtype LR_set is dependency_type range buildrun .. extra_runtime;
   type verdiff is (newbuild, rebuild, change);
   subtype AF is Integer range 0 .. 15;
   type disktype is mod 2**64;

   conspindex : constant String := "/Mk/Misc/conspiracy_variants";
   port_dates : constant String := "/Mk/Misc/port_dates";
   unkinindex : constant String := "/unkindness_variants";

   type port_dates_record is
      record
         creation  : CAL.Time;
         lastmod   : CAL.Time;
      end record;

   package dates_crate is new CON.Hashed_Maps
     (Key_Type        => HT.Text,
      Element_Type    => port_dates_record,
      Hash            => port_hash,
      Equivalent_Keys => HT.equivalent);

   type catalog_record is
      record
         lastmod64 : disktype;
         origin    : HT.Text;
      end record;

   function "<" (L, R : catalog_record) return Boolean;

   package catalog_crate is new CON.Ordered_Sets
     (Element_Type => catalog_record);

   --  subroutines for populate_port_data
   procedure prescan_ports_tree
     (conspiracy : String;
      unkindness : String;
      sysrootver : sysroot_characteristics);

   procedure prescan_conspiracy_index_for_distfiles
     (conspiracy : String;
      unkindness : String;
      sysrootver : sysroot_characteristics);

   procedure prescan_unkindness
     (unkindness    : String;
      compiled_BS   : String);

   procedure populate_port_data
     (conspiracy    : String;
      compiled_BS   : String;
      target        : port_index;
      always_build  : Boolean;
      sysrootver    : sysroot_characteristics);

   procedure parallel_deep_scan
     (conspiracy    : String;
      compiled_BS   : String;
      sysrootver    : sysroot_characteristics;
      success       : out Boolean;
      show_progress : Boolean);

   procedure populate_set_depends
     (target        : port_index;
      tuple         : String;
      dtype         : dependency_type);

   procedure populate_option
     (target        : port_index;
      option_name   : String;
      setting       : Boolean);

   procedure skeleton_compiler_data
     (conspiracy    : String;
      compiled_BS   : String;
      target        : port_index;
      sysrootver    : sysroot_characteristics);

   procedure iterate_reverse_deps;
   procedure iterate_drill_down;
   procedure prune_orphaned_build_depends;

   procedure drill_down (next_target : port_index; circular_flag : in out Boolean);

    --  some helper routines
   function get_max_lots return scanners;
   function convert_tuple_to_portkey (tuple : String) return String;
   function extract_subpackage (tuple : String) return String;
   function tohex (value : AF) return Character;
   function display_kmg (number : disktype) return String;

   --  Given a port ID, search for existing package in the packages directory
   --  If the exact package exists, return " (rebuild <version>)"
   --  If no package exists, return " (new)"
   --  If previous package exists, return " (<oldversion> => <version>)"
   function version_difference (id : port_id; kind : out verdiff) return String;

   --  Don't bother with parallel scan on unkindness, just get the distfiles now.
   procedure linear_scan_unkindness_for_distfiles (compiled_BS : String);

   --  Split conspiracy up equally between available scanners looking for distfiles
   procedure parallel_distfile_scan
     (conspiracy    : String;
      sysrootver    : sysroot_characteristics;
      success       : out Boolean;
      show_progress : Boolean);

   --  Set the portlist as if the user provided a list of every port via command line
   procedure set_portlist_to_everything;

   --  create a block of upstream ports.  Each line is semi-colon delimited.
   --  The first field is namebase-variant
   --  The second field is the relative link
   --  The third field is the tagline
   function blocked_text_block (port : port_index) return String;

   --  create an index block of all ravenports, sorted by last-modified order
   function catalog_row_block
     (crate    : dates_crate.Map;
      catcrate : catalog_crate.Set) return String;

   --  Loop to generate all webpages (includes custom ports)
   procedure serially_generate_web_pages
     (www_site   : String;
      sysrootver : sysroot_characteristics;
      success    : out Boolean);

   --  Single web page generator (called by parent loop)
   --  Returns true if web page generation was successful
   function generate_single_page
     (port       : port_index;
      workzone   : String;
      www_site   : String;
      conspiracy : String;
      unkindness : String;
      cdatetime  : CAL.Time;
      mdatetime  : CAL.Time;
      sysrootver : sysroot_characteristics)
      return Boolean;

   --  Generates searchable catalog index
   --  Returns true if web page generation was successful
   function generate_catalog_index
     (www_site : String;
      crate    : dates_crate.Map;
      catcrate : catalog_crate.Set) return Boolean;

   --  Extract dependencies, store them
   --  Web site generation requires two complete passes
   procedure store_port_dependencies
     (port       : port_index;
      conspiracy : String;
      unkindness : String;
      sysrootver : sysroot_characteristics);

   --  Slurp creation and last-modification timestamp of each port
   procedure scan_port_dates
     (conspiracy : String;
      crate      : in out dates_crate.Map;
      catcrate   : in out catalog_crate.Set);

   --  Slurp last-modification timestamps of each buildsheet in unkindness
   procedure scan_unkindness_buildsheets
     (comp_unkind : String;
      crate       : in out dates_crate.Map);

   --  Generate missing or obsolete buildsheets and remove from crate's checklist crate
   --  Returns true if index needs to be regenerated
   function generate_buildsheets
     (comp_unkind : String;
      crate       : in out dates_crate.Map) return Boolean;

   --  Returns true if unkindness source files are newer than existing buildsheet
   function unkindness_source_newer
     (BS_modtime  : Ada.Calendar.Time;
      bucket_name : String) return Boolean;

   --  Given the directory of the unkindness port files, generate a buildsheet at the
   --  given filename.  However, if BROKEN[ALL] is set, don't generate and return True;
   function generate_unkindness_buildsheet
     (unkindness_srcdir : String;
      new_buildsheet    : String) return Boolean;

end PortScan.Scan;
