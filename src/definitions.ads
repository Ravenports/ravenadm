--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Definitions is

   pragma Pure;

   raven_version_major : constant String := "0";
   raven_version_minor : constant String := "72";
   copyright_years     : constant String := "2015-2017";

   raven_tool        : constant String := "ravenadm";
   variant_standard  : constant String := "standard";
   contact_nobody    : constant String := "nobody";
   contact_automaton : constant String := "automaton";
   dlgroup_main      : constant String := "main";
   dlgroup_none      : constant String := "none";
   options_none      : constant String := "none";
   options_all       : constant String := "all";
   broken_all        : constant String := "all";
   boolean_yes       : constant String := "yes";
   homepage_none     : constant String := "none";
   spkg_complete     : constant String := "complete";
   spkg_docs         : constant String := "docs";
   spkg_examples     : constant String := "examples";
   ports_default     : constant String := "floating";
   default_ssl       : constant String := "libressl";
   default_mysql     : constant String := "oracle-5.7";
   default_lua       : constant String := "5.3";
   default_perl      : constant String := "5.26";
   default_pgsql     : constant String := "9.6";
   default_php       : constant String := "7.1";
   default_python3   : constant String := "3.6";
   default_ruby      : constant String := "2.4";
   default_tcltk     : constant String := "8.6";
   default_firebird  : constant String := "2.5";
   default_compiler  : constant String := "gcc7";
   compiler_version  : constant String := "7.2.0";
   previous_compiler : constant String := "7.1.0";
   binutils_version  : constant String := "2.29";
   previous_binutils : constant String := "2.28";
   arc_ext           : constant String := ".txz";
   jobs_per_cpu      : constant := 2;

   type supported_opsys is (dragonfly, freebsd, netbsd, openbsd, sunos, linux, macos);
   type supported_arch  is (x86_64, i386, aarch64);

   type cpu_range is range 1 .. 32;
   type scanners  is range cpu_range'First .. cpu_range'Last;
   type builders  is range cpu_range'First .. cpu_range'Last * jobs_per_cpu;
   type count_type is (total, success, failure, ignored, skipped);

   --  Modify following with post-patch sed accordingly
   platform_type  : constant supported_opsys := dragonfly;
   host_localbase : constant String := "/raven";
   host_pkg8      : constant String := host_localbase & "/sbin/pkg-static";
   ravenexec      : constant String := host_localbase & "/libexec/ravenexec";

end Definitions;
