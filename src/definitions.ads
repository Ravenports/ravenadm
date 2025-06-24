--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Definitions is

   pragma Pure;

   raven_version_major : constant String := "3";
   raven_version_minor : constant String := "45";
   copyright_years     : constant String := "2015-2025";

   raven_tool        : constant String := "ravenadm";
   variant_standard  : constant String := "std";
   contact_nobody    : constant String := "nobody";
   contact_automaton : constant String := "automaton";
   dlgroup_main      : constant String := "main";
   dlgroup_none      : constant String := "none";
   options_none      : constant String := "none";
   options_all       : constant String := "all";
   broken_all        : constant String := "all";
   boolean_yes       : constant String := "yes";
   homepage_none     : constant String := "none";
   spkg_complete     : constant String := "set";
   spkg_docs         : constant String := "docs";
   spkg_examples     : constant String := "examples";
   spkg_nls          : constant String := "nls";
   spkg_man          : constant String := "man";
   spkg_dev          : constant String := "dev";
   spkg_info         : constant String := "info";
   ports_default     : constant String := "floating";
   default_ssl       : constant String := "libressl";
   default_mysql     : constant String := "oracle-8.0";
   default_lua       : constant String := "5.4";
   default_perl      : constant String := "5.38";
   default_pgsql     : constant String := "16";
   default_php       : constant String := "8.3";
   default_python3   : constant String := "3.12";
   default_ruby      : constant String := "3.3";
   default_tcltk     : constant String := "8.6";

   default_binutils  : constant String := "ravensys-binutils:" & variant_standard;
   binutils_version  : constant String := "2.43.1";
   previous_binutils : constant String := "2.41";

   ports_compiler    : constant String := "ravensys-gcc";
   compiler_version  : constant String := "14.2.0";
   previous_compiler : constant String := "13.2.0";

   arc_ext           : constant String := ".rvn";
   jobs_per_cpu      : constant := 2;
   task_stack_limit  : constant := 10_000_000;

   type supported_opsys is (dragonfly, freebsd, netbsd, openbsd, sunos, linux, macos, midnightbsd);
   type supported_arch  is (x86_64, i386, aarch64);

   type cpu_range is range 1 .. 64;
   type scanners  is range cpu_range'First .. cpu_range'Last;
   type builders  is range cpu_range'First .. cpu_range'Last * jobs_per_cpu;
   type count_type is (total, success, failure, ignored, skipped);

   --  Modify following with post-patch sed accordingly
   platform_type  : constant supported_opsys := dragonfly;
   host_localbase : constant String := "/raven";
   raven_var      : constant String := "/var/ravenports";
   host_rvn       : constant String := host_localbase & "/sbin/rvn";
   ravenexec      : constant String := host_localbase & "/libexec/ravenexec";  --  FreeBSD only

end Definitions;
