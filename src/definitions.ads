--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Definitions is

   pragma Pure;

   raven_version_major : constant String := "0";
   raven_version_minor : constant String := "00";
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
   jobs_per_cpu      : constant := 2;

   type supported_opsys is (dragonfly, freebsd, netbsd, openbsd, sunos, linux, macos);
   type supported_arch  is (x86_64, i386, aarch64);

   type cpu_range is range 1 .. 32;
   type scanners  is range cpu_range'First .. cpu_range'Last;
   type builders  is range cpu_range'First .. cpu_range'Last * jobs_per_cpu;

   -- Modify following with post-patch sed accordingly
   platform_type : constant supported_opsys := dragonfly;

end Definitions;
