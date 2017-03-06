--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Definitions is

   pragma Pure;

   raven_version_major : constant String := "0";
   raven_version_minor : constant String := "00";
   copyright_years     : constant String := "2017";

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

   type supported_opsys is (dragonfly, freebsd, netbsd, openbsd, sunos, linux, macos);
   type supported_arch  is (x86_64, i386, aarch64);

end Definitions;
