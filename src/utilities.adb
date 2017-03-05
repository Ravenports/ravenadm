--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package body Utilities is

   --------------------------------------------------------------------------------------------
   --  lower_opsys
   --------------------------------------------------------------------------------------------
   function lower_opsys (opsys : supported_opsys) return String is
   begin
      case opsys is
         when dragonfly => return "dragonfly";
         when freebsd   => return "freebsd";
         when netbsd    => return "netbsd";
         when openbsd   => return "openbsd";
         when sunos     => return "sunos";
         when linux     => return "linux";
         when macos     => return "darwin";
      end case;
   end lower_opsys;


   --------------------------------------------------------------------------------------------
   --  mixed_opsys
   --------------------------------------------------------------------------------------------
   function mixed_opsys (opsys : supported_opsys) return String is
   begin
      case opsys is
         when dragonfly => return "DragonFly";
         when freebsd   => return "FreeBSD";
         when netbsd    => return "NetBSD";
         when openbsd   => return "OpenBSD";
         when sunos     => return "SunOS";
         when linux     => return "Linux";
         when macos     => return "Darwin";
      end case;
   end mixed_opsys;

end Utilities;
