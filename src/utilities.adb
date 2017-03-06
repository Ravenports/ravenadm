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


   --------------------------------------------------------------------------------------------
   --  cpu_arch
   --------------------------------------------------------------------------------------------
   function cpu_arch (arch : supported_arch) return String is
   begin
      case arch is
         when x86_64  => return "x86_64";
         when i386    => return "i386";
         when aarch64 => return "aarch64";
      end case;
   end cpu_arch;


   --------------------------------------------------------------------------------------------
   --  mixed_opsys
   --------------------------------------------------------------------------------------------
   function valid_lower_opsys (candidate : String) return Boolean is
   begin
      for opsys in supported_opsys'Range loop
         if candidate = lower_opsys (opsys) then
            return True;
         end if;
      end loop;
      return False;
   end valid_lower_opsys;


   --------------------------------------------------------------------------------------------
   --  valid_cpu_arch
   --------------------------------------------------------------------------------------------
   function valid_cpu_arch (candidate : String) return Boolean is
   begin
      for arch in supported_arch'Range loop
         if candidate = cpu_arch (arch) then
            return True;
         end if;
      end loop;
      return False;
   end valid_cpu_arch;

end Utilities;
