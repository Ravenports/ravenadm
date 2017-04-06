--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions; use Definitions;

package Port_Specification.Transform is

   --  Given:
   --     variant
   --     standard arch
   --     opsys (operating system)
   --     osrelease (string)
   --     osmajor   (string)
   --     osversion (string)
   --     and the current option settings (if variant is not "standard"):
   --  Apply all the changes dictated by option helpers and the IGNORE calculation
   procedure apply_directives
     (specs         : in out Portspecs;
      arch_standard : supported_arch;
      osmajor       : String);

   --  For non-standard variants, set options defaults as directed by VOPTS
   --  For standard variant, set options default by OPT_ON values
   procedure set_option_defaults
     (specs         : in out Portspecs;
      variant       : String;
      opsys         : supported_opsys;
      arch_standard : supported_arch;
      osrelease     : String);

   procedure set_option_to_default_values (specs : in out Portspecs);

   procedure set_outstanding_ignore
     (specs         : in out Portspecs;
      variant       : String;
      opsys         : supported_opsys;
      arch_standard : supported_arch;
      osrelease     : String;
      osmajor       : String);

   procedure shift_extra_patches
     (specs         : Portspecs;
      extract_dir   : String);

private

   BUILD    : constant String := "build";
   BUILDRUN : constant String := "buildrun";
   RUN      : constant String := "run";

   --  Returns true if all '0' .. '9', and also single '.' if it's not in first or last place.
   function release_format (candidate : String) return Boolean;

   --  Given X, X.Y or X.YY, returns X*100, X*100+Y or X*100+YY
   function centurian_release (release : String) return Natural;

   --  Implement less-than and greater-than OS Major comparision
   function LTE (gen_release, spec_release : String) return Boolean;
   function GTE (gen_release, spec_release : String) return Boolean;

   procedure apply_cpe_module
     (specs         : in out Portspecs;
      arch_standard : supported_arch;
      osmajor       : String);

   procedure apply_gmake_module     (specs : in out Portspecs);
   procedure apply_libtool_module   (specs : in out Portspecs);
   procedure apply_libiconv_module  (specs : in out Portspecs);
   procedure apply_info_presence    (specs : in out Portspecs);
   procedure apply_ccache           (specs : in out Portspecs);
   procedure apply_pkgconfig_module (specs : in out Portspecs);
   procedure apply_ncurses_module   (specs : in out Portspecs);
   procedure apply_perl_module      (specs : in out Portspecs);
   procedure apply_bison_module     (specs : in out Portspecs);

   procedure apply_gettext_runtime_module (specs : in out Portspecs);
   procedure apply_gettext_tools_module   (specs : in out Portspecs);

   procedure apply_curly_bracket_conversions (specs : in out Portspecs);
   procedure apply_cbc_string_crate (crate : in out string_crate.Vector);

   function argument_present (specs : Portspecs; module, argument : String) return Boolean;
   function no_arguments_present (specs : Portspecs; module : String) return Boolean;

end Port_Specification.Transform;
