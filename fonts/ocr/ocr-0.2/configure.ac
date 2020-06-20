#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

#
# Configuration source for OCR fonts
#
# This file (configure.ac) is released to the public domain by its author,
# Matthew Skala, but be aware that the generated configure script may be
# subject to claims by the Free Software Foundation or others.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# Matthew Skala
# http://ansuz.sooke.bc.ca/
# mskala@ansuz.sooke.bc.ca
#

#
############################################################################
#
# Override Autoconf's argument parsing - must be done here,
# before AC_INIT has a chance to run
#

# General macro for editing a definition
m4_define([TSUKU_DEFINITION_SUBST],
          [m4_define([$1],
                     m4_bpatsubst(m4_dquote(m4_defn([$1])),
                                  [$2],
                                  [$3]))])

# Delete an option from the argument parser
m4_define([TSUKU_DELETE_CMDLINE_OPTION],
          [TSUKU_DEFINITION_SUBST([_AC_INIT_PARSE_ARGS],
                                  [\
  -$1[^)]*)\
.*;;\
  -$1=\*[^)]*)\
.*;;\
],
                                  [\
  # Deleted $1 option\
])
           TSUKU_DEFINITION_SUBST([_AC_INIT_PARSE_ARGS],
                                  [\
  -$1[^)]*)\
    # Obsolete.*\
.*;;\
],
                                  [\
  # Deleted obsolete $1 option\
])
           ])

# exec-prefix is special because it's an install dir option without "dir"
TSUKU_DELETE_CMDLINE_OPTION([exec-prefix])
TSUKU_DEFINITION_SUBST([_AC_INIT_PARSE_ARGS],
                         [for ac_var in\([^#]*\)\bexec_prefix\b],
                         [for ac_var in\1])

# Delete unwanted non-install-dir options
TSUKU_DELETE_CMDLINE_OPTION([gas])
TSUKU_DELETE_CMDLINE_OPTION([nfp])
TSUKU_DELETE_CMDLINE_OPTION([program-prefix])
TSUKU_DELETE_CMDLINE_OPTION([program-suffix])
TSUKU_DELETE_CMDLINE_OPTION([program-transform-name])
# may as well do this now - removes some processing and a help stanza
AC_DEFUN([AC_ARG_PROGRAM],[])
TSUKU_DELETE_CMDLINE_OPTION([-x])
TSUKU_DELETE_CMDLINE_OPTION([x-includes])
TSUKU_DELETE_CMDLINE_OPTION([x-libraries])

# Delete an install dir option from both parser and help
m4_define([TSUKU_DELETE_INSTDIR_OPTION],
          [TSUKU_DELETE_CMDLINE_OPTION([$1dir])
           TSUKU_DEFINITION_SUBST([_AC_INIT_PARSE_ARGS],
                                  [for ac_var in\([^#]*\).\b$1dir\b],
                                  [for ac_var in\1])
           TSUKU_DEFINITION_SUBST([_AC_INIT_HELP],
                                  [  --$1dir=DIR.*\
],
                                  [])])

# Delete nearly all of the default install dirs
TSUKU_DELETE_INSTDIR_OPTION([bin])
TSUKU_DELETE_INSTDIR_OPTION([data])
TSUKU_DELETE_INSTDIR_OPTION([dvi])
TSUKU_DELETE_INSTDIR_OPTION([html])
TSUKU_DELETE_INSTDIR_OPTION([include])
TSUKU_DELETE_INSTDIR_OPTION([info])
TSUKU_DELETE_INSTDIR_OPTION([lib])
TSUKU_DELETE_INSTDIR_OPTION([libexec])
TSUKU_DELETE_INSTDIR_OPTION([info])
TSUKU_DELETE_INSTDIR_OPTION([locale])
TSUKU_DELETE_INSTDIR_OPTION([localstate])
TSUKU_DELETE_INSTDIR_OPTION([man])
TSUKU_DELETE_INSTDIR_OPTION([oldinclude])
TSUKU_DELETE_INSTDIR_OPTION([ps])
TSUKU_DELETE_INSTDIR_OPTION([sbin])
TSUKU_DELETE_INSTDIR_OPTION([sharedstate])
TSUKU_DELETE_INSTDIR_OPTION([sysconf])

# generate a "case" pattern matching chunk
#   TSUKU_CASE_PATTERN_CHUNK(option,shortest,before,after)
m4_define([TSUKU_CASE_PATTERN_CHUNK],
  [$3$1$4[]m4_if([$1],[$2],[],
  [TSUKU_CASE_PATTERN_CHUNK(m4_substr($1,0,m4_eval(m4_len($1)-1)),
      [$2],[$3],[$4])])])

# Add a new installation directory
#   TSUKU_ADD_INSTDIR_OPTION(dirname,under,default,help,abbrev,casebefore)
m4_define([TSUKU_ADD_INSTDIR_OPTION],
          [TSUKU_DEFINITION_SUBST([_AC_INIT_PARSE_ARGS],
                                  [AC_SUBST(\[$2\].*)dnl\
],
                                  [\&AC_SUBST([$1dir],['${$2}/$3'])dnl\
])
           TSUKU_DEFINITION_SUBST([_AC_INIT_HELP],
                                  [\
\(.*--$2=.*\)\
],
                                  m4_expand([\
\1\
AS_HELP_STRING([--$1dir=DIR],m4_expand([$4 @<:@m4_toupper([$2])/$3@:>@]))\
]))
           TSUKU_DEFINITION_SUBST([_AC_INIT_PARSE_ARGS],
                                  [\
\(.*$6.*\)\
],
                                  [\
  -$1dir[]TSUKU_CASE_PATTERN_CHUNK([$1dir],[$5],[ | --],[])@:}@
    ac_prev=$1dir ;;
  -$1dir=*TSUKU_CASE_PATTERN_CHUNK([$1dir],[$5],[ | --],[=*])@:}@
    $1dir=$ac_optarg ;;\
\
\1\
])
           TSUKU_DEFINITION_SUBST([_AC_INIT_PARSE_ARGS],
                                  [for ac_var in\([^#]*.\b$2\)\b],
                                  [for ac_var in\1 $1dir])])

# Add our new directories
TSUKU_ADD_INSTDIR_OPTION([font],[datarootdir],[fonts],[fonts],
  [f],[obsolete gas option])
TSUKU_ADD_INSTDIR_OPTION([afm],[fontdir],[afm],
  [Adobe Font Metrics],
  [afm],[ --prefix ])
TSUKU_ADD_INSTDIR_OPTION([type1],[fontdir],[type1],
  [Postscript Type 1 fonts],
  [ty],[ --prefix ])
TSUKU_ADD_INSTDIR_OPTION([ttf],[fontdir],[TTF],
  [TrueType fonts],
  [tt],[ --prefix ])
TSUKU_ADD_INSTDIR_OPTION([otf],[fontdir],[OTF],
  [OpenType fonts],
  [ot],[ --prefix ])

# Reduce the ridiculous verbosity of Autoconf's help message
TSUKU_DEFINITION_SUBST([_AC_INIT_HELP],
                       [ to adapt to many kinds of systems],[])
TSUKU_DEFINITION_SUBST([_AC_INIT_HELP],
                       [of some of the useful variables],
                       [of some useful variables])
TSUKU_DEFINITION_SUBST([_AC_INIT_HELP],
                       [Defaults for the options are specified in brackets],
                       [Defaults are specified in brackets])
TSUKU_DEFINITION_SUBST([_AC_INIT_HELP],
                       [\barch.*-independent \b],
                       [])
TSUKU_DEFINITION_SUBST([_AC_INIT_HELP],
                       [\
.*(\[--exec-prefix=EPREFIX.*\
.*\])\(.\)[^:]*\
Fine tuning of the installation directories:\
],
                       [\1\
])

#
############################################################################
#
# Autoconf initialization
#
AC_PREREQ([2.63])
AC_INIT([OCR],
	[0.2], [mskala@ansuz.sooke.bc.ca], [ocr],
	[[http://tsukurimashou.sourceforge.jp/]])
AC_PRESERVE_HELP_ORDER
AC_CONFIG_AUX_DIR([.])
AM_INIT_AUTOMAKE([foreign color-tests dist-zip no-dist-gzip
  parallel-tests silent-rules])
AC_CONFIG_SRCDIR([msk-ocra.mp])
AC_CONFIG_MACRO_DIR([m4])
AC_REVISION([$Id: configure.ac 1723 2012-09-26 21:06:32Z mskala $])
AC_COPYRIGHT([public domain])
AC_SUBST([release_date],["September 27, 2012"])
AM_SILENT_RULES
#
############################################################################
#
# Checks for programs.
#
AC_PROG_GREP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_MKDIR_P
AC_PATH_PROG([AUTOCONF],[autoconf])
AC_PATH_PROG([AUTOMAKE],[automake])
AC_ARG_VAR([EXPECT],[Expect])
AC_PATH_PROG([EXPECT],[expect])
AM_CONDITIONAL([COND_EXPECT],[test '!' "x$EXPECT" = "x"])
AC_ARG_VAR([FONTFORGE],[Fontforge])
AC_PATH_PROG([FONTFORGE],[fontforge])
AX_CHECK_GNU_MAKE
AC_ARG_VAR([MPOST],[MetaPost])
AC_PATH_PROG([MPOST],[mpost])
AC_ARG_VAR([PERL],[Perl])
AC_PATH_PROG([PERL],[perl])
AC_ARG_VAR([T1ASM],[t1asm])
AC_ARG_VAR([XDVIPDFMX],[xdvipdfmx])
AC_PATH_PROG([XDVIPDFMX],[xdvipdfmx])
AC_ARG_VAR([XELATEX],[XeLaTeX])
AC_PATH_PROG([XELATEX],[xelatex])
#
############################################################################
#
# Check for parasite build
#
AC_MSG_CHECKING([whether we are building as a parasite])
AM_CONDITIONAL([COND_PARASITE],
  [grep Tsukurimashou ../Makefile > /dev/null 2> /dev/null])
AM_COND_IF([COND_PARASITE],
  [AC_MSG_RESULT([yes])
   T1ASM=../t1asm/t1asm],
  [AC_MSG_RESULT([no])
   AC_PATH_PROG([T1ASM],[t1asm])
  ])
AC_SUBST([T1ASM])
#
############################################################################
#
# Checks for system services
#
AC_SYS_LONG_FILE_NAMES
AX_AM_JOBSERVER
#
############################################################################
#
# User-settable options
#
# Define user options
#
AC_ARG_ENABLE([colour-build],
  [AS_HELP_STRING([--disable-colour-build],
    [turn off ANSI colour in build scripts])],
    [],
    [enable_colour_build=yes])
AC_ARG_ENABLE([color-build],[dnl],[],[enable_color_build=yes])
AC_ARG_ENABLE([docs],
  [AS_HELP_STRING([--disable-docs],
    [turn off PDF documentation])],
    [],
    [enable_docs=yes])
#
# Process user options
#
tsu_colour=yes
AS_IF([test "x$enable_colour_build" = "xyes"],
  [],[tsu_colour=no])
AS_IF([test "x$enable_color_build" = "xyes"],
  [],[tsu_colour=no])
AM_CONDITIONAL([COND_COLOUR],[test '!' "$tsu_colour" = no])
#
AM_CONDITIONAL([COND_DOCS],[test '!' "$enable_docs" = no])
AC_SUBST([enable_docs])
#
#
AS_IF([test "x$prefix" = "xNONE"],[prefix="$ac_default_prefix"])
#
AS_IF([test "x$enable_silent_rules" = "x"],
  [enable_silent_rules=yes
   AM_DEFAULT_VERBOSITY=0])
#
AC_MSG_CHECKING([whether FontForge accepts AddExtrema(1)])
echo '#!/usr/local/bin/fontforge' > ffaetest.pe
echo 'New();AddExtrema(1);' >> ffaetest.pe
AS_IF([$FONTFORGE ffaetest.pe >& /dev/null],
  [ae_arg_okay=yes],[ae_arg_okay=no])
AC_SUBST([ae_arg_okay])
AC_MSG_RESULT([$ae_arg_okay])
#
############################################################################
#
# Generate output
#
AC_SUBST([andt],[])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
