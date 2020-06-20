# ===========================================================================
#      http://www.gnu.org/software/autoconf-archive/ax_am_jobserver.html
# ===========================================================================
#
# SYNOPSIS
#
#   AX_AM_JOBSERVER([default_value])
#
# DESCRIPTION
#
#   Enables the use of make's jobserver for the purpose of parallel building
#   by passing the -j option to make.
#
#   The option --enable-jobserver is added to configure which can accept a
#   yes, no, or an integer. The integer is the number of separate jobs to
#   allow. If 'yes' is given, then the is assumed to be one more than the
#   number of CPUs (determined through AX_COUNT_CPUS). If the value of no is
#   given, then the jobserver is disabled. The default value is given by the
#   first argument of the macro, or 'yes' if the argument is omitted.
#
#   This macro makes use of AX_AM_MACROS, so you must add the following line
#
#     @INC_AMINCLUDE@
#
#   to your Makefile.am files.
#
# MODIFIED
#
#   By Matthew Skala, <mskala@ansuz.sooke.bc.ca>, 24 September 2012, to
#   make the -j flag conditional, removing a "-jN forced in submake"
#   warning when used in nested packages.  This probably requires GNU Make,
#   but that was likely required already.  It also requires a variable named
#   percent, because literal percent signs seem to break things here.
#
# LICENSE
#
#   Copyright (c) 2008 Michael Paul Bailey <jinxidoru@byu.net>
#
#   Copying and distribution of this file, with or without modification, are
#   permitted in any medium without royalty provided the copyright notice
#   and this notice are preserved. This file is offered as-is, without any
#   warranty.

#serial 8

AC_DEFUN([AX_AM_JOBSERVER], [
    AC_REQUIRE([AX_COUNT_CPUS])
    AC_REQUIRE([AX_AM_MACROS])
    AC_ARG_ENABLE( jobserver,
    [  --enable-jobserver@<:@=no/yes/@%:@@:>@  use up to @%:@ make jobs, yes=CPUs+1 @<:@m4_ifval([$1],[$1],[yes])@:>@],,
    [enable_jobserver=m4_ifval([$1],[$1],[yes])])
    AS_IF([test "x$enable_jobserver" = "xyes"],
      [enable_jobserver=$CPU_COUNT
       ((enable_jobserver++))])
    m4_pattern_allow(AM_MAKEFLAGS)
    AS_IF([test "x$enable_jobserver" != "xno"],
      [AX_ADD_AM_MACRO([AM_MAKEFLAGS += \$(if \$(filter -j\$(percent),\$(MAKEFLAGS)),,-j$enable_jobserver )
])])
])
