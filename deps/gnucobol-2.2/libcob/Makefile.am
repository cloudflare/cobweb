#
# Makefile gnucobol/libcob
#
# Copyright (C) 2003-2012, 2014, 2017 Free Software Foundation, Inc.
# Written by Keisuke Nishida, Roger While, Simon Sobisch
#
# This file is part of GnuCOBOL.
#
# The GnuCOBOL compiler is free software: you can redistribute it
# and/or modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# GnuCOBOL is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GnuCOBOL.  If not, see <http://www.gnu.org/licenses/>.

lib_LTLIBRARIES = libcob.la
libcob_la_SOURCES = common.c move.c numeric.c strings.c \
	fileio.c call.c intrinsic.c termio.c screenio.c cobgetopt.c
#ToDo Man Page
#dist_man_MANS = libcob.3
#COBCRUN = $(top_builddir)/bin/cobcrun$(EXEEXT)

libcob_la_CFLAGS = $(AM_CFLAGS) -I$(top_srcdir) $(CODE_COVERAGE_CFLAGS)
libcob_la_LIBADD = $(LIBCOB_LIBS) $(CODE_COVERAGE_LIBS)
libcob_la_LDFLAGS = $(COB_FIX_LIBTOOL) -version-info 4:0:0 -no-undefined
AM_LDFLAGS = $(COB_FIX_LIB)

EXTRA_DIST = coblocal.h system.def
pkgincludedir = $(includedir)/libcob
pkginclude_HEADERS = common.h cobgetopt.h exception.def

# FIXME: needs GNU make, even if not enabled
# Add rules for code-coverage testing, as defined by AX_CODE_COVERAGE
#CODE_COVERAGE_BRANCH_COVERAGE=1
#@ CODE_COVERAGE_RULES @
#CODE_COVERAGE_LCOV_OPTIONS =  --no-external

#libcob.3: $(top_srcdir)/bin/cobcrun.c $(top_srcdir)/configure.ac | $(COBCRUN)
#	$(HELP2MAN) --output=$@ --info-page=$(PACKAGE) $(COBCRUN) -specialflag

install-data-hook:
	rm -f $(includedir)/libcob/byteswap.h
	rm -f $(includedir)/libcob/call.h
	rm -f $(includedir)/libcob/fileio.h
	rm -f $(includedir)/libcob/termio.h
	rm -f $(includedir)/libcob/screenio.h
	rm -f $(includedir)/libcob/move.h
	rm -f $(includedir)/libcob/numeric.h
	rm -f $(includedir)/libcob/strings.h
	rm -f $(includedir)/libcob/intrinsic.h
	rm -f $(includedir)/libcob/codegen.h
	rm -f $(includedir)/libcob/system.def
