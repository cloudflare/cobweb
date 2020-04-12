#
# Makefile gnucobol/extras
#
# Copyright (C) 2012, 2014, 2017 Free Software Foundation, Inc.
# Written by Roger While, Simon Sobisch
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

extrasdir = @COB_LIBRARY_PATH@
extras_DATA = CBL_OC_DUMP.$(COB_MODULE_EXT)

EXTRA_DIST = CBL_OC_DUMP.cob
CLEANFILES = $(extras_DATA)

SUFFIXES = .cob .$(COB_MODULE_EXT)
.cob.$(COB_MODULE_EXT):
	. $(top_builddir)/tests/atconfig && . $(top_builddir)/tests/atlocal extras-$@ \
		&& $$COBC -m -Wall -O -o $@ $<
