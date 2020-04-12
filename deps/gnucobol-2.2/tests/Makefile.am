#
# Makefile gnucobol/tests
#
# Copyright (C) 2003-2012, 2014-2017 Free Software Foundation, Inc.
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

SUBDIRS = cobol85

TESTSUITE = $(srcdir)/testsuite

testsuite_sources = \
	testsuite.src/used_binaries.at \
	testsuite.src/configuration.at \
	testsuite.src/syn_copy.at \
	testsuite.src/syn_definition.at \
	testsuite.src/syn_file.at \
	testsuite.src/syn_functions.at \
	testsuite.src/syn_misc.at \
	testsuite.src/syn_move.at \
	testsuite.src/syn_multiply.at \
	testsuite.src/syn_occurs.at \
	testsuite.src/syn_redefines.at \
	testsuite.src/syn_refmod.at \
	testsuite.src/syn_reportwriter.at \
	testsuite.src/syn_screen.at \
	testsuite.src/syn_set.at \
	testsuite.src/syn_subscripts.at \
	testsuite.src/syn_value.at \
	testsuite.src/listings.at \
	testsuite.src/run_accept.at \
	testsuite.src/run_extensions.at \
	testsuite.src/run_file.at \
	testsuite.src/run_functions.at \
	testsuite.src/run_fundamental.at \
	testsuite.src/run_initialize.at \
	testsuite.src/run_misc.at \
	testsuite.src/run_refmod.at \
	testsuite.src/run_reportwriter.at \
	testsuite.src/run_returncode.at \
	testsuite.src/run_subscripts.at \
	testsuite.src/data_binary.at \
	testsuite.src/data_display.at \
	testsuite.src/data_packed.at \
	testsuite.src/data_pointer.at \
	testsuite.src/numeric-dump.cob \
	testsuite.src/numeric-display.cob

EXTRA_DIST = $(srcdir)/package.m4 $(TESTSUITE) \
	$(srcdir)/testsuite.at $(srcdir)/listings-sed.sh \
	$(testsuite_sources)

DISTCLEANFILES = atconfig

check-local: $(TESTSUITE)
	@echo testing for working diff && diff - "$(TESTSUITE)" < "$(TESTSUITE)" \
		|| (echo "Error: no working 'diff' in PATH" && false)
	@$(TESTSUITE) $(TESTSUITEFLAGS) || rm -f testsuite.dir/at-job-fifo
	@rm -f testsuite.dir/at-job-fifo

clean-local:
	rm -rf *.dir *.log


# targets that are only logical targets instead of files
.PHONY: test checkall

test:
	@echo testing for perl && perl -v 1>/dev/null \
		|| (echo "Error: no working 'perl' in PATH" && false)
	cd cobol85 && $(MAKE) $(AM_MAKEFLAGS) test

checkall: check test

$(srcdir)/package.m4: $(top_srcdir)/configure.ac
	@echo "creating $(srcdir)/package.m4"
	@{								   \
	  echo '# Signature of the current package.';			   \
	  echo 'm4_define([AT_PACKAGE_NAME],	  [@PACKAGE_NAME@])';	   \
	  echo 'm4_define([AT_PACKAGE_TARNAME],	  [@PACKAGE_TARNAME@])';   \
	  echo 'm4_define([AT_PACKAGE_VERSION],	  [@PACKAGE_VERSION@])';   \
	  echo 'm4_define([AT_PACKAGE_STRING],	  [@PACKAGE_STRING@])';    \
	  echo 'm4_define([AT_PACKAGE_BUGREPORT], [@PACKAGE_BUGREPORT@])'; \
	  echo 'm4_define([AT_PACKAGE_URL],       [@PACKAGE_URL@])'; \
	} > $(srcdir)/package.m4

$(TESTSUITE): $(testsuite_sources) $(srcdir)/package.m4 $(srcdir)/testsuite.at
	autom4te --language=autotest -I $(srcdir) -I $(srcdir)/testsuite.src -o $(srcdir)/testsuite $(srcdir)/testsuite.at
	chmod +x $(srcdir)/testsuite

