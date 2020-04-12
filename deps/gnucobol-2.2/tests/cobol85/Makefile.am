#
# Makefile gnucobol/tests/cobol85
#
# Copyright (C) 2003-2012, 2015-2017 Free Software Foundation, Inc.
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

MODULES_ALL = NC SM IC SQ RL ST SG OB IF DB IX DBNOIX RW CM

# currently untested modules: RW and CM
if COB_MAKE_IX
MODULES = NC SM IC SQ RL ST SG OB IF DB IX
MODULES_RUN = NC_RUN SM_RUN IC_RUN SQ_RUN RL_RUN \
	ST_RUN SG_RUN OB_RUN IF_RUN DB_RUN IX_RUN
else
MODULES = NC SM IC SQ RL ST SG OB IF DBNOIX
MODULES_RUN = NC_RUN SM_RUN IC_RUN SQ_RUN RL_RUN \
	ST_RUN SG_RUN OB_RUN IF_RUN DBNOIX_RUN
endif

EXTRA_DIST = EXEC85.conf.in expand.pl report.pl summary.pl summary.txt \
	summarynoix.txt NC.txt SM.txt IC.txt SQ.txt RL.txt IX.txt \
	ST.txt SG.txt OB.txt IF.txt DB.txt DBNOIX.txt RW.txt

#CLEANFILES = EXEC85 summary.log

COB85DIR = "`cd $(srcdir) && pwd`"

# MAKEFLAGS = --no-print-directory

# targets that are only logical targets instead of files
.PHONY: test diff-summary diff $(MODULES_RUN)

NC_RUN: NC
	@cd NC && $(MAKE) -k test

SM_RUN: SM
	@cd SM && $(MAKE) -k test

IC_RUN: IC
	@cd IC && $(MAKE) -k test

SQ_RUN: SQ
	@cd SQ && $(MAKE) -k test

RL_RUN: RL
	@cd RL && $(MAKE) -k test

ST_RUN: ST
	@cd ST && $(MAKE) -k test

SG_RUN: SG
	@cd SG && $(MAKE) -k test

OB_RUN: OB
	@cd OB && $(MAKE) -k test

IF_RUN: IF
	@cd IF && $(MAKE) -k test

DB_RUN: DB
	@cd DB && $(MAKE) -k test

IX_RUN: IX
	@cd IX && $(MAKE) -k test

DBNOIX_RUN: DBNOIX
	@cd DBNOIX && $(MAKE) -k test

RW_RUN: RW
	@cd RW && $(MAKE) -k test

CM_RUN: CM
	@cd CM && $(MAKE) -k test


test: $(MODULES_RUN)
	@echo
	@perl $(srcdir)/summary.pl $(MODULES) > summary.log
	$(MAKE) diff-summary

summary.log:
	@perl $(srcdir)/summary.pl $(MODULES) > summary.log

diff: summary.log
	@for m in $(MODULES); do \
		(diff $(srcdir)/$$m.txt $$m/report.txt) \
	done
	$(MAKE) diff-summary

diff-summary:
	@echo "Comparing total test results"
if COB_MAKE_IX
	diff $(srcdir)/summary.txt summary.log
else
	diff $(srcdir)/summarynoix.txt summary.log
endif
	@echo

newcob.val.Z:
	@if ! test -f "newcob.val"; then \
		echo "Trying to download newcob.val.Z...";  \
		wget "http://www.itl.nist.gov/div897/ctg/suites/newcob.val.Z";  \
	fi

newcob.val: newcob.val.Z
	@if ! test -f "newcob.val"; then \
		if test "$(<D)" != "."; then \
			echo "Copying $< to current directory..." && cp "$<" .; \
		fi; \
		echo "Unpacking newcob.val.Z..."; \
		gunzip "$(<F)" || gzip -d "$(<F)" || uncompress "$(<F)"; \
	fi
	@if ! test -f "newcob.val"; then \
		echo "newcob.val missing, automatic download and uncompress did not work"; \
		exit 1; \
	fi

clean-local:
	rm -rf copy copyalt $(MODULES_ALL) EXEC85 summary.log EXEC85.cob

$(MODULES_ALL): newcob.val EXEC85$(EXEEXT) $(srcdir)/EXEC85.conf.in
	@echo "Building module directory $@ ..."
	@mkdir -p ./$@
	@echo "*SELECT-MODULE `echo $@ | sed 's/\(..\).*/\1/'`" > ./$@/EXEC85.conf
	@cat $(srcdir)/EXEC85.conf.in >> ./$@/EXEC85.conf
#	setting NEWCOB_VAL to the full name for EXEC85
#	(for make not supporting $(<D) this would be empty -> guess newcob.val is in builddir)
	@if test "$(<D)" = "." -o "$(<D)" = ""; then \
		export NEWCOB_VAL=../newcob.val; \
	else \
		export NEWCOB_VAL=$<; \
	fi; \
	. ../atconfig && . ../atlocal NIST_EXEC85_$@ && cd $@ &&  $$COBCRUN_DIRECT ../EXEC85
	@perl $(srcdir)/expand.pl $@/newcob.tmp $@
#	@rm -f ./$@/newcob.tmp ./$@/newcob.log ./$@/EXEC85.conf
	@echo "test:" > $@/Makefile
	@echo "	@echo" >> $@/Makefile
	@echo "	@echo \"Performing tests for module directory $@\"" >> $@/Makefile
	@echo "	@. ../../atconfig && . ../../atlocal NIST_$@   && perl $(COB85DIR)/report.pl" >> $@/Makefile
	@echo "	@echo" >> $@/Makefile
	@echo "	@echo \"Comparing test results for module directory $@\"" >> $@/Makefile
	@echo "	diff $(COB85DIR)/$@.txt report.txt || true" >> $@/Makefile
	@echo "	@echo" >> $@/Makefile
#	@echo "	@. ../../atconfig && . ../../atlocal NIST_O-$@ && perl $(COB85DIR)/report.pl -O" >> $@/Makefile
#	@echo "	diff $(COB85DIR)/$@.txt report.txt || true" >> $@/Makefile
	@echo "Finished module directory $@."

EXEC85$(EXEEXT): newcob.val
	@echo "Extracting EXEC85 program from newcob.val"
#	setting NEWCOB_VAL to the full name for the sed invocation
#	(for make not supporting $< this would be empty -> guess newcob.val is in builddir)
	@if test "$(<D)" = "." -o "$(<D)" = ""; then \
		NEWCOB_VAL=newcob.val; \
	else \
		NEWCOB_VAL=$<; \
	fi; \
	sed -e '/^\*END/,$$d' -e '1,/^\*HEADER/d' \
	-e 's/^002700.*/           "NEWCOB_VAL" ORGANIZATION LINE SEQUENTIAL./' \
	-e 's/^003000.*/           "newcob.tmp" ORGANIZATION LINE SEQUENTIAL./' \
	-e 's/^003100.*//' \
	-e 's/^003400.*/           "unused"./' \
	-e 's/^003700.*/           "newcob.log"./' \
	-e 's/^004000.*/           "EXEC85.conf" ORGANIZATION LINE SEQUENTIAL./' \
	"$$NEWCOB_VAL" > EXEC85.cob
	@echo "Compiling EXEC85 program"
	@. ../atconfig && . ../atlocal EXEC85 && $$COBC -std=cobol85 -x EXEC85.cob
