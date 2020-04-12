#!/bin/sh
# cobcinfo.sh gnucobol/doc
#
# Copyright (C) 2010,2012, 2016-2017 Free Software Foundation, Inc.
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

# use GREP from configure, passed when called from Makefile
GREP_ORIG="$GREP";
if test "x$GREP" = "x"; then GREP=grep; fi

# test for grep -A
if test "$1" != "fixtimestamps"; then
   $GREP -A2 test /dev/null 2>/dev/null
   if test "$?" -ne 1; then
      GREP=ggrep
      $GREP -A2 test /dev/null 2>/dev/null
      if test "$?" -ne 1; then
         echo "error: grep not working, re-run with GREP=/path/to/gnu-grep"
         echo "       GREP is currently \"$GREP_ORIG\""
         exit 1
      fi
   fi
fi


# Make sure to source atconfig/atlocal before running this shell
# to use the currently compiled version of cobc
# (is done in the makefile for automated calls)

# Function for creating the actual file and check
_create_file () {
	echo "$0: creating $1"
	case "$1" in
		"cbhelp.tex")
			echo "@verbatim"               > $1
			cobc -q --help                 >>$1
			echo "@end verbatim"           >>$1
			;;
		"cbchelp.tex")
			echo "@verbatim"               > $1
			cobcrun -q --help              >>$1
			echo "@end verbatim"           >>$1
			;;
		"cbrese.tex")
			echo "@verbatim"               > $1
			cobc -q --list-reserved        >>$1
			echo "@end verbatim"           >>$1
			;;
		"cbintr.tex")
			echo "@verbatim"               > $1
			cobc -q --list-intrinsics      >>$1
			echo "@end verbatim"           >>$1
			;;
		"cbsyst.tex")
			echo "@verbatim"               > $1
			cobc -q --list-system          >>$1
			echo "@end verbatim"           >>$1
			;;
		"cbmnem.tex")
			echo "@verbatim"               > $1
			cobc -q --list-mnemonics       >>$1
			echo "@end verbatim"           >>$1
			;;
		"cbconf.tex")
			echo "@verbatim"               > $1
			cat $confdir/default.conf \
			| $GREP -A9999 "http://www.gnu.org/licenses/" \
			| tail -n +2 \
			                               >>$1
			echo "@end verbatim"           >>$1
			;;
		"cbrunt.tex")
			# First section, as it is formatted different
			cat $confdir/runtime.cfg \
			| $GREP -A400 -m1 "##" \
			| cut -b2- \
			| sed -e 's/^#\( .*\)/@section\1\n/g' \
			      -e 's/^ //g' \
			      -e 's/{/@{/g' \
			      -e 's/}/@}/g' \
			      -e 's/  \([^ ].*\)  / @code{\1} /g' \
			      -e 's/  \([^ ].*\)$/ @code{\1}/g' \
			      -e 's/^$/@\*/g' \
			                               > $1
			lines=$(expr 20 + $(cat $1 | wc -l))
			# All other sections
			echo "@verbatim"               >>$1
			tail -n +$lines $confdir/runtime.cfg \
			| cut -b2- \
			| sed -e 's/^#\( .*\)/@end verbatim\n@section\1\n@verbatim/g' \
			       -e 's/^ //g' \
			                               >>$1
			echo "@end verbatim"           >>$1
			;;
	esac
}

docdir=`dirname $0`
confdir=$docdir/../config

case "$1" in
	"")
		_create_file "cbhelp.tex"
		_create_file "cbchelp.tex"
		_create_file "cbrese.tex"
		_create_file "cbintr.tex"
		_create_file "cbsyst.tex"
		_create_file "cbmnem.tex"
		_create_file "cbconf.tex"
		_create_file "cbrunt.tex"
		;;
	"help")
		_create_file "cbhelp.tex"
		_create_file "cbchelp.tex"
		;;
	"lists")
		_create_file "cbrese.tex"
		_create_file "cbintr.tex"
		_create_file "cbsyst.tex"
		_create_file "cbmnem.tex"
		;;
	"conf")
		_create_file "cbconf.tex"
		_create_file "cbrunt.tex"
		;;
	"cbhelp.tex")
		_create_file "cbhelp.tex"
		;;
	"cbchelp.tex")
		_create_file "cbchelp.tex"
		;;
	"cbrese.tex")
		_create_file "cbrese.tex"
		;;
	"cbintr.tex")
		_create_file "cbintr.tex"
		;;
	"cbsyst.tex")
		_create_file "cbsyst.tex"
		;;
	"cbmnem.tex")
		_create_file "cbmnem.tex"
		;;
	"cbconf.tex")
		_create_file "cbconf.tex"
		;;
	"cbrunt.tex")
		_create_file "cbrunt.tex"
		;;
	"fixtimestamps")
		echo $0: touch tex-includes
		for file in $docdir/*.tex; do
			echo " touch $file"
			touch $file
		done
		echo $0: touch tex-results
		for file in $docdir/gnucobol.*; do
			if test "$file" = "$docdir/gnucobol.texi"; then continue; fi
			echo " touch $file"
			touch $file
		done
		;;
	*)
		echo "$0: ERROR: called with unsupported option $1"
		exit 1;
esac
