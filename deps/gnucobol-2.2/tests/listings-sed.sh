#! /bin/sh
#
# listings-sed.sh gnucobol/tests
#
# Copyright (C) 2016-2017 Free Software Foundation, Inc.
# Written by Simon Sobisch, David Pitts
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

# Necessary sed replacements for unifying a listing

# Note: We cater for a maximum version string of 14:
#       Mayor (2) '.' Minor (2) '.' Patchlevel (8 - as some people place a date here)
# Note: We replace the date two times, as not all systems have %e modifier in C
#       and use %E in this case ("Mon Feb 04" instead of "Mon Feb  4").

date1=$(date +"%a %b %e")
date2=$(date +"%a %b %d")

if test "$3" = "once"; then
	sed \
	-e 's/GnuCOBOL [0-9][0-9]*\.[0-9][0-9]*[-devalphabetarc]*[0-9]*\.[0-9][0-9]*  */GnuCOBOL V.R.P               /g' \
	-e 's/[0-2][0-9]:[0-6][0-9]:[0-9][0-9] [0-9][0-9][0-9][0-9]$/HH:MM:SS YYYY/g' \
	-e 's/'"$date1"'/DDD MMM dd/g' \
	-e 's/'"$date2"'/DDD MMM dd/g' \
	<"$1" >"$2"
else
	sed \
	-e 's/GnuCOBOL [0-9][0-9]*\.[0-9][0-9]*[-devalphabetarc]*[0-9]*\.[0-9][0-9]*  */GnuCOBOL V.R.P          /g' \
	-e 's/[0-2][0-9]:[0-6][0-9]:[0-9][0-9] [0-9][0-9][0-9][0-9]/HH:MM:SS YYYY/g' \
	-e 's/'"$date1"'/DDD MMM dd/g' \
	-e 's/'"$date2"'/DDD MMM dd/g' \
	<"$1" >"$2"
fi


