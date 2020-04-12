#
# gnucobol/tests/cobol85/expand.pl
#
# Copyright (C) 2001-2012 Free Software Foundation, Inc.
# Written by Keisuke Nishida, Roger While
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

open (IN, shift) or die;
my $moddir = shift;

while (<IN>) {
	s/\x0d\x0a|\x0d|\x0a//g;
	if (/^      \*HEADER,([^,]*),([^, ]*)(,([^,]*),([^, ]*))?/) {
		my ($type, $prog, $subt, $subr) = ($1, $2, $4, $5);
		my $module = $moddir;
		my $name = '';
		if ($subt) {
			if ($subt eq "SUBPRG") {
				$name = "$subr.SUB";
			} elsif ($subt eq "SUBRTN") {
				$name = "lib/$subr.CBL";
				mkdir "$module/lib",0755 unless (-e "$module/lib");
			}
		} elsif ($type eq "COBOL") {
			$name = "$prog.CBL";
		} elsif ($type eq "DATA*") {
			if (substr($prog, 0, 2) eq $module) {
				$name = "$prog.DAT";
			}
		} elsif ($type eq "CLBRY") {
			if ($prog eq "ALTL1") {
				$module = "copyalt";
				$name = "ALTLB" unless (-e "copyalt/ALTLB");
			} else {
				$module = "copy";
				$name = "$prog" unless (-e "copy/$prog");
			}
		}
		if ($name) {
			mkdir $module,0755 unless (-e $module);
			open (OUT, "> $module/$name") or die;
			while (<IN>) {
				last if /^      \*END/;
				if ($type eq "DATA*" and length >= 80) {
					s/\x0d\x0a|\x0d|\x0a//g;
				}
				print OUT;
			}
		} else {
			while (<IN>) {
				last if /^      \*END/;
			}
		}
	}
}
