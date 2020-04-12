#
# gnucobol/tests/cobol85/report.pl
#
# Copyright (C) 2001-2012, 2016-2017 Free Software Foundation, Inc.
# Written by Keisuke Nishida, Roger While, Simon Sobisch, Edward Hart
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

use strict;
use warnings;

$SIG{INT} = sub { die "\nInterrupted\n" };
$SIG{QUIT} = sub { die "\nInterrupted\n" };
$SIG{PIPE} = sub { die "\nInterrupted\n" };
$SIG{TERM} = sub { die "\nInterrupted\n" };

my $opt = shift;

my $compile;
my $compile_module;

# change to 1 if executable doesn't work / cobcrun test should be done
my $force_cobcrun = 0;

my $cobc = $ENV{"COBC"};
my $cobcrun = $ENV{"COBCRUN"};
my $cobcrun_direct = $ENV{"COBCRUN_DIRECT"};

if ($opt) {
	$opt = "-std=cobol85 $opt"
} else {
	$opt = "-std=cobol85"
}

if (defined $cobc) {
	$cobc = "$cobc $opt";
} else {
	$cobc = "cobc $opt";
}

if (!defined $cobcrun) {
	$cobcrun = "cobcrun";
}

if (defined $cobcrun_direct) {
    	$cobcrun_direct = "$cobcrun_direct ";
} else {
	$cobcrun_direct = "";
}

# temporary directory (used for fifos)
my $tmpdir = $ENV{"TMPDIR"};
if (!defined $tmpdir) {
	$tmpdir = $ENV{"TEMP"};
	if (!defined $tmpdir) {
		$tmpdir = $ENV{"TMP"};
		if (!defined $tmpdir) {
			$tmpdir = "/tmp";
		}
	}
}


$compile_module = "$cobc -m";
if ($force_cobcrun) {
	$compile = $compile_module;
} else {
	$compile = "$cobc -x";
}

my $num_progs = 0;
my $test_skipped = 0;
my $compile_error = 0;
my $execute_error = 0;

my $total_all = 0;
my $total_pass = 0;
my $total_fail = 0;
my $total_deleted = 0;
my $total_inspect = 0;
my $total_ok = 0;
my $ret = 0;
my $db103m = 0;

$ENV{"COB_SWITCH_1"} = "ON";
$ENV{"COB_SWITCH_2"} = "OFF";

$ENV{"COB_DISABLE_WARNINGS"} = "Y";

# DB103M should be executed twice with differing
# runtime DEBUG switch.
# Dealt with lower down in the code

# Skip DB203A if no ISAM configured
my %skip;
if ($ENV{'COB_HAS_ISAM'} eq "no") {
	$skip{DB203A} = 1;
}

# OBNC1M tests the STOP literal statement and requires user input with a final kill.
my %raw_input;
$raw_input{OBNC1M} = "\n\n\n\n\n\n\n\n\003"; # 8 newlines + kill character
my %to_kill;
$to_kill{OBNC1M} = 1;

# NC114M test the compiler listing along to other parts.
my %cobc_flags;
$cobc_flags{NC114M} = "-t NC114M.lst";

# NC302M tests the compiler flagging of obsolete features, including STOP literal.
$cobc_flags{NC302M} = "-Wobsolete";
$raw_input{NC302M} = "\n";

# DB304M tests the compiler flagging of obsolete features
$cobc_flags{DB304M} = "-Wobsolete";

# Compile only programs

# The following tests are for compiler flagging and cannot run without abends.
# TO-DO: automatically check cobc emits the right number of warnings with
# -Wobsolete (ignore high subset checking).
my %comp_only;
$comp_only{NC401M} = 1;
$comp_only{RL301M} = 1;
$comp_only{RL401M} = 1;
$comp_only{IC401M} = 1;
$comp_only{IX301M} = 1;
$comp_only{IX401M} = 1;
$comp_only{SQ303M} = 1;
$comp_only{SQ401M} = 1;
$comp_only{ST301M} = 1;

# Until RECEIVE is implemented, DB205A contains an infinite loop.
$comp_only{DB205A} = 1;

# Programs that do not produce any meaningful test results
# However they must execute successfully
my %no_output;
$no_output{NC110M} = 1;
$no_output{NC214M} = 1;
$no_output{OBSQ3A} = 1;
$no_output{ST102A} = 1;
$no_output{ST109A} = 1;
$no_output{ST110A} = 1;
$no_output{ST112M} = 1;
$no_output{ST113M} = 1;
$no_output{ST115A} = 1;
$no_output{ST116A} = 1;
$no_output{ST120A} = 1;
$no_output{ST122A} = 1;
$no_output{ST123A} = 1;
$no_output{DB301M} = 1;
$no_output{DB302M} = 1;
$no_output{DB303M} = 1;
$no_output{DB305M} = 1;
$no_output{IF402M} = 1;

$cobc_flags{SM206A} = "-fdebugging-line";

# Programs that won't run correctly with enabled runtime checks
# TODO for later: only deactivate specific checks by -fno-ec-...
my %no_debug;
$no_debug{DB101A} = 1;
$no_debug{DB104A} = 1;
$no_debug{DB201A} = 1;
$no_debug{DB202A} = 1;
$no_debug{DB203A} = 1;
$no_debug{DB204A} = 1;

# Programs that need to be "visual" inspected
# NC113M: inspected additional to normal tests for output of hex values
# SQ101M, SQ201M, SQ207M, SQ208M, SQ209M, SQ210M: send report.log to printer and check result
#

open (LOG, "> report.txt") or die;
print LOG "Filename    total pass fail deleted inspect\n";
print LOG "--------    ----- ---- ---- ------- -------\n";

my $in;
foreach $in (glob("lib/*.CBL")) {
	print "$compile_module $in\n";
	$ret = system ("trap 'exit 77' INT QUIT TERM PIPE; $compile_module $in");
	if ($ret != 0) {
		if (($ret >> 8) == 77) {
			die "Interrupted\n";
		}
		print "Unexpected status $ret for module $in\n";
	}
}

foreach $in (sort (glob("*.{CBL,SUB}"))) {
	my $exe = $in;
	my $cmd;
	my $subt;

	$exe =~ s/\.CBL//;
	$exe =~ s/\.SUB//;

	printf LOG "%-12s", $in;
	if ($skip{$exe}) {
		$test_skipped++;
		print LOG "  ----- test skipped -----\n";
		next;
	}

	if (-e "./$exe.DAT") {
		if ($force_cobcrun) {
			$cmd = "$cobcrun $exe < $exe.DAT";
		} else {
			$cmd = "$cobcrun_direct./$exe < $exe.DAT";
		}
	} else {
		if ($force_cobcrun) {
			$cmd = "$cobcrun $exe";
		} else {
			$cmd = "$cobcrun_direct./$exe";
		}
	}

	$num_progs++;
	my $compile_current = $compile;
	if ($cobc_flags{$exe}) {
		$compile_current = "$compile_current $cobc_flags{$exe}";
	}
	if ($exe =~ /^SM/) {
		$compile_current = "$compile_current -I ../copy";
	}
	if (!$no_debug{$exe}) {
		$compile_current = "$compile_current -debug";
	}
	$compile_current = "$compile_current $in";
	if ($comp_only{$exe}) {
		print "$compile_current\n";
	} else {
		print "$compile_current && $cmd\n";
	}
	$compile_current = "$compile_current 1> $exe.cobc.out 2>&1";

	my $total = 0;
	my $pass = 0;
	my $fail = 0;
	my $deleted = 0;
	my $inspect = 0;

	$ret = system ("trap 'exit 77' INT QUIT TERM PIPE; $compile_current");
	if ($ret != 0) {
		if (($ret >> 8) == 77) {
			die "Interrupted\n";
		}
		$compile_error++;
		print LOG "  ===== compile error =====\n";
		next;
	}

	# Some programs need to be checked for compiler warnings
	#if ($exe eq "NC302M" || $exe eq "DB304M") {
	#	$total = 7; --> aus Quelle übernehmen
	#	open (my $COBC_OUT, '<', "$exe.cobc.out");
	#	while (<$COBC_OUT>) {
	#		if
	#		if (/ warning: ([A-Z-]+) .* obsolete /) {
	#			$pass += 1;
	#			next;
	#		}
	#	}
	#}

	unlink "$exe.cobc.out" if (-s "$exe.cobc.out" == 0);

	if ($comp_only{$exe}) {
		printf LOG ("    0    0    0       0       0 OK\n");
		$total_ok++;
		next;
	}


	if ($in =~ /\.CBL/) {
		if ($ENV{'DB_HOME'}) {
			$ret = system ("trap 'exit 77' INT QUIT TERM PIPE; rm -f XXXXX*; rm -f $ENV{'DB_HOME'}/XXXXX*");
		} else {
			$ret = system ("trap 'exit 77' INT QUIT TERM PIPE; rm -f XXXXX*");
		}
		if (($ret >> 8) == 77) {
			die "Interrupted\n";
		}
	}

	$subt = substr($exe, 0, 2);
	if ($exe eq "DB102A") {
		$ENV{"COB_SET_DEBUG"} = "N";
	} elsif ($subt eq "DB") {
		$ENV{"COB_SET_DEBUG"} = "Y";
	} else {
		$ENV{"COB_SET_DEBUG"} = "N";
	}
	if ($subt eq "RW") {
		$ENV{"DD_XXXXX049"} = "$exe.rep";
	}
	$ENV{"REPORT"} = "$exe.log";
	if ($raw_input{$exe}) {
		$cmd = "$cmd < $exe.inp";
		system ("echo \"$raw_input{$exe}\" > $exe.inp");
	}

testrepeat:
	if (!$to_kill{$exe}) {
		$ret = system ("trap 'exit 77' INT QUIT TERM PIPE; $cmd > $exe.out");
	} else {
		$ret = system ("trap 'exit 77' INT QUIT TERM PIPE; $cmd > $exe.out 2>/dev/null");
	}

	if ($ret != 0 && !($ret >> 2 && $to_kill{$exe})) {
		if (($ret >> 8) == 77) {
			die "Interrupted\n";
		}
		$execute_error++;
		print LOG "***** execute error $ret *****\n";
		next;
	}
	if ($no_output{$exe}) {
		$total = 1;
		$pass = 1;
	} elsif (open (my $PRT, '<', $ENV{"REPORT"})) {

		# NC107A: check hex values in report
		if ($exe eq "NC107A") {
			binmode($PRT);
			while (<$PRT>) {
				if (/^ *([0-9]+) *OF *([0-9]+) *TESTS WERE/) {
					$total += $2;
					$pass += $1;
				} elsif (/^ *([0-9NO]+) *TEST\(S\) ([A-Z]+)/) {
					my $num = $1 eq "NO" ? 0 : $1;
					if ($2 eq "FAILED") {
						$fail += $num;
					} elsif ($2 eq "DELETED") {
						$deleted += $num;
					}
				} elsif (/^\*\*\* INFORMATION \*\*\*        (.{20})     ([A-Z-]+) /) {
					if (("$2" eq "ZERO"       && "$1" eq " 000000000000000000 ") ||
					    ("$2" eq "SPACE"      && "$1" eq "                    ") ||
					    ("$2" eq "QUOTE"      && "$1" eq "\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"") ||
					    ("$2" eq "HIGH-VALUE" && "$1" eq "\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377") ||
					    ("$2" eq "LOW-VALUE"  && "$1" eq "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000") ) {
						$pass += 1;
					} else {
						$fail += 1;
					}
				}
			}

		# NC113M needs to be "visual" inspected for tests being sequential
		} elsif ($exe eq "NC113M") {
			my $seqcount = 0;
			while (<$PRT>) {
				if (/^ MARGIN TESTING *MAR-TEST-([0-9]+) /) {
					$seqcount += 1;
					if ($seqcount eq $1) {
						$pass += 1;
					} else {
						$fail += 1;
						$seqcount = $1;
					}
				} elsif (/^ *([0-9]+) *TESTS REQUIRE VISUAL INSPECTION/) {
					$total += $1;
				}
			}

		# NC114M TODO: check the listing, removing the "inspect" entries

		# NC121M/NC220M: needs to be inspected for identical display output
		} elsif ($exe eq "NC121M" || $exe eq "NC220M") {
			my $line = my $line2 ="";
			if (open (my $fh, '<', "$exe.out")) {
				<$fh>;
				$line = <$fh>;
				$line2 = <$fh>;
			}
			while (<$PRT>) {
				if (/^ *([0-9]+) *OF *([0-9]+) *TESTS WERE/) {
					$total += $2;
					$pass += $1;
				} elsif (/^ *([0-9NO]+) *TEST\(S\) ([A-Z]+)/) {
					my $num = $1 eq "NO" ? 0 : $1;
					if ($2 eq "FAILED") {
						$fail += $num;
					} elsif ($2 eq "DELETED") {
						$deleted += $num;
					}
				} elsif (/^\*\*\* INFORMATION \*\*\* *([0-9A-Z-]+) /) {
					chomp $line;
					if ($line eq $1) {
						$pass += 1;
					} else {
						$fail += 1;
					}
					$line = $line2;
				}
			}

		# NC135A: needs to be inspected for table output
		} elsif ($exe eq "NC135A") {
			my $seqcount = 0;
			while (<$PRT>) {
				if (/^ *([0-9]+) *OF *([0-9]+) *TESTS WERE/) {
					$total += $2;
					$pass += $1;
				} elsif (/^ *([0-9NO]+) *TEST\(S\) ([A-Z]+)/) {
					my $num = $1 eq "NO" ? 0 : $1;
					if ($2 eq "FAILED") {
						$fail += $num;
					} elsif ($2 eq "DELETED") {
						$deleted += $num;
					}
				} elsif (/^   (\d+  )+/) {
					while ($_ =~ /(\d+)  /g) {
						$seqcount += 1;
						if ($seqcount != $1) {
							$fail += 1;
							last;
						}
						if ($seqcount == 300) {
							$pass += 1;
						}
					}
				}
			}

		# normal test procedure
		} else {
			while (<$PRT>) {
				if (/^ *([0-9]+) *OF *([0-9]+) *TESTS WERE/) {
					$total += $2;
					$pass += $1;
				} elsif (/^ *([0-9NO]+) *TEST\(S\) ([A-Z]+)/) {
					my $num = $1 eq "NO" ? 0 : $1;
					if ($2 eq "FAILED") {
						$fail += $num;
					} elsif ($2 eq "DELETED") {
						$deleted += $num;
					} elsif ($2 eq "REQUIRE") {
						$inspect += $num;
					}
				} elsif (/^ *([0-9]+) *TESTS REQUIRE VISUAL INSPECTION/) {
					$total += $1;
					$inspect += $1;
				}
			}
		}
	}
	printf LOG ("%5s %4s %4s %7s %7s %s\n",
		$total, $pass, $fail, $deleted, $inspect,
		$fail == 0 ? "OK" : "");
	$total_all += $total;
	$total_pass += $pass;
	$total_fail += $fail;
	$total_deleted += $deleted;
	$total_inspect += $inspect;
	$total_ok++ if $fail == 0;
	if ($exe eq "DB103M" && $db103m == 0) {
		$db103m = 1;
		$ENV{"COB_SET_DEBUG"} = "N";
		$num_progs++;
		print "Reexecution with runtime DEBUG off ./DB103M\n";
		printf LOG "%-12s", $in;
		goto testrepeat;
	}
	unlink "$exe.out" if (-s "$exe.out" == 0);
}

print LOG "--------    ----- ---- ---- ------- -------\n";
printf LOG ("Total       %5s %4s %4s %7s %7s\n\n",
	    $total_all, $total_pass, $total_fail, $total_deleted,
	    $total_inspect);

printf LOG ("Number of programs:    %2s\n", $num_progs);
printf LOG ("Successfully executed: %2s\n", $total_ok);
printf LOG ("Compile error:         %2s\n", $compile_error);
printf LOG ("Execute error:         %2s\n", $execute_error);
