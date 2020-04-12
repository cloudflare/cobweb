
		                 GnuCOBOL
		https://www.gnu.org/software/gnucobol/
		https://sourceforge.net/projects/open-cobol
		https://savannah.gnu.org/projects/gnucobol

GnuCOBOL is a free (like both in "free speech" and in "free beer")
COBOL compiler, formerly known as OpenCOBOL.
It implements a substantial part of the COBOL 85, COBOL 2002 and COBOL 2014
standards, as well as many extensions included in other COBOL compilers.

GnuCOBOL translates COBOL into C and compiles the translated code
using the native C compiler on various platforms, including Unix/Linux,
Mac OS X, and Microsoft Windows. 

Although many have participated, most development thanks go to

    Roger While
    Keisuke Nishida

This package contains the following subdirectories:

    cobc        COBOL compiler
    libcob      COBOL run-time library
    bin         COBOL driver program
    build_aux   Helper scripts
    lib         Helper routines for missing OS functionality
    config      Configuration files
    po          International messages
    doc         'info' and 'pdf' files
    tests       Test suite (GnuCOBOL and framework for COBOL85)
    extras      useful COBOL programs 

All programs except those in lib and libcob are distributed under
the GNU General Public License.  See COPYING for details.

Programs in lib and libcob are distributed under the GNU Lesser
General Public License.  See COPYING.LESSER for details.

See AUTHORS for the author of each file.

============
Requirements
============

  ***
    NOTE
    For all the following packages (required or optional),
    BOTH runtime AND development components are necessary.
  ***
  ***
    NOTE
    All the following packages are normally part of a Linux
    distribution. Cygwin distribution also has these as installable
    packages, other operating systems also may have repositories for
    these - eg. MAC OS, CentOS and others all have package repositories.
    ALWAYS install the distribution packages when available !!
  ***

GnuCOBOL REQUIRES one of the following external libraries to be installed
for implementation of decimal arithmetic:

    BOTH runtime AND development components required.

  o GNU MP (libgmp) 4.1.2 or later
    http://gmplib.org

  OR

  o MPIR (libgmp - MPIR gmp-compat) 1.3.1 or later
    (prefered when compiling on Windows with other compilers than GCC)
    http://mpir.org

    GNU MP and MPIR are distributed under GNU Lesser General Public License.

    NOTE
    Please ALWAYS use the distro package whenever possible !!
    See NOTE above.


GnuCOBOL MAY require the following external libraries to be installed:

    ***
    NOTE - libltdl is NOT needed when installing on Linux,
    SUN Solaris, MAC OS, CentOS or Windows
        (including Cygwin, MingW and native windows).
    It is also NOT needed with later versions of AIX and HP-UX.
    (AIX >= 5.1 and HP-UX >= 11.1 are known to NOT require this).
    (Check if you have the "dlopen" function).
    ***

  o GNU Libtool (libltdl)
    http://www.gnu.org/software/libtool/libtool.html

    libltdl is used to implement dynamic CALL statements.

    GNU Libtool is distributed under GNU Lesser General Public License.


The following libraries ARE required WHEN :

1) Indexed-Sequential file I/O (ISAM) is used

    BOTH runtime AND development components required.

    One of the following:

  o Berkeley DB (libdb) 4.1 or later
    http://www.oracle.com/
    http://www.oracle.com/technology/products/berkeley-db/db/index.html

    Berkeley DB is distributed under Oracles own open-source license.
    Note that if you linked your software with Berkeley DB,
    you must distribute the source code of your software along with your
    software, or you have to pay royalty to Oracle.

  o VBISAM - ISAM file handler (libvbisam) 2.0 or later
    http://sourceforge.net/projects/vbisam/

    VBISAM is distributed under GNU Lesser General Public License.

  o DISAM File handler (libdisam)
    http://www.isamcentral.com

    DISAM is distributed under the proprietary License
    "Byte Designs Ltd. DISAM Software License".

2) SCREEN SECTION and/or extended ACCEPT/DISPLAY is used

    BOTH runtime AND development components required.

    One of the following:

  o Ncurses (ncurses or ncursesw) 5.2 or later
    http://www.gnu.org/software/ncurses/ncurses.html

    Ncurses is distributed under a BSD style license.

  o Unix curses

  o PDCurses (pdcurses) for MinGW/native windows ports
    http://pdcurses.sourceforge.net

============

============
Installation
============

** NOTE **
   The default installation path for GnuCOBOL is /usr/local.
   The installation path may be changed by specifying --prefix=<dir>
   as a parameter to the configure.
   Further parameters may be specified to affect
   include/library search paths.
   Execute ./configure --help for further details.

To generate/install GnuCOBOL :

**************************************

 Configure and build
    ./configure
    make

 Here you may run
    make check
 to run a series of GnuCOBOL test programs (must do!)
 This MUST succeed - If not, please report.

 You may optionally perform a series of COBOL85 tests.
    make test
 It is recommended that you perform these tests.

** NOTE **
   The language interpreter "perl" is required to run COBOL85 tests.
   If you build in Cygwin/MSYS you must use a Cygwin/MSYS version of perl.
 
** NOTE **
   Running "make test" will try to download the COBOL85
   testsuite if it is missing.
   For details see tests/cobol85/README.

 If you want to run both tests you can run
    make checkall 

 Install
    make install

** NOTE **
   You generally need super-user privileges to execute "make install"
   unless you changed the installation directory with
   "./configure --prefix=<dir>" and have full access to <dir>.

** NOTE **
   On Linux systems, if you are installing for the
   -first- time, you may need to run "ldconfig" (as root).
   In fact, it does not hurt if you always do this.

** NOTE **
   On some Red Hat (Fedora) installations and
   possibly other Linux distros, /usr/local/lib
   is NOT automatically searched at runtime. 
   Edit /etc/ld.so.conf (or the equivalent file) and add
   /usr/local/lib to the file.
   Rerun "ldconfig".

**************************************

If you think you have a problem or just want to
record the make output, just redirect the output thus :
    make 1>mymake.log 2>&1
    make install 1>myinstall.log 2>&1

**************************************

You can get back to a clean installation status by running :
    make distclean

**************************************


============

The following is only interesting for advanced use.
A normal user should not have recourse to use these
options.

There are many configure options (see configure --help for a full list),
these are the most important ones:

  --with-db			Use Berkeley DB >= 4.1 (libdb) (ISAM handler)
				This is the default

  --without-db			Do not use Berkeley DB / any other ISAM handler
				You will not be able to use indexed I/O

  --with-vbisam			Use VBISAM (libvbisam) (ISAM handler)

  --with-dl			Use the system dynamic linker
				This is the default

  --without-dl			Use ltdl for dynamic program loading

  --with-patch-level=<n>	Set internal patch level to n (default 0)

  --with-varseq=<n>		Define the default format for variable
				length sequential files.

				The default may be overridden at run time by
				setting the environment variable
				COB_VARSEQ_FORMAT to 0, 1, 2, or 3.

				For values of 0, 1 and 2, four bytes are
				written preceding each record. The format of
				these four bytes for values of 0, 1, 2 is
				as follows :
				n = 0 (default)
					The first 2 bytes are the record length
					in big-endian order. This is compatible
					with mainframe. Bytes 3 and 4 are set
					to binary 0.
				n = 1
					The 4 bytes are the record length in
					big-endian order.
				n = 2
					The 4 bytes are the record length in
					native machine order (int).
					(This was previously the default)

				For the value of 3, two bytes are written
				preceding each record :
				n = 3
					The first 2 bytes are the record length
					in big-endian order. The record follows
					immediately after beginning at byte 3.

  --enable-debug		Add '-g' debug option to make

============

============
Development
============

If you wish to hack the GnuCOBOL source or build from version control,
see HACKING.
