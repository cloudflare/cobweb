      *>----------------------------------------------------------------
      *> Authors:   Brian Tiffin, Asger Kjelstrup, Simon Sobisch,
      *>            Roger While
      *> Purpose:   Hex Dump display
      *> Tectonics: cobc -m -std=mf -O2 CBL_OC_DUMP.cob
      *>     Usage: export OC_DUMP_EXT=1 for explanatory text on dumps
      *>            (memory address and dump length)
      *>            export OC_DUMP_EXT=Y for extended explanatory text
      *>            (architecture   and endian-order plus above)
      *>----------------------------------------------------------------
      *>
      *>  This file is part of GnuCOBOL.
      *>
      *>  The GnuCOBOL compiler is free software: you can redistribute
      *>  it and/or modify it under the terms of the GNU General Public
      *>  License as published by the Free Software Foundation, either
      *>  version 3 of the License, or (at your option) any later
      *>  version.
      *>
      *>  GnuCOBOL is distributed in the hope that it will be useful,
      *>  but WITHOUT ANY WARRANTY; without even the implied warranty of
      *>  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      *>  GNU General Public License for more details.
      *>
      *>  You should have received a copy of the GNU General Public
      *>  License along with GnuCOBOL.
      *>  If not, see <http://www.gnu.org/licenses/>.

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      CBL_OC_DUMP.
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  addr                  usage pointer.
       01  counter               usage binary-long unsigned.
       01  byline                usage binary-long unsigned.
       01  len                   usage binary-long unsigned.
       01  was-called-before     usage binary-long unsigned value 0.
           88  called-before     value 1.

       01  char-set              pic x(06).
           88 is-ascii           value 'ASCII'.
           88 is-ebdic           value 'EBCDIC'.
           88 is-unknown         value '?'.
       01  architecture          pic x(06).
           88 is-32-bit          value '32-bit'.
           88 is-64-bit          value '64-bit'.
       01  endian-order          pic x(13).
           88 is-big-endian-no   value 'little endian'.
           88 is-big-endian-yes  value 'big endian'.
       01  dots                  pic x value '.'.
       01  dump-dots             pic x.

       01  disp-line.
           03  offset            pic 999999.
           03                    pic xx     value space.
           03  hex-line          pic x(48).
           03  hex-line-red redefines hex-line.
               05  occurs 16.
                   07  hex-disp-val  pic xx.
                   07                pic x.
           03                    pic xx     value space.
           03  show              pic x(16).

       01  extended-infos        pic x.
           88 show-extended-infos      values '1', '2', 'Y', 'y'.
           88 show-very-extended-infos values '2', 'Y', 'y'.

       01  len-display           pic ZZZZZ9.

       01  byte                  pic x.
       01  byte-redef redefines  byte  usage binary-char unsigned.

       01  hex-tab               pic x(512) value
           "000102030405060708090a0b0c0d0e0f" &
           "101112131415161718191a1b1c1d1e1f" &
           "202122232425262728292a2b2c2d2e2f" &
           "303132333435363738393a3b3c3d3e3f" &
           "404142434445464748494a4b4c4d4e4f" &
           "505152535455565758595a5b5c5d5e5f" &
           "606162636465666768696a6b6c6d6e6f" &
           "707172737475767778797a7b7c7d7e7f" &
           "808182838485868788898a8b8c8d8e8f" &
           "909192939495969798999a9b9c9d9e9f" &
           "a0a1a2a3a4a5a6a7a8a9aaabacadaeaf" &
           "b0b1b2b3b4b5b6b7b8b9babbbcbdbebf" &
           "c0c1c2c3c4c5c6c7c8c9cacbcccdcecf" &
           "d0d1d2d3d4d5d6d7d8d9dadbdcdddedf" &
           "e0e1e2e3e4e5e6e7e8e9eaebecedeeef" &
           "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff".
       01  hex-tab-red redefines hex-tab.
           03  hex-vals          pic xx     occurs 256.

       LINKAGE SECTION.
       01  buffer                any length.
       01  valuelen              any numeric.
      *>----------------------------------------------------------------
       PROCEDURE DIVISION USING buffer valuelen.
       MAIN SECTION.
       MAIN00.
           if number-of-call-parameters < 1
              display 'CBL_OC_DUMP: No parameter supplied'
                       upon SYSERR
              end-display
              goback
           end-if

           if not called-before
              *> First time through
              set called-before to true
              *> If wanted, set dot to something other than point
              accept dump-dots from environment 'OC_DUMP_DOTS'
                     not on exception
                         move dump-dots to dots
              end-accept
              *> Discover if running ASCII or EBCDIC
       >>IF   CHARSET = 'ASCII'
              set  is-ascii   to true
       >>ELIF CHARSET = 'EBCDIC'
              set  is-ebdic   to true
       >>ELSE
              set  is-unknown to true
       >>END-IF
              *> Discover endianness
       >>IF ENDIAN = "BIG"
              set  is-big-endian-yes to true
       >>ELSE
              set  is-big-endian-no  to true
       >>END-IF

              *> Get and display characteristics and headline
              accept extended-infos from environment 'OC_DUMP_EXT'
              end-accept

              if show-very-extended-infos
                 *> Stuff that we only need to display once
                 *> Longer pointers in 64-bit architecture
       >>IF P64 SET
                 set  is-64-bit to true
       >>ELSE
                 set  is-32-bit to true
       >>END-IF

                 display 'Program runs on '
                         architecture ' architecture. '
                         upon SYSERR
                 end-display
                 display 'Character set is '
                         function trim (char-set) '.'
                         upon SYSERR
                 end-display
                 display 'Byte order is '
                         function trim (endian-order)
                         upon SYSERR
                 end-display
              end-if
           end-if

           *> Get the length of the parameter
           call 'C$PARAMSIZE' using 1
                giving len
           end-call

           *> Check if the user specified a length
           if number-of-call-parameters > 1
              if valuelen not numeric
                 display 'CBL_OC_DUMP: Length parameter is not numeric'
                         upon SYSERR
                 end-display
                 goback
              end-if
              if valuelen < 0
                 display 'CBL_OC_DUMP: Invalid length parameter: '
                         valuelen
                         upon SYSERR
                 end-display
                 goback
              end-if
              if valuelen < len
                 move valuelen to len
              end-if
           end-if

           if show-extended-infos
              display ' '
                      upon SYSERR
              end-display
              if len > 0
                 set addr      to address of buffer
                 display 'Dump of memory beginning at address: '
                          addr
                          upon SYSERR
                 end-display
              end-if
              move len to len-display
              display 'Length of memory dump is: ' len-display
                       upon SYSERR
              end-display
           end-if

           *> Do we have anything to dump?
           if len = 0
              display ' '
                      upon SYSERR
              end-display
              display 'CBL_OC_DUMP: Nothing to dump.'
                      upon SYSERR
              end-display
              goback
           end-if

           *> Ensure that the passed size is not too big
           if len > 999998
              move 999998 to len, len-display
              display 'CBL_OC_DUMP: Warning, only the first '
                      len-display  ' bytes are shown!'
                      upon SYSERR
              end-display
           end-if

           display ' '
                   upon SYSERR
           end-display
           display 'Offset  ' &
                   'HEX-- -- -- -5 -- -- -- -- 10 ' &
                   '-- -- -- -- 15 --   ' &
                   'CHARS----1----5-'
                   upon SYSERR
           end-display

           *> Main loop
           perform varying counter from 0 by 16
                   until   counter  >=   len
              move spaces  to hex-line show
              perform varying byline from 1 by 1
                      until   byline  >  16
                 if (counter + byline) > len
                    exit perform
                 end-if
                 move buffer (counter + byline : 1) to byte
                 move hex-vals (byte-redef + 1) to
                      hex-disp-val (byline)
                 move byte to show (byline:1)
              end-perform
              *> Check printable characters
              call "CBL_GC_PRINTABLE" using show dots
              end-call
              move counter to offset
              display disp-line
                      upon SYSERR
              end-display
           end-perform

           display ' '
                   upon SYSERR
           end-display

           goback
           .
       end program CBL_OC_DUMP.
