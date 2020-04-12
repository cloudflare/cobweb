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
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 X-P1          PIC 9(1) VALUE 1
                        @USAGE@.
       01 X-P2          PIC 9(2) VALUE 12
                        @USAGE@.
       01 X-P3          PIC 9(3) VALUE 123
                        @USAGE@.
       01 X-P4          PIC 9(4) VALUE 1234
                        @USAGE@.
       01 X-P5          PIC 9(5) VALUE 12345
                        @USAGE@.
       01 X-P6          PIC 9(6) VALUE 123456
                        @USAGE@.
       01 X-P7          PIC 9(7) VALUE 1234567
                        @USAGE@.
       01 X-P8          PIC 9(8) VALUE 12345678
                        @USAGE@.
       01 X-P9          PIC 9(9) VALUE 123456789
                        @USAGE@.
       01 X-P10         PIC 9(10) VALUE 1234567890
                        @USAGE@.
       01 X-P11         PIC 9(11) VALUE 12345678901
                        @USAGE@.
       01 X-P12         PIC 9(12) VALUE 123456789012
                        @USAGE@.
       01 X-P13         PIC 9(13) VALUE 1234567890123
                        @USAGE@.
       01 X-P14         PIC 9(14) VALUE 12345678901234
                        @USAGE@.
       01 X-P15         PIC 9(15) VALUE 123456789012345
                        @USAGE@.
       01 X-P16         PIC 9(16) VALUE 1234567890123456
                        @USAGE@.
       01 X-P17         PIC 9(17) VALUE 12345678901234567
                        @USAGE@.
       01 X-P18         PIC 9(18) VALUE 123456789012345678
                        @USAGE@.
       01 X-N1          PIC S9(1) VALUE -1
                        @USAGE@.
       01 X-N2          PIC S9(2) VALUE -12
                        @USAGE@.
       01 X-N3          PIC S9(3) VALUE -123
                        @USAGE@.
       01 X-N4          PIC S9(4) VALUE -1234
                        @USAGE@.
       01 X-N5          PIC S9(5) VALUE -12345
                        @USAGE@.
       01 X-N6          PIC S9(6) VALUE -123456
                        @USAGE@.
       01 X-N7          PIC S9(7) VALUE -1234567
                        @USAGE@.
       01 X-N8          PIC S9(8) VALUE -12345678
                        @USAGE@.
       01 X-N9          PIC S9(9) VALUE -123456789
                        @USAGE@.
       01 X-N10         PIC S9(10) VALUE -1234567890
                        @USAGE@.
       01 X-N11         PIC S9(11) VALUE -12345678901
                        @USAGE@.
       01 X-N12         PIC S9(12) VALUE -123456789012
                        @USAGE@.
       01 X-N13         PIC S9(13) VALUE -1234567890123
                        @USAGE@.
       01 X-N14         PIC S9(14) VALUE -12345678901234
                        @USAGE@.
       01 X-N15         PIC S9(15) VALUE -123456789012345
                        @USAGE@.
       01 X-N16         PIC S9(16) VALUE -1234567890123456
                        @USAGE@.
       01 X-N17         PIC S9(17) VALUE -12345678901234567
                        @USAGE@.
       01 X-N18         PIC S9(18) VALUE -123456789012345678
                        @USAGE@.
       PROCEDURE        DIVISION.
           DISPLAY X-P1
           END-DISPLAY.
           DISPLAY X-P2
           END-DISPLAY.
           DISPLAY X-P3
           END-DISPLAY.
           DISPLAY X-P4
           END-DISPLAY.
           DISPLAY X-P5
           END-DISPLAY.
           DISPLAY X-P6
           END-DISPLAY.
           DISPLAY X-P7
           END-DISPLAY.
           DISPLAY X-P8
           END-DISPLAY.
           DISPLAY X-P9
           END-DISPLAY.
           DISPLAY X-P10
           END-DISPLAY.
           DISPLAY X-P11
           END-DISPLAY.
           DISPLAY X-P12
           END-DISPLAY.
           DISPLAY X-P13
           END-DISPLAY.
           DISPLAY X-P14
           END-DISPLAY.
           DISPLAY X-P15
           END-DISPLAY.
           DISPLAY X-P16
           END-DISPLAY.
           DISPLAY X-P17
           END-DISPLAY.
           DISPLAY X-P18
           END-DISPLAY.
           DISPLAY X-N1
           END-DISPLAY.
           DISPLAY X-N2
           END-DISPLAY.
           DISPLAY X-N3
           END-DISPLAY.
           DISPLAY X-N4
           END-DISPLAY.
           DISPLAY X-N5
           END-DISPLAY.
           DISPLAY X-N6
           END-DISPLAY.
           DISPLAY X-N7
           END-DISPLAY.
           DISPLAY X-N8
           END-DISPLAY.
           DISPLAY X-N9
           END-DISPLAY.
           DISPLAY X-N10
           END-DISPLAY.
           DISPLAY X-N11
           END-DISPLAY.
           DISPLAY X-N12
           END-DISPLAY.
           DISPLAY X-N13
           END-DISPLAY.
           DISPLAY X-N14
           END-DISPLAY.
           DISPLAY X-N15
           END-DISPLAY.
           DISPLAY X-N16
           END-DISPLAY.
           DISPLAY X-N17
           END-DISPLAY.
           DISPLAY X-N18
           END-DISPLAY.
           STOP RUN.
