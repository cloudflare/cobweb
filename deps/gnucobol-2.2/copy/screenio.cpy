      *>  Copyright (C) 2008-2012, 2015-2016  Free Software Foundation, Inc.
      *>  Written by Roger While
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


      *>   Colors
       78  COB-COLOR-BLACK     VALUE 0.
       78  COB-COLOR-BLUE      VALUE 1.
       78  COB-COLOR-GREEN     VALUE 2.
       78  COB-COLOR-CYAN      VALUE 3.
       78  COB-COLOR-RED       VALUE 4.
       78  COB-COLOR-MAGENTA   VALUE 5.
       78  COB-COLOR-YELLOW    VALUE 6.
       78  COB-COLOR-WHITE     VALUE 7.

      *>
      *> Values that may be returned in CRT STATUS (or COB-CRT-STATUS)
      *> Normal return - Value 0000
       78  COB-SCR-OK          VALUE 0.

      *>  Function keys - Values 1xxx
       78  COB-SCR-F1          VALUE 1001.
       78  COB-SCR-F2          VALUE 1002.
       78  COB-SCR-F3          VALUE 1003.
       78  COB-SCR-F4          VALUE 1004.
       78  COB-SCR-F5          VALUE 1005.
       78  COB-SCR-F6          VALUE 1006.
       78  COB-SCR-F7          VALUE 1007.
       78  COB-SCR-F8          VALUE 1008.
       78  COB-SCR-F9          VALUE 1009.
       78  COB-SCR-F10         VALUE 1010.
       78  COB-SCR-F11         VALUE 1011.
       78  COB-SCR-F12         VALUE 1012.
       78  COB-SCR-F13         VALUE 1013.
       78  COB-SCR-F14         VALUE 1014.
       78  COB-SCR-F15         VALUE 1015.
       78  COB-SCR-F16         VALUE 1016.
       78  COB-SCR-F17         VALUE 1017.
       78  COB-SCR-F18         VALUE 1018.
       78  COB-SCR-F19         VALUE 1019.
       78  COB-SCR-F20         VALUE 1020.
       78  COB-SCR-F21         VALUE 1021.
       78  COB-SCR-F22         VALUE 1022.
       78  COB-SCR-F23         VALUE 1023.
       78  COB-SCR-F24         VALUE 1024.
       78  COB-SCR-F25         VALUE 1025.
       78  COB-SCR-F26         VALUE 1026.
       78  COB-SCR-F27         VALUE 1027.
       78  COB-SCR-F28         VALUE 1028.
       78  COB-SCR-F29         VALUE 1029.
       78  COB-SCR-F30         VALUE 1030.
       78  COB-SCR-F31         VALUE 1031.
       78  COB-SCR-F32         VALUE 1032.
       78  COB-SCR-F33         VALUE 1033.
       78  COB-SCR-F34         VALUE 1034.
       78  COB-SCR-F35         VALUE 1035.
       78  COB-SCR-F36         VALUE 1036.
       78  COB-SCR-F37         VALUE 1037.
       78  COB-SCR-F38         VALUE 1038.
       78  COB-SCR-F39         VALUE 1039.
       78  COB-SCR-F40         VALUE 1040.
       78  COB-SCR-F41         VALUE 1041.
       78  COB-SCR-F42         VALUE 1042.
       78  COB-SCR-F43         VALUE 1043.
       78  COB-SCR-F44         VALUE 1044.
       78  COB-SCR-F45         VALUE 1045.
       78  COB-SCR-F46         VALUE 1046.
       78  COB-SCR-F47         VALUE 1047.
       78  COB-SCR-F48         VALUE 1048.
       78  COB-SCR-F49         VALUE 1049.
       78  COB-SCR-F50         VALUE 1050.
       78  COB-SCR-F51         VALUE 1051.
       78  COB-SCR-F52         VALUE 1052.
       78  COB-SCR-F53         VALUE 1053.
       78  COB-SCR-F54         VALUE 1054.
       78  COB-SCR-F55         VALUE 1055.
       78  COB-SCR-F56         VALUE 1056.
       78  COB-SCR-F57         VALUE 1057.
       78  COB-SCR-F58         VALUE 1058.
       78  COB-SCR-F59         VALUE 1059.
       78  COB-SCR-F60         VALUE 1060.
       78  COB-SCR-F61         VALUE 1061.
       78  COB-SCR-F62         VALUE 1062.
       78  COB-SCR-F63         VALUE 1063.
       78  COB-SCR-F64         VALUE 1064.
      *>  Exception keys - Values 2xxx
       78  COB-SCR-PAGE-UP     VALUE 2001.
       78  COB-SCR-PAGE-DOWN   VALUE 2002.
       78  COB-SCR-KEY-UP      VALUE 2003.
       78  COB-SCR-KEY-DOWN    VALUE 2004.
       78  COB-SCR-ESC         VALUE 2005.
       78  COB-SCR-PRINT       VALUE 2006.
       78  COB-SCR-TAB         VALUE 2007.
       78  COB-SCR-BACK-TAB    VALUE 2008.
       78  COB-SCR-KEY-LEFT    VALUE 2009.
       78  COB-SCR-KEY-RIGHT   VALUE 2010.
      *>  The following exception keys are currently *only* returned
      *>  on ACCEPT OMITTED
       78  COB-SCR-INSERT      VALUE 2011.
       78  COB-SCR-DELETE      VALUE 2012.
       78  COB-SCR-BACKSPACE   VALUE 2013.
       78  COB-SCR-KEY-HOME    VALUE 2014.
       78  COB-SCR-KEY-END     VALUE 2015.      
      *>  Input validation - Values 8xxx
       78  COB-SCR-NO-FIELD    VALUE 8000.
       78  COB-SCR-TIME-OUT    VALUE 8001.
      *>  Other errors - Values 9xxx
       78  COB-SCR-FATAL       VALUE 9000.
       78  COB-SCR-MAX-FIELD   VALUE 9001.
