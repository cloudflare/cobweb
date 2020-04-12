       01  SQLDA.
           03  SQLDAID            PIC X(8)           VALUE "SQLDA  ".
           03  SQLDABC            USAGE BINARY-LONG  VALUE 0.
           03  SQLN               USAGE BINARY-SHORT VALUE 0.
           03  SQLD               USAGE BINARY-SHORT VALUE 0.
           03  SQLVAR OCCURS 1 TO 1489 TIMES DEPENDING ON SQLD.
              05  SQLTYPE         USAGE BINARY-SHORT.
              05  SQLLEN          USAGE BINARY-SHORT.
              05  FILLER          PIC X(4).
              05  SQLDATA         USAGE POINTER.
              05  SQLIND          USAGE POINTER.
              05  SQLNAME.
                 07  SQLNAMEL     PIC USAGE BINARY-SHORT.
                 07  SQLNAMEC     PIC X(30).

      *> SQLTYPE

       78  ESQL-DATE-CHAR              VALUE 384.
       78  ESQL-DATE-CHAR-NULL         VALUE 385.
       78  ESQL-DATE-REC               VALUE 386.
       78  ESQL-DATE-REC-NULL          VALUE 387.
       78  ESQL-TIME-CHAR              VALUE 388.
       78  ESQL-TIME-CHAR-NULL         VALUE 389.
       78  ESQL-TIME-REC               VALUE 390.
       78  ESQL-TIME-REC-NULL          VALUE 391.
       78  ESQL-TIMESTAMP-CHAR         VALUE 392.
       78  ESQL-TIMESTAMP-CHAR-NULL    VALUE 393.
       78  ESQL-TIMESTAMP-REC          VALUE 394.
       78  ESQL-TIMESTAMP-REC-NULL     VALUE 395.
       78  ESQL-LONGVARBINARY          VALUE 404.
       78  ESQL-LONGVARBINARY-NULL     VALUE 405.
       78  ESQL-LONGVARCHAR            VALUE 408.
       78  ESQL-LONGVARCHAR-NULL       VALUE 409.
       78  ESQL-BINARY                 VALUE 444.
       78  ESQL-BINARY-NULL            VALUE 445.
       78  ESQL-VARBINARY              VALUE 446.
       78  ESQL-VARBINARY-NULL         VALUE 447.
       78  ESQL-VARCHAR                VALUE 448.
       78  ESQL-VARCHAR-NULL           VALUE 449.
       78  ESQL-CHARVARYING            VALUE 450.
       78  ESQL-CHARVARYING-NULL       VALUE 451.
       78  ESQL-CHAR                   VALUE 452.
       78  ESQL-CHAR-NULL              VALUE 453.
       78  ESQL-CHAR-FIXED             VALUE 454.
       78  ESQL-CHAR-FIXED-NULL        VALUE 455.
       78  ESQL-DOUBLE                 VALUE 480.
       78  ESQL-DOUBLE-NULL            VALUE 481.
       78  ESQL-REAL                   VALUE 482.
       78  ESQL-REAL-NULL              VALUE 483.
       78  ESQL-DECIMAL                VALUE 484.
       78  ESQL-DECIMAL-NULL           VALUE 485.
       78  ESQL-INTEGER                VALUE 496.
       78  ESQL-INTEGER-NULL           VALUE 497.
       78  ESQL-SMALLINT               VALUE 500.
       78  ESQL-SMALLINT-NULL          VALUE 501.
       78  ESQL-TINYINT                VALUE 502.
       78  ESQL-TINYINT-NULL           VALUE 503.
