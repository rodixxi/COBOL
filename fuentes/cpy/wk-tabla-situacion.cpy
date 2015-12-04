       01  WK-DETALLE-SITUACION          PIC X(7).
       01  WK-SITUACION                  PIC 9.  
       01  TAB-SITUACION.
            03 FILLER                   PIC X(08)
            VALUE "1Fraude ".
            03 FILLER                   PIC X(08)
            VALUE "3En mora".
            03 FILLER                   PIC X(08)
            VALUE "5Seven  ".
            03 FILLER                   PIC X(08)
            VALUE "7Normal ".
       01  FILLER REDEFINES TAB-SITUACION.
            03 TAB-SITUACION-DETALLE OCCURS 4
               INDEXED BY SIT-INDEX.
               05 TAB-CTAS-SITUACION         PIC 9.
               05 TAB-CTAS-SITUACION-DETALLE PIC X(7).