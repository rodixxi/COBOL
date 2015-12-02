       01  WK-DETALLE-SITUACION          PIC X(7).
       01  WK-SITUACION                  PIC 9.  
       01  TAB-APERTURA.
            03 FILLER                   PIC X(14)
            VALUE "1Fraude ".
            03 FILLER                   PIC X(14)
            VALUE "3En mora".
            03 FILLER                   PIC X(14)
            VALUE "5Seven  ".
            03 FILLER                   PIC X(14)
            VALUE "7Normal ".
       01  TAB-APERTURA-BYINDEX REDEFINES TAB-APERTURA.
            03 TAB-APERTURA-DETALLE OCCURS 4
               INDEXED BY SIT-INDEX.
               05 TAB-CTAS-APERTURA         PIC 9.
               05 TAB-CTAS-APERTURA-DETALLE PIC X(7).