       77  WK-DETALLE-APERTURA          PIC X(13).
       77  WK-APERTURA                  PIC 9.  
       01  TAB-APERTURA.
            03 FILLER                   PIC X(14)
            VALUE "1Normal       ".
            03 FILLER                   PIC X(14)
            VALUE "2Ampliar      ".
            03 FILLER                   PIC X(14)
            VALUE "3Internacional".
            03 FILLER                   PIC X(14)
            VALUE "6Estudio      ".
            03 FILLER                   PIC X(14)
            VALUE "7F.Docum      ".
            03 FILLER                   PIC X(14)
            VALUE "8Denegada     ".
            03 FILLER                   PIC X(14)
            VALUE "9Analisis     ".
       01  TAB-APERTURA-BYINDEX REDEFINES TAB-APERTURA.
            03 TAB-APERTURA-DETALLE OCCURS 7
               INDEXED BY APER-INDEX.
               05 TAB-CTAS-APERTURA         PIC 9.
               05 TAB-CTAS-APERTURA-DETALLE PIC X(13).