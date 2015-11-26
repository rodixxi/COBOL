       01  TAB-MESES                    PIC X(36) 
           VALUE "ENEFEBAMRABRMAYJUNJULAGOSETOCTNOVDIC".
       01  FILLER REDEFINES TAB-MESES.
           03 TAB-MES                   PIC X(3) OCCURS 12.
