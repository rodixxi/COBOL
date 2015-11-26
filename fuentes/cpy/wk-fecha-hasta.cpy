       01  WK-FECHA-HASTA               PIC 9(08).
       01  FILLER REDEFINES WK-FECHA-HASTA.
            03 WK-FECHA-HASTA-ANHIO      PIC 9999.
            03 WK-FECHA-HASTA-MES        PIC 99.
            03 WK-FECHA-HASTA-DIA        PIC 99.  
       
       01  WK-FECHA-HASTA-ED.
            03 WK-FECHA-HASTA-MES-ED     PIC X(03).
            03 FILLER                    PIC X VALUE "-".
            03 WK-FECHA-HASTA-ANHIO-ED   PIC 9999.

