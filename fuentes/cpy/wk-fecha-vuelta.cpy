       01  WK-FECHA                     PIC 9(08).
       01  FILLER REDEFINES  WK-FECHA.
            03 WK-FEC-ANHIO             PIC X(04).
            03 WK-FEC-MES               PIC X(02).
            03 WK-FEC-DIA               PIC X(02).
          
       01  WK-FECHA-ED.
            03 WK-FEC-DIA-ED            PIC X(02).
            03 FILLER                   PIC X VALUE "/".
            03 WK-FEC-MES-ED            PIC X(02).
            03 FILLER                   PIC X VALUE "/".
            03 WK-FEC-ANHIO-ED          PIC X(04).
