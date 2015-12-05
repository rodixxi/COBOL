       01  WK-HS                        PIC 9(08).
       01  FILLER REDEFINES WK-HS.
            03 WK-HS-HORA               PIC 99.
            03 WK-HS-MIN                PIC 99.
            03 FILLER                   PIC 9(04).

       01  WK-HS-ED.
            03 WK-HS-HORA-ED            PIC 99.
            03 FILLER                   PIC X VALUE ":".
            03 WK-HS-MIN-ED             PIC 99.  