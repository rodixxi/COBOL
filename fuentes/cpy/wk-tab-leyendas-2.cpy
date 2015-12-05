       01  TAB-LEYENDAS-COMPLETA.
           03 FILLER           PIC X(23) 
           VALUE "ALTA YA EXISTE         ".
           03 FILLER           PIC X(23) 
           VALUE "MODIFICACION NO EXISTE ".
           03 FILLER           PIC X(23) 
           VALUE "BAJA NO EXISTE         ".
           03 FILLER           PIC X(23) 
           VALUE "TOTALES NO BALANCEAN   ".
       01  FILLER REDEFINES TAB-LEYENDAS-COMPLETA.
           03 TAB-LEYENDA     PIC X(23) OCCURS 4.