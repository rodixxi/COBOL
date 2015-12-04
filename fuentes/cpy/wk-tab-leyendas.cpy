       01  TAB-LEYENDAS-COMPLETA.
           03 FILLER           PIC X(23) 
           VALUE "COD. MOV. INCORRECTO   ".
           03 FILLER           PIC X(23) 
           VALUE "CAMPOS VACIOS EN ALTA  ".
           03 FILLER           PIC X(23) 
           VALUE "MODIFICACION SIN CUENTA".
           03 FILLER           PIC X(23) 
           VALUE "MODIFICACION SIN DATOS ".
           03 FILLER           PIC X(23) 
           VALUE "BAJA SIN CUENTA        ".
       01  FILLER REDEFINES TAB-LEYENDAS-COMPLETA.
           03 TAB-LEYENDA     PIC X(23) OCCURS 5.