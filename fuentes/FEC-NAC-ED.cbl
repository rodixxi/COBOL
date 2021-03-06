      * =================================================================== */
      *                                                                     */
      *   COBOLNAME.CBL                                                     */
      *   (C) 2008 AUTHOR                                                   */
      *                                                                     */
      *   Recive fecha aaaammdd y lo transforma en dd/mm/aaaa, dd-mm-aaaa   */
      *   y MMM-aaaa                                                       .*/
      * =================================================================== */
      *PROGRAM DESCRIPTION
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FC-NAC-ED.
       AUTHOR. CRESPILLO RODRIGO ANDRES.
       ENVIRONMENT DIVISION.
      *---------------------------------------------------------------- 
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  TAB-MESES                    PIC X(36) 
           VALUE "ENEFEBAMRABRMAYJUNJULAGOSETOCTNOVDIC".
       01  FILLER REDEFINES TAB-MESES.
           03 TAB-MES                   PIC X(3) OCCURS 12.  

       01  FECHA-ENTRADA                    PIC 9(08).
       01  FILLER REDEFINES FECHA-ENTRADA.
           03 FECHA-ENTRADA-ANHIO           PIC 9999.
           03 FECHA-ENTRADA-MES             PIC 99.
           03 FECHA-ENTRADA-DIA             PIC 99.

       01  FECHA-SALIDA-1.
           03 FECHA-SALIDA-1-DIA            PIC 99.
           03 FILLER                        PIC X VALUE "/".
           03 FECHA-SALIDA-1-MES            PIC 99.
           03 FILLER                        PIC X VALUE "/".
           03 FECHA-SALIDA-1-ANHIO          PIC 9999.

       01  FECHA-SALIDA-2. 
           03 FECHA-SALIDA-2-DIA            PIC 99.
           03 FILLER                        PIC X VALUE "-".
           03 FECHA-SALIDA-2-MES            PIC 99.
           03 FILLER                        PIC X VALUE "-".
           03 FECHA-SALIDA-2-ANHIO          PIC 9999.

       01  FECHA-SALIDA-3.
           03 FECHA-SALIDA-3-MES            PIC XXX.
           03 FILLER                        PIC X VALUE "-".
           03 FECHA-SALIDA-3-ANHIO          PIC 9999.

       LINKAGE SECTION.

       01  LK-FECHA-ENTRADA                 PIC 9(08).
       01  LK-FECHA-SALIDA-1                PIC X(10).  
       01  LK-FECHA-SALIDA-2                PIC X(10).
       01  LK-FECHA-SALIDA-3                PIC X(08).

      *----------------------------------------------------------------
       PROCEDURE DIVISION USING LK-FECHA-ENTRADA 
                                LK-FECHA-SALIDA-1
                                LK-FECHA-SALIDA-2
                                LK-FECHA-SALIDA-3.
       CONTROL-PROG.
           PERFORM ENTRADA     THRU F-ENTRADA
           PERFORM MV-FECHA-1  THRU F-MV-FECHA-1
           PERFORM MV-FECHA-2  THRU F-MV-FECHA-2
           PERFORM MV-FECHA-3  THRU F-MV-FECHA-3
           PERFORM SALIDA      THRU F-SALIDA
           EXIT PROGRAM.
       
       ENTRADA.
           MOVE LK-FECHA-ENTRADA TO FECHA-ENTRADA.
       F-ENTRADA. EXIT.

       MV-FECHA-1.
           MOVE FECHA-ENTRADA-ANHIO TO FECHA-SALIDA-1-ANHIO
           MOVE FECHA-ENTRADA-MES   TO FECHA-SALIDA-1-MES 
           MOVE FECHA-ENTRADA-DIA   TO FECHA-SALIDA-1-DIA.
       F-MV-FECHA-1.EXIT.

       MV-FECHA-2.
           MOVE FECHA-ENTRADA-ANHIO TO FECHA-SALIDA-2-ANHIO
           MOVE FECHA-ENTRADA-MES   TO FECHA-SALIDA-2-MES 
           MOVE FECHA-ENTRADA-DIA   TO FECHA-SALIDA-2-DIA.
       F-MV-FECHA-2.EXIT.

       MV-FECHA-3.
           MOVE TAB-MES(FECHA-ENTRADA-MES) TO FECHA-SALIDA-3-MES
           MOVE FECHA-ENTRADA-ANHIO        TO FECHA-SALIDA-3-ANHIO.
       F-MV-FECHA-3.EXIT.   

       SALIDA.
           MOVE FECHA-SALIDA-1 TO LK-FECHA-SALIDA-1
           MOVE FECHA-SALIDA-2 TO LK-FECHA-SALIDA-2
           MOVE FECHA-SALIDA-3 TO LK-FECHA-SALIDA-3.
       F-SALIDA. EXIT.   

       END PROGRAM.
