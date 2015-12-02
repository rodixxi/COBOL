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

       COPY "\COBOL\fuentes\cpy\wk-tab-meses.cpy".

       LINKAGE SECTION.
           
       01  FECHA-ENTRADA.
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
           03 FECHA-SALIDA-1-DIA            PIC 99.
           03 FILLER                        PIC X VALUE "-".
           03 FECHA-SALIDA-1-MES            PIC 99.
           03 FILLER                        PIC X VALUE "-".
           03 FECHA-SALIDA-1-ANHIO          PIC 9999.

       01  FECHA-SALIDA-3.
           03 FECHA-SALIDA-3-MES            PIC XXX.
           03 FILLER                        PIC X VALUE "-".
           03 FECHA-SALIDA-3-ANHIO          PIC 9999.
      *----------------------------------------------------------------
       PROCEDURE DIVISION USING FECHA-ENTRADA 
                                FECHA-SALIDA-1
                                FECHA-SALIDA-2
                                FECHA-SALIDA-3.
       CONTROL-PROG.
           PERFORM MV-FECHA-1  THRU F-MV-FECHA-1
           PERFORM MV-FECHA-2  THRU F-MV-FECHA-2
           PERFORM MV-FECHA-3  THRU F-MV-FECHA-3
           EXIT PROGRAM.
       
       MV-FECHA-1.
           MOVE FECHA-ENTRADA-ANHIO THRU FECHA-SALIDA-1-ANHIO
           MOVE FECHA-ENTRADA-MES   THRU FECHA-SALIDA-1-MES 
           MOVE FECHA-ENTRADA-DIA   THRU FECHA-SALIDA-1-DIA.
       F-MV-FECHA-1.EXIT.

       MV-FECHA-2.
           MOVE FECHA-ENTRADA-ANHIO THRU FECHA-SALIDA-2-ANHIO
           MOVE FECHA-ENTRADA-MES   THRU FECHA-SALIDA-2-MES 
           MOVE FECHA-ENTRADA-DIA   THRU FECHA-SALIDA-2-DIA.
       F-MV-FECHA-2.EXIT.

       MV-FECHA-1.
           MOVE TAB-MES(FECHA-ENTRADA-ANHIO) THRU FECHA-SALIDA-3-ANHIO
           MOVE FECHA-ENTRADA-MES            THRU FECHA-SALIDA-3-MES.
       F-MV-FECHA-1.EXIT.      

       END PROGRAM.
