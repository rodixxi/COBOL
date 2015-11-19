      * =================================================================== */
      *                                                                     */
      *   COBOLNAME.CBL                                                     */
      *   (C) 2008 AUTHOR                                                   */
      *                                                                     */
      *   DESCRIPTION                                                       */
      *                                                                    .*/
      * =================================================================== */
      *PROGRAM DESCRIPTION
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIS001. 
       AUTHOR. CRESPILLO RODRIGO ANDRES.
       INSTALLATION.
       DATE-WRITTEN. 18/11/2015.
       DATE-COMPILED.
      *---------------------------------------------------------------- 
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. NOMBRE COMPUTADIR FUENTE.
       OBJECT-COMPUTER. NOMBRE COMPUTADOR OBJETO.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO
           ASSIGN TO "\COBOL\arch\mcuentas2"
           ORGANIZATION IS SEQUENTIAL.
      *----------------------------------------------------------------     
       DATA DIVISION.

       FILE SECTION.
       
       COPY "\COBOL\fuentes\cpy\fd-ctas-reg.fds".
            
       WORKING-STORAGE SECTION.
       77  WK-FINAL                     PIC 9(01).
       77  WK-CONTADOR-BAJA             PIC 9(04).

       LINKAGE SECTION.
       SCREEN SECTION.
      *----------------------------------------------------------------
       PROCEDURE DIVISION.
       CONTROL-PROG.
           PERFORM INICIO     THRU F-INICIO
           PERFORM PROCESO    THRU F-PROCESO 
           PERFORM FINAL-PROG THRU F-FINAL-PROG
           GOBACK.
      
      * ABRE ARCHIVO Y ANHADE ENCABEZADO
       INICIO.
           PERFORM ABRIR-ARCHIVO THRU F-ABRIR-ARCHIVO.
       F-INICIO.

      * ABRE EL ARCHIVO
       ABRIR-ARCHIVO.
           OPEN INPUT ARCHIVO.
       F-ABRIR-ARCHIVO.
       
       PROCESO.
           PERFORM UNTIL WK-FINAL= 1
              READ ARCHIVO AT END 
                   MOVE 1 TO WK-FINAL
                   EXIT PERFORM CYCLE
              END-READ
              EVALUATE CTAS-FECHA-BAJA
                   WHEN 0 ADD 1 TO WK-CONTADOR-BAJA
              END-EVALUATE 
           END-PERFORM. 
       F-PROCESO.
       

       FINAL-PROG.
           PERFORM CERRAR-ARCHIVO    THRU F-CERRAR-ARCHIVO
           PERFORM VERIFICAR-TOTALES THRU F-VERIFICAR-TOTALES.
       F-FINAL-PROG.
      
       VERIFICAR-TOTALES. 
           DISPLAY "Cuentas en Baja: "   
           AT 1016 WK-CONTADOR-BAJA CONVERT
           DISPLAY MESSAGE "Enter para continuar"
           END-DISPLAY.
       F-VERIFICAR-TOTALES.

       CERRAR-ARCHIVO.
           CLOSE ARCHIVO.
       F-CERRAR-ARCHIVO.
       
      *----------------------------------------------------------------