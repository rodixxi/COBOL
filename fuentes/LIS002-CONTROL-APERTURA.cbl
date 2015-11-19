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
       77  WK-APERTURA-NORMAL           PIC 9(04).
       77  WK-APERTURA-AMPLIAR          PIC 9(04).
       77  WK-APERTURA-INTERNACIONAL    PIC 9(04).
       77  WK-APERTURA-ESTUDIO          PIC 9(04).
       77  WK-APERTURA-FDOCUM           PIC 9(04).
       77  WK-APERTURA-DENEGADA         PIC 9(04).
       77  WK-APERTURA-ANALISIS         PIC 9(04).

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
              PERFORM CONTADORES-APERTURA THRU F-CONTADORES-APERTURA
           END-PERFORM.
       F-PROCESO.
       
       CONTADORES-APERTURA.
           EVALUATE CTAS-APERTURA
              WHEN 1 ADD 1 TO WK-APERTURA-NORMAL
              WHEN 2 ADD 1 TO WK-APERTURA-AMPLIAR
              WHEN 3 ADD 1 TO WK-APERTURA-INTERNACIONAL
              WHEN 6 ADD 1 TO WK-APERTURA-ESTUDIO
              WHEN 7 ADD 1 TO WK-APERTURA-FDOCUM
              WHEN 8 ADD 1 TO WK-APERTURA-DENEGADA
              WHEN 9 ADD 1 TO WK-APERTURA-ANALISIS
           END-EVALUATE.
       F-CONTADORES-APERTURA.

       FINAL-PROG.
           PERFORM CERRAR-ARCHIVO    THRU F-CERRAR-ARCHIVO
           PERFORM VERIFICAR-TOTALES THRU F-VERIFICAR-TOTALES.
       F-FINAL-PROG.
      
       VERIFICAR-TOTALES. 
           DISPLAY "Normal: "   
           AT 1016 WK-APERTURA-NORMAL CONVERT
           DISPLAY "Ampliar: " 
           AT 1216 WK-APERTURA-AMPLIAR CONVERT
           DISPLAY "Internacional :"   
           AT 1416 WK-APERTURA-INTERNACIONAL CONVERT
           DISPLAY "Estudio: "   
           AT 1616 WK-APERTURA-ESTUDIO CONVERT
           DISPLAY "F.Docum: " 
           AT 1816 WK-APERTURA-FDOCUM CONVERT
           DISPLAY "Denegada :"   
           AT 2016 WK-APERTURA-DENEGADA CONVERT
           DISPLAY "Analisis :"   
           AT 2216 WK-APERTURA-ANALISIS CONVERT
           DISPLAY MESSAGE "Enter para continuar"
           END-DISPLAY.
       F-VERIFICAR-TOTALES.

       CERRAR-ARCHIVO.
           CLOSE ARCHIVO.
       F-CERRAR-ARCHIVO.
       
      *----------------------------------------------------------------