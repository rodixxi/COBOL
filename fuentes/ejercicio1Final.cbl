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
       PROGRAM-ID. EJERCICIO1. 
       AUTHOR. CRESPILLO RODRIGO ANDRES.
       INSTALLATION.
       DATE-WRITTEN. 13/11/2015.
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
           ASSIGN TO "D:\COBOL\arch\M-ALUMNOS.txt".
           SELECT LISTADO
           ASSIGN TO "D:\COBOL\listado\LISTADO.txt".
      *----------------------------------------------------------------     
       DATA DIVISION.

       FILE SECTION.
       FD  ARCHIVO.
       01  REG-ARCHIVO.
            03 ARCHIVO-DNI              PIC 9(08).
            03 ARCHIVO-NOMBRE           PIC X(30).
            03 ARCHIVO-FECHA            PIC 9(08).
       FD  LISTADO.
       01  REG-LIS                      PIC X(60).

       WORKING-STORAGE SECTION.
       77  WK-LEIDOS                    PIC 9(09).
       77  WK-FINAL                     PIC 9(01).
       77  WK-LINEA                     PIC 9(09).

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
       
       01  TITULO-01.
            03 TIT-FECHA                PIC X(10).
            03 FILLER                   PIC X(8) VALUE SPACES.
            03 FILLER                   PIC X(15) VALUE 
            "LISTADO ALUMNOS".
            03 FILLER                   PIC X(18) VALUE SPACES.
            03 FILLER                   PIC X(06) VALUE "HOJA: ".
            03 TIT-HOJA                 PIC 9(03).
           
       01  TITULO-LINE                  PIC X(60) VALUE ALL "-".

       01  TITULO-03.                    
            03 FILLER                   PIC X(30) VALUE "NOMBRE".
            03 FILLER                   PIC X(14) VALUE "DNI".
            03 FILLER                   PIC X(16) VALUE 
            "FECHA NACIMIENTO".

       01  LIN-DETALLE.
            03 LIN-DET-NOMBRE           PIC X(30).
            03 FILLER                   PIC X(6) VALUE SPACES.
            03 LIN-DET-DNI              PIC X(08).
            03 FILLER                   PIC X(06) VALUE SPACES.
            03 LIN-DET-FECHA-NACIMIENTO PIC X(10).

       01  TITULO-BOTTOM-FINAL.
            03 FILLER                   PIC X(13) VALUE
            "TOTAL ALUMNOS".
            03 LIN-TOT-ALUMN            PIC ZZZZZZZ9.

       01  COMPLETADOR                  PIC X(60) VALUE SPACES.

       LINKAGE SECTION.
       SCREEN SECTION.
      *----------------------------------------------------------------
       PROCEDURE DIVISION.
       CONTROL-PROG.
           PERFORM INICIO     THRU F-INICIO
           PERFORM PROCESO    THRU F-PROCESO 
           TEST BEFORE UNTIL WK-FINAL <> 0 
           PERFORM FINAL-PROG THRU F-FINAL-PROG
           GOBACK.
      
      * ABRE ARCHIVO Y ANHADE ENCABEZADO
       INICIO.
           PERFORM ABRIR-ARCHIVO THRU F-ABRIR-ARCHIVO
           PERFORM ENCABEZAR     THRU F-ENCABEZAR.
       F-INICIO.

      * ABRE EL ARCHIVO
       ABRIR-ARCHIVO.
           OPEN INPUT ARCHIVO
           OPEN OUTPUT LISTADO.
       F-ABRIR-ARCHIVO.

       ENCABEZAR.
      * ACEPTA HORA DEL SISTEMA Y LA PONE EN EL LISTADO 
           ACCEPT WK-FECHA FROM CENTURY-DATE

           PERFORM MOVER-FECHA THRU F-MOVER-FECHA
           MOVE WK-FECHA-ED  TO TIT-FECHA
           
           ADD 1 TO TIT-HOJA
           
      * IMPRIME ENCABEZADO
           WRITE REG-LIS FROM TITULO-01
           WRITE REG-LIS FROM TITULO-LINE
           WRITE REG-LIS FROM TITULO-03
           WRITE REG-LIS FROM TITULO-LINE

           MOVE 4 TO WK-LINEA.

       F-ENCABEZAR.
       
       MOVER-FECHA.
           MOVE WK-FEC-ANHIO TO WK-FEC-ANHIO-ED
           MOVE WK-FEC-MES   TO WK-FEC-MES-ED
           MOVE WK-FEC-DIA   TO WK-FEC-DIA-ED.
       F-MOVER-FECHA.
       
       PROCESO.
           PERFORM LEER-ARCHIVO THRU F-LEER-ARCHIVO

           INITIALIZE LIN-DETALLE
      * SI EL CONTADOR ES MAYOR A 58 AGREGO 1 HOJA
           IF WK-LINEA > 59
      *      ADD 1 TO TIT-HOJA
      *      WRITE REG-LIS FROM TITULO-LINE
      *      WRITE REG-LIS FROM COMPLETADOR
            PERFORM ENCABEZAR
           END-IF
           
           PERFORM DETALLE THRU F-DETALLE.
       F-PROCESO.
       
       LEER-ARCHIVO. 
      * LEEMOS HASTA EL FINAL DEL ARCHIVO
           READ ARCHIVO 
            AT END 
            MOVE 1 TO WK-FINAL
           END-READ
           
      * AGREGAMOS 1 AL CONTADOR DE ALUMNOS     
           
           ADD 1 TO WK-LEIDOS.
       F-LEER-ARCHIVO.

       DETALLE.
           MOVE ARCHIVO-DNI    TO LIN-DET-DNI
           MOVE ARCHIVO-NOMBRE TO LIN-DET-NOMBRE
           MOVE ARCHIVO-FECHA  TO WK-FECHA
           PERFORM MOVER-FECHA THRU F-MOVER-FECHA
           MOVE WK-FECHA-ED    TO LIN-DET-FECHA-NACIMIENTO

           WRITE REG-LIS FROM LIN-DETALLE
           ADD 1 TO WK-LINEA.
       F-DETALLE.

       FINAL-PROG.
      *COMPLETA LAS LINEAS FALTANTES HASTA EL FINAL
      *     IF WK-LINEA > 58
            PERFORM TOTALES THRU F-TOTALES
      *     END-IF

           PERFORM CERRAR-ARCHIVO THRU F-CERRAR-ARCHIVO.
       F-FINAL-PROG.

      * COMPLETAR.
      *     WRITE REG-LIS FROM COMPLETADOR
      *     ADD 1 TO WK-LINEA. 
      * F-COMPLETAR.
      
       TOTALES.
      * IMPRIME PIE DE PAGINA CON TOTAL DE ALUMNOS
           IF WK-LINEA > 58
            PERFORM ENCABEZAR
           END-IF
           MOVE WK-LEIDOS TO LIN-TOT-ALUMN
           WRITE REG-LIS FROM TITULO-LINE
           WRITE REG-LIS FROM TITULO-BOTTOM-FINAL.
       F-TOTALES.

       CERRAR-ARCHIVO.
           CLOSE ARCHIVO
                 LISTADO.
       F-CERRAR-ARCHIVO.
      *----------------------------------------------------------------