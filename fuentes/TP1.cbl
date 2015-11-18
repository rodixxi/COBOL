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
           ASSIGN TO "D:\COBOL\arch\mcuentas"
           ORGANIZATION IS SEQUENTIAL.
           SELECT LISTADO
           ASSIGN TO "D:\COBOL\listado\listadoTP"
           ORGANIZATION IS SEQUENTIAL.
      *----------------------------------------------------------------     
       DATA DIVISION.

       FILE SECTION.
       FD  ARCHIVO.
       01  CTAS-REG.
            03 CTAS-DOCUMENTO           PIC 9(08).
            03 CTAS-APELLIDO            PIC X(20).
            03 CTAS-NOMBRE              PIC X(20).
            03 CTAS-FECHA-NAC.           
             05 CTAS-FECHA-NAC-ANHO      PIC 9999.
             05 CTAS-FECHA-NAC-MES       PIC 99.
             05 CTAS-FECHA-NAC-DIA       PIC 99.
            03 CTAS-PROVINCIA           PIC X(01).
            03 CTAS-APERTURA            PIC 9(01).
            03 CTAS-SITUACION           PIC 9(01).
            03 CTAS-SALDO               PIC S9(10)V99.
            03 CTAS-FECHA-BAJA          PIC 9(08).
            03 FILLER                   PIC X(21).

       FD  LISTADO.
       01  REG-LIS                      PIC X(100).

       WORKING-STORAGE SECTION.
       77  WK-LEIDOS                    PIC 9(09).
       77  WK-FINAL                     PIC 9(01).
       77  WK-LINEA                     PIC 9(06).
       77  WK-LINEA-IMPRESA             PIC 9(06).
       77  WK-DETALLE-APERTURA          PIC X(13).

       01  TAB-APERTURA.
            03 FILLER                   PIC X(14)
            VALUE "1Normal       ".
            03 FILLER                   PIC X(14)
            VALUE "2Ampliar      ".
            03 FILLER                   PIC X(14)
            VALUE "3Internacional".
            03 FILLER                   PIC X(14)
            VALUE "6Estudio      ".
            03 FILLER                   PIC X(14)
            VALUE "7F.Docum      ".
            03 FILLER                   PIC X(14)
            VALUE "8Denegada     ".
            03 FILLER                   PIC X(14)
            VALUE "9Analisis     ".
       01  TAB-APERTURA-BYINDEX REDEFINES TAB-APERTURA.
            03 TAB-APERTURA-DETALLE OCCURS 7
               INDEXED BY APER-INDEX.
               05 TAB-CTAS-APERTURA         PIC 9.
               05 TAB-CTAS-APERTURA-DETALLE PIC X(13).


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
       01  WK-APERTURA                  PIC 9.       
       01  TITULO-01.
            03 TIT-FECHA                PIC X(10).
            03 FILLER                   PIC X(31) VALUE SPACES.
            03 FILLER                   PIC X(18) VALUE 
            "LISTADO DE CUENTAS".
            03 FILLER                   PIC X(32) VALUE SPACES.
            03 FILLER                   PIC X(06) VALUE "HOJA: ".
            03 TIT-HOJA                 PIC 9(03).
           
       01  TITULO-LINE                  PIC X(100) VALUE ALL "_".

       01  TITULO-03.
            03 FILLER                   PIC X(09) VALUE "Documento". 
            03 FILLER                   PIC X(05) VALUE ALL SPACES.                  
            03 FILLER                   PIC X(08) VALUE "Apellido".
            03 FILLER                   PIC X(18) VALUE ALL SPACES.
            03 FILLER                   PIC X(06) VALUE "Nombre".
            03 FILLER                   PIC X(18) VALUE ALL SPACES.
            03 FILLER                   PIC X(08) VALUE "Fec.Nac.".
            03 FILLER                   PIC X(06) VALUE ALL SPACES.
            03 FILLER                   PIC X(04) VALUE "Prov".
            03 FILLER                   PIC X(07) VALUE ALL SPACES.
            03 FILLER                   PIC X(11) VALUE "Apertura".            

       01  LIN-DETALLE.
            03 L-DOC                    PIC X(08).
            03 FILLER                   PIC X(05) VALUE ALL SPACES.
            03 L-APE                    PIC X(10).
            03 FILLER                   PIC X(05) VALUE ALL SPACES.
            03 L-NOM                    PIC X(30).
            03 FILLER                   PIC X(05) VALUE ALL SPACES.
            03 L-FNAC                   PIC X(10).
            03 FILLER                   PIC X(07) VALUE ALL SPACES.
            03 L-PRV                    PIC X.
            03 FILLER                   PIC X(06) VALUE ALL SPACES.
            03 L-APER                   PIC X(13).

       01  TITULO-BOTTOM-FINAL.
            03 FILLER                   PIC X(18) VALUE
            "TOTAL DE CUENTAS: ".
            03 LIN-TOT-ALUMN            PIC ZZZZZ9.


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
           IF CTAS-FECHA-NAC-MES = 12 AND (CTAS-APERTURA = 1 
           OR CTAS-APERTURA = 2 OR CTAS-APERTURA = 3)

            INITIALIZE LIN-DETALLE
      * SI EL CONTADOR ES MAYOR A 64 AGREGO 1 HOJA
            IF WK-LINEA > 64
      *      
             PERFORM ENCABEZAR
            END-IF
           
            PERFORM DETALLE THRU F-DETALLE
           END-IF.
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
           MOVE CTAS-DOCUMENTO    TO L-DOC
           MOVE CTAS-APELLIDO     TO L-APE
           MOVE CTAS-NOMBRE       TO L-NOM
           MOVE CTAS-FECHA-NAC    TO WK-FECHA
           PERFORM MOVER-FECHA    THRU F-MOVER-FECHA
           MOVE WK-FECHA-ED       TO L-FNAC
           MOVE CTAS-PROVINCIA    TO L-PRV
           MOVE CTAS-APERTURA     TO WK-APERTURA
           PERFORM DETALLE-APERTURA THRU F-DETALLE-APERTURA
           MOVE WK-DETALLE-APERTURA TO L-APER

           WRITE REG-LIS FROM LIN-DETALLE
           ADD 1 TO WK-LINEA-IMPRESA
           ADD 1 TO WK-LINEA.
       F-DETALLE.

       DETALLE-APERTURA.
           SET APER-INDEX TO 1
           SEARCH TAB-APERTURA-DETALLE
            WHEN TAB-CTAS-APERTURA(APER-INDEX) = WK-APERTURA
             MOVE TAB-CTAS-APERTURA-DETALLE(APER-INDEX)
             TO WK-DETALLE-APERTURA
           END-SEARCH.
       F-DETALLE-APERTURA.

       FINAL-PROG.
           PERFORM TOTALES           THRU F-TOTALES
           PERFORM CERRAR-ARCHIVO    THRU F-CERRAR-ARCHIVO
           PERFORM VERIFICAR-TOTALES THRU F-VERIFICAR-TOTALES.
       F-FINAL-PROG.
      
       TOTALES.
      * IMPRIME PIE DE PAGINA CON TOTAL DE ALUMNOS
           IF WK-LINEA > 63
            PERFORM ENCABEZAR
           END-IF
           MOVE WK-LINEA-IMPRESA TO LIN-TOT-ALUMN
           WRITE REG-LIS FROM TITULO-LINE
           WRITE REG-LIS FROM TITULO-BOTTOM-FINAL.
       F-TOTALES.

       VERIFICAR-TOTALES.
           IF WK-LEIDOS <> WK-LINEA-IMPRESA
            DISPLAY "Cantidad de Cuentas no balancea" UPON CONSOLE
           END-IF.
       F-VERIFICAR-TOTALES.

       CERRAR-ARCHIVO.
           CLOSE ARCHIVO
                 LISTADO.
       F-CERRAR-ARCHIVO.
      *----------------------------------------------------------------