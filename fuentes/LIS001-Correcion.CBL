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
           SELECT M-CUENTAS
           ASSIGN TO
           "d:/curso cobol/2015/evaluaciones/crespillo/arch\mcuentas"
           ORGANIZATION IS SEQUENTIAL.
           SELECT LISTADO
           ASSIGN TO
           "d:/curso cobol/2015/evaluaciones/crespillo/listado\LISTADO"
           ORGANIZATION IS SEQUENTIAL.
      *----------------------------------------------------------------
       DATA DIVISION.

       FILE SECTION.
       
       COPY "cpy\fd-ctas-reg.fds".

       FD  LISTADO.
       01  REG-LIS                      PIC X(100).

       WORKING-STORAGE SECTION.

       COPY "cpy\wk-tabla-aperturas.cpy".
       COPY "cpy\wk-fecha-vuelta.cpy".

       77  WK-LEIDOS                    PIC 9(09).
       77  WK-FINAL                     PIC 9(01).
       77  WK-LINEA                     PIC 9(06) VALUE 90.
       77  WK-LINEA-IMPRESA             PIC 9(06).

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
           PERFORM FINAL-PROG THRU F-FINAL-PROG
           GOBACK.

      * ABRE ARCHIVO Y ANHADE ENCABEZADO
       INICIO.
           PERFORM ABRIR-ARCHIVO THRU F-ABRIR-ARCHIVO
      * ACEPTA HORA DEL SISTEMA Y LA PONE EN EL LISTADO
           ACCEPT WK-FECHA FROM CENTURY-DATE
           PERFORM MOVER-FECHA   THRU F-MOVER-FECHA
           MOVE WK-FECHA-ED        TO TIT-FECHA.
       F-INICIO.

      * ABRE EL ARCHIVO
       ABRIR-ARCHIVO.
           OPEN INPUT M-CUENTAS
           OPEN OUTPUT LISTADO.
       F-ABRIR-ARCHIVO.

       ENCABEZAR.
           ADD 1 TO TIT-HOJA

      * IMPRIME ENCABEZADO
           IF TIT-HOJA = 1
              WRITE REG-LIS FROM TITULO-01   AFTER 0
           ELSE
              WRITE REG-LIS FROM TITULO-01   AFTER PAGE
           END-IF
           WRITE REG-LIS FROM TITULO-LINE AFTER 2
           WRITE REG-LIS FROM TITULO-03   AFTER 1
           WRITE REG-LIS FROM TITULO-LINE AFTER 1

           MOVE 5 TO WK-LINEA.

       F-ENCABEZAR.

       PROCESO.
           PERFORM UNTIL 1 = 0
              READ M-CUENTAS AT END
                             MOVE 1 TO WK-FINAL
                             EXIT PERFORM
              END-READ

              IF CTAS-FECHA-NAC-MES = 12 AND
                 (CTAS-APERTURA >= 0 AND <= 3)
                 INITIALIZE LIN-DETALLE
      * SI EL CONTADOR ES MAYOR A 64 AGREGO 1 HOJA
                 IF WK-LINEA > 30
                    PERFORM ENCABEZAR
                 END-IF
                 PERFORM DETALLE THRU F-DETALLE
                 ADD 1 TO WK-LEIDOS
              END-IF
           END-PERFORM.
       F-PROCESO.


       DETALLE.
           MOVE CTAS-DOCUMENTO      TO L-DOC
           MOVE CTAS-APELLIDO       TO L-APE
           MOVE CTAS-NOMBRE         TO L-NOM
           MOVE CTAS-FECHA-NAC      TO WK-FECHA
           PERFORM MOVER-FECHA    THRU F-MOVER-FECHA
           MOVE WK-FECHA-ED         TO L-FNAC
           MOVE CTAS-PROVINCIA      TO L-PRV
           MOVE CTAS-APERTURA       TO WK-APERTURA
           PERFORM DETALLE-APERTURA THRU F-DETALLE-APERTURA
           MOVE WK-DETALLE-APERTURA TO L-APER

           WRITE REG-LIS FROM LIN-DETALLE
           ADD 1 TO WK-LINEA-IMPRESA
           ADD 1 TO WK-LINEA.
       F-DETALLE.

       FINAL-PROG.
           PERFORM TOTALES           THRU F-TOTALES
           PERFORM CERRAR-ARCHIVO    THRU F-CERRAR-ARCHIVO
           PERFORM VERIFICAR-TOTALES THRU F-VERIFICAR-TOTALES.
       F-FINAL-PROG.

       TOTALES.
      * IMPRIME PIE DE PAGINA CON TOTAL DE ALUMNOS
           IF WK-LINEA > 30
            PERFORM ENCABEZAR
           END-IF
           MOVE WK-LEIDOS TO LIN-TOT-ALUMN
           WRITE REG-LIS FROM TITULO-LINE
           WRITE REG-LIS FROM TITULO-BOTTOM-FINAL.
       F-TOTALES.

       VERIFICAR-TOTALES.

           DISPLAY MESSAGE "Leidos: " WK-LEIDOS
                           " Impresos: " WK-LINEA-IMPRESA H"0A"
                           "Enter para continuar"

           IF WK-LEIDOS <> WK-LINEA-IMPRESA
            DISPLAY MESSAGE "Cantidad de Cuentas no balancea"
           END-IF.
       F-VERIFICAR-TOTALES.

       CERRAR-ARCHIVO.
           CLOSE M-CUENTAS
                 LISTADO.
       F-CERRAR-ARCHIVO.

       COPY "cpy\procedure-fecha-vuelta.cpy".
       COPY "cpy\procedure-search-detalle.cpy".
      *----------------------------------------------------------------
