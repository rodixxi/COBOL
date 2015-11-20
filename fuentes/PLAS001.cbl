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
           SELECT LISTADO
           ASSIGN TO "\COBOL\listado\LIS-XLS.xls"
           ORGANIZATION IS SEQUENTIAL.
       COPY "\COBOL\fuentes\cpy\plasticos.sel".
      *----------------------------------------------------------------     
       DATA DIVISION.

       FILE SECTION.
       
       COPY "\COBOL\fuentes\cpy\fd-ctas-reg.fds".
       COPY "\COBOL\fuentes\cpy\plasticos.fds".

       FD  LISTADO.
       01  REG-LIS                      PIC X(100).

       WORKING-STORAGE SECTION.

       COPY "\COBOL\fuentes\cpy\wk-tabla-aperturas.cpy".
       COPY "\COBOL\fuentes\cpy\wk-fecha-vuelta.cpy".

       77  WK-FINAL                     PIC 9.
       77  WK-LEIDOS                    PIC 9(04).
       77  WK-FINAL                     PIC 9(01).
       77  WK-LINEA-GRABADA             PIC 9(04).
       
       01  TAB-MESES                    PIC X(36) 
           VALUE "ENEFEBAMRABRMAYJUNJULAGOSETOCTNOVDIC".
       01  FILLER REDEFINES TAB-MESES 
           03 TAB-MES                   PIC X(3) OCCURS 12.

       01  TITULO-01.
            03 TIT-FECHA                PIC X(10).
            03 FILLER                   PIC X(25) VALUE SPACES.
            03 FILLER                   PIC X(30) VALUE 
            "LISTADO DE PLASTICOS A REPONER".
            03 FILLER                   PIC X(26) VALUE SPACES.
            03 FILLER                   PIC X(06) VALUE "HOJA: ".
            03 TIT-HOJA                 PIC 9(03).
           
       01  TITULO-LINE                  PIC X(100) VALUE ALL "_".

       01  TITULO-03.
            03 FILLER                   PIC X(09) VALUE "Cuenta". 
            03 FILLER                   PIC X(05) VALUE ALL SPACES.                  
            03 FILLER                   PIC X(08) VALUE "Titular".
            03 FILLER                   PIC X(18) VALUE ALL SPACES.
            03 FILLER                   PIC X(06) VALUE "Prv".
            03 FILLER                   PIC X(18) VALUE ALL SPACES.
            03 FILLER                   PIC X(08) VALUE "Aper".
            03 FILLER                   PIC X(06) VALUE ALL SPACES.
            03 FILLER                   PIC X(04) VALUE "Plastico".
            03 FILLER                   PIC X(07) VALUE ALL SPACES.
            03 FILLER                   PIC X(11) VALUE "Est".
            03 FILLER                   PIC X(07) VALUE ALL SPACES.
            03 FILLER                   PIC X(11) VALUE "Hasta".            

       01  LIN-DETALLE.
            03 L-DOC                    PIC 9(08).
            03 FILLER                   PIC X(05) VALUE ALL SPACES.
            03 L-NOM-CORTO              PIC X(25).
            03 FILLER                   PIC X(05) VALUE ALL SPACES.
            03 L-PROV                   PIC X.
            03 FILLER                   PIC X(05) VALUE ALL SPACES.
            03 L-APER                   PIC 9.
            03 FILLER                   PIC X(07) VALUE ALL SPACES.
            03 L-PLAS                   PIC X(19).
            03 FILLER                   PIC X(06) VALUE ALL SPACES.
            03 L-EST                    PIC XX.
            03 FILLER                   PIC X(06) VALUE ALL SPACES.
            03 L-FHAS                   PIC X(06).

       01  TITULO-BOTTOM-LEIDAS.
            03 FILLER                   PIC X(20) VALUE
            "Cuestas Leidas....: ".
            03 LIN-TOT-ALUMN            PIC ZZZZZ9.
       01  TITULO-BOTTOM-LEIDAS.
            03 FILLER                   PIC X(20) VALUE
            "Plasticos a repoer: ".
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
           PERFORM ENCABEZAR     THRU F-ENCABEZAR.
       F-INICIO.

      * ABRE EL ARCHIVO
       ABRIR-ARCHIVO.
           OPEN INPUT ARCHIVO
           OPEN INPUT M-PLASTICOS
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
       
       PROCESO.
           PERFORM UNTIL WK-FINAL= 1
              READ ARCHIVO AT END 
                   MOVE 1 TO WK-FINAL
                   EXIT PERFORM CYCLE
              END-READ
           
              IF CTAS-FECHA-BAJA = 0 AND
              (CTAS-APERTURA = 1 OR  2 OR 3)
              
                    MOVE CTAS-DOCUMENTO TO PLAS-DOCUMENTO
                    READ M-PLASTICO
                         INVALID KEY
                         DISPLAY MESSAGE "Documento no Encontrado"
                         END-DISPLAY
                    END-READ
                    IF PLAS-ESTADO = "EX"
                         
                        INITIALIZE LIN-DETALLE
      * SI EL CONTADOR ES MAYOR A 64 AGREGO 1 HOJA
                        IF WK-LINEA > 64
                             PERFORM ENCABEZAR
                        END-IF
                        PERFORM DETALLE THRU F-DETALLE
                        ADD 1 TO WK-LEIDOS
                    END-IF
              END-IF
           END-PERFORM.
       F-PROCESO.
       

       DETALLE.
           MOVE CTAS-DOCUMENTO       TO L-DOC
           MOVE PLAS-NOMBRE-CORTO    TO L-NOM-CORTO
           MOVE CTAS-PROVINCIA       TO L-PROV
           MOVE CTAS-APERTURA        TO WK-APERTURA
           PERFORM DETALLE-APERTURA  THRU F-DETALLE-APERTURA
           MOVE WK-DETALLE-APERTURA  TO L-APER
           MOVE PLAS-PLASTICO        TO L-PLAS
           MOVE PLAS-ESTADO          TO L-EST
           
           

           WRITE REG-LIS FROM LIN-DETALLE
           ADD 1 TO WK-LINEA-IMPRESA
           ADD 1 TO WK-LINEA.
       F-DETALLE.
       
       GENERAR-LISTADO.
           INITIALIZE WK-SIN-PLAS
                      PLAS-REG
           MOVE CTAS-DOCUMENTO TO PLAS-DOCUMENTO
           READ M-PLASTICOS KEY IS PLAS-CLAVE-1 INVALID KEY
                       

       FINAL-PROG.
           PERFORM CERRAR-ARCHIVO    THRU F-CERRAR-ARCHIVO
           PERFORM VERIFICAR-TOTALES THRU F-VERIFICAR-TOTALES.
       F-FINAL-PROG.
      
       VERIFICAR-TOTALES. 
           DISPLAY "Leidos: "   AT 1016 WK-LEIDOS CONVERT
           DISPLAY "Considerados: " AT 1216 WK-LINEA-CONSIDERADA CONVERT
           DISPLAY "Grabadas :"   AT 1416 WK-LINEA-GRABADA CONVERT
           DISPLAY MESSAGE "Enter para continuar"
           END-DISPLAY
           IF WK-LINEA-GRABADA <> WK-LINEA-CONSIDERADA
              DISPLAY MESSAGE "Cuentas no balancean"
              END-DISPLAY
           END-IF.
       F-VERIFICAR-TOTALES.

       CERRAR-ARCHIVO.
           CLOSE ARCHIVO
                 LISTADO.
       F-CERRAR-ARCHIVO.

       COPY "\COBOL\fuentes\cpy\procedure-fecha-vuelta.cpy".
       COPY "\COBOL\fuentes\cpy\procedure-search-detalle.cpy".
      *----------------------------------------------------------------