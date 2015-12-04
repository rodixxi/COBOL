      * =================================================================== */
      *                                                                     */
      *   RIE001.CBL                                                       */
      *   CRESPILLO RODRIGO ANDRES                                          */
      *                                                                     */
      *   listado de las cuentas en riesgo                                 .*/
      * =================================================================== */
      *PROGRAM DESCRIPTION
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RIE001. 
       AUTHOR. CRESPILLO RODRIGO ANDRES.
       INSTALLATION.
       DATE-WRITTEN. 02/12/2015.
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
           ASSIGN TO "\COBOL\arch\mcuentas-parcial.idx"
           ORGANIZATION INDEXED
                           ACCESS DYNAMIC
                           RECORD KEY CTAS-CLAVE
                 ALTERNATE RECORD KEY CTAS-CLAVE-1
                           WITH DUPLICATES
                 ALTERNATE RECORD KEY CTAS-CLAVE-2 = CTAS-PROVINCIA
                           WITH DUPLICATES
                           FILE STATUS DB-STAT.
           SELECT LISTADO
           ASSIGN TO "\COBOL\listado\LISTADO-RIE001"
           ORGANIZATION IS SEQUENTIAL.
       COPY "\COBOL\fuentes\cpy\plasticos.sel".
      *----------------------------------------------------------------     
       DATA DIVISION.

       FILE SECTION.
       
       COPY "\COBOL\fuentes\cpy\fd-ctas-reg-idx.fds".

       FD  M-PLASTICOS.
       COPY "\COBOL\fuentes\cpy\plasticos.fds".

       FD  LISTADO.
       01  REG-LIS                      PIC X(100).

       WORKING-STORAGE SECTION.

       COPY "\COBOL\fuentes\cpy\wk-fecha-vuelta.cpy".
       COPY "\COBOL\fuentes\cpy\wk-fecha-hasta.cpy".
       COPY "\COBOL\fuentes\cpy\wk-codigo-plastico.cpy".
       COPY "\COBOL\fuentes\cpy\wk-tab-meses.cpy".
       COPY "\COBOL\fuentes\cpy\wk-tabla-situacion.cpy".


       77  WK-FINAL                     PIC 9.
       77  WK-LINEA                     PIC 9(04).
       77  WK-LEIDOS                    PIC 9(04).
       77  WK-PLAS-FINAL                PIC 9.
       77  WK-SIN-PLAS                  PIC 9.
       77  WK-LISTADO                   PIC 9(04).
       77  WK-SELECCIONADO              PIC 9(04).
       77  WK-SELECCIONADO-CONDICION    PIC 9.

       01  DB-STAT                      PIC X(02).

       01  WK-HS                        PIC 9(08).
       01  FILLER REDEFINES WK-HS.
            03 WK-HS-HORA               PIC 99.
            03 WK-HS-MIN                PIC 99.
            03 FILLER                   PIC 9(04).

       01  WK-HS-ED.
            03 WK-HS-HORA-ED            PIC 99.
            03 FILLER                   PIC X VALUE ":".
            03 WK-HS-MIN-ED             PIC 99.  
       
       01  TITULO-01.
            03 TIT-FECHA                PIC X(10).
            03 FILLER                   PIC X(03) VALUE " - ".
            03 TIT-HORA                 PIC X(05).
            03 FILLER                   PIC X(19) VALUE SPACES.
            03 FILLER                   PIC X(37) VALUE 
            "LISTADO DE CUENTAS EN RIESGO - RIE001".
            03 FILLER                   PIC X(18) VALUE SPACES.
            03 FILLER                   PIC X(05) VALUE "HOJA: ".
            03 TIT-HOJA                 PIC 9(03).
           
       01  TITULO-LINE                  PIC X(100) VALUE ALL "_".

       01  TITULO-03.
            03 FILLER                   PIC X(06) VALUE "Cuenta". 
            03 FILLER                   PIC X(13) VALUE ALL SPACES.                  
            03 FILLER                   PIC X(07) VALUE "Titular".
            03 FILLER                   PIC X(13) VALUE ALL SPACES.
            03 FILLER                   PIC X(03) VALUE "Sit".
            03 FILLER                   PIC X(04) VALUE ALL SPACES.
            03 FILLER                   PIC X(03) VALUE "Ape".
            03 FILLER                   PIC X(08) VALUE ALL SPACES.
            03 FILLER                   PIC X(08) VALUE "Plastico".
            03 FILLER                   PIC X(06) VALUE ALL SPACES.
            03 FILLER                   PIC X(03) VALUE "Est".
            03 FILLER                   PIC X(04) VALUE ALL SPACES.
            03 FILLER                   PIC X(05) VALUE "Hasta".
            03 FILLER                   PIC X(08) VALUE ALL SPACES.
            03 FILLER                   PIC X(05) VALUE "Saldo".          

       01  LIN-DETALLE.
            03 L-DOC                    PIC 9(08).
            03 FILLER                   PIC X(02) VALUE ALL SPACES.
            03 L-NOM-CORTO              PIC X(25).
            03 FILLER                   PIC X(02) VALUE ALL SPACES.
            03 L-SIT                    PIC X(07).
            03 FILLER                   PIC X(03) VALUE ALL SPACES.
            03 L-APER                   PIC 9.
            03 FILLER                   PIC X(03) VALUE ALL SPACES.
            03 L-PLAS                   PIC X(19).
            03 FILLER                   PIC X(02) VALUE ALL SPACES.
            03 L-EST                    PIC XX.
            03 FILLER                   PIC X(03) VALUE ALL SPACES.
            03 L-FHAS                   PIC X(08).
            03 FILLER                   PIC X(04) VALUE ALL SPACES.
            03 L-SDO                    PIC ----.--9,99.

       01  TITULO-BOTTOM-LEIDOS.
            03 FILLER                   PIC X(20) VALUE
            "Cuestas Leidas...: ".
            03 LIN-TOT-LEIDOS           PIC ZZZZZ9.
       01  TITULO-BOTTOM-RIESGO.
            03 FILLER                   PIC X(20) VALUE
            "Cuentas de riesgo: ".
            03 LIN-TOT-CTAS-RIESGO      PIC ZZZZZ9.

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
           ACCEPT WK-FECHA       FROM CENTURY-DATE
           PERFORM MOVER-FECHA   THRU F-MOVER-FECHA
           MOVE WK-FECHA-ED      TO TIT-FECHA 
           ACCEPT WK-HS          FROM TIME
           PERFORM MOVER-HS      THRU F-MOVER-HS
           MOVE WK-HS-ED         TO TIT-HORA
           PERFORM ENCABEZAR     THRU F-ENCABEZAR.
       F-INICIO. EXIT.

       MOVER-HS.
           MOVE WK-HS-HORA       TO WK-HS-HORA-ED
           MOVE WK-HS-MIN        TO WK-HS-MIN-ED.
       F-MOVER-HS. EXIT.

      * ABRE EL ARCHIVO
       ABRIR-ARCHIVO.
           OPEN INPUT M-CUENTAS
           OPEN INPUT M-PLASTICOS
           OPEN OUTPUT LISTADO.
       F-ABRIR-ARCHIVO. EXIT.

       ENCABEZAR.          
           ADD 1 TO TIT-HOJA           
      * IMPRIME ENCABEZADO
           IF TIT-HOJA = 1
              WRITE REG-LIS FROM TITULO-01 AFTER 0
           ELSE
              WRITE REG-LIS FROM TITULO-01 AFTER PAGE
           END-IF
           WRITE REG-LIS FROM TITULO-LINE
           WRITE REG-LIS FROM TITULO-03
           WRITE REG-LIS FROM TITULO-LINE
           MOVE 4 TO WK-LINEA.
       F-ENCABEZAR. EXIT.
       
       PROCESO.
           START M-CUENTAS KEY NOT < CTAS-CLAVE-1
           END-START
           PERFORM UNTIL WK-FINAL= 1
              READ M-CUENTAS NEXT AT END 
                   MOVE 1 TO WK-FINAL
                   EXIT PERFORM CYCLE
              END-READ  
              ADD 1 TO WK-LEIDOS              
              INITIALIZE WK-SIN-PLAS
                         LIN-DETALLE
                         WK-PLAS-FINAL
                         WK-SELECCIONADO-CONDICION
              IF CTAS-FECHA-BAJA = 0
                 IF CTAS-APERTURA = 2 OR 3
                    IF CTAS-SITUACION = 3 OR 5
                       ADD 1  TO WK-SELECCIONADO
                       MOVE 1 TO WK-SELECCIONADO-CONDICION
                    END-IF
                 END-IF
              END-IF
              IF WK-SELECCIONADO-CONDICION = 1
                 MOVE CTAS-DOCUMENTO TO PLAS-DOCUMENTO
                 START M-PLASTICOS KEY GREATER OR EQUAL PLAS-CLAVE-1 
                 INVALID KEY
                         MOVE 1 TO WK-SIN-PLAS
                         DISPLAY MESSAGE "Documento no Encontrado"
                         END-DISPLAY
                         EXIT PERFORM CYCLE
                 END-START
                 PERFORM UNTIL WK-PLAS-FINAL = 1
                    READ M-PLASTICOS NEXT AT END
                                             MOVE 1 TO WK-PLAS-FINAL
                                             EXIT PERFORM CYCLE
                    END-READ
                    IF CTAS-DOCUMENTO <> PLAS-DOCUMENTO
                       EXIT PERFORM 
                    END-IF                                                
      * SI EL CONTADOR ES MAYOR A 64 AGREGO 1 HOJA
                    IF WK-LINEA > 64
                       PERFORM ENCABEZAR THRU F-ENCABEZAR
                    END-IF
                    PERFORM DETALLE THRU F-DETALLE                            
                 END-PERFORM                
              END-IF
           END-PERFORM.
       F-PROCESO. EXIT.
       

       DETALLE.
           MOVE CTAS-DOCUMENTO       TO L-DOC
           MOVE PLAS-NOMBRE-CORTO    TO L-NOM-CORTO
           MOVE CTAS-SITUACION       TO WK-SITUACION
           PERFORM DETALLE-SITUACION THRU F-DETALLE-SITUACION
           MOVE WK-DETALLE-SITUACION TO L-SIT
           MOVE CTAS-APERTURA        TO L-APER
           IF WK-SIN-PLAS = 0
           THEN
                MOVE PLAS-PLASTICO        TO WK-PLAS-PLASTICO
           ELSE 
                MOVE 9999999999999999     TO WK-PLAS-PLASTICO
           END-IF
           PERFORM CODIGO-PLASTICO   THRU F-CODIGO-PLASTICO
           MOVE WK-PLAS-PLASTICO-ED  TO L-PLAS
           MOVE PLAS-ESTADO          TO L-EST
           MOVE PLAS-FECHA-HASTA     TO WK-FECHA-HASTA
           PERFORM FECHA-HASTA       THRU F-FECHA-HASTA
           MOVE WK-FECHA-HASTA-ED    TO L-FHAS
           MOVE CTAS-SALDO           TO L-SDO      
           WRITE REG-LIS             FROM LIN-DETALLE
           ADD 1 TO WK-LISTADO
           ADD 1 TO WK-LINEA.
       F-DETALLE. EXIT.    
                  
       FINAL-PROG.
           PERFORM TOTALES           THRU F-TOTALES
           PERFORM CERRAR-ARCHIVO    THRU F-CERRAR-ARCHIVO
           PERFORM VERIFICAR-TOTALES THRU F-VERIFICAR-TOTALES.
       F-FINAL-PROG. EXIT.

       TOTALES.
      * IMPRIME PIE DE PAGINA CON TOTAL DE ALUMNOS
           IF WK-LINEA > 63
              PERFORM ENCABEZAR
           END-IF
           MOVE WK-LEIDOS    TO LIN-TOT-LEIDOS
           MOVE WK-LISTADO   TO LIN-TOT-CTAS-RIESGO
           WRITE REG-LIS FROM TITULO-LINE
           WRITE REG-LIS FROM TITULO-BOTTOM-LEIDOS
           WRITE REG-LIS FROM TITULO-BOTTOM-RIESGO.
       F-TOTALES. EXIT.
      
       VERIFICAR-TOTALES. 
           DISPLAY "Cuentas leidas: "   
           AT 1016 WK-LEIDOS CONVERT
           DISPLAY "Cuentas seleccionadas: " 
           AT 1216 WK-SELECCIONADO CONVERT
           DISPLAY "Cuentas listadas: " 
           AT 1416 WK-LISTADO CONVERT
           DISPLAY MESSAGE "Enter para continuar"
           IF WK-SELECCIONADO <> WK-LISTADO
              DISPLAY MESSAGE "Cuentas no balancean"
              END-DISPLAY
           END-IF.
       F-VERIFICAR-TOTALES. EXIT.

       CERRAR-ARCHIVO.
           CLOSE M-CUENTAS
                 M-PLASTICOS
                 LISTADO.
       F-CERRAR-ARCHIVO. EXIT.

       COPY "\COBOL\fuentes\cpy\procedure-fecha-hasta.cpy".
       COPY "\COBOL\fuentes\cpy\procedure-codigo-plastico.cpy".
       COPY "\COBOL\fuentes\cpy\procedure-fecha-vuelta.cpy".
       COPY "\COBOL\fuentes\cpy\procedure-search-situacion.cpy".
      *----------------------------------------------------------------