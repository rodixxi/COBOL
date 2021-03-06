      * =================================================================== */
      *                                                                     */
      *   LIS002.CBL                                                        */
      *   (C)  CRESPILLO RODRIGO ANDRES                                     */
      *                                                                     */
      *   Crea una hoja de calculo usando M-CUENTAS para aquellos que       */
      *   no estan de baja y que CTAS-APERTURA sean 6, 7 o 9                */
      *   Al finalizar muestra la cantidad de archivos leidos, considerados */
      *   y grabados, estos dos ultimos deben considir                      */
      * =================================================================== */
      *PROGRAM DESCRIPTION
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIS002. 
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
           ASSIGN TO "\COBOL\arch\mcuentas"
           ORGANIZATION IS SEQUENTIAL.
           SELECT LISTADO
           ASSIGN TO "\COBOL\listado\LIS-XLS.xls"
           ORGANIZATION IS SEQUENTIAL.
      *----------------------------------------------------------------     
       DATA DIVISION.

       FILE SECTION.
       
       COPY "\COBOL\fuentes\cpy\fd-ctas-reg.fds".

       FD  LISTADO.
       01  REG-XLS                      PIC X(100).

       WORKING-STORAGE SECTION.

       COPY "\COBOL\fuentes\cpy\wk-tabla-aperturas.cpy".
       COPY "\COBOL\fuentes\cpy\wk-fecha-vuelta.cpy".

       77  WK-LEIDOS                    PIC 9(04).
       77  WK-FINAL                     PIC 9(01).
       77  WK-LINEA-GRABADA             PIC 9(04).
       77  WK-LINEA-CONSIDERADA         PIC 9(04).

       01  TITULO-01.
            03 FILLER                   PIC X(18) VALUE 
            "LISTADO DE CUENTAS".
            03 FILLER                   PIC X(30) VALUE SPACES.
            03 TIT-FECHA                PIC X(10).
            
       01  LIN-DETALLE.
            03 X-DOC                    PIC 9(08).
            03 X-APE                    PIC X(20).
            03 X-NOM                    PIC X(20).
            03 X-FNAC                   PIC 9(8).
            03 X-PRV                    PIC X.
            03 X-APER                   PIC X(15).
            03 X-SIT                    PIC X.

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
           PERFORM ENCABEZAR     THRU F-ENCABEZAR.
       F-INICIO. EXIT.

      * ABRE EL ARCHIVO
       ABRIR-ARCHIVO.
           OPEN INPUT M-CUENTAS
           OPEN OUTPUT LISTADO.
       F-ABRIR-ARCHIVO. EXIT.

       ENCABEZAR.           
      * IMPRIME EN EL EXIL
           INITIALIZE REG-XLS
           STRING TITULO-01 INTO REG-XLS
           END-STRING 
           WRITE REG-XLS
           INITIALIZE REG-XLS
           STRING
                 "Documento" H"09"
                 "Apellido" H"09"
                 "Nombre" H"09"
                 "Fecha Nacimiento" H"09"
                 "Provincia" H"09"
                 "Apertura" H"09"
                 "Situacion" H"09"
                 INTO REG-XLS
           END-STRING
           WRITE REG-XLS.
       F-ENCABEZAR. EXIT.
       
       PROCESO.
           PERFORM UNTIL WK-FINAL= 1
              READ M-CUENTAS AT END 
                   MOVE 1 TO WK-FINAL
                   EXIT PERFORM CYCLE
              END-READ
              ADD 1 TO WK-LEIDOS
              IF CTAS-FECHA-BAJA = 0 AND 
              (CTAS-APERTURA = 6 OR 
              CTAS-APERTURA = 7  OR 
              CTAS-APERTURA = 9)
                  ADD 1 TO WK-LINEA-CONSIDERADA     
                  INITIALIZE LIN-DETALLE
      * SI EL CONTADOR ES MAYOR A 64 AGREGO 1 HOJA
                  PERFORM DETALLE THRU F-DETALLE
              END-IF
           END-PERFORM.
       F-PROCESO. EXIT.
       

       DETALLE.
           MOVE CTAS-DOCUMENTO      TO X-DOC
           MOVE CTAS-APELLIDO       TO X-APE
           MOVE CTAS-NOMBRE         TO X-NOM
           MOVE CTAS-FECHA-NAC      TO X-FNAC
           MOVE CTAS-PROVINCIA      TO X-PRV
           MOVE CTAS-APERTURA       TO WK-APERTURA
           PERFORM DETALLE-APERTURA THRU F-DETALLE-APERTURA
           MOVE WK-DETALLE-APERTURA TO X-APER
           MOVE CTAS-SITUACION      TO X-SIT           
           INITIALIZE REG-XLS
           STRING
                 X-DOC H"09"
                 X-APE H"09"
                 X-NOM H"09"
                 X-FNAC H"09"
                 X-PRV H"09"
                 X-APER H"09"
                 X-SIT H"09"
                 INTO REG-XLS
           END-STRING
           WRITE REG-XLS
           ADD 1 TO WK-LINEA-GRABADA.
       F-DETALLE. EXIT.

       FINAL-PROG.
           PERFORM CERRAR-ARCHIVO    THRU F-CERRAR-ARCHIVO
           PERFORM VERIFICAR-TOTALES THRU F-VERIFICAR-TOTALES.
       F-FINAL-PROG. EXIT.
      
       VERIFICAR-TOTALES. 
           DISPLAY "Leidos: "      AT 1016 WK-LEIDOS CONVERT
           DISPLAY "Considerados: "AT 1216 WK-LINEA-CONSIDERADA CONVERT
           DISPLAY "Grabadas :"    AT 1416 WK-LINEA-GRABADA CONVERT
           DISPLAY MESSAGE "Enter para continuar"
           IF WK-LINEA-GRABADA <> WK-LINEA-CONSIDERADA
              DISPLAY MESSAGE "Cuentas no balancean"
           END-IF.
       F-VERIFICAR-TOTALES. EXIT.

       CERRAR-ARCHIVO.
           CLOSE M-CUENTAS
                 LISTADO.
       F-CERRAR-ARCHIVO. EXIT.

       COPY "\COBOL\fuentes\cpy\procedure-fecha-vuelta.cpy".
       COPY "\COBOL\fuentes\cpy\procedure-search-detalle.cpy".
      *----------------------------------------------------------------