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
           ASSIGN TO "E:\COBOL\arch\mcuentas2"
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
            
       WORKING-STORAGE SECTION.
       77  WK-FINAL                     PIC 9(01).
       77  WK-APERTURA-NORMAL           PIC 9(04).
       77  WK-APERTURA-AMPLIAR          PIC 9(04).
       77  WK-APERTURA-INTERNACIONAL    PIC 9(04).
       77  WK-APERTURA-ESTUDIO          PIC 9(04).
       77  WK-APERTURA-FDOCUM           PIC 9(04).
       77  WK-APERTURA-DENEGADA         PIC 9(04).

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
       COPY "E:\COBOL\fuentes\cpy\wk-fecha-vuelta.cpy".
       
       01  WK-APERTURA                  PIC 9.
              
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
      * IMPRIME EN EL EXIL
           INITIALIZE REG-XLS
           STRING TITULO-01 INTO REG-XLS
           END-STRING 
           WRITE REG-XLS.
       F-ENCABEZAR.
       
       PROCESO.
           PERFORM UNTIL WK-FINAL= 1
              READ ARCHIVO AT END 
                   MOVE 1 TO WK-FINAL
                   EXIT PERFORM CYCLE
              END-READ
              ADD 1 TO WK-LEIDOS
              IF CTAS-FECHA-BAJA = 0 AND (CTAS-APERTURA = 6 
              OR CTAS-APERTURA = 7 OR CTAS-APERTURA = 9)
                    ADD 1 TO WK-LINEA-CONSIDERADA     
                    INITIALIZE LIN-DETALLE
      * SI EL CONTADOR ES MAYOR A 64 AGREGO 1 HOJA
                    PERFORM DETALLE THRU F-DETALLE
               END-IF
           END-PERFORM.
       F-PROCESO.
       

       DETALLE.
           MOVE CTAS-DOCUMENTO    TO X-DOC
           MOVE CTAS-APELLIDO     TO X-APE
           MOVE CTAS-NOMBRE       TO X-NOM
           MOVE CTAS-FECHA-NAC    TO X-FNAC
           MOVE CTAS-PROVINCIA    TO X-PRV
           MOVE CTAS-APERTURA     TO WK-APERTURA
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
       
       COPY "E:\COBOL\fuentes\cpy\prodecure-fecha-vuelta.cpy".
      *----------------------------------------------------------------