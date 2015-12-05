      * ========================================================================== */
      *                                                                            */
      *   COBOLNAME.CBL                                                            */
      *   (C) 2008 AUTHOR                                                          */
      *                                                                            */
      *   ABM de cuentas                                                           */
      *                                                                            */
      * ========================================================================== */
      *PROGRAM DESCRIPTION
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CTAS003.
       AUTHOR. CRESPILLO RODRIGO ANDRES.
       INSTALLATION.
       DATE-WRITTEN. 05/12/2015.
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
           SELECT MOV-CTAS
           ASSIGN TO "\COBOL\arch\mov-cuentas.prn"
           ORGANIZATION IS SEQUENTIAL.
           SELECT LISTADO
           ASSIGN TO "\COBOL\listado\LISTADO-CTAS003"
           ORGANIZATION IS SEQUENTIAL.   
           COPY "\COBOL\fuentes\cpy\mcuentas-idx.sel".      
      *----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.

       COPY "\COBOL\fuentes\cpy\fd-ctas-reg-idx.fds".

       COPY "\COBOL\fuentes\cpy\fd-mov-ctas.fds".

       FD  LISTADO.
       01  REGISTRO            PIC X(110).

       WORKING-STORAGE SECTION.

       COPY "\COBOL\fuentes\cpy\wk-tab-leyendas-2.cpy".

       77  WK-MOV-LEIDOS       PIC 9(04).
       77  WK-ALTAS-OK         PIC 9(04).
       77  WK-BAJAS-OK         PIC 9(04).
       77  WK-MODIF-OK         PIC 9(04).
       77  WK-ALTA-ERRORES     PIC 9(04).
       77  WK-MODIF-ERRORES    PIC 9(04).
       77  WK-BAJA-ERRORES     PIC 9(04).
       77  WK-HOJA             PIC 9(03).
       77  WK-FECHA            PIC 9(08).          
       77  WK-FECHA-ED-1       PIC X(10).      
       77  WK-FECHA-ED-2       PIC X(10).     
       77  WK-FECHA-ED-3       PIC X(08). 
       77  WK-LINEA            PIC 99. 
       77  WK-FINAL            PIC 9.
       77  WK-TOTALES          PIC 9(04).
       77  WK-CTAS-ENCONTRADA  PIC 9.
       77  WK-CTAS-FINAL       PIC 9.

       01  DB-STAT             PIC X(02).

       01  LIS-REG.
           03 FILLER           PIC X(01) VALUE SPACES.
           03 L-MOV            PIC X.
           03 FILLER           PIC X(03) VALUE SPACES.
           03 L-CTA            PIC 9(08).
           03 FILLER           PIC X(02) VALUE SPACES.
           03 L-APE            PIC X(20).
           03 FILLER           PIC X(02) VALUE SPACES.
           03 L-NOM            PIC X(20).
           03 FILLER           PIC X(07) VALUE SPACES.           
           03 L-PROV           PIC X.
           03 FILLER           PIC X(08) VALUE SPACES.
           03 L-FNAC           PIC X(10).
           03 FILLER           PIC X(04) VALUE SPACES.
           03 L-OBS            PIC X(23).

       01  TIT-TITULO.
           03 TIT-TITULO-FECHA PIC X(10).
           03 FILLER           PIC X(32) VALUE SPACES.
           03 FILLER           PIC X(27)
           VALUE "VALIDADOR DE MOV. DE CUETAS".
           03 FILLER           PIC X(32) VALUE SPACES.
           03 FILLER           PIC X(06) VALUE "Hoja: ".
           03 TIT-TITULO-HOJA  PIC 9(03).

       01  TIT-LINEA           PIC X(110) VALUE ALL "_".

       01  TIT-DETALLE.
           03 FILLER           PIC X(03) VALUE "Mov".
           03 FILLER           PIC X(02) VALUE SPACES.
           03 FILLER           PIC X(06) VALUE "Cuenta".
           03 FILLER           PIC X(10) VALUE SPACES.
           03 FILLER           PIC X(08) VALUE "Apellido".
           03 FILLER           PIC X(14) VALUE SPACES.
           03 FILLER           PIC X(06) VALUE "Nombre".
           03 FILLER           PIC X(13) VALUE SPACES.
           03 FILLER           PIC X(05) VALUE "Prov.".
           03 FILLER           PIC X(06) VALUE SPACES.
           03 FILLER           PIC X(10) VALUE "Fecha Nac.".
           03 FILLER           PIC X(09) VALUE SPACES.
           03 FILLER           PIC X(13) VALUE "Observaciones". 

       01  TIT-LEIDOS.
           03 TIT-LEIDOS-DET   PIC X(16) VALUE "MOV. LEIDOS...: ".
           03 TIT-LEIDOS-NUM   PIC ZZZ9.     
       01  TIT-ALTAS.
           03 TIT-ALTAS-DET    PIC X(16) VALUE "ALTAS OK......: ".
           03 TIT-ALTAS-NUM    PIC ZZZ9.
       01  TIT-ALTAS-E.
           03 TIT-ALTAS-DET-E  PIC X(16) VALUE "ALTAS C/ERROR.: ".
           03 TIT-ALTAS-NUM-E  PIC ZZZ9.
       01  TIT-BAJAS.
           03 TIT-BAJAS-DET    PIC X(16) VALUE "BAJAS OK......: ".
           03 TIT-BAJAS-NUM    PIC ZZZ9.
       01  TIT-BAJAS-E.
           03 TIT-BAJAS-DET-E  PIC X(16) VALUE "BAJAS C/ERROR.: ".
           03 TIT-BAJAS-NUM-E  PIC ZZZ9.
       01  TIT-MODIF.
           03 TIT-MODIF-DET    PIC X(16) VALUE "MODIF. OK.....: ".
           03 TIT-MODIF-NUM    PIC ZZZ9.
       01  TIT-MODIF-E.
           03 TIT-MODIF-DET-E  PIC X(16) VALUE "MODIF. C/ERROR: ".
           03 TIT-MODIF-NUM-E  PIC ZZZ9.

      *----------------------------------------------------------------
       PROCEDURE DIVISION.

       CONTROL-PROG.
           PERFORM INICIO     THRU F-INICIO
           PERFORM PROCESO    THRU F-PROCESO 
           PERFORM FINAL-PROG THRU F-FINAL-PROG
           GOBACK.
      
       INICIO.
           PERFORM ABRIR-ARCHIVO THRU F-ABRIR-ARCHIVO
           ACCEPT WK-FECHA       FROM CENTURY-DATE
           CALL "FEC-NAC-ED" USING WK-FECHA
                                   WK-FECHA-ED-1 
                                   WK-FECHA-ED-2
                                   WK-FECHA-ED-3
           MOVE WK-FECHA-ED-1    TO TIT-TITULO-FECHA
           PERFORM ENCABEZAR     THRU F-ENCABEZAR.
       F-INICIO. EXIT.

       ABRIR-ARCHIVO.
           OPEN INPUT  MOV-CTAS
           OPEN OUTPUT LISTADO
           OPEN I-O    M-CUENTAS.
       F-ABRIR-ARCHIVO. EXIT.

       ENCABEZAR.          
           ADD 1 TO TIT-TITULO-HOJA           
      * IMPRIME ENCABEZADO
           IF TIT-TITULO-HOJA = 1
              WRITE REGISTRO FROM TIT-TITULO AFTER 0
           ELSE
              WRITE REGISTRO FROM TIT-TITULO AFTER PAGE
           END-IF
           WRITE REGISTRO FROM TIT-LINEA
           WRITE REGISTRO FROM TIT-DETALLE
           WRITE REGISTRO FROM TIT-LINEA
           MOVE 4 TO WK-LINEA.
       F-ENCABEZAR. EXIT.

       PROCESO.
           INITIALIZE WK-MOV-LEIDOS
           PERFORM UNTIL WK-FINAL = 1
              READ MOV-CTAS AT END
                               MOVE 1 TO WK-FINAL
                               EXIT PERFORM CYCLE
              END-READ
              ADD 1 TO WK-MOV-LEIDOS
              INITIALIZE LIS-REG
              EVALUATE M-MOV
                 WHEN "A" PERFORM PROCESO-A THRU F-PROCESO-A
                 WHEN "B" PERFORM PROCESO-B THRU F-PROCESO-B
                 WHEN "M" PERFORM PROCESO-M THRU F-PROCESO-M
              END-EVALUATE
           END-PERFORM. 
       F-PROCESO. EXIT. 
 
       DETALLE.
           MOVE M-MOV  TO L-MOV
           MOVE M-CTA  TO L-CTA
           MOVE M-APE  TO L-APE
           MOVE M-NOM  TO L-NOM
           MOVE M-PROV TO L-PROV
           IF NOT ( M-FNAC = 0 )
              MOVE M-FNAC TO WK-FECHA
              CALL "FEC-NAC-ED" USING WK-FECHA
                                      WK-FECHA-ED-1 
                                      WK-FECHA-ED-2
                                      WK-FECHA-ED-3
              MOVE WK-FECHA-ED-2 TO L-FNAC
           END-IF.
       F-DETALLE. EXIT.

       PROCESO-A.
           PERFORM BUSCAR THRU F-BUSCAR
           IF WK-CTAS-ENCONTRADA = 1
              PERFORM DETALLE     THRU F-DETALLE
              MOVE TAB-LEYENDA(1) TO L-OBS
              WRITE REGISTRO      FROM LIS-REG
              ADD 1 TO WK-ALTA-ERRORES
           ELSE 
              PERFORM ALTA THRU F-ALTA
              ADD 1 TO WK-ALTAS-OK
           END-IF.
       F-PROCESO-A. EXIT.

       ALTA.
           MOVE M-CTA  TO CTAS-DOCUMENTO
           MOVE M-APE  TO CTAS-APELLIDO
           MOVE M-NOM  TO CTAS-NOMBRE
           MOVE M-FNAC TO CTAS-FECHA-NAC
           MOVE M-PROV TO CTAS-PROVINCIA   
           WRITE CTAS-REG INVALID KEY
                                  DISPLAY MESSAGE "I-O ERROR".
       F-ALTA. EXIT.

       PROCESO-M.
           PERFORM BUSCAR THRU F-BUSCAR
           IF NOT ( WK-CTAS-ENCONTRADA = 1 )
              PERFORM DETALLE     THRU F-DETALLE
              MOVE TAB-LEYENDA(2) TO L-OBS
              WRITE REGISTRO      FROM LIS-REG
              ADD 1 TO WK-MODIF-ERRORES
           ELSE
              PERFORM MODIFICACION THRU F-MODIFICACION
              ADD 1 TO WK-MODIF-OK
           END-IF.
       F-PROCESO-M. EXIT.

       MODIFICACION.
           IF NOT ( M-CTA = L-CTA )
              MOVE M-CTA  TO CTAS-DOCUMENTO
           END-IF
           IF NOT ( M-APE = L-APE )
              MOVE M-APE  TO CTAS-APELLIDO
           END-IF
           IF NOT ( M-NOM = L-NOM )
              MOVE M-NOM  TO CTAS-NOMBRE
           END-IF
           IF NOT ( M-FNAC = L-FNAC )
              MOVE M-FNAC TO CTAS-FECHA-NAC
           END-IF
           IF NOT ( M-PROV = L-PROV )
              MOVE M-PROV TO CTAS-PROVINCIA   
           END-IF
           REWRITE CTAS-REG INVALID KEY
                                  DISPLAY MESSAGE "I-O ERROR".
       F-MODIFICACION. EXIT.

       PROCESO-B.
           PERFORM BUSCAR THRU F-BUSCAR
           IF NOT ( WK-CTAS-ENCONTRADA = 1 )
              PERFORM DETALLE     THRU F-DETALLE
              MOVE TAB-LEYENDA(3) TO L-OBS
              WRITE REGISTRO      FROM LIS-REG
              ADD 1 TO WK-BAJA-ERRORES
           ELSE
              PERFORM BAJA THRU F-BAJA
              ADD 1 TO WK-BAJAS-OK
           END-IF.
       F-PROCESO-B. EXIT.

       BAJA.
           ACCEPT WK-FECHA FROM CENTURY-DATE
           MOVE WK-FECHA TO CTAS-FECHA-BAJA
           REWRITE CTAS-REG INVALID KEY
                                  DISPLAY MESSAGE "I-O ERROR".
       F-BAJA. EXIT.

       BUSCAR.
           INITIALIZE WK-CTAS-FINAL
                      WK-CTAS-ENCONTRADA
      * BUSCA SI EXISTE EL DOCUMENTO EN M-CUENTAS
           MOVE M-CTA TO CTAS-DOCUMENTO
           START M-CUENTAS KEY GREATER OR EQUAL CTAS-CLAVE
           INVALID KEY
                 MOVE 1 TO WK-CTAS-FINAL       
           END-START
           PERFORM UNTIL WK-CTAS-FINAL = 1
                READ M-CUENTAS NEXT AT END
                     MOVE 1 TO WK-CTAS-FINAL
                     EXIT PERFORM CYCLE
                 END-READ
                 IF M-CTA <> CTAS-DOCUMENTO
                   EXIT PERFORM
                ELSE
                   MOVE 1 TO WK-CTAS-ENCONTRADA
                 END-IF 
           END-PERFORM.
       F-BUSCAR. EXIT.

       FINAL-PROG.
           PERFORM VERIFICAR-TOTALES THRU F-VERIFICAR-TOTALES
           PERFORM TOTALES           THRU F-TOTALES
           PERFORM CERRAR-ARCHIVO    THRU F-CERRAR-ARCHIVO.
       F-FINAL-PROG. EXIT.

       TOTALES.
      * IMPRIME PIE DE PAGINA CON TOTAL DE ALUMNOS
           IF WK-LINEA > 58
              PERFORM ENCABEZAR
           END-IF
           MOVE WK-MOV-LEIDOS TO TIT-LEIDOS-NUM
           MOVE WK-ALTAS-OK   TO TIT-ALTAS-NUM
           MOVE WK-BAJAS-OK   TO TIT-BAJAS-NUM
           MOVE WK-MODIF-OK   TO TIT-MODIF-NUM
           MOVE WK-ALTA-ERRORES  TO TIT-ALTAS-NUM-E
           MOVE WK-BAJA-ERRORES  TO TIT-BAJAS-NUM-E
           MOVE WK-MODIF-ERRORES TO TIT-MODIF-NUM-E
           WRITE REGISTRO FROM TIT-LINEA
           WRITE REGISTRO FROM TIT-LEIDOS
           WRITE REGISTRO FROM TIT-ALTAS
           WRITE REGISTRO FROM TIT-ALTAS-E
           WRITE REGISTRO FROM TIT-BAJAS
           WRITE REGISTRO FROM TIT-BAJAS-E
           WRITE REGISTRO FROM TIT-MODIF
           WRITE REGISTRO FROM TIT-MODIF-E
           IF WK-LINEA > 63
              PERFORM ENCABEZAR
           END-IF.
       F-TOTALES. EXIT.
       
       VERIFICAR-TOTALES.
           COMPUTE WK-TOTALES = WK-ALTAS-OK + 
                                WK-BAJAS-OK + 
                                WK-MODIF-OK + 
                                WK-ALTA-ERRORES +
                                WK-BAJA-ERRORES +
                                WK-MODIF-ERRORES
           IF WK-TOTALES <> WK-MOV-LEIDOS
              INITIALIZE L-MOV
              INITIALIZE L-CTA
              INITIALIZE L-APE
              INITIALIZE L-NOM
              INITIALIZE L-PROV
              INITIALIZE L-FNAC
              MOVE TAB-LEYENDA(4) TO L-OBS
              WRITE REGISTRO FROM LIS-REG
           ELSE
              DISPLAY MESSAGE "Proceso finalizado con normalidad" 
           END-IF.
       F-VERIFICAR-TOTALES. EXIT.

       CERRAR-ARCHIVO.
           CLOSE MOV-CTAS
                 LISTADO
                 M-CUENTAS.
       F-CERRAR-ARCHIVO. EXIT.

       END PROGRAM.
