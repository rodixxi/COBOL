      * ========================================================================== */
      *                                                                            */
      *   COBOLNAME.CBL                                                            */
      *   (C) 2008 AUTHOR                                                          */
      *                                                                            */
      *   DESCRIPTION                                                              */
      *                                                                            */
      * ========================================================================== */
      *PROGRAM DESCRIPTION
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CTAS001.
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
           ASSIGN TO "\COBOL\listado\mv-cuentas.prn"
           ORGANIZATION IS SEQUENTIAL.       
      *----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.

       COPY "\COBOL\fuentes\cpy\fd-mov-ctas.fds".

       FD  MV-CUENTAS.
       01  MV-REG.
           03 MV-MOV            PIC X.
           03 MV-CTA            PIC 9(08).
           03 MV-APE            PIC X(20).
           03 MV-NOM            PIC X(20).
           03 MV-FNAC           PIC 9(08).
           03 MV-PROV           PIC X.

       WORKING-STORAGE SECTION.

       COPY "\COBOL\fuentes\cpy\wk-tab-leyendas.cpy".
       COPY "\COBOL\fuentes\cpy\wk-hora-ed.cpy".

       77  WK-MOV-LEIDOS       PIC 9(04).
       77  WK-ALTAS-OK         PIC 9(04).
       77  WK-BAJAS-OK         PIC 9(04).
       77  WK-MODIF-OK         PIC 9(04).
       77  WK-ERRORES          PIC 9(04).
       77  WK-HOJA             PIC 9(02).
       77  WK-FECHA            PIC 9(08).          
       77  WK-FECHA-ED-1       PIC X(10).      
       77  WK-FECHA-ED-2       PIC X(10).     
       77  WK-FECHA-ED-3       PIC X(08). 
       77  WK-LINEA            PIC 99. 
       77  WK-FINAL            PIC 9.
       77  WK-TOTALES          PIC 9(04).

       01  LIS-REG.
           03 L-MOV            PIC X.
           03 L-CTA            PIC 9(08).
           03 L-APE            PIC X(20).
           03 L-NOM            PIC X(20).
           03 L-FNAC           PIC X(10).
           03 L-PROV           PIC XX.
       
       01  TIT-TITULO.
           03 TIT-FECHA        PIC X(10).
           03 TIT-HORA         PIC X(05)

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
           ACCEPT WK-HS FROM TIME 
           MOVE WK-HS-HORA       TO TIT-HORA
           PERFORM VENTANA       THRU F-VENTANA
           PERFORM ENCABEZADO    THRU F-ENCABEZADO.
       F-INICIO. EXIT.

       ABRIR-ARCHIVO.
           OPEN INPUT MOV-CTAS
           OPEN OUTPUT MV-CUENTAS.
       F-ABRIR-ARCHIVO. EXIT.

       VENTANA.
           DISPLAY BOX AT 0101
               SIZE 80
               LINES 25 
               ERASE
           END-DISPLAY
       F-VENTANA. EXIT.

       ENCABEZADO.
      * GENERA EL TOP DE LA VENTANA CON FECHA, TITULO Y HORA
           
           DISPLAY TIT-FECHA AT 0201 
           DISPLAY "CORRECCION DE MOV. DE CUENTAS" AT 0232
           DISPLAY TIT-HORA AT 0273 
           DISPLAY LINE SIZE 80 AT LINE 03.
       F-ENCABEZADO. EXIT.

       PROCESO.
           INITIALIZE WK-MOV-LEIDOS
           PERFORM UNTIL WK-FINAL = 1
              READ MOV-CTAS AT END
                               MOVE 1 TO WK-FINAL
                               EXIT PERFORM CYCLE
              END-READ
              ADD 1 TO WK-MOV-LEIDOS
              INITIALIZE LIS-REG
              PERFORM DETALLE THRU F-DETALLE
              EVALUATE M-MOV
                 WHEN "A" PERFORM PROCESO-A THRU F-PROCESO-A
                 WHEN "B" PERFORM PROCESO-B THRU F-PROCESO-B
                 WHEN "M" PERFORM PROCESO-M THRU F-PROCESO-M
                 WHEN OTHER PERFORM PROCESO-OTROS THRU F-PROCESO-OTROS
              END-EVALUATE
           END-PERFORM. 
       F-PROCESO. EXIT. 

       PROCESO-OTROS.
           MOVE  
           PERFORM DETALLE     THRU F-DETALLE          
           MOVE TAB-LEYENDA(1) TO L-OBS
           WRITE REGISTRO      FROM LIS-REG
           ADD 1 TO WK-ERRORES.
       F-PROCESO-OTROS. EXIT. 
 
       DETALLE.
           MOVE MOV-MOV  TO L-MOV 
           MOVE MOV-CTA  TO L-CTA 
           MOVE MOV-APE  TO L-APE 
           MOVE MOV-NOM  TO L-NOM 
           MOVE MOV-FNAC TO L-FNAC
           MOVE MOV-PROV TO L-PROV
           DISPLAY "COD. MOC. : " AT 0502 L-MOV  CONVERT
           DISPLAY "DOCUMENTO : " AT 0602 L-CTA  CONVERT
           DISPLAY "APELLIDO  : " AT 0702 L-APE  CONVERT
           DISPLAY "NOMBRE    : " AT 0802 L-NOM  CONVERT
           DISPLAY "FECHA NAC.: " AT 0902 L-FNAC CONVERT
           DISPLAY "PROVINCIA : " AT 1002 L-PROV CONVERT
           DISPLAY "CONTICUAR: "  AT 1202.
       F-DETALLE. EXIT.

       PROCESO-A.
           IF M-CTA = SPACE
              OR M-FNAC = SPACE
              OR  M-APE = SPACE
              OR M-NOM = SPACE
              OR M-PROV = SPACE
              PERFORM DETALLE     THRU F-DETALLE
              MOVE TAB-LEYENDA(2) TO L-OBS
              WRITE REGISTRO      FROM LIS-REG
              ADD 1 TO WK-ERRORES
           ELSE 
              ADD 1 TO WK-ALTAS-OK
           END-IF.
       F-PROCESO-A. EXIT.

       PROCESO-M.
           IF M-CTA = SPACE
              PERFORM DETALLE     THRU F-DETALLE
              MOVE TAB-LEYENDA(3) TO L-OBS
              WRITE REGISTRO      FROM LIS-REG
              ADD 1 TO WK-ERRORES
           ELSE
              IF M-FNAC = SPACE
                 OR M-APE = SPACE
                 OR M-NOM = SPACE
                 OR M-PROV = SPACE
                 PERFORM DETALLE     THRU F-DETALLE
                 MOVE TAB-LEYENDA(4) TO L-OBS
                 WRITE REGISTRO      FROM LIS-REG
                 ADD 1 TO WK-ERRORES
              ELSE
                 ADD 1 TO WK-MODIF-OK
              END-IF
           END-IF.
       F-PROCESO-M. EXIT.

       PROCESO-B.
           IF M-CTA = SPACE
              PERFORM DETALLE     THRU F-DETALLE
              MOVE TAB-LEYENDA(5) TO L-OBS
              WRITE REGISTRO      FROM LIS-REG
              ADD 1 TO WK-ERRORES
           ELSE
              ADD 1 TO WK-BAJAS-OK
           END-IF.
       F-PROCESO-B. EXIT.

       FINAL-PROG.
           PERFORM TOTALES           THRU F-TOTALES
           PERFORM CERRAR-ARCHIVO    THRU F-CERRAR-ARCHIVO.
       F-FINAL-PROG. EXIT.

       TOTALES.
      * IMPRIME PIE DE PAGINA CON TOTAL DE ALUMNOS
           IF WK-LINEA > 60
              PERFORM ENCABEZAR
           END-IF
           MOVE WK-MOV-LEIDOS TO TIT-LEIDOS-NUM
           MOVE WK-ALTAS-OK   TO TIT-ALTAS-NUM
           MOVE WK-BAJAS-OK   TO TIT-BAJAS-NUM
           MOVE WK-MODIF-OK   TO TIT-MODIF-NUM
           MOVE WK-ERRORES    TO TIT-ERRORES-NUM
           WRITE REGISTRO FROM TIT-LINEA
           WRITE REGISTRO FROM TIT-LEIDOS
           WRITE REGISTRO FROM TIT-ALTAS
           WRITE REGISTRO FROM TIT-BAJAS
           WRITE REGISTRO FROM TIT-MODIF
           WRITE REGISTRO FROM TIT-ERRORES
           IF WK-LINEA > 63
              PERFORM ENCABEZAR
           END-IF
           PERFORM VERIFICAR-TOTALES THRU F-VERIFICAR-TOTALES.
       F-TOTALES. EXIT.
       
       VERIFICAR-TOTALES.
           COMPUTE WK-TOTALES = WK-ALTAS-OK + 
                                WK-BAJAS-OK + 
                                WK-MODIF-OK + 
                                WK-ERRORES
           IF WK-TOTALES <> WK-MOV-LEIDOS
              WRITE REGISTRO FROM "TOTALES NO BALANCEAN"
           END-IF.
       F-VERIFICAR-TOTALES. EXIT.

       CERRAR-ARCHIVO.
           CLOSE MOV-CTAS
                 LISTADO.
       F-CERRAR-ARCHIVO. EXIT.

       END PROGRAM.
