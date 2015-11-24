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
       DATE-WRITTEN. 19/11/2015.
       DATE-COMPILED.
      *---------------------------------------------------------------- 
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. NOMBRE COMPUTADIR FUENTE.
       OBJECT-COMPUTER. NOMBRE COMPUTADOR OBJETO.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       COPY "\COBOL\fuentes\cpy\mcuentas-idx.sel".       
       COPY "\COBOL\fuentes\cpy\plasticos.sel".
      *----------------------------------------------------------------     
       DATA DIVISION.

       FILE SECTION.
       
       COPY "\COBOL\fuentes\cpy\fd-ctas-idx.fds".

       FD  PLASTICOS.
       COPY "\COBOL\fuentes\cpy\plasticos.fds".

       WORKING-STORAGE SECTION.

       COPY "\COBOL\fuentes\cpy\wk-fecha-vuelta.cpy".


       77  WK-CTAS-FINAL                PIC 9.
       77  WK-LINEA                     PIC 9(04).
       77  WK-LEIDOS                    PIC 9(04).
       77  WK-PLAS-FINAL                PIC 9.
       77  WK-SIN-PLAS                  PIC 9.
       77  WK-PLASTICOS                 PIC 9(04).
       77  WK-PLASTICOS-CONCIDERADO     PIC 9(04).
       77  WK-DOCUMENTO                 PIC 9(08).

       01  DB-STAT                      PIC X(02).

       01  WK-HS                        PIC 9(08).
       01  FILLER REDEFINES WK-HS.
           03 WK-HS-HORA                PIC 99.
           03 WK-HS-MINUTOS             PIC 99.
           03 FILLER                    PIC 9(04).

       01  WK-HS-ED.
           03 WK-HS-HORA-ED            PIC 99.
           03 FILLER                   PIC X VALUE ":".
           03 WK-HS-MINUTOS-ED         PIC 99.

       
       01  TAB-MESES                    PIC X(36) 
           VALUE "ENEFEBAMRABRMAYJUNJULAGOSETOCTNOVDIC".
       01  FILLER REDEFINES TAB-MESES.
           03 TAB-MES                   PIC X(3) OCCURS 12.
       
       01  WK-FECHA-HASTA               PIC 9(08).
       01  FILLER REDEFINES WK-FECHA-HASTA.
           03 WK-FECHA-HASTA-ANHIO      PIC 9999.
           03 WK-FECHA-HASTA-MES        PIC 99.
           03 WK-FECHA-HASTA-DIA        PIC 99.
           
       
       01  WK-FECHA-HASTA-ED.
           03 WK-FECHA-HASTA-MES-ED     PIC X(03).
           03 FILLER                    PIC X VALUE "-".
           03 WK-FECHA-HASTA-ANHIO-ED   PIC 9999.

       01  WK-PLAS-PLASTICO             PIC 9(16).
       01  FILLER REDEFINES WK-PLAS-PLASTICO.
           03 WK-PLASTICO-1             PIC X(04).
           03 WK-PLASTICO-2             PIC X(04).
           03 WK-PLASTICO-3             PIC X(04).
           03 WK-PLASTICO-4             PIC X(04).            

       01  WK-PLAS-PLASTICO-ED.
           03 WK-PLASTICO-1-ED          PIC X(04).
           03 FILLER                    PIC X VALUE "-".
           03 WK-PLASTICO-2-ED          PIC X(04).
           03 FILLER                    PIC X VALUE "-".
           03 WK-PLASTICO-3-ED          PIC X(04).
           03 FILLER                    PIC X VALUE "-".
           03 WK-PLASTICO-4-ED          PIC X(04).




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
           PERFORM VENTANA       THRU F-VENTANA.
           
       F-INICIO.

      * ABRE EL ARCHIVO
       ABRIR-ARCHIVO.
           OPEN INPUT M-CUENTAS
           OPEN INPUT PLASTICOS.
       F-ABRIR-ARCHIVO.

       VENTANA.
           DISPLAY BOX AT 0101
               SIZE 80
               LINES 25
               ERASE
           END DISPLAY
           PERFORM ENCABEZADO THRU F-ENCABEZADO.
       F-VENTANA.

       ENCABEZADO.
           ACCEPT WK-FECHA FROM CENTURY-DATE
           ACCEPT WK-HS FROM TIME 
           PERFORM MOVER-FECHA THRU F-MOVER-FECHA
           DISPLAY AT 0201 WK-FECHA-ED
           DISPLAY "CONSULTA DE CUENTAS" AT 0232
           DISPLAY AT 0273 WK-HS-ED
           DISPLAY LINE SIZE 80 AT LINE 03.
       F-ENCABEZADO.

       EDITAR-HS.
           MOVE WK-HS-HORA TO WK-HS-HORA-ED
           MOVE WK-HS-MIN  TO WK-HS-MINUTOS-ED.
       F-EDITAR-HS.
       
       PROCESO.
           INITIALIZE WK-CTAS-FINAL
           MOVE WK-DOCUMENTO TO CTAS-DOCUMENTO
           START M-CUENTAS KEY GREATER OR EQUAL CTAS-CLAVE
           INVALID KEY
                DISPLAY MESSAGE "Cuenta invalida"
                END-DISPLAY
                EXIT PARAGRAPH
           END-START
           PERFORM UNTIL WK-CTAS-FINAL = 1
                READ M-CUENTAS NEXT AT END
                     MOVE 1 TO WK-CTAS-FINAL
                     EXIT PERFORM CYCLE
                END-READ
                IF WK-DOCUMENTO <> CTAS-DOCUMENTO
                   EXIT PERFORM
                END-IF
              
                  INITIALIZE WK-SIN-PLAS
                             WK-PLAS-FINAL
                  
                  MOVE CTAS-DOCUMENTO TO PLAS-DOCUMENTO
                  START PLASTICOS KEY GREATER OR EQUAL PLAS-CLAVE-1 
                  INVALID KEY
                        MOVE 1 TO WK-SIN-PLAS
                        DISPLAY MESSAGE "Sin Plasticos"
                        END-DISPLAY
                        EXIT PERFORM CYCLE
                  END-START
                  PERFORM UNTIL WK-PLAS-FINAL = 1
                        READ PLASTICOS NEXT AT END
                            MOVE 1 TO WK-PLAS-FINAL
                            EXIT PERFORM CYCLE
                        END-READ
                  IF CTAS-DOCUMENTO <> PLAS-DOCUMENTO
                        EXIT PERFORM
                  END-IF  
                                                       
                  PERFORM DETALLE THRU F-DETALLE
                            
                  END-PERFORM                
                    
           END-PERFORM.
       F-PROCESO.
       

       DETALLE.
           MOVE CTAS-DOCUMENTO       TO L-DOC
           MOVE PLAS-NOMBRE-CORTO    TO L-NOM-CORTO
           MOVE CTAS-PROVINCIA       TO L-PROV
           MOVE CTAS-APERTURA        TO L-APER
           IF WK-SIN-PLAS = 0
           THEN
                MOVE PLAS-PLASTICO        TO WK-PLAS-PLASTICO
           ELSE 
                MOVE 9999999999999999     TO WK-PLAS-PLASTICO
           END-IF
           PERFORM CODIGO-PLASTICO   THRU F-CODIGO-PLASTICO
           MOVE PLAS-ESTADO          TO L-EST
           PERFORM FECHA-HASTA       THRU F-FECHA-HASTA      
           WRITE REG-LIS FROM LIN-DETALLE
           ADD 1 TO WK-PLASTICOS
           ADD 1 TO WK-LINEA.
       F-DETALLE.  

       FECHA-HASTA.
           MOVE PLAS-FECHA-HASTA TO WK-FECHA-HASTA
           MOVE TAB-MES (WK-FECHA-HASTA-MES) TO WK-FECHA-HASTA-MES-ED
           MOVE WK-FECHA-HASTA-ANHIO TO WK-FECHA-HASTA-ANHIO-ED
           MOVE WK-FECHA-HASTA-ED TO L-FHAS.
       F-FECHA-HASTA.   

       CODIGO-PLASTICO.
           MOVE WK-PLASTICO-1 TO WK-PLASTICO-1-ED
           MOVE WK-PLASTICO-2 TO WK-PLASTICO-2-ED
           MOVE WK-PLASTICO-3 TO WK-PLASTICO-3-ED
           MOVE WK-PLASTICO-4 TO WK-PLASTICO-4-ED
           MOVE WK-PLAS-PLASTICO-ED TO L-PLAS.
       F-CODIGO-PLASTICO.
                  
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
           MOVE WK-LEIDOS    TO LIN-TOT-ALUMN
           MOVE WK-PLASTICOS TO LIN-TOT-PLAS-EX
           WRITE REG-LIS FROM TITULO-LINE
           WRITE REG-LIS FROM TITULO-BOTTOM-LEIDOS
           WRITE REG-LIS FROM TITULO-BOTTOM-REPONER.
       F-TOTALES.
      
       VERIFICAR-TOTALES. 
           DISPLAY "Cuentas leidas: "   AT 1016 WK-LEIDOS CONVERT
           DISPLAY "Plasticos a reponer: " 
           AT 1216 WK-PLASTICOS CONVERT
           DISPLAY "consi: "   AT 1416 WK-PLASTICOS-CONCIDERADO CONVERT
           DISPLAY "extra: "   AT 1016 WK-PLASTICOS CONVERT
           DISPLAY MESSAGE "Enter para continuar"
           END-DISPLAY
           IF WK-PLASTICOS-CONCIDERADO <> WK-PLASTICOS
              DISPLAY MESSAGE "Cuentas no balancean"
              END-DISPLAY
           END-IF.
       F-VERIFICAR-TOTALES.

       CERRAR-ARCHIVO.
           CLOSE M-CUENTAS
                 PLASTICOS
                 LISTADO.
       F-CERRAR-ARCHIVO.

       COPY "\COBOL\fuentes\cpy\procedure-fecha-vuelta.cpy".
      *----------------------------------------------------------------