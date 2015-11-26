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
       COPY "\COBOL\fuentes\cpy\wk-tabla-aperturas.cpy".


       77  WK-CTAS-FINAL                PIC 9.
       77  WK-DOCUMENTO-CORRECTO        PIC 9.
       77  WK-LINEA                     PIC 9(04).
       77  WK-LEIDOS                    PIC 9(04).
       77  WK-PLAS-FINAL                PIC 9.
       77  WK-SIN-PLAS                  PIC 9.
       77  WK-PLASTICOS                 PIC 9(04).
       77  WK-PLASTICOS-CONCIDERADO     PIC 9(04).
       77  WK-DOCUMENTO                 PIC 9(08).
       77  WK-DETALLE-PROVINCIA         PIC X(31).
       77  WK-DETALLE-PROVINCIA-ED      PIC X(35).
       77  WK-DETALLE-APERTURA-ED       PIC X(17).

       01  WK-CTAS-SALDO-ED             PIC 99.999.999,99.     


       01  DB-STAT                      PIC X(02).

       01  WK-FEC-ED-2
           03 WK-FEC-DIA-ED-2           PIC 99.
           03 FILLER                    PIC X VALUE "-".
           03 WK-FEC-MES-ED-2           PIC 99.
           03 FILLER                    PIC X VALUE "-".
           03 WK-FEC-ANHIO-ED-2         PIC 9999.


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

       01  TAB-PROVINCIAS
           03 FILLER                    PIC X(32)
              VALUE "CCiudad Autónoma de Buenos Aires".
           03 FILLER                    PIC X(32)
              VALUE "BBuenos Aires                   ".
           03 FILLER                    PIC X(32)
              VALUE "KCatamarca                      ".
           03 FILLER                    PIC X(32)
              VALUE "XCórdoba                        ".
           03 FILLER                    PIC X(32)
              VALUE "WCorrientes                     ".
           03 FILLER                    PIC X(32)
              VALUE "EEntre Ríos                     ".
           03 FILLER                    PIC X(32)
              VALUE "YJujuy                          ".
           03 FILLER                    PIC X(32)
              VALUE "MMendoza                        ".
           03 FILLER                    PIC X(32)
              VALUE "FLa Rioja                       ".
           03 FILLER                    PIC X(32)
              VALUE "ASalta                          ".
           03 FILLER                    PIC X(32)
              VALUE "JSan Juan                       ".
           03 FILLER                    PIC X(32)
              VALUE "DSan Luis                       ".
           03 FILLER                    PIC X(32)
              VALUE "SSanta Fe                       ".
           03 FILLER                    PIC X(32)
              VALUE "GSantiago del Estero            ".
           03 FILLER                    PIC X(32)
              VALUE "TTucumán                        ".
           03 FILLER                    PIC X(32)
              VALUE "HChaco                          ".
           03 FILLER                    PIC X(32)
              VALUE "UChubut                         ".
           03 FILLER                    PIC X(32)
              VALUE "PFormosa                        ".
           03 FILLER                    PIC X(32)
              VALUE "NMisiones                       ".
           03 FILLER                    PIC X(32)
              VALUE "QNeuquén                        ".
           03 FILLER                    PIC X(32)
              VALUE "LLa Pampa                       ".
           03 FILLER                    PIC X(32)
              VALUE "RRío Negro                      ".
           03 FILLER                    PIC X(32)
              VALUE "ZSanta Cruz                     ".
           03 FILLER                    PIC X(32)
              VALUE "VTierra del Fuego               ".
       01  TAB-PROVINCIAS-IDX REDEFINES TAB-PROVINCIAS-IDX
           03 TAB-PROVINCIAS-DETALLE OCCURS 24 INDEXED BY PROV-INDEX
              05 TAB-PROVINCIAS-COD     PIC X.
              05 TAB-PROVINCIAS-DETALLE PIC X(31)
       
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

       01  WK-NOMBRE-COMPLETO           PIC X(40) VALUES SPACES.


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
           PERFORM PEDIR-DOCUMENTO THRU F-PEDIR-DOCUMENTO
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
       PEDIR-DOCUMENTO
           INITIALIZE WK-DOCUMENTO-CORRECTO
           PERFORM UNTIL WK-DOCUMENTO-CORRECTO = 1
               DISPLAY "DOCUMENTO: " AT 0402
               ACCEPT WK-DOCUMENTO AT 0413
               IF WK-DOCUMENTO = 0
                  DISPLAY MESSAGE "Vuelva a Ingresar"
                  END-DISPLAY
                  EXIT PERFORM
               END-IF 
               IF WK-DOCUMENTO = 9 OR 99999999
                  DISPLAY MESSAGE "Se finaliza la consulta"
                  END-DISPLAY
                  PERFORM CERRAR-ARCHIVO
                  EXIT PROGRAM
               END-IF
               IF WK-DOCUMENTO > 0
                  WK-DOCUMENTO-CORRECTO = 1
                  EXIT PERFORM CYCLE
               END-IF
           END-PERFORM.
       F-PEDIR-DOCUMENTO.

       DETALLE.
           PERFORM GENERAR-NOMBRE THRU F-GENERAR-NOMBRE
           DISPLAY "Titular:" AT 0602 WK-NOMBRE-COMPLETO
           PERFORM GENERAR-FECHA-NAC THRU F-GENERAR-FECHA-NAC
           DISPLAY "Fecha Nac.:" AT 0702 WK-FEC-ED-2
           PERFORM GENERER-PROVINCIA THRU F-GENERER-PROVINCIA
           DISPLAY "Provincia: " AT 0802 WK-DETALLE-PROVINCIA-ED
           PERFORM GENERAR-SALDO THRU F-GENERAR-SALDO
           DISPLAY "Saldo : " AT 0902 WK-CTAS-SALDO-ED CONVERT
           PERFORM GENERAR-FECHA-BAJA THRU F-GENERAR-FECHA-BAJA
           DISPLAY "Fecha Baja :" AT 1002 WK-FEC-ED-2


       F-DETALLE.
      * ------------>>> Aca quedamos papa <<<------------

       GENERAR-FECHA-BAJA.
           MOVE CTAS-FECHA-BAJA TO WK-FECHA
           PERFORM MOVER-FECHA-2 THRU F-MOVER-FECHA-2.
       F-GENERAR-FECHA-BAJA. EXIT.

       GENERAR-FECHA-NAC.
           MOVE CTAS-FECHA-NAC TO WK-FECHA
           PERFORM MOVER-FECHA-2 THRU F-MOVER-FECHA-2.
       F-GENERAR-FECHA-NAC.

       GENERAR-SALDO.
           MOVE CTAS-SALDO TO WK-CTAS-SALDO-ED.
       F-GENERAR-SALDO.

       GENERER-APERTURA.
           PERFORM DETALLE-APERTURA THRU F-DETALLE-APERTURA
           STRING CTAS-APERTURA        DELIMITED BY SPACE
                  " - "                DELIMITED BY SIZE
                  WK-DETALLE-APERTURA  DELIMITED BY SPACE
              INTO WK-DETALLE-PROVINCIA-ED
           END-STRING.
       F-GENERER-APERTURA. 

       GENERER-PROVINCIA.
           PERFORM DETALLE-PROVINCIA THRU F-DETALLE-PROVINCIA
           STRING CTAS-PROVINCIA       DELIMITED BY SPACE
                  " - "                DELIMITED BY SIZE
                  WK-DETALLE-PROVINCIA DELIMITED BY SPACE
              INTO WK-DETALLE-PROVINCIA-ED
           END-STRING.
       F-GENERER-PROVINCIA. 

       DETALLE-PROVINCIA.
           SET PROV-INDEX TO 1
           SEARCH TAB-PROVINCIAS-DETALLE
            WHEN TAB-PROVINCIAS-IDX(PROV-INDEX) = CTAS-PROVINCIA
             MOVE TAB-PROVINCIAS-DETALLE(PROV-INDEX)
             TO WK-DETALLE-PROVINCIA
           END-SEARCH.
       F-DETALLE-PROVINCIA. 
       
       GENERAR-NOMBRE.
           STRING CTAS-APELLIDO DELIMITED BY SPACE
                  " "           DELIMITED BY SIZE
                  CTAS-NOMBRE   DELIMITED BY SPACE
              INTO WK-NOMBRE-COMPLETO
           END-STRING
       F-GENERAR-NOMBRE.

       MOVER-FECHA-2.
           MOVE WK-FEC-ANHIO TO WK-FEC-ANHIO-ED-2
           MOVE WK-FEC-MES   TO WK-FEC-MES-ED-2
           MOVE WK-FEC-DIA   TO WK-FEC-DIA-ED-2.
       F-MOVER-FECHA-2.

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

       CERRAR-ARCHIVO.
           CLOSE M-CUENTAS
                 PLASTICOS.
       F-CERRAR-ARCHIVO.

       COPY "\COBOL\fuentes\cpy\procedure-fecha-vuelta.cpy".
       COPY "\COBOL\fuentes\cpy\procedure-search-detalle.cpy".
      *----------------------------------------------------------------