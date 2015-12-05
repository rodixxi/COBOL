      * =========================================================== */
      *                                                             */
      *   RIE001.CBL                                                */
      *   CRESPILLO RODRIGO ANDRES                                 */
      *                                                           */
      *   listado de las cuentas en riesgo                        .*/
      * ========================================================== */
      *PROGRAM DESCRIPTION
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONCTAS. 
       AUTHOR. CRESPILLO RODRIGO ANDRES.
       INSTALLATION.
       DATE-WRITTEN. 24/11/2015.
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
       
       COPY "\COBOL\fuentes\cpy\fd-ctas-reg-idx.fds".

       FD  M-PLASTICOS.
       COPY "\COBOL\fuentes\cpy\plasticos.fds".

       WORKING-STORAGE SECTION.
       
       COPY "\COBOL\fuentes\cpy\wk-tabla-aperturas.cpy".
       COPY "\COBOL\fuentes\cpy\wk-codigo-plastico.cpy".
       COPY "\COBOL\fuentes\cpy\wk-tabla-provincias.cpy".
       COPY "\COBOL\fuentes\cpy\wk-tabla-situacion.cpy".
       COPY "\COBOL\fuentes\cpy\wk-hora-ed.cpy".

       77  WK-CTAS-FINAL                PIC 9.
       77  WK-DOCUMENTO-CORRECTO        PIC 9.
       77  WK-PLAS-FINAL                PIC 9.
       77  WK-SIN-PLAS                  PIC 9.
       77  WK-DETALLE-PLASTICO          PIC 9.
       77  WK-CONTINUAR-RESP            PIC XX.
       77  WK-CONTINUAR                 PIC 9.
       77  WK-CUENTA-VALIDA             PIC 9.       
       77  WK-CONTINUAR-RESP-CORRECT    PIC 9.
       77  WK-FECHA                     PIC 9(08).
       77  WK-FECHA-ED-1                PIC X(10).
       77  WK-FECHA-ED-2                PIC X(10).
       77  WK-FECHA-ED-3                PIC X(8).


       01  WK-DOCUMENTO                 PIC 9(08).

       01  WK-PLAS-ESTADO              PIC XX.

       01  WK-DETALLE-PLASTICO         PIC X(70).

       01  WK-FECHA-HASTA-ED           PIC X(08).

       01  WK-FECHA-NAC-ED             PIC X(10).

       01  WK-FECHA-BAJA-ED            PIC X(10).
       01  WK-FECHA-BAJA-ED            PIC X(10).

       01  WK-CODIGO                   PIC X(70).
       01  WK-DETALLE                  PIC X(70).
       01  WK-CODIGO-DETALLE           PIC X(70). 

       01  WK-DETALLE-SITUACION-ED     PIC X(11).

       01  WK-DETALLE-APERTURA-ED       PIC X(17).

       01  WK-DETALLE-PROVINCIA-ED      PIC X(35).

       01  WK-CTAS-SALDO-ED             PIC 99.999.999,99.

       01  DB-STAT                      PIC X(02).

       01  WK-FECHA-TIT                 PIC X(10).

       01  WK-NOMBRE-COMPLETO           PIC X(40) VALUES SPACES.


       LINKAGE SECTION.
       SCREEN SECTION.
      *----------------------------------------------------------------
       PROCEDURE DIVISION.

       CONTROL-PROG.
           PERFORM INICIO     THRU F-INICIO
           INITIALIZE WK-CONTINUAR
           PERFORM UNTIL WK-CONTINUAR = 1
              PERFORM PROCESO    THRU F-PROCESO
              PERFORM CONTINUAR  THRU F-CONTINUAR
           END-PERFORM 
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
           OPEN INPUT M-PLASTICOS.
       F-ABRIR-ARCHIVO.

       VENTANA.
      * CREA EL BOX DE LA VENTANA
           DISPLAY BOX AT 0101
               SIZE 80
               LINES 25
           END-DISPLAY
           PERFORM ENCABEZADO THRU F-ENCABEZADO.
       F-VENTANA.

       ENCABEZADO.
      * GENERA EL TOP DE LA VENTANA CON FECHA, TITULO Y HORA
           ACCEPT WK-FECHA FROM CENTURY-DATE
           CALL "FEC-NAC-ED" USING WK-FECHA
                                   WK-FECHA-ED-1 
                                   WK-FECHA-ED-2
                                   WK-FECHA-ED-3
           ACCEPT WK-HS FROM TIME 
           MOVE WK-FECHA-ED-1 TO WK-FECHA-TIT
           DISPLAY "" AT 0201 WK-FECHA-TIT
           DISPLAY "CONSULTA DE CUENTAS" AT 0232
           DISPLAY "" AT 0273 WK-HS-ED
           DISPLAY LINE SIZE 80 AT LINE 03.
       F-ENCABEZADO.

       EDITAR-HS.
      * EDITA HS
           MOVE WK-HS-HORA TO WK-HS-HORA-ED
           MOVE WK-HS-MIN  TO WK-HS-MINUTOS-ED.
       F-EDITAR-HS.
       
       PROCESO.
           INITIALIZE WK-CUENTA-VALIDA
           PERFORM UNTIL WK-CUENTA-VALIDA = 1
              PERFORM PEDIR-DOCUMENTO THRU F-PEDIR-DOCUMENTO
              INITIALIZE WK-CTAS-FINAL
      * BUSCA SI EXISTE EL DOCUMENTO EN M-CUENTASk
              MOVE WK-DOCUMENTO TO CTAS-DOCUMENTO
              START M-CUENTAS KEY GREATER OR EQUAL CTAS-CLAVE
              INVALID KEY
                   DISPLAY MESSAGE "Cuenta invalida"
                   END-DISPLAY
                   EXIT PERFORM CYCLE         
              END-START
              PERFORM UNTIL WK-CTAS-FINAL = 1
                   READ M-CUENTAS NEXT AT END
                        MOVE 1 TO WK-CTAS-FINAL
                        EXIT PERFORM CYCLE
                   END-READ
                   IF WK-DOCUMENTO <> CTAS-DOCUMENTO
                      EXIT PERFORM
                   ELSE
                      PERFORM DETALLE          THRU F-DETALLE
                      PERFORM BUSCAR-PLASTICOS THRU F-BUSCAR-PLASTICOS
                      MOVE 1 TO WK-CUENTA-VALIDA
                   END-IF 
              END-PERFORM
           END-PERFORM.
       F-PROCESO.
       
       BUSCAR-PLASTICOS.
           INITIALIZE WK-SIN-PLAS
                      WK-PLAS-FINAL
           MOVE CTAS-DOCUMENTO TO PLAS-DOCUMENTO
           START M-PALSTICOS KEY GREATER OR EQUAL PLAS-CLAVE-1 
           INVALID KEY
                   MOVE 1 TO WK-SIN-PLAS
                   DISPLAY MESSAGE "Sin Plasticos"
                   END-DISPLAY
                   EXIT PERFORM CYCLE
           END-START
           PERFORM UNTIL WK-PLAS-FINAL = 1
                   READ M-PALSTICOS NEXT AT END
                        MOVE 1 TO WK-PLAS-FINAL
                        DISPLAY "Sin Plasticos" AT 2402
                        EXIT PERFORM CYCLE
                   END-READ
                   IF CTAS-DOCUMENTO <> PLAS-DOCUMENTO
                      EXIT PERFORM
                   ELSE
                      PERFORM DETALLE-PLASTICO THRU F-DETALLE-PLASTICO
                   END-IF  
           END-PERFORM.
       F-BUSCAR-PLASTICOS.

       DETALLE-PLASTICO.
           MOVE PLAS-PLASTICO        TO WK-PLAS-PLASTICO
           PERFORM CODIGO-PLASTICO   THRU F-CODIGO-PLASTICO
           MOVE PLAS-FECHA-HASTA     TO WK-FECHA
           MOVE PLAS-ESTADO          TO WK-PLAS-ESTADO
           CALL "FEC-NAC-ED" USING WK-FECHA, WK-FECHA-ED-1, 
                                   WK-FECHA-ED-2, WK-FECHA-ED-3.
           MOVE WK-FECHA-ED-3        TO WK-FECHA-HASTA-ED
           STRING "Plastico  : "
                  WK-CODIGO-PLASTICO-ED
                  " - "
                  "Estado: "
                  WK-PLAS-ESTADO
                  " - "
                  "Vto.: "
                  WK-FECHA-HASTA-ED
              INTO WK-DETALLE-PLASTICO
           END-STRING
           DISPLAY AT 1502 WK-DETALLE-PLASTICO.
       F-DETALLE-PALSTICO. EXIT.  

       PEDIR-DOCUMENTO.
      * PIDE EL DOCUMENTO Y COPRUEBA SI ES CORRECTO
           INITIALIZE WK-DOCUMENTO-CORRECTO
           PERFORM UNTIL WK-DOCUMENTO-CORRECTO = 1
               DISPLAY "DOCUMENTO: " AT 0402
               ACCEPT WK-DOCUMENTO AT 0413
               IF WK-DOCUMENTO = 0
                  DISPLAY "Vuelva a ingresar el dato" AT 2402
                  EXIT PERFORM
               END-IF 
               IF WK-DOCUMENTO = 9 OR 99999999
                  DISPLAY "Se finaliza la consulta" AT 2402
                  END-DISPLAY
                  PERFORM CERRAR-ARCHIVO
                  GOBACK
               END-IF
               IF WK-DOCUMENTO > 0
                  MOVE 1 TO WK-DOCUMENTO-CORRECTO
                  EXIT PERFORM CYCLE
               END-IF
           END-PERFORM.
       F-PEDIR-DOCUMENTO. EXIT.

       DETALLE.
           PERFORM GENERAR-NOMBRE     THRU F-GENERAR-NOMBRE
           DISPLAY "Titular   : "     AT 0602 WK-NOMBRE-COMPLETO
           PERFORM GENERAR-FECHA-NAC  THRU F-GENERAR-FECHA-NAC
           DISPLAY "Fecha Nac.: "     AT 0702 WK-FECHA-NAC-ED
           PERFORM GENERER-PROVINCIA  THRU F-GENERER-PROVINCIA
           DISPLAY "Provincia : "     AT 0802 WK-DETALLE-PROVINCIA-ED
           PERFORM GENERER-APERTURA   THRU F-GENERER-APERTURA
           DISPLAY "Apertura  : "     AT 0902 WK-DETALLE-APERTURA-ED
           PERFORM GENERER-SITUACION  THRU F-GENERER-SITUACION
           DISPLAY "Situacion : "     AT 1002 WK-DETALLE-SITUACION-ED
           PERFORM GENERAR-SALDO      THRU F-GENERAR-SALDO
           DISPLAY "Saldo     : "     AT 1102 WK-CTAS-SALDO-ED CONVERT
           PERFORM GENERAR-FECHA-BAJA THRU F-GENERAR-FECHA-BAJA
           DISPLAY "Fecha Baja: "     AT 1202 WK-FEC-ED-2.
       F-DETALLE. EXIT.

       GENERAR-FECHA-BAJA.
           MOVE CTAS-FECHA-BAJA TO WK-FECHA
           CALL "FEC-NAC-ED" USING WK-FECHA
                                   WK-FECHA-ED-1 
                                   WK-FECHA-ED-2
                                   WK-FECHA-ED-3
           MOVE WK-FECHA-ED-2   TO WK-FECHA-BAJA-ED.
       F-GENERAR-FECHA-BAJA. EXIT.

       GENERAR-FECHA-NAC.
           MOVE CTAS-FECHA-NAC TO WK-FECHA
           CALL "FEC-NAC-ED" USING WK-FECHA
                                   WK-FECHA-ED-1 
                                   WK-FECHA-ED-2
                                   WK-FECHA-ED-3
           MOVE WK-FECHA-ED-2  TO WK-FECHA-NAC-ED.
       F-GENERAR-FECHA-NAC. EXIT.

       GENERAR-SALDO.
           MOVE CTAS-SALDO TO WK-CTAS-SALDO-ED.
       F-GENERAR-SALDO. EXIT.

       GENERER-SITUACION.
           PERFORM DETALLE-SITUACION THRU F-DETALLE-SITUACION
           MOVE CTAS-SITUACION       TO WK-CODIGO
           MOVE WK-DETALLE-SITUACION TO WK-DETALLE
           PERFORM COMBINAR-CODIGO-DETALLE
           THRU F-COMBINAR-CODIGO-DETALLE
           MOVE WK-CODIGO-DETALLE    TO WK-DETALLE-SITUACION-ED.
       F-GENERER-SITUACION. EXIT.

       GENERER-APERTURA.
           PERFORM DETALLE-APERTURA THRU F-DETALLE-APERTURA
           MOVE CTAS-APERTURA       TO WK-CODIGO
           MOVE WK-DETALLE-APERTURA TO WK-DETALLE
           PERFORM COMBINAR-CODIGO-DETALLE 
           THRU F-COMBINAR-CODIGO-DETALLE
           MOVE WK-CODIGO-DETALLE   TO WK-DETALLE-APERTURA-ED.
       F-GENERER-APERTURA. EXIT.

       GENERER-PROVINCIA.
           PERFORM DETALLE-PROVINCIA THRU F-DETALLE-PROVINCIA
           MOVE CTAS-PROVINCIA       TO WK-CODIGO
           MOVE WK-DETALLE-PROVINCIA TO WK-DETALLE
           PERFORM COMBINAR-CODIGO-DETALLE 
           THRU F-COMBINAR-CODIGO-DETALLE 
           MOVE WK-CODIGO-DETALLE    TO WK-DETALLE-PROVINCIA-ED.
       F-GENERER-PROVINCIA. EXIT.

       COMBINAR-CODIGO-DETALLE.
           INITIALIZE WK-CODIGO-DETALLE
           STRING WK-CODIGO    DELIMITED BY SPACE
                  " - "        DELIMITED BY SIZE
                  WK-DETALLE   DELIMITED BY SPACE
              INTO WK-CODIGO-DETALLE
           END-STRING.
       F-COMBINAR-CODIGO-DETALLE. EXIT.

       DETALLE-PROVINCIA.
           SET PROV-INDEX TO 1
           SEARCH TAB-PROVINCIAS-IDX
            WHEN TAB-PROVINCIAS-COD(PROV-INDEX) = CTAS-PROVINCIA
             MOVE TAB-PROVINCIAS-DETALLE(PROV-INDEX)
             TO WK-DETALLE-PROVINCIA
           END-SEARCH.
       F-DETALLE-PROVINCIA. EXIT.
       
       GENERAR-NOMBRE.
           STRING CTAS-APELLIDO DELIMITED BY SPACE
                  " "           DELIMITED BY SIZE
                  CTAS-NOMBRE   DELIMITED BY SPACE
              INTO WK-NOMBRE-COMPLETO
           END-STRING.
       F-GENERAR-NOMBRE. EXIT.

       CONTINUAR.
           DISPLAY "Contunua: " AT 2202
           INITIALIZE WK-CONTINUAR-RESP-CORRECT
                      WK-CONTINUAR-RESP
           PERFORM UNTIL WK-CONTINUAR-RESP-CORRECT = 1
              ACCEPT WK-CONTINUAR-RESP AT 2213
              DISPLAY "( SI / NO )" AT 2216
              IF WK-CONTINUAR-RESP = "SI" OR "NO"
                 IF WK-CONTINUAR-RESP = "SI"
                    EXIT PERFORM 
                 ELSE
                    MOVE 1 TO WK-CONTINUAR
                 END-IF
              ELSE
                 EXIT PERFORM CYCLE
              END-IF
           END-PERFORM.
       F-CONTINUAR. EXIT.

       FINAL-PROG.
           PERFORM CERRAR-ARCHIVO    THRU F-CERRAR-ARCHIVO.
       F-FINAL-PROG. EXIT.

       CERRAR-ARCHIVO.
           CLOSE M-CUENTAS
                 M-PALSTICOS.
       F-CERRAR-ARCHIVO. EXIT.

       COPY "\COBOL\fuentes\cpy\procedure-search-detalle.cpy".
       COPY "\COBOL\fuentes\cpy\procedure-codigo-plastico.cpy".
       COPY "\COBOL\fuentes\cpy\procedure-search-situacion.cpy".
      *----------------------------------------------------------------