*      IDENTIFICATION DIVISION.
       PROGRAM-ID. LISTADO-ALUMNOS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO ASSIGN TO "D:\COBOL\arch\M-ALUMNOS.txt".
           
           SELECT LISTADO ASSIGN TO "D:\COBOL\listado\LISTADO.txt".
           
       DATA DIVISION.
       FILE SECTION.
       
       FD ARCHIVO. 
       
       01 REG-ARCHIVO.
          02 ARCH-DNI           PIC 9(08).
          02 ARCH-NOMBRE        PIC X(10).
          02 ARCH-FEC-NAC       PIC X(10).
          
       FD LISTADO. 
       
       01 REG-LIS               PIC X(80).
          
       WORKING-STORAGE SECTION.
       
       77 WK-LEIDOS             PIC 9(09).
       77 WK-FINAL              PIC 9(01).
       77 WK-LIN                PIC 9(09).
       
       
       
       
       01 TITULO-1.
          02 TIT1-FECHA         PIC X(08).
          02 FILLER             PIC X(15)   VALUE SPACES.
          02 FILLER             PIC X(16)   VALUE "LISTADO ALUMNOS ".
          02 FILLER             PIC X(15)   VALUE SPACES.   
          02 FILLER             PIC X(15)   VALUE "HOJA: ".
          02 TIT1-HOJA          PIC 9(03).
          
       01 TITULO-2               PIC X(80)   VALUE ALL "-".
       
       01 TITULO-3.
          02 FILLER             PIC X(05)  VALUE SPACES.
          02 FILLER             PIC X(08)  VALUE "D N I ".
          02 FILLER             PIC X(15)  VALUE SPACES.
          02 FILLER             PIC X(07)  VALUE "NOMBRE ".
          02 FILLER             PIC X(15)  VALUE SPACES.
          02 FILLER             PIC X(18)  VALUE "FECHA NACIMIENTO ".
          
       01 LIN-DETALLE.
          02 FILLER             PIC X(05)  VALUE SPACES.
          02 LIN-DNI            PIC X(08).
          02 FILLER             PIC X(10)  VALUE SPACES.
          02 LIN-NOMBRE         PIC X(10).
          02 FILLER             PIC X(15)  VALUE SPACES.
          02 LIN-FEC-NAC        PIC X(10).
          
       01 LIN-TOTAL.
          02 FILLER             PIC X(15)  VALUE "TOTAL ALUMNOS ".
          02 LIN-TOT-ALUM       PIC ZZZ99.
          
       01 WK-FECHA              PIC 9(08).
       01 FILLER REDEFINES  WK-FECHA.
          02 WK-FEC-ANIO        PIC X(04).
          02 WK-FEC-MES         PIC X(02).
          02 WK-FEC-DIA         PIC X(02).
          
       01 WK-FECHA-ED.
          02 WK-FEC-DIA-ED     PIC X(02).
          02 FILLER            PIC X(01)   VALUE "/".
          02 WK-FEC-MES-ED     PIC X(02).
          02 FILLER            PIC X(01)   VALUE "/".
          02 WK-FEC-ANIO-ED    PIC X(04).
                                   
                                    
                                        

       PROCEDURE DIVISION.
       CONTROL-PROG.
           PERFORM INICIO       THRU F-INICIO
           PERFORM PROCESO      THRU F-PROCESO
           PERFORM FIN          THRU F-FIN
           GOBACK.
           
           
                
       INICIO.
           PERFORM ABRIR-ARCHIVO THRU F-ABRIR-ARCHIVO
           PERFORM ENCABEZAR     THRU F-ENCABEZAR.
       F-INICIO.
       
      *  ABRIR-ARCHIVOS_AQUI ABRIMOS LOS ARCHIVOS   
       ABRIR-ARCHIVO.
           OPEN INPUT ARCHIVO
           OPEN OUTPUT LISTADO.
       F-ABRIR-ARCHIVO.
       
       
       ENCABEZAR. 
      * AQUI SE ACEPTA LA HORA DE SISTEMA 
            ACCEPT WK-FECHA FROM century-date
           
            MOVE WK-FEC-ANIO   TO WK-FEC-ANIO-ED
            MOVE WK-FEC-MES    TO WK-FEC-MES-ED
            MOVE WK-FEC-DIA    TO WK-FEC-DIA-ED
            MOVE WK-FECHA-ED   TO TIT1-FECHA
           
            MOVE 1 TO TIT1-HOJA
      * Y MUESTRA PANTALLA      
            WRITE REG-LIS FROM TITULO1
            WRITE REG-LIS FROM TITULO2
            WRITE REG-LIS FROM TITULO3
            WRITE REG-LIS FROM TITULO2.
       F-ENCABEZAR.
       
       
           
       PROCESO.
           PERFORM LEER-ARCHIVO THRU F-LEER-ARCHIVO
      * INICIALIZO CONTADOR DE LINEAS EN 4 PARA DESCONTAR TITULOS     
           MOVE 4 TO WK-LIN
           
                INITIALIZE LIN-DETALLE
      * SI EL CONTADOR ES MAYOR A 60 AGREGO 1 NUM HOJA          
                IF WK-LIN > 60 
                   ADD 1 TO TIT1-HOJA
      * ENCABEZO PROX HOJA             
                   PERFORM ENCABEZAR
                END-IF
             
            PERFORM DETALLE THRU F-DETALLE.
       F-PROCESO.
       
       
      * LEEMOS ARCHIVO HASTA FINAL DE ARCHIVO 
       LEER-ARCHIVO.
            
              READ ARCHIVO 
                   AT END
                      MOVE 1 TO WK-FINAL
                 
              END-READ  
           
      * AQUI SE AGREGA 1 AL CONTADOR DE ALUMNOS          
              ADD  1 TO WK-LEIDOS
             . 
       F-LEER-ARCHIVO.
           
       
      * IMPRIMO LISTADO
       DETALLE. 
            MOVE ARCH-DNI     TO LIN-DNI
            MOVE ARCH-NOMBRE  TO LIN-NOMBRE 
            MOVE ARCH-FEC-NAC TO LIN-FEC-NAC
            
            WRITE REG-LIS FROM LIN-DETALLE.
       F-DETALLE. 
       
         
       
       FIN. 
                   
      * SI EL CONTADOR ES MAYOR A 60 ENCABEZO Y MUESTRO CANTIDAD ALUMNOS
      * SINO SOLO MUESTRO CANT DE ALUMNOS      
             IF WK-LIN > 60 
               PERFORM ENCABEZAR
               PERFORM TOTAL-ALUMNO
              
             ELSE 
               PERFORM TOTAL-ALUMNO
            END-IF   
            
            PERFORM CERRAR-ARCHIVO.
       F-FIN.
            
      * AQUI SE IMPRIMEN CANTIDAD DE ALUNMOS     
       TOTAL-ALUMNO.  
            MOVE WK-LEIDOS  TO LIN-TOT-ALUM
            
           
            WRITE REG-LIS FROM LIN-TOTAL
            ADD 1 TO WK-LIN.
            
      * CIERRA ARCHIVOS     
       CERRAR-ARCHIVO.
           CLOSE ARCHIVO
                 LISTADO.