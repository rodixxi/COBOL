       ******************************************
       *           COBOL                        *
       ******************************************

       *******************
       *  IDENTIFICATION *
       *******************
       IDENTIFICATION DIVISION.

       PROGRAM-ID. Calculadora.
       AUTHOR. Maria Julieta Morellato.
       DATE-WRITTEN. 12-11-2015.
       *****************
       *  ENVIRONMENT  *
       *****************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       **********
       *   DATA *
       **********
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WK-FECHA      PIC 9(8).
       01 FILLER REDEFINES WK-FECHA
           02 WK-FECHA-AAAA     PIC 9999.
           02 WK-FECHA-MM       PIC 99.
           02 WK-FECHA-DD       PIC 99.

       01 WK-FEC-ORDEN.
           02 WK-FEC-ORDEN-DD   PIC 99.
           02 WK-FEC-ORDEN-MM   PIC 99.
           02 WK-FEC-ORDEN-AAAA PIC 9999.

       01 WK-NUM-1      PIC S9(2)V99 VALUE ZEROS.
       01 WK-NUM-2      PIC S9(2)V99 VALUE ZEROS.
       01 WK-RESULTADO  PIC S9(4)V99 VALUE ZEROS.
       77 WK-OPERADOR   PIC X        VALUE SPACE.
       77 WK-FECHA-ED   PIC 99/99/9999.
       77 WK-RES-EDIT   PIC +++9,ZZ.

       *************
       * PROCEDURE *
       *************
       PROCEDURE DIVISION.
       INICIO.

       * Limpia la pantalla.
       DISPLAY " " AT 0101 ERASE EOS.

       * Busca la fecha del equipo, la invierte y la muestra-
       PERFORM MOSTRAR-FEC THRU F-MOSTRAR-FEC
       
       * Calcula
       PERFORM PROCESO THRU F-PROCESO
       
       GOBACK.


       MOSTRAR-FEC.
           ACCEPT WK-FECHA FROM CENTURY-DATE.
           MOVE WK-FECHA-AAAA TO WK-FEC-ORDEN-AAAA
               WK-FECHA-MM   TO WK-FEC-ORDEN-MM
               WK-FECHA-DD   TO WK-FEC-ORDEN-DD

           MOVE WK-FEC-ORDEN TO WK-FECHA-ED.

           DISPLAY "FECHA ACTUAL: " AT 0101 CONTROL "BCOLOR=WHITE, FCOLOR=BLACK".
           DISPLAY WK-FECHA-ED      AT 0116 CONTROL "BCOLOR=WHITE, FCOLOR=BLACK". 
       F-MOSTRAR-FEC. EXIT.


       PROCESO.
           DISPLAY "INGRESE EL PRIMER NUMERO:  " AT 0201.
           ACCEPT WK-NUM-1 AT 0228 PROMPT.

           DISPLAY "INGRESE EL SEGUNDO NUMERO: " AT 0301.
           ACCEPT WK-NUM-2 AT 0328 PROMPT.

           DISPLAY "INGRESE EL OPERADOR:       " AT 0401.
           ACCEPT WK-OPERADOR AT 0428 PROMPT.

           IF WK-OPERADOR = "+" THEN
                ADD WK-NUM-1, WK-NUM-2 GIVING WK-RESULTADO.
           END-IF.

           IF WK-OPERADOR = "-" THEN
                SUBTRACT WK-NUM-1 FROM WK-NUM-2 GIVING WK-RESULTADO.
           END-IF.

           IF WK-OPERADOR = "*" THEN
                MULTIPLY WK-NUM-1 BY WK-NUM-2 GIVING WK-RESULTADO.
           END-IF.

           IF WK-OPERADOR = "/" THEN

                IF WK-NUM-2 THEN
                        DISPLAY MESSAGE "ERROR" H"0A" "NO EXISTE LA DIVISION POR CERO".
                ELSE
                        DIVIDE WK-NUM-1 BY WK-NUM-2 GIVING WK-RESULTADO.
                ENF-IF.
           END-IF.

           MOVE WK-RESULTADO TO WK-RES-EDIT.
           DISPLAY "EL RESULTADO ES:           " AT 0601.
           DISPLAY WK-RES-EDIT AT 0628.

       F-PROCESO. EXIT.