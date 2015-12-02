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
       FILE-CONTROL.
           SELECT MOV-CTAS
           ASSIGN TO "\COBOL\arch\mov-cuentas.prm"
           ORGANIZATION IS SEQUENTIAL.
           SELECT LISTADO
           ASSIGN TO "\COBOL\arch\LISTADO-MOV"
           ORGANIZATION IS SEQUENTIAL.       
      *----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.

       FD  MOV-CTAS.
       01  MOV-REG.
           03 M-MOV            PIC X.
           03 M-CTA            PIC 9(08).
           03 M-APE            PIC X(20).
           03 M-NOM            PIC X(20).
           03 M-FNAC           PIC 9(08).
           03 M-PROV           PIC X.

       FD  LISTADO.
       01  REGISTRO            PIC X(100).

       WORKING-STORAGE SECTION.

       01  LIS-REG.
           03 L-MOV            PIC X.
           03 L-CTA            PIC 9(08).
           03 L-APE            PIC X(20).
           03 L-NOM            PIC X(20).
           03 L-FNAC           PIC 9(10).
           03 L-PROV           PIC X.
           03 L-OBS            PIC X(20).




       SCREEN SECTION.

      *----------------------------------------------------------------
       PROCEDURE DIVISION.

      
       END PROGRAM.
