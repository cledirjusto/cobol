       IDENTIFICATION DIVISION.
       PROGRAM-ID.	EXMEDIA.
       AUTHOR.		CLEDIR.
       INSTALLATION.	FATEC-SP.
       DATE-WRITTEN.	02/03/2013.
       DATE-COMPILED.
       SECURITY.	APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS. LE O REGISTRO DE ENTRADA CAD-ENT, CALCULA A MEDIA E GRAVA 
      *O ARQUIVO DE SAIDA CAD-SAI.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
           SOURCE-COMPUTER.	IBM-PC.
           OBJECT-COMPUTER.	IBM-PC.
           SPECIAL-NAMES.	DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           
           SELECT CAD-ENT ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT CAD-SAI ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CAD-ENT 
           LABEL RECORD ARE STANDARD
           VALUE  OF FILE-ID IS "CAD-ENT.DAT".

       01 REG-ENT.

           03 COD-ENT		PIC 9(04).
           03 NOTA1-ENT		PIC 9(02)V9(02).
           03 NOTA2-ENT		PIC 99V99.
           03 SEXO-ENT		PIC X(01).

       FD CAD-SAI
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CAD-SAI.DAT".

       01 REG-SAI.
           03 COD-SAI 		PIC 9(04).
           03 MEDIA-SAI		PIC 9(02)V99.

       WORKING-STORAGE SECTION.

       77 FIM-ARQ	PIC X(03)	VALUE "NAO".
       77 SOMA-NOTA	PIC 9(02)V9(02)	VALUE ZEROS.
       77 WMEDIA	PIC 99V99	VALUE ZEROS.

       PROCEDURE DIVISION.

       PGM-EXEMPLO00.

           PERFORM INICIO.

           PERFORM PRINCIPAL
              UNTIL FIM-ARQ EQUAL "SIM".

           PERFORM TERMINO.

           STOP RUN.

       INICIO.

           OPEN INPUT CAD-ENT
                OUTPUT CAD-SAI.

           PERFORM LEITURA.

       LEITURA.

           READ CAD-ENT
              AT END
              MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.

           PERFORM CALCMEDIA.
           PERFORM GRAVACAO.
           PERFORM LEITURA.

       CALCMEDIA.

           ADD NOTA1-ENT
               NOTA2-ENT
               GIVING SOMA-NOTA.

           DIVIDE SOMA-NOTA BY 2 GIVING WMEDIA.

       GRAVACAO.

           MOVE COD-ENT TO COD-SAI.
           MOVE WMEDIA TO MEDIA-SAI.
           WRITE REG-SAI.

       TERMINO.
           
           CLOSE CAD-ENT
                 CAD-SAI.      