       IDENTIFICATION DIVISION.
           PROGRAM-ID.		EX01.
           AUTHOR.		CLEDIR JUSTO.
           INSTALLATION.	FATEC-SP.
           DATE-WRITTEN.	23/02/2013.
           DATE-COMPILED.
           SECURITY.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.		IBM-PC.
       OBJECT-COMPUTER.		IBM-PC.
       SPECIAL-NAMES.		DECIMAL-POINT IS COMMA.	

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CAD-CLI1 ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CAD-CLI2 ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CAD-CLI1
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CAD-CLI1.DAT".
		   
       01 REG-ENT.
           02 COD-ENT		PIC 9(05).
           02 NOME-ENT		PIC X(20).
		   
       FD CAD-CLI2
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CAD-CLI2.DAT".
		   
       01 REG-SAI.
           02 NOME-SAI		PIC X(20).
           02 COD-SAI		PIC 9(05).
		   
           WORKING-STORAGE SECTION.
           77 FIM-ARQ		PIC X(03) VALUE "NAO".

       PROCEDURE DIVISION.
	   
       PGM-EXEMPLO00.
           PERFORM INICIO.
           PERFORM PRINCIPAL
             UNTIL FIM-ARQ EQUAL "SIM".
             
           PERFORM TERMINO.

       STOP RUN.

       INICIO.

           OPEN INPUT CAD-CLI1
                OUTPUT CAD-CLI2.
           PERFORM LEITURA.

       LEITURA.
           READ CAD-CLI1
           AT END
           MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
           PERFORM GRAVACAO.
           PERFORM LEITURA.

       GRAVACAO.
           MOVE COD-ENT TO COD-SAI.
           MOVE NOME-ENT TO NOME-SAI.
           WRITE REG-SAI.

       TERMINO.
           CLOSE   CAD-CLI1
                   CAD-CLI2.
