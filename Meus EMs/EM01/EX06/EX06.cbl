       IDENTIFICATION DIVISION.
           PROGRAM-ID.			EX06.
           AUTHOR.			CLEDIR JUSTO.
           INSTALLATION.		FATEC-SP.
           DATE-WRITTEN.		03/03/2013.
           DATE-COMPILED.
           SECURITY.

	  
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.		IBM-PC.
       OBJECT-COMPUTER.		IBM-PC.
       SPECIAL-NAMES.		DECIMAL-POINT IS COMMA.
	   
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADENT ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADSAI ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
		   
       DATA DIVISION.
       FILE SECTION.
       FD CADENT
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADENT.DAT".
		   
       01 REG-ENT.
           02 MATRICULA-ENT	PIC 9(05).
           02 NOME-ENT		PIC X(30).
           02 SALARIOB-ENT	PIC 9(05)V99.
		   
       FD CADSAI
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADSAI.DAT".
		   
       01 REG-SAI.
           02 MATRICULA-SAI	PIC 9(05).
           02 NOME-SAI		PIC X(30).
           02 SALARIOB-SAI      PIC 9(5)V99.
		   
           WORKING-STORAGE SECTION.
           77 FIM-ARQ		PIC X(03) VALUE "NAO".
		   
       PROCEDURE DIVISION.
	   
       PGM-EXEMPLO06.
           PERFORM INICIO.
           PERFORM PRINCIPAL
             UNTIL FIM-ARQ EQUAL "SIM".
             
           PERFORM TERMINO.

       STOP RUN.

       INICIO.

           OPEN INPUT CADENT
                OUTPUT CADSAI.
           PERFORM LEITURA.

       LEITURA.
           READ CADENT
           AT END
           MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
           PERFORM VERIFICACAO.
           PERFORM LEITURA.

       VERIFICACAO.
           IF SALARIOB-ENT > 3000
           PERFORM GRAVACAO.

       GRAVACAO.
           MOVE MATRICULA-ENT TO MATRICULA-SAI.
           MOVE NOME-ENT TO NOME-SAI.
           MOVE SALARIOB-ENT TO SALARIOB-SAI.
           WRITE REG-SAI.

       TERMINO.
           CLOSE   CADENT
                   CADSAI.
