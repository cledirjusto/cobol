       IDENTIFICATION DIVISION.
           PROGRAM-ID.		EX02.
           AUTHOR.		CLEDIR JUSTO.
           INSTALLATION.	FATEC-SP.
           DATE-WRITTEN.	03/03/2013.
           DATE-COMPILED.
           SECURITY.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.		IBM-PC.
       OBJECT-COMPUTER.		IBM-PC.
       SPECIAL-NAMES.		DECIMAL-POINT IS COMMA.	

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADALU ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADATU ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CADALU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADALU.DAT".
		   
       01 REG-ENT.
           02 NUMERO		PIC 9(05).
           02 NOME		PIC X(20).
           02 SEXO		PIC X(01).
           02 DD		PIC 9(02).
           02 MM		PIC 9(02).
           02 AAAA		PIC 9(04).
		   
       FD CADATU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADATU.DAT".
		   
       01 REG-SAI.
           02 NUMERO-SAI	PIC 9(05).
           02 NOME-SAI		PIC X(20).
           02 DD-SAI		PIC 9(02).
           02 MM-SAI		PIC 9(02).
           02 AAAA-SAI          PIC 9(04).
		   
           WORKING-STORAGE SECTION.
           77 FIM-ARQ		PIC X(03) VALUE "NAO".

       PROCEDURE DIVISION.
	   
       PGM-EX02.
           PERFORM INICIO.
           PERFORM PRINCIPAL
             UNTIL FIM-ARQ EQUAL "SIM".
             
           PERFORM TERMINO.

       STOP RUN.

       INICIO.

           OPEN INPUT CADALU
                OUTPUT CADATU.
           PERFORM LEITURA.

       LEITURA.
           READ CADALU
           AT END
           MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
           PERFORM GRAVACAO.
           PERFORM LEITURA.

       GRAVACAO.
           MOVE NUMERO TO NUMERO-SAI.
           MOVE NOME TO NOME-SAI.
           MOVE DD TO DD-SAI.
           MOVE MM TO MM-SAI.
           MOVE AAAA TO AAAA-SAI.
           WRITE REG-SAI.

       TERMINO.
           CLOSE   CADALU
                   CADATU.
