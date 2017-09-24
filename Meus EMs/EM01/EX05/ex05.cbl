       IDENTIFICATION DIVISION.
           PROGRAM-ID.		EX05.
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
           SELECT CADAPR ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CADALU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADALU.DAT".
		   
       01 REG-ENT.
           02 NUMERO-ENT	PIC 9(05).
           02 NOME-ENT		PIC X(20).
           02 NOTA1             PIC 9(02)V99.
           02 NOTA2             PIC 9(02)V99.
           02 FALTAS            PIC 9(02).
		   
       FD CADAPR
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADAPR.DAT".
		   
       01 REG-SAI.
           02 NUMERO-SAI	PIC 9(05).
           02 NOME-SAI		PIC X(20).
           02 MEDIA             PIC 9(02)V99.
		   
           WORKING-STORAGE SECTION.
           77 FIM-ARQ		PIC X(03) VALUE "NAO".
           77 MEDIAFINAL        PIC 9(02)V99 VALUE ZEROS.

       PROCEDURE DIVISION.
	   
       PGM-EXE05.
           PERFORM INICIO.
           PERFORM PRINCIPAL
             UNTIL FIM-ARQ EQUAL "SIM".
             
           PERFORM TERMINO.

       STOP RUN.

       INICIO.

           OPEN INPUT CADALU
                OUTPUT CADAPR.
           PERFORM LEITURA.

       LEITURA.
           READ CADALU
           AT END
           MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
           PERFORM SELECAO.
           PERFORM LEITURA.

       SELECAO.
           COMPUTE MEDIAFINAL = (NOTA1 + NOTA2)/2.
           IF MEDIAFINAL NOT < 7 AND FALTAS NOT > 18
           PERFORM GRAVACAO.

       GRAVACAO.
           MOVE NUMERO-ENT TO NUMERO-SAI.
           MOVE NOME-ENT TO NOME-SAI.
           MOVE MEDIAFINAL TO MEDIA.
           WRITE REG-SAI.

       TERMINO.
           CLOSE   CADALU
                   CADAPR.
