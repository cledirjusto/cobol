       IDENTIFICATION DIVISION.
           PROGRAM-ID.		EX07.
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
           02 NUMERO-ENT	PIC 9(05).
           02 NOME-ENT		PIC X(20).
           02 NOTA1             PIC 9(02)V99.
           02 NOTA2             PIC 9(02)V99.
           02 NOTA3             PIC 9(02)V99.
           02 NOTA4             PIC 9(02)V99.
           02 SEXO              PIC X(01).
		   
       FD CADATU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADATU.DAT".
		   
       01 REG-SAI.
           02 NUMERO-SAI	PIC 9(05).
           02 NOME-SAI		PIC X(20).
           02 MEDIA             PIC 9(02)V99.
           02 SEXO-SAI              PIC X(01).
		   
           WORKING-STORAGE SECTION.
           77 FIM-ARQ		PIC X(03) VALUE "NAO".
           77 MEDIAFINAL        PIC 9(02)V99 VALUE ZEROS.

       PROCEDURE DIVISION.
	   
       PGM-EXE07.
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
           PERFORM SELECAO.
           PERFORM LEITURA.

       SELECAO.
           
           IF SEXO = "F" OR "f"
           PERFORM GRAVACAO.

       GRAVACAO.
           COMPUTE MEDIAFINAL = (NOTA1 + NOTA2 + NOTA3 + NOTA4)/4.
           MOVE NUMERO-ENT TO NUMERO-SAI.
           MOVE NOME-ENT TO NOME-SAI.
           MOVE MEDIAFINAL TO MEDIA.
           MOVE SEXO TO SEXO-SAI.
           WRITE REG-SAI.

       TERMINO.
           CLOSE   CADALU
                   CADATU.
