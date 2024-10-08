       IDENTIFICATION DIVISION.
       PROGRAM-ID.	EX03.
       AUTHOR.          CLEDIR JUSTO.
       INSTALLATION.   	FATEC-SP.
       DATE-WRITTEN.   	27/04/2013.
       DATE-COMPILED.
       SECURITY. APENAS O AUTOR PODE MODIFICA-LO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADENT ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT RELSAI ASSIGN TO DISK.

       DATA DIVISION.
       FILE SECTION.

       FD  CADENT
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADENT.DAT".
       01  REG-CLI.
           02 CODIGO	  PIC 9(07).
           02 NOME-ENT	  PIC X(30).
           02 SALARIO     PIC 9(5)V9(2).
           02 SEXO	  PIC X(1).
  
       FD  RELSAI
           LABEL RECORD IS OMITTED.
       01  REG-ATR     PIC X(80).

       WORKING-STORAGE SECTION.
       77  FIM-ARQ     PIC X(03) VALUE "NAO".
       77  CT-LIN      PIC 9(02) VALUE 20.
       77  CT-PAG      PIC 9(02) VALUE ZEROES.
       77  TOTCLI      PIC 9(05) VALUE ZEROES.
       77  TOTSAL      PIC 9(09)V9(02).

       01 CAB-01.
          02 FILLER    PIC X(70) VALUE SPACES.
          02 FILLER    PIC X(05) VALUE "PAG. ".
          02 VAR-PAG   PIC 99.
          02 FILLER    PIC X(03) VALUE SPACES.

       01 CAB-02.
          02 FILLER    PIC X(21) VALUE SPACES.
          02 FILLER    PIC X(25) VALUE "LISTAGEM DOS CLIENTES DO ".
          02 FILLER    PIC X(13) VALUE "SEXO FEMININO".
          02 FILLER    PIC X(21) VALUE SPACES.

       01 CAB-03.
          02 FILLER    PIC X(23) VALUE SPACES.
          02 FILLER    PIC X(23) VALUE "COM SALARIOS SUPERIORES".
          02 FILLER    PIC X(11) VALUE " A 5.000,00".
          02 FILLER    PIC X(23) VALUE SPACES.
       
       01 CAB-04.
          02 FILLER    PIC X(10)  VALUE SPACES.
          02 FILLER    PIC X(6)  VALUE "CODIGO".
          02 FILLER    PIC X(14) VALUE SPACES.
          02 FILLER    PIC X(4)  VALUE "NOME".
          02 FILLER    PIC X(26) VALUE SPACES.
          02 FILLER    PIC X(07) VALUE "SALARIO".
          02 FILLER    PIC X(13)  VALUE SPACES.

       01 CAB-05.
          02 FILLER    PIC X(18) VALUE "Total de Clientes ".
          02 FILLER    PIC X(11) VALUE "impressos: ".
          02 TOTCL     PIC 9(04).
          02 FILLER    PIC X(47) VALUE SPACES.

       01 CAB-06.
          02 FILLER    PIC X(19) VALUE "Total de Salarios: ".
          02 TOTSA     PIC 9(09)V9(02).
          02 FILLER    PIC X(50) VALUE SPACES.

       01 DETALHE.
          02 FILLER    PIC X(10)  VALUE SPACES.
          02 COD       PIC 9(07).
          02 FILLER    PIC X(05) VALUE SPACES.
          02 NOM       PIC X(30).
          02 FILLER    PIC X(10) VALUE SPACES.
          02 SAL       PIC 9(5)V9(2).

       
       PROCEDURE             DIVISION.
       
       PGM-EX03.
	  PERFORM INICIO.
	  PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
      	  PERFORM FIM.
	  STOP RUN.

       INICIO.
          OPEN INPUT  CADENT
               OUTPUT RELSAI.
	  PERFORM LEITURA.

       LEITURA.
          READ CADENT AT END MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
          IF SALARIO NOT< 5000 and SEXO = "F"
          PERFORM IMPRESSAO.
	  PERFORM LEITURA.


       IMPRESSAO.
          IF CT-LIN GREATER THAN 19
	      PERFORM CABECALHO.
          PERFORM IMPDET.

       IMPDET.
          MOVE CODIGO TO COD.
          MOVE NOME-ENT TO NOM.
          MOVE SALARIO     TO SAL.
          WRITE REG-ATR FROM DETALHE AFTER ADVANCING 1 LINE.
	  ADD 1 TO CT-LIN.
          ADD 1 TO TOTCLI.
          ADD SALARIO TO TOTSAL.

       CABECALHO.
	  ADD 1 TO CT-PAG.
	  MOVE CT-PAG TO VAR-PAG.
          MOVE SPACES TO REG-ATR.
          WRITE REG-ATR AFTER ADVANCING PAGE.
          WRITE REG-ATR FROM CAB-01 AFTER ADVANCING 1 LINE.
          WRITE REG-ATR FROM CAB-02 AFTER ADVANCING 1 LINES.
	  WRITE REG-ATR FROM CAB-03 AFTER ADVANCING 1 LINES.
          WRITE REG-ATR FROM CAB-04 AFTER ADVANCING 2 LINES.
	  MOVE ZEROES TO CT-LIN.


       FIM.
          MOVE TOTCLI TO TOTCL.
          WRITE REG-ATR FROM CAB-05 AFTER ADVANCING 1 LINES.
          MOVE TOTSAL TO TOTSA.
          WRITE REG-ATR FROM CAB-06 AFTER ADVANCING 1 LINES.
          CLOSE CADENT
                RELSAI.
                      
