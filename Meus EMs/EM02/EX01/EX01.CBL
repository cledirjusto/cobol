       IDENTIFICATION DIVISION.
          PROGRAM-ID.      EX01.
          AUTHOR.   CLEDIR JUSTO.
          INSTALLATION.   	FATEC-SP.
          DATE-WRITTEN.   	21/04/2013.
          DATE-COMPILED.
          SECURITY. APENAS O AUTOR PODE MODIFICA-LO.

       ENVIRONMENT    DIVISION.
       CONFIGURATION SECTION.
          SOURCE-COMPUTER. IBM-PC.
          OBJECT-COMPUTER. IBM-PC.
          SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
          FILE-CONTROL.
          SELECT CADSOC1 ASSIGN TO DISK
          ORGANIZATION IS LINE SEQUENTIAL.
          SELECT CADSOC2 ASSIGN TO DISK          
          ORGANIZATION IS LINE SEQUENTIAL.
          SELECT RELSOCIO ASSIGN TO DISK.

       DATA    DIVISION.
       FILE SECTION.

       FD  CADSOC1
          LABEL RECORD ARE STANDARD
          VALUE OF FILE-ID IS "CADSOC1.DAT".
          01 REG-SOC1.
          02 CODIGO-PAGAMENTO1     PIC 9(02).
          02 NUMERO-SOCIO1         PIC 9(06).
          02 NOME-SOCIO1           PIC X(30).
          02 VALOR-PAGAMENTO1      PIC 9(09)V9(02).
  
       FD  CADSOC2
          LABEL RECORD ARE STANDARD
          VALUE OF FILE-ID IS "CADSOC2.DAT".
          01 REG-SOC2.
          02 NUMERO-SOCIO2         PIC 9(06).
          02 NOME-SOCIO2           PIC X(30).
          02 VALOR-PAGAMENTO2      PIC 9(09)V9(02).

       FD  RELSOCIO
          LABEL RECORD IS OMITTED.
          01  REGSOCIO     PIC X(80).

       WORKING-STORAGE SECTION.
       77  FIM-ARQ            PIC X(03) VALUE "NAO".
       77  CT-LIN             PIC 9(02) VALUE 30.
       77  CT-PAG             PIC 9(02) VALUE ZEROES.
       77  VALOR-TOTAL        PIC 9(12)V9(02) VALUE ZEROES.
       77  TOTAL-SOCIOS       PIC 9(05) VALUE ZEROES. 

       01 CAB-01.
          02 FILLER    PIC X(70) VALUE SPACES.          
          02 FILLER    PIC X(05) VALUE "PAG. ".
          02 VAR-PAG   PIC 99.
          02 FILLER    PIC X(03) VALUE SPACES.

       01 CAB-02.
          02 FILLER    PIC X(80) VALUE SPACES.          

       01 CAB-03.
          02 FILLER    PIC X(25) VALUE SPACES.
          02 FILLER    PIC X(28) VALUE "RELACAO DE SOCIOS ATRASADOS".
          02 FILLER    PIC X(27) VALUE SPACES.
       
       01 CAB-04.
          02 FILLER    PIC X(80) VALUE SPACES.    

       01 CAB-05.
          02 FILLER    PIC X(05)  VALUE SPACES.
          02 FILLER    PIC X(15)  VALUE "NUMERO DO SOCIO".
          02 FILLER    PIC X(12)  VALUE SPACES.
          02 FILLER    PIC X(13)  VALUE "NOME DO SOCIO".
          02 FILLER    PIC X(12) VALUE SPACES.
          02 FILLER    PIC X(18) VALUE "VALOR DO PAGAMENTO".
          02 FILLER    PIC X(05)  VALUE SPACES.

       01 CAB-06.
          02 FILLER    PIC X(80) VALUE SPACES. 

       01 CAB-07.
          02 FILLER    PIC X(05)  VALUE SPACES.
          02 FILLER    PIC X(26)  VALUE "TOTAL DE SOCIOS ATRASADOS:".
          02 FILLER    PIC X(03)  VALUE SPACES.
          02 TOT       PIC 9(05).

       01 CAB-08.
          02 FILLER    PIC X(05)  VALUE SPACES.
          02 FILLER    PIC X(21)  VALUE "VALOR TOTAL ATRASADO:".
          02 FILLER    PIC X(03)  VALUE SPACES.
          02 ATRASADO  PIC 9(12)V9(02).


       01 DETALHE.
          02 FILLER    PIC X(05) VALUE SPACES.
          02 NUM       PIC 9(06).
          02 FILLER    PIC X(21) VALUE SPACES.
          02 NOM       PIC X(30).
          02 FILLER    PIC X(1) VALUE SPACES.
          02 VALOR     PIC 9(09)V9(02).
          02 FILLER    PIC X(1) VALUE SPACES.
 
       PROCEDURE DIVISION.
       
       PGM-EX01.
	  PERFORM INICIO.
	  PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
      	  PERFORM FIM.
	  STOP RUN.

       INICIO.
          OPEN INPUT  CADSOC1
               OUTPUT CADSOC2
                      RELSOCIO.
       PERFORM LEITURA.

       LEITURA.
          READ CADSOC1 AT END MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
          PERFORM VERIFICACAO.
	  PERFORM LEITURA.

       VERIFICACAO.
          IF CODIGO-PAGAMENTO1 EQUAL TO 01
              PERFORM EMDIA.
          IF CODIGO-PAGAMENTO1 EQUAL TO 02
              PERFORM IMPRESSAO.

       EMDIA.
          MOVE NUMERO-SOCIO1 TO NUMERO-SOCIO2.
          MOVE NOME-SOCIO1 TO NOME-SOCIO2.
          MOVE VALOR-PAGAMENTO1 TO VALOR-PAGAMENTO2.
          WRITE REG-SOC2.


       IMPRESSAO.
          IF CT-LIN GREATER THAN 29
	      PERFORM CABECALHO.
              PERFORM IMPDET.

       IMPDET.
          MOVE NUMERO-SOCIO1 TO NUM.
          MOVE NOME-SOCIO1   TO NOM.
          MOVE VALOR-PAGAMENTO1 TO VALOR.
          ADD 1 TO TOTAL-SOCIOS.
          ADD VALOR-PAGAMENTO1 TO VALOR-TOTAL.
          WRITE REGSOCIO FROM DETALHE AFTER ADVANCING 1 LINE.
	  ADD 1 TO CT-LIN.

       CABECALHO.
	  ADD 1 TO CT-PAG.
	  MOVE CT-PAG TO VAR-PAG.
          MOVE SPACES TO REGSOCIO.
          WRITE REGSOCIO AFTER ADVANCING PAGE.
          WRITE REGSOCIO FROM CAB-01 AFTER ADVANCING 1 LINE.
          WRITE REGSOCIO FROM CAB-02 AFTER ADVANCING 1 LINES.
	  WRITE REGSOCIO FROM CAB-03 AFTER ADVANCING 1 LINES.
          WRITE REGSOCIO FROM CAB-04 AFTER ADVANCING 1 LINES.
	  WRITE REGSOCIO FROM CAB-05 AFTER ADVANCING 1 LINES.
	  WRITE REGSOCIO FROM CAB-06 AFTER ADVANCING 1 LINES.
	  MOVE ZEROES TO CT-LIN.


       FIM.
          WRITE REGSOCIO FROM CAB-02 AFTER ADVANCING 1 LINES.
          MOVE TOTAL-SOCIOS TO TOT.
          WRITE REGSOCIO FROM CAB-07 AFTER ADVANCING 1 LINES.
          MOVE VALOR-TOTAL TO ATRASADO.
          WRITE REGSOCIO FROM CAB-08 AFTER ADVANCING 1 LINES.
          CLOSE CADSOC1
                CADSOC2
                RELSOCIO.
        
