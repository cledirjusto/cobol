       IDENTIFICATION DIVISION.
          PROGRAM-ID.      EX05.
          AUTHOR.   CLEDIR JUSTO.
          INSTALLATION.   	FATEC-SP.
          DATE-WRITTEN.   	27/04/2013.
          DATE-COMPILED.
          SECURITY. APENAS O AUTOR PODE MODIFICA-LO.

       ENVIRONMENT    DIVISION.
       CONFIGURATION SECTION.
          SOURCE-COMPUTER. IBM-PC.
          OBJECT-COMPUTER. IBM-PC.
          SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
          FILE-CONTROL.
          SELECT ARQALU ASSIGN TO DISK
          ORGANIZATION IS LINE SEQUENTIAL.
          SELECT ARQREP ASSIGN TO DISK          
          ORGANIZATION IS LINE SEQUENTIAL.
          SELECT RELAPROV ASSIGN TO DISK.

       DATA    DIVISION.
       FILE SECTION.

       FD  ARQALU
          LABEL RECORD ARE STANDARD
          VALUE OF FILE-ID IS "ARQALU.DAT".
          01 REG-ALU.
          02 MATRICULA-ENT.
            03 MATR                    PIC 9(06).
            03 DIGI                    PIC 9(01).
          02 NOME-ENT                  PIC X(30).
          02 NOTA1-ENT                 PIC 9(02)V9(02).
          02 NOTA2-ENT                 PIC 9(02)V9(02).
          02 NOTA3-ENT                 PIC 9(02)V9(02).
          02 FALTAS-ENT                PIC 9(02).
          02 SEXO-ENT                  PIC X(01).

  
       FD  ARQREP
          LABEL RECORD ARE STANDARD
          VALUE OF FILE-ID IS "ARQREP.DAT".
          01 REG-REP.
          02 MATRICULA-SAI             PIC 9(07).
          02 NOME-SAI                  PIC X(30).
          02 NOTA1-SAI                 PIC 9(02)V9(02).
          02 NOTA2-SAI                 PIC 9(02)V9(02).
          02 NOTA3-SAI                 PIC 9(02)V9(02).
          02 FALTAS-SAI                PIC 9(02).
          02 SEXO-SAI                  PIC X(01).

       FD  RELAPROV
          LABEL RECORD IS OMITTED.
          01  REGAPROV     PIC X(80).

       WORKING-STORAGE SECTION.
       77  FIM-ARQ            PIC X(03) VALUE "NAO".
       77  CT-LIN             PIC 9(02) VALUE 30.
       77  CT-PAG             PIC 9(02) VALUE ZEROES.
       77  SOMA               PIC 9(03).
       77  MEDIA              PIC 9(02)V9(02).
       77  TOTAPROV           PIC 9(03) VALUE ZEROES.
       77  MEDIAGERAL         PIC 9(03)V9(02) VALUE ZEROES.
       77  MEDIATOT           PIC 9(03)V9(02) VALUE ZEROES.
 
       01 CAB-01.
          02 FILLER    PIC X(21) VALUE SPACES.
          02 FILLER    PIC X(18) VALUE "RELACAO DE ALUNOS ".
          02 FILLER    PIC X(09) VALUE "APROVADOS".  
          02 FILLER    PIC X(22) VALUE SPACES.       
          02 FILLER    PIC X(05) VALUE "PAG. ".
          02 VAR-PAG   PIC 99.
          02 FILLER    PIC X(03) VALUE SPACES.
           

       01 CAB-02.
          02 FILLER    PIC X(05)  VALUE SPACES.
          02 FILLER    PIC X(06)  VALUE "NUMERO".
          02 FILLER    PIC X(17)  VALUE SPACES.
          02 FILLER    PIC X(13)  VALUE "NOME DO ALUNO".
          02 FILLER    PIC X(18) VALUE SPACES.
          02 FILLER    PIC X(05) VALUE "MEDIA".
          02 FILLER    PIC X(10)  VALUE SPACES.
          02 FILLER    PIC X(06) VALUE "FALTAS".


       01 CAB-03.
          02 FILLER    PIC X(05)  VALUE SPACES.
          02 FILLER    PIC X(26)  VALUE "TOTAL DE ALUNOS APROVADOS:".
          02 FILLER    PIC X(01)  VALUE SPACES.
          02 TOTAPRO   PIC 9(05).

       01 CAB-04.
          02 FILLER    PIC X(05)  VALUE SPACES.
          02 FILLER    PIC X(21)  VALUE "MEDIA GERAL DA TURMA:".
          02 FILLER    PIC X(01)  VALUE SPACES.
          02 MEDIAGERA  PIC 9(03)V9(02).

       01 CAB-05.
          02 FILLER    PIC X(80)  VALUE SPACES.


       01 DETALHE.
          02 FILLER    PIC X(05) VALUE SPACES.
          02 MAT       PIC 9(06).
          02 FILLER    PIC X(01) VALUE "-".
          02 DIG       PIC 9(01).
          02 FILLER    PIC X(18) VALUE SPACES.
          02 NOM       PIC X(30).
          02 FILLER    PIC X(5) VALUE SPACES.
          02 MEDI      PIC 9(02)V9(02).
          02 FILLER    PIC X(5) VALUE SPACES.
          02 FALT      PIC 9(02).
          02 FILLER    PIC X(5) VALUE SPACES.
 
       PROCEDURE DIVISION.
       
       PGM-EX05.
	  PERFORM INICIO.
	  PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
      	  PERFORM FIM.
	  STOP RUN.

       INICIO.
          OPEN INPUT  ARQALU
               OUTPUT ARQREP
                      RELAPROV.
       PERFORM LEITURA.

       LEITURA.
          READ ARQALU AT END MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
          PERFORM VERIFICACAO.
	  PERFORM LEITURA.

       VERIFICACAO.
          ADD NOTA1-ENT NOTA2-ENT NOTA3-ENT GIVING SOMA.
          DIVIDE SOMA BY 3 GIVING MEDIA.
          IF MEDIA NOT< 7 AND FALTAS-ENT < 16 
              PERFORM IMPRESSAO.
          IF MEDIA NOT> 7 OR FALTAS-ENT > 15
              PERFORM REPROVADO.

       REPROVADO.
          MOVE MATRICULA-ENT TO MATRICULA-SAI.
          MOVE NOME-ENT TO NOME-SAI.
          MOVE NOTA1-ENT TO NOTA1-SAI.
          MOVE NOTA2-ENT TO NOTA2-SAI.
          MOVE NOTA3-ENT TO NOTA3-SAI.
          MOVE FALTAS-ENT TO FALTAS-SAI.
          MOVE SEXO-ENT TO SEXO-SAI.
          WRITE REG-REP.


       IMPRESSAO.
          IF CT-LIN GREATER THAN 29
	      PERFORM CABECALHO.
              PERFORM IMPDET.

       IMPDET.
          ADD NOTA1-ENT NOTA2-ENT NOTA3-ENT GIVING SOMA.
          DIVIDE SOMA BY 3 GIVING MEDIA
          MOVE MATR TO MAT.
          MOVE DIGI TO DIG.
          MOVE NOME-ENT TO NOM.
          MOVE MEDIA TO MEDI.
          MOVE FALTAS-ENT TO FALT.
          ADD 1 TO TOTAPROV.
          ADD MEDIA TO MEDIAGERAL.
          WRITE REGAPROV FROM DETALHE AFTER ADVANCING 1 LINE.
	  ADD 1 TO CT-LIN.

       CABECALHO.
	  ADD 1 TO CT-PAG.
	  MOVE CT-PAG TO VAR-PAG.
          MOVE SPACES TO REGAPROV.
          WRITE REGAPROV AFTER ADVANCING PAGE.
          WRITE REGAPROV FROM CAB-01 AFTER ADVANCING 1 LINE.
          WRITE REGAPROV FROM CAB-02 AFTER ADVANCING 2 LINES.
          WRITE REGAPROV FROM CAB-05 AFTER ADVANCING 2 LINES.
          MOVE ZEROES TO CT-LIN.


       FIM.
          MOVE TOTAPROV TO TOTAPRO.
          WRITE REGAPROV FROM CAB-03 AFTER ADVANCING 1 LINES.
          DIVIDE MEDIAGERAL BY TOTAPROV GIVING MEDIATOT.
          MOVE MEDIATOT TO MEDIAGERA.
          WRITE REGAPROV FROM CAB-04 AFTER ADVANCING 1 LINES.
          CLOSE ARQALU
                ARQREP
                RELAPROV.
        
