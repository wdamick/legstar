       PROCESS XOPTS(SP APOST)
       PROCESS NOSEQ LIB OPTIMIZE(FULL)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. T1CONTXT.
      *****************************************************************
      * OVERVIEW                                                      *
      * --------                                                      *
      * Output only DPL program, used to track the CICS context       *
      *****************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER. IBM-390 WITH DEBUGGING MODE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

        01  FILEA.   COPY DFH0CFIL.
        01  RESPONSE     PIC S9(8).

       LINKAGE SECTION.
       01 DFHCOMMAREA.
          05 COM-USER-ID         PIC X(8).
          05 COM-TRANSACTION-ID  PIC X(4).
          05 COM-TASK-ID         COMP-3 PIC S9(7).
          05 COM-TERM-ID         PIC X(4).
          05 COM-COMMAREA-SIZE   PIC 9(8) BINARY.

       PROCEDURE DIVISION.

           EXEC CICS ASSIGN USERID(COM-USER-ID) END-EXEC.
           MOVE EIBTRNID TO COM-TRANSACTION-ID.
           COMPUTE COM-TASK-ID = EIBTASKN.
           MOVE EIBTRMID TO COM-TERM-ID.
           COMPUTE COM-COMMAREA-SIZE = EIBCALEN.

           EXEC CICS RETURN END-EXEC.

           GOBACK.

       END PROGRAM T1CONTXT.
