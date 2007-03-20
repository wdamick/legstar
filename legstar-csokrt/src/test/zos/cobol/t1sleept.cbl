       PROCESS XOPTS(SP APOST)
       PROCESS NOSEQ LIB OPTIMIZE(FULL)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. T1SLEEPT.
      *****************************************************************
      * OVERVIEW                                                      *
      * --------                                                      *
      * Receives a number of seconds to sleep in order to simulate    *
      * CICS application response time.                               *
      *****************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER. IBM-390 WITH DEBUGGING MODE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  W-SECONDS    PIC 9(8) BINARY VALUE ZERO.

 
       LINKAGE SECTION.
       01 DFHCOMMAREA.
          05 COM-SLEEP-TIME      PIC 9(8).
          05 COM-USER-ID         PIC X(8).
          05 COM-TRANSACTION-ID  PIC X(4).
          05 COM-TASK-ID         PIC S9(7).
          05 COM-TERM-ID         PIC X(4).
          05 COM-COMMAREA-SIZE   PIC 9(8).

       PROCEDURE DIVISION.

           COMPUTE W-SECONDS = COM-SLEEP-TIME.
           EXEC CICS DELAY FOR SECONDS(W-SECONDS) END-EXEC.
           
           EXEC CICS ASSIGN USERID(COM-USER-ID) END-EXEC.
           MOVE EIBTRNID TO COM-TRANSACTION-ID.
           COMPUTE COM-TASK-ID = EIBTASKN.
           IF EIBTRMID NOT = LOW-VALUES
              MOVE EIBTRMID TO COM-TERM-ID
           ELSE
              MOVE SPACES TO COM-TERM-ID
           END-IF.
           COMPUTE COM-COMMAREA-SIZE = EIBCALEN.

           EXEC CICS RETURN END-EXEC.

           GOBACK.

       END PROGRAM T1SLEEPT.
