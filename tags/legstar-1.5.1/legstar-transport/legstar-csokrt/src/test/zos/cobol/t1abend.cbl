       PROCESS XOPTS(SP APOST)
       PROCESS NOSEQ LIB OPTIMIZE(FULL)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. T1ABEND.
      *****************************************************************
      * OVERVIEW                                                      *
      * --------                                                      *
      * Generates the type of abend requested by client.              *
      *****************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER. IBM-390 WITH DEBUGGING MODE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  W-STRUCT.
           05  W-PACKED  PIC S9(5) COMP-3 VALUE ZERO.
           05  FILLER    PIC X(3) VALUE SPACES.
 
       LINKAGE SECTION.
       01 DFHCOMMAREA.
          05 COM-ABEND PIC X(04).

       PROCEDURE DIVISION.
           EVALUATE COM-ABEND
      *
      * ASRA is simulated by moving invalid packed data
      *   
              WHEN 'ASRA'
                   MOVE LOW-VALUES TO W-STRUCT
                   ADD 1 TO W-PACKED
              WHEN OTHER
                   EXEC CICS ABEND ABCODE(COM-ABEND) END-EXEC
           END-EVALUATE.

           EXEC CICS RETURN END-EXEC.

           GOBACK.

       END PROGRAM T1ABEND.
