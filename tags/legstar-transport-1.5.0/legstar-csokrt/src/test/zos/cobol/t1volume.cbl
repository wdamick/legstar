       PROCESS XOPTS(SP APOST)
       PROCESS NOSEQ LIB OPTIMIZE(FULL)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. T1VOLUME.
      *****************************************************************
      * OVERVIEW                                                      *
      * --------                                                      *
      * Receives a large commarea and sends back a large commarea.    *
      *****************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER. IBM-390 WITH DEBUGGING MODE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  W-EYE-CATCHER PIC X(16) VALUE SPACES.
 
       LINKAGE SECTION.
       01 DFHCOMMAREA.
          05 COM-START-EYE-CATCHER PIC X(16).
          05 COM-BULK              PIC X(32735).
          05 COM-END-EYE-CATCHER   PIC X(16).

       PROCEDURE DIVISION.
      *
      * Check data received
      *   
           IF (COM-START-EYE-CATCHER NOT = 'PGM=IGYCRCTL,REG' OR
               COM-END-EYE-CATCHER NOT = 'PARM=(''NODYNAM,L')
               MOVE 'ERROR RECEIVING' TO COM-START-EYE-CATCHER
           ELSE
      *
      * Swap start and end eye catchers 
      *   
	           MOVE COM-END-EYE-CATCHER TO W-EYE-CATCHER
	           MOVE COM-START-EYE-CATCHER TO COM-END-EYE-CATCHER
	           MOVE W-EYE-CATCHER TO COM-START-EYE-CATCHER
           END-IF.

           EXEC CICS RETURN END-EXEC.

           GOBACK.

       END PROGRAM T1VOLUME.
