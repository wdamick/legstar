       PROCESS XOPTS(APOST)
       PROCESS NOSEQ LIB OPTIMIZE(FULL)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EZACICSE.
      *****************************************************************
      * OVERVIEW                                                      *
      * --------                                                      *
      * SAMPLE CICS SOCKETS SECURITY EXIT                             *
      *****************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
      *****************************************************************
      *        W O R K I N G    S T O R A G E    S E C T I O N        *
      *****************************************************************
       WORKING-STORAGE SECTION.
       
       01  W-RESP                 PIC S9(8) COMP VALUE ZERO.
       01  W-RESP2                PIC S9(8) COMP VALUE ZERO.
       
      *
      * Error messages
      *
      *              1         2         3         4         5         6
      *     123456789012345678901234567890123456789012345678901234567890
      *
       01  W-ERROR-MESSAGE        PIC X(132) VALUE SPACES.
       01  W-ERROR-INVREQ         PIC X(40) VALUE
           'SEE01600 CICS returned INVREQ on VERIFY.'.
       01  W-ERROR-INVREQ-13      PIC X(87) VALUE
           'SEE01613 There is an unknown return code in ESMRESP from the
      -    ' external security manager.'.
       01  W-ERROR-INVREQ-18      PIC X(73) VALUE
           'SEE01618 The CICS external security manager interface is not 
      -    'initialized.'.
       01  W-ERROR-INVREQ-29      PIC X(57) VALUE
           'SEE01629 The external security manager is not responding.'.
       01  W-ERROR-INVREQ-32      PIC X(76) VALUE
           'SEE01632 The userid field contains a blank character in an i
      -    'nvalid position.'.
       01  W-ERROR-NOTAUTH        PIC X(41) VALUE
           'SEE07000 CICS returned NOTAUTH on VERIFY.'.
       01  W-ERROR-NOTAUTH-02     PIC X(115) VALUE
           'SEE07002 The supplied password is wrong. If the external sec
      -    'urity manager is RACF, the revoke count is incremented.'.
       01  W-ERROR-NOTAUTH-03     PIC X(36) VALUE
           'SEE07003 A new password is required.'.
       01  W-ERROR-NOTAUTH-19     PIC X(31) VALUE
           'SEE07019 The USERID is revoked.'.
       01  W-ERROR-USERIDERR      PIC X(43) VALUE
           'SEE06900 CICS returned USERIDERR on VERIFY.'.
       01  W-ERROR-USERIDERR-08   PIC X(66) VALUE
           'SEE06908 The USERID is not known to the external security ma
      -    'nager.'.
       01  W-ERROR-VERIFYERR      PIC X(51) VALUE
           'SEE06901 CICS VERIFY returned an unknown resp code.'.

      *
      * Socket API interface
      *
       01 SOK-SEND                PIC X(16) VALUE 'SEND'.
       01 SOK-FLAGS               PIC 9(8) BINARY VALUE 0.
       01 SOK-LENGTH              PIC 9(8) BINARY VALUE 0.
       01 SOK-ERRNO               PIC 9(8) BINARY VALUE 0.
       01 SOK-RETCODE             PIC S9(8) BINARY VALUE 0.
       
      *****************************************************************
      *            L I N K A G E       S E C T I O N                  *
      *****************************************************************
       LINKAGE SECTION.
       01 DFHCOMMAREA.
          05 SOK-TRANS-ID                PIC X(4).
          05 SOK-DATA-AREA.
             10 LEG-REQUEST-USERID       PIC X(8).
             10 LEG-REQUEST-PASSWORD     PIC X(8).
             10 FILLER                   PIC X(19).
          05 SOK-DATA-LEVEL              PIC X(1).
             88 REGULAR-FORMAT               VALUE '0'.
             88 EXPANDED-FORMAT              VALUE '1'.
          05 FILLER                      PIC X(4).
          05 SOK-ACTION                  PIC X(2).
             88 INTERVAL-CONTROL             VALUE 'IC'.
             88 TASKL-CONTROL                VALUE 'KC'.
             88 TRANSIENT-DATA               VALUE 'TD'.
          05 SOK-IC-TIME                 PIC X(6).
          05 SOK-ADDRESS-FAMILY          PIC 9(4) COMP-5.
          05 SOK-CLIENT-PORT             PIC 9(4) COMP-5.
          05 SOK-CLIENT-IPV4-ADDRESS     PIC 9(8) COMP-5.
          05 SOK-REPLY-SWITCH            PIC X(1).
             88 TRANSACTION-REJECTED         VALUE '0'.
             88 TRANSACTION-ACCEPTED         VALUE '1'.
          05 SOK-REPLY-SWITCH-2          PIC X(1).
             88 SECEXIT-REPLIES              VALUE '0'.
             88 LISTENER-REPLIES             VALUE '1'.
          05 SOK-REPLY-TERMID            PIC X(4).
          05 SOK-SOCKET                  PIC 9(4) COMP-5.
          05 SOK-REPLY-USERID            PIC X(8).
          05 SOK-SERVER-IPV4-ADDRESS     PIC 9(8) COMP-5.
          05 SOK-SERVER-PORT             PIC 9(4) COMP-5.
          05 SOK-SERVER-IPV6-ADDRESS     PIC X(16).
          05 SOK-SERVER-IPV6-SCOPEID     PIC 9(8) COMP-5.
          05 SOK-CLIENT-IPV6-ADDRESS     PIC X(16).
          05 SOK-CLIENT-IPV6-SCOPEID     PIC 9(8) COMP-5.
          05 FILLER              PIC X(40).
          05 SOK-DATA-LEN     PIC 9(4) COMP-5.
          05 SOK-DATA-AREA-2 OCCURS 1 TO 4096
             DEPENDING ON SOK-DATA-LEN   PIC X.
          
      *****************************************************************
      *    P R O C E D U R E  D I V I S I O N   S E C T I O N         *
      *****************************************************************
       PROCEDURE DIVISION.

           SET TRANSACTION-ACCEPTED TO TRUE.
           SET LISTENER-REPLIES TO TRUE.
           
           PERFORM CHECK-CREDENTIALS THRU
               END-CHECK-CREDENTIALS.
           
           EXEC CICS RETURN END-EXEC.

           GOBACK.

      *---------------------------------------------------------------*
      * VERIFY THE USER/PASSWORD PASSED IN THE DATA AREA              *
      *---------------------------------------------------------------*
       CHECK-CREDENTIALS.
       
           EXEC CICS VERIFY
               PASSWORD   (LEG-REQUEST-PASSWORD)
               USERID     (LEG-REQUEST-USERID)
               RESP       (W-RESP)
               RESP2      (W-RESP2)
           END-EXEC.
           
           IF (W-RESP NOT = DFHRESP(NORMAL))
               PERFORM FORMAT-ERROR-REPLY THRU
                   END-FORMAT-ERROR-REPLY
      *
      * Notify the client that something went wrong
      *
             PERFORM SEND-ERROR-REPLY THRU
                 END-SEND-ERROR-REPLY
             SET TRANSACTION-REJECTED TO TRUE 
             SET SECEXIT-REPLIES TO TRUE
           ELSE
      *
      * Impersonate the USERID that was verified
      *
             MOVE LEG-REQUEST-USERID TO SOK-REPLY-USERID
           END-IF.
           
       END-CHECK-CREDENTIALS. EXIT.

      *---------------------------------------------------------------*
      * PREPARE A MEANINGFUL ERROR MESSAGE                            *
      *---------------------------------------------------------------*
       FORMAT-ERROR-REPLY.
       
           EVALUATE W-RESP
           
               WHEN DFHRESP(INVREQ)
                    EVALUATE W-RESP2
                        WHEN 13
                             MOVE W-ERROR-INVREQ-13 TO W-ERROR-MESSAGE
                        WHEN 18
                             MOVE W-ERROR-INVREQ-18 TO W-ERROR-MESSAGE
                        WHEN 29
                             MOVE W-ERROR-INVREQ-29 TO W-ERROR-MESSAGE
                        WHEN 32
                             MOVE W-ERROR-INVREQ-32 TO W-ERROR-MESSAGE
                        WHEN OTHER
                             MOVE W-ERROR-INVREQ TO W-ERROR-MESSAGE
                    END-EVALUATE
                    
               WHEN DFHRESP(NOTAUTH)
                    EVALUATE W-RESP2
                        WHEN 2
                             MOVE W-ERROR-NOTAUTH-02 TO W-ERROR-MESSAGE
                        WHEN 3
                             MOVE W-ERROR-NOTAUTH-03 TO W-ERROR-MESSAGE
                        WHEN 19
                             MOVE W-ERROR-NOTAUTH-19 TO W-ERROR-MESSAGE
                        WHEN OTHER
                             MOVE W-ERROR-NOTAUTH TO W-ERROR-MESSAGE
                    END-EVALUATE
                    
               WHEN DFHRESP(USERIDERR)
                    EVALUATE W-RESP2
                        WHEN 8
                             MOVE W-ERROR-USERIDERR-08
                               TO W-ERROR-MESSAGE
                        WHEN OTHER
                             MOVE W-ERROR-USERIDERR TO W-ERROR-MESSAGE
                    END-EVALUATE
                    
                WHEN OTHER
                    MOVE W-ERROR-VERIFYERR TO W-ERROR-MESSAGE
                    
           END-EVALUATE.
           
        END-FORMAT-ERROR-REPLY. EXIT.
           
      *---------------------------------------------------------------*
      * SEND ERROR NOTIFICATION BACK TO CLIENT                        *
      *---------------------------------------------------------------*
       SEND-ERROR-REPLY.
       
            COMPUTE SOK-LENGTH = LENGTH OF W-ERROR-MESSAGE.
       
            CALL 'EZASOKET' USING SOK-SEND SOK-SOCKET SOK-FLAGS 
                            SOK-LENGTH W-ERROR-MESSAGE
                            SOK-ERRNO SOK-RETCODE.
            IF (SOK-ERRNO NOT = 0)
                DISPLAY 'EZACICSE SOCKET ERROR NO=' SOK-ERRNO
                        ' RESP CODE=' SOK-RETCODE
            END-IF.
            
       END-SEND-ERROR-REPLY. EXIT.

       END PROGRAM EZACICSE.
