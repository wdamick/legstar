       PROCESS XOPTS(APOST)
       PROCESS NOSEQ LIB OPTIMIZE(FULL)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LSC2WSSA.
      *****************************************************************
      * OVERVIEW                                                      *
      * --------                                                      *
      * Sample transaction calling a remote service using LegStar     *
      * c2wsrt C API.                                                 *
      * The HTTP body contains a formatted LegStarMessage.            *
      * It is expected that the receiver will use LegStar COBOL       *
      * binding to convert payload to a Java object.                  *
      *                                                               *
      * Program generated by LegStar Mainframe to Jaxws generator.    *
      * Generated on 2008-11-08 09:04:17                              *
      * Follow the TODO markers to customize this program.            *
      *****************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
      *****************************************************************
      *        W O R K I N G    S T O R A G E    S E C T I O N        *
      *****************************************************************
       WORKING-STORAGE SECTION.
       
      *---------------------------------------------------------------*
      *  C2WS API parameters                                          *
      *---------------------------------------------------------------*
      * Address of c2ws service provider.
      *    
       77  C2WS-SERVICE-URI            PIC X(57) VALUE
           'http://192.168.0.2:8080/c2ws-cultureinfo/cultureinfoProxy'.
      *    
      * C2ws service credentials.
      *    
       77  C2WS-USERID                 PIC X(8) VALUE
           '        '.
       77  C2WS-PASSWORD               PIC X(8) VALUE
           '        '.
      *    
      * Service requested.
      *    
       77  C2WS-SERVICE-NAME           PIC X(11) VALUE
           'cultureinfo'.
           
      *---------------------------------------------------------------*
      *  Constants                                                    *
      *---------------------------------------------------------------*
       77  OK-CODE                     PIC S9(8) BINARY VALUE 0.
       77  ERROR-CODE                  PIC S9(8) BINARY VALUE -1.
       77  THIS-TRACE-ID               PIC X(13) VALUE 'LSC2WSSA'.
 
      *---------------------------------------------------------------*
      * Structure shared with c2ws C API.                             *
      * C Structures are aligned on natural storage boundaries so we  *
      * need to specify SYNCHRONIZED.                                 *
      * The last character of each string is reserved to hold a       *
      * C string delimiter.                                           *
      *---------------------------------------------------------------*
       01  TRACE-PARMS SYNCHRONIZED.
           05 TRACE-ID                 PIC X(17) VALUE SPACES.
           05 TRACE-MODE               PIC S9(8) BINARY VALUE 1.
              88 TRACES-OFF       VALUE 0.
              88 TRACES-ON        VALUE 1.
           05 ERROR-MESSAGE            PIC X(266) VALUE SPACES.

       01  WS-INVOKE-PARMS SYNCHRONIZED.
           05  WS-URI                  PIC X(513) VALUE SPACES.
           05  WS-SERVICE-NAME         PIC X(33) VALUE SPACES.
           05  WS-REQUEST-DATA         POINTER VALUE NULL.
           05  WS-REQUEST-DATA-LEN     PIC S9(8) BINARY VALUE ZERO.
           05  WS-REPLY-DATA           POINTER VALUE NULL.
           05  WS-REPLY-DATA-LEN       PIC S9(8) BINARY VALUE ZERO.
           05  WS-OPTIONS.
               10  WS-CONNECT-TIMEOUT  PIC 9(9) BINARY VALUE 3.
               10  WS-RECV-TIMEOUT     PIC 9(9) BINARY VALUE 10.
               10  WS-PROXY-URI        PIC X(513) VALUE SPACES.
               10  WS-USERID           PIC X(33) VALUE SPACES.
               10  WS-PASSWORD         PIC X(33) VALUE SPACES.
           
      *---------------------------------------------------------------*
      *  Work variables                                               *
      *---------------------------------------------------------------*
       01  WS-RESP                     PIC S9(8) COMP VALUE ZERO.
       01  WS-RESP2                    PIC S9(8) COMP VALUE ZERO.

      *---------------------------------------------------------------*
      *  Request parameters expected by target web service            *
      *---------------------------------------------------------------*
       01 COM-REQUEST.
           05 GetInfo.
               10 arg0.
                   15 cultureCode PIC X(32).
                   15 decimalNumber PIC 9(7)V9(2) COMP-3.

       
      *****************************************************************
      *            L I N K A G E       S E C T I O N                  *
      *****************************************************************
       LINKAGE SECTION.

      *---------------------------------------------------------------*
      *  Reply parameters as returned by target web service           *
      *---------------------------------------------------------------*
       01 COM-REPLY.
           05 GetInfoResponse.
               10 R-return.
                   15 currencySymbol PIC X(32).
                   15 displayCountry PIC X(32).
                   15 displayLanguage PIC X(32).
                   15 formattedDate PIC X(32).
                   15 formattedDecimalNumber PIC X(32).
                   15 serverCultureInfo.
                       20 cultureCode PIC X(32).
                       20 displayCountry0 PIC X(32).
                       20 displayLanguage0 PIC X(32).

               
      *****************************************************************
      *    P R O C E D U R E  D I V I S I O N   S E C T I O N         *
      *****************************************************************
       PROCEDURE DIVISION.

           PERFORM PROLOG THRU
               END-PROLOG.

           PERFORM INVOKE-SERVICE THRU
               END-INVOKE-SERVICE.
               
           PERFORM EPILOG THRU
               END-EPILOG.

           GOBACK.
       
      *---------------------------------------------------------------*
      *  Initialize the c2ws API. You can turn traces on and specify  *
      *  a trace identifier.                                          *
      *---------------------------------------------------------------*
       PROLOG.

           DISPLAY
               'LSC2WSSA STARTING ==============================='. 
      *
      * Initialize c2ws API passing trace parameters
      *    
           MOVE THIS-TRACE-ID      TO TRACE-ID.
           
           CALL 'init' USING dfheiblk TRACE-PARMS
                       RETURNING WS-RESP.
           IF (WS-RESP NOT = OK-CODE)
               MOVE 'INITIALIZE-C2WS-API failed' TO ERROR-MESSAGE
               PERFORM ABORT-PROGRAM THRU
                   END-ABORT-PROGRAM
           END-IF.

      *
      * Setup invoke parameters
      *    
           MOVE C2WS-SERVICE-URI   TO WS-URI.
           MOVE C2WS-USERID        TO WS-USERID.
           MOVE C2WS-PASSWORD      TO WS-PASSWORD.
           MOVE C2WS-SERVICE-NAME  TO WS-SERVICE-NAME.

           PERFORM SET-REQUEST THRU
               END-SET-REQUEST.

           SET WS-REQUEST-DATA     TO ADDRESS OF COM-REQUEST.
           MOVE LENGTH OF COM-REQUEST TO WS-REQUEST-DATA-LEN.
           
           DISPLAY 'PROLOG ENDED'.
           
       END-PROLOG.   EXIT.
      
      *---------------------------------------------------------------*
      *  Populate the request parameters                              *
      *---------------------------------------------------------------*
       SET-REQUEST.

           DISPLAY 'SET-REQUEST STARTED'.

           MOVE 'fr-FR' TO cultureCode OF COM-REQUEST.
           MOVE 20569.25 TO decimalNumber OF COM-REQUEST.
           
           DISPLAY 'SET-REQUEST ENDED'.

       END-SET-REQUEST.   EXIT.
       
      *---------------------------------------------------------------*
      *  Invoke target service and analyze response                   *
      *---------------------------------------------------------------*
       INVOKE-SERVICE.

           DISPLAY 'ABOUT TO INVOKE-SERVICE'.
      *
      * Invoke target web service
      *    
           CALL 'invoke' USING WS-INVOKE-PARMS
                         RETURNING WS-RESP.
           IF (WS-RESP NOT = OK-CODE)
               PERFORM ABORT-PROGRAM THRU
                   END-ABORT-PROGRAM
           END-IF.
           
           SET ADDRESS OF COM-REPLY TO WS-REPLY-DATA.

           PERFORM PRINT-RESULTS THRU
               END-PRINT-RESULTS.

           DISPLAY 'INVOKE-SERVICE SUCCESS'.
           
       END-INVOKE-SERVICE.   EXIT.
      
      *---------------------------------------------------------------*
      *  Display results returned from target web service             *
      *---------------------------------------------------------------*
       PRINT-RESULTS.
       
           STRING 'INVOKE-SERVICE success. Server language is '
                  DELIMITED BY SIZE
                  displayLanguage
                  DELIMITED BY SPACE
                  INTO ERROR-MESSAGE.
           EXEC CICS SEND TEXT FROM(ERROR-MESSAGE) FREEKB END-EXEC.
       
           DISPLAY 'Response data length=' WS-REPLY-DATA-LEN.
           
           DISPLAY 'currencySymbol=' currencySymbol OF COM-REPLY.
           DISPLAY 'displayCountry=' displayCountry OF COM-REPLY.
           DISPLAY 'displayLanguage=' displayLanguage OF COM-REPLY.
           DISPLAY 'formattedDate=' formattedDate OF COM-REPLY.
           DISPLAY 'formattedDecimalNumber='
                               formattedDecimalNumber OF COM-REPLY.
           DISPLAY 'cultureCode=' cultureCode OF COM-REPLY.
           DISPLAY 'displayCountry0=' displayCountry0 OF COM-REPLY.
           DISPLAY 'displayLanguage0=' displayLanguage0 OF COM-REPLY.
           
       END-PRINT-RESULTS.   EXIT.
       
      *---------------------------------------------------------------*
      *  Terminate program.                                           *
      *---------------------------------------------------------------*
       EPILOG.

           PERFORM EXIT-PROGRAM THRU
               END-EXIT-PROGRAM.
           
       END-EPILOG.   EXIT.

      *---------------------------------------------------------------*
      *  Free keyboard and return to CICS                             *
      *---------------------------------------------------------------*
       EXIT-PROGRAM.
       
           EXEC CICS SEND CONTROL FREEKB END-EXEC.
           
           DISPLAY 'LSC2WSSA STOPPING ==============================='.
           EXEC CICS RETURN END-EXEC.

       END-EXIT-PROGRAM.   EXIT.

      *---------------------------------------------------------------*
      *  Something went wrong. Report error and exit.                 *
      *---------------------------------------------------------------*
       ABORT-PROGRAM.
           
           PERFORM DISPLAY-ERROR-MESSAGE THRU
               END-DISPLAY-ERROR-MESSAGE.
               
           PERFORM EXIT-PROGRAM THRU
               END-EXIT-PROGRAM.

       END-ABORT-PROGRAM.   EXIT.

      *---------------------------------------------------------------*
      *  Display error messages                                       *
      *---------------------------------------------------------------*
       DISPLAY-ERROR-MESSAGE.

           EXEC CICS SEND TEXT FROM(ERROR-MESSAGE) FREEKB END-EXEC. 
           DISPLAY '************************************************'.
           DISPLAY '* ', ERROR-MESSAGE.
           DISPLAY '* COMPLETION CODE : ', WS-RESP.
           DISPLAY '* REASON CODE     : ', WS-RESP2.
           DISPLAY '************************************************'.

       END-DISPLAY-ERROR-MESSAGE.   EXIT.

       END PROGRAM LSC2WSSA.