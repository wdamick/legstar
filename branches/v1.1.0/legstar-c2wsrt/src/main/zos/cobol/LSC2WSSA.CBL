       PROCESS XOPTS(APOST)
       PROCESS NOSEQ LIB OPTIMIZE(FULL)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LSC2WSSA.
      *****************************************************************
      * OVERVIEW                                                      *
      * --------                                                      *
      * Sample transaction calling a remote Web service using legstar *
      * c2wsrt C API.                                                 *
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
      * Address of c2ws gateway.
      *    
       77  C2WS-GATEWAY-URI            PIC X(50) VALUE
           'http://192.168.0.5:8080/legstar-c2wsrt/C2wsGateway'.
      *    
      * C2ws gateway credentials.
      *    
       77  C2WS-USERID                 PIC X(5) VALUE 'alice'.
       77  C2WS-PASSWORD               PIC X(12) VALUE 'inwonderland'.
      *    
      * Service requested.
      *    
       77  C2WS-SERVICE-NAME           PIC X(11) VALUE 'CultureInfo'.
           
      *---------------------------------------------------------------*
      *  Constants                                                    *
      *---------------------------------------------------------------*
       77  OK-CODE                     PIC S9(8) BINARY VALUE 0.
       77  ERROR-CODE                  PIC S9(8) BINARY VALUE -1.
       77  THIS-TRACE-ID               PIC X(13) VALUE 'LSC2WSSA'.
 
      *---------------------------------------------------------------*
      * Structure shared with c2ws API                                *
      * C Structures are aligned on natural storage boundaries so we  *
      * specify SYNCHRONIZED.                                         *
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
       01  WS-RDISP                    PIC +9(8) VALUE ZERO.

      *---------------------------------------------------------------*
      *  Request parameters expected by target web service            *
      *---------------------------------------------------------------*
       01 COM-REQUEST.
           02 cultureCode PIC X(32).
           02 decimalNumber PIC 9(7)V9(2) COMP-3.
       
      *****************************************************************
      *            L I N K A G E       S E C T I O N                  *
      *****************************************************************
       LINKAGE SECTION.

      *---------------------------------------------------------------*
      *  Reply parameters as returned by target web service           *
      *---------------------------------------------------------------*
       01 COM-REPLY.
           02 currencySymbol PIC X(32).
           02 displayCountry PIC X(32).
           02 displayLanguage PIC X(32).
           02 formattedDate PIC X(32).
           02 formattedDecimalNumber PIC X(32).
           02 serverCultureInfo.
               03 cultureCode PIC X(32).
               03 displayCountry0 PIC X(32).
               03 displayLanguage0 PIC X(32).
               
      *****************************************************************
      *    P R O C E D U R E  D I V I S I O N   S E C T I O N         *
      *****************************************************************
       PROCEDURE DIVISION.

           IF TRACES-ON
               DISPLAY
                   'LSC2WSSA STARTING ===============================' 
           END-IF.
           
           PERFORM INITIALIZE-C2WS-API THRU
               END-INITIALIZE-C2WS-API.  

           PERFORM SET-REQUEST THRU
               END-SET-REQUEST.

           PERFORM INVOKE-SERVICE THRU
               END-INVOKE-SERVICE.
               
           IF TRACES-ON
	           PERFORM PRINT-RESULTS THRU
	               END-PRINT-RESULTS 
           END-IF.
               
           IF TRACES-ON
               DISPLAY
                   'LSC2WSSA STOPPING ===============================' 
           END-IF.
           EXEC CICS RETURN END-EXEC.

           GOBACK.
       
      *---------------------------------------------------------------*
      *  Initialize the c2ws API. You can turn traces on and specify  *
      *  a trace identifier.                                          *
      *---------------------------------------------------------------*
       INITIALIZE-C2WS-API.
       
           MOVE THIS-TRACE-ID TO TRACE-ID.
           
           CALL 'init' USING dfheiblk TRACE-PARMS
                       RETURNING WS-RESP.
           IF (WS-RESP NOT = OK-CODE)
               DISPLAY 'INITIALIZE-C2WS-API failed'
               EXEC CICS RETURN END-EXEC
           END-IF.
           
       END-INITIALIZE-C2WS-API.   EXIT.
      
      *---------------------------------------------------------------*
      *  Populate the request parameters                              *
      *---------------------------------------------------------------*
       SET-REQUEST.
       
           IF TRACES-ON
	           DISPLAY 'START SET-REQUEST' 
           END-IF.
           
           MOVE 'fr-FR' TO cultureCode OF COM-REQUEST.
           MOVE 20569.25 TO decimalNumber OF COM-REQUEST.
           
           IF TRACES-ON
	           DISPLAY 'SET-REQUEST ENDED' 
           END-IF.
           
       END-SET-REQUEST.   EXIT.
       
      *---------------------------------------------------------------*
      *  Invoke target web service                                    *
      *---------------------------------------------------------------*
       INVOKE-SERVICE.
           IF TRACES-ON
	           DISPLAY 'ABOUT TO RUN INVOKE-SERVICE' 
           END-IF.
      *
      * Prepare invoke parameter set
      *    
           MOVE C2WS-GATEWAY-URI   TO WS-URI.
           MOVE C2WS-SERVICE-NAME  TO WS-SERVICE-NAME.
           SET WS-REQUEST-DATA     TO ADDRESS OF COM-REQUEST.
           MOVE LENGTH OF COM-REQUEST TO WS-REQUEST-DATA-LEN.
           MOVE C2WS-USERID        TO WS-USERID.
           MOVE C2WS-PASSWORD      TO WS-PASSWORD.
      *
      * Invoke target web service
      *    
           CALL 'invoke' USING WS-INVOKE-PARMS
                         RETURNING WS-RESP.
           IF (WS-RESP NOT = OK-CODE)
               COMPUTE WS-RDISP = WS-RESP
               DISPLAY 'INVOKE-SERVICE failed. Return code=' WS-RDISP
               EXEC CICS RETURN END-EXEC
           END-IF.
           
           SET ADDRESS OF COM-REPLY TO WS-REPLY-DATA.

           IF TRACES-ON
	           DISPLAY 'INVOKE-SERVICE SUCCESS' 
           END-IF.
           
       END-INVOKE-SERVICE.   EXIT.
      
      *---------------------------------------------------------------*
      *  Display results returned from target web service             *
      *---------------------------------------------------------------*
       PRINT-RESULTS.
       
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
       
       END PROGRAM LSC2WSSA.
