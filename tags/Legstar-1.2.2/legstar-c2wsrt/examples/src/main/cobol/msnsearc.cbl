       PROCESS XOPTS(APOST)
       PROCESS NOSEQ LIB OPTIMIZE(FULL)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MSNSEARC.
      *****************************************************************
      * OVERVIEW                                                      *
      * --------                                                      *
      * Sample transaction calling a remote service using LegStar     *
      * c2wsrt C API.                                                 *
      *                                                               *
      * This programs does a search using the MSN Live search engine. *
      * This search engine is available as a SOAP Web Service at:     *
      *    http://soap.search.msn.com/webservices.asmx?wsdl           *
      * The service requires an application ID.                       *
      * When you use LegStar Schema generation using this WSDL, by    *
      * default all strings are 32 characters long. This will create  *
      * an issue with AppID which needs to be 40 characters. To fix   *
      * this you need to edit the generated XML Schema and change the *
      * COBOL annotation for AppID to specify:                        *
      * - byteLength="40"                                             *
      * - picture="X(40)"                                             *
      * Also change Query to:                                         *
      * - byteLength="128".                                           *
      * - picture="X(128)"                                            *
      * Similarly, you might want to increase the size of Description *
      * in the Result structure from 32 to 256 otherwise results will *
      * most certainly be truncated.                                  *
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
       77  C2WS-SERVICE-URI            PIC X(53) VALUE
           'http://192.168.0.2:8080/c2ws-MSNSearch/MSNSearchProxy'.
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
       77  C2WS-SERVICE-NAME           PIC X(9) VALUE
           'MSNSearch'.
           
      *---------------------------------------------------------------*
      *  Constants                                                    *
      *---------------------------------------------------------------*
       77  OK-CODE                     PIC S9(8) BINARY VALUE 0.
       77  ERROR-CODE                  PIC S9(8) BINARY VALUE -1.
       77  THIS-TRACE-ID               PIC X(13) VALUE 'MSNSEARC'.
 
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
           05 R-Search.
               10 Flags--C PIC 9(9) BINARY.
               10 SortBy--C PIC 9(9) BINARY.
               10 ResultFields--C PIC 9(9) BINARY.
               10 R-string--C PIC 9(9) BINARY.
               10 SourceRequest--C PIC 9(9) BINARY.
               10 Request.
                   15 AppID PIC X(40).
                   15 Query PIC X(128).
                   15 CultureInfo PIC X(32).
                   15 SafeSearch PIC X(32).
                   15 Flags PIC X(32) OCCURS 1 TO 10 DEPENDING ON
                       Flags--C.
                   15 Location.
                       20 Latitude COMP-2.
                       20 Longitude COMP-2.
                       20 Radius COMP-2.
                   15 Requests.
                       20 SourceRequest OCCURS 0 TO 10 DEPENDING ON
                           SourceRequest--C.
                           25 R-Source PIC X(32).
                           25 Offset PIC 9(9) COMP-5.
                           25 R-Count PIC 9(9) COMP-5.
                           25 FileType PIC X(32).
                           25 SortBy PIC X(32) OCCURS 1 TO 10 DEPENDING
                               ON SortBy--C.
                           25 ResultFields PIC X(32) OCCURS 1 TO 10
                               DEPENDING ON ResultFields--C.
                           25 SearchTagFilters.
                               30 R-string PIC X(32) OCCURS 0 TO 10
                                   DEPENDING ON R-string--C.

       
      *****************************************************************
      *            L I N K A G E       S E C T I O N                  *
      *****************************************************************
       LINKAGE SECTION.

      *---------------------------------------------------------------*
      *  Reply parameters as returned by target web service           *
      *---------------------------------------------------------------*
       01 COM-REPLY.
           05 SearchResponse.
               10 SearchTag--C PIC 9(9) BINARY.
               10 Result--C PIC 9(9) BINARY.
               10 SourceResponse--C PIC 9(9) BINARY.
               10 Response.
                   15 Responses.
                       20 SourceResponse OCCURS 0 TO 10 DEPENDING ON
                           SourceResponse--C.
                           25 R-Source PIC X(32).
                           25 Offset PIC 9(9) COMP-5.
                           25 Total PIC 9(9) COMP-5.
                           25 RecourseQuery PIC X(32).
                           25 Results.
                               30 Result OCCURS 0 TO 10 DEPENDING ON
                                   Result--C.
                                   35 R-Title PIC X(32).
                                   35 Description PIC X(256).
                                   35 Url PIC X(32).
                                   35 DisplayUrl PIC X(32).
                                   35 CacheUrl PIC X(32).
                                   35 R-Source0 PIC X(32).
                                   35 SearchTags PIC X(32).
                                   35 Phone PIC X(32).
                                   35 DateTime.
                                       40 Year PIC 9(9) COMP-5.
                                       40 Month PIC 9(9) COMP-5.
                                       40 R-Day PIC 9(9) COMP-5.
                                       40 Hour PIC 9(9) COMP-5.
                                       40 Minute PIC 9(9) COMP-5.
                                       40 Second PIC 9(9) COMP-5.
                                   35 R-Address.
                                       40 AddressLine PIC X(32).
                                       40 PrimaryCity PIC X(32).
                                       40 SecondaryCity PIC X(32).
                                       40 Subdivision PIC X(32).
                                       40 PostalCode PIC X(32).
                                       40 CountryRegion PIC X(32).
                                       40 FormattedAddress PIC X(32).
                                   35 Location.
                                       40 Latitude COMP-2.
                                       40 Longitude COMP-2.
                                       40 Radius COMP-2.
                                   35 SearchTagsArray.
                                       40 SearchTag OCCURS 0 TO 10
                                           DEPENDING ON SearchTag--C.
                                           45 Name PIC X(32).
                                           45 R-Value PIC X(32).
                                   35 Summary PIC X(32).
                                   35 ResultType PIC X(32).
                                   35 Image.
                                       40 ImageURL PIC X(32).
                                       40 ImageWidth PIC 9(9) COMP-5.
                                       40 ImageHeight PIC 9(9) COMP-5.
                                       40 ImageFileSize PIC 9(9) COMP-5.
                                       40 ThumbnailURL PIC X(32).
                                       40 ThumbnailWidth PIC 9(9) COMP-5
                                           .
                                       40 ThumbnailHeight PIC 9(9)
                                           COMP-5.
                                       40 ThumbnailFileSize PIC 9(9)
                                           COMP-5.
                                   35 Video.
                                       40 PlayUrl PIC X(32).
                                       40 SourceTitle PIC X(32).
                                       40 Format PIC X(32).
                                       40 RunTime PIC 9(9) COMP-5.
                                       40 Width PIC 9(9) COMP-5.
                                       40 Height PIC 9(9) COMP-5.
                                       40 FileSize PIC 9(9) COMP-5.
                                       40 StaticThumbnail.
                                           45 URL PIC X(32).
                                           45 Format0 PIC X(32).
                                           45 Width0 PIC 9(9) COMP-5.
                                           45 Height0 PIC 9(9) COMP-5.
                                           45 FileSize0 PIC 9(9) COMP-5.
                                       40 MotionThumbnail.
                                           45 URL0 PIC X(32).
                                           45 Format1 PIC X(32).
                                           45 RunTime0 PIC 9(9) COMP-5.
                                           45 Width1 PIC 9(9) COMP-5.
                                           45 Height1 PIC 9(9) COMP-5.
                                           45 FileSize1 PIC 9(9) COMP-5.

               
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
               'MSNSEARC STARTING ==============================='. 
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

           MOVE ZERO TO Flags--C OF COM-REQUEST.
           MOVE ZERO TO SortBy--C OF COM-REQUEST.
           MOVE ZERO TO ResultFields--C OF COM-REQUEST.
           MOVE ZERO TO R-string--C OF COM-REQUEST.
           MOVE 1 TO SourceRequest--C OF COM-REQUEST.
      *  You should specify your own Microsoft LIVE application ID
           MOVE '5588C3ACE949315B3ECAADDA908611BDF5D8D5AA'
             TO AppID OF COM-REQUEST.
           MOVE 'Mainframe' TO Query OF COM-REQUEST.
           MOVE 'en-US' TO CultureInfo OF COM-REQUEST.
           MOVE 'Moderate' to SafeSearch OF COM-REQUEST.
           MOVE ZERO TO Latitude OF COM-REQUEST.
           MOVE ZERO TO Longitude OF COM-REQUEST.
           MOVE ZERO TO Radius OF COM-REQUEST.
           MOVE 'Web' TO R-Source OF COM-REQUEST(1).
           MOVE ZERO TO Offset OF COM-REQUEST(1).
           MOVE 1 TO R-Count OF COM-REQUEST(1).
           MOVE SPACES TO FileType OF COM-REQUEST(1).
           
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
       
           STRING 'INVOKE-SERVICE success. First hit is '
                  DELIMITED BY SIZE
                  Description OF COM-REPLY(1, 1)
                  DELIMITED BY SIZE
                  INTO ERROR-MESSAGE.
           EXEC CICS SEND TEXT FROM(ERROR-MESSAGE) FREEKB END-EXEC.
           DISPLAY 'Response data length=' WS-REPLY-DATA-LEN.

           DISPLAY 'SourceResponse--C ='
                    SourceResponse--C OF COM-REPLY.
           DISPLAY 'R-Source(1)=' R-Source OF COM-REPLY(1).
           DISPLAY 'Total(1)=' Total OF COM-REPLY(1).
           DISPLAY 'R-Title(1, 1)=' R-Title OF COM-REPLY(1, 1).
           DISPLAY 'Description(1, 1)='
                    Description OF COM-REPLY(1, 1).
           
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
           
           DISPLAY 'MSNSEARC STOPPING ==============================='.
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

       END PROGRAM MSNSEARC.
