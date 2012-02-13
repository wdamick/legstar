       PROCESS XOPTS(APOST)
       PROCESS NOSEQ LIB OPTIMIZE(FULL)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MSNSEARC.
      *****************************************************************
      * OVERVIEW                                                      *
      * --------                                                      *
      * Sample transaction calling a remote service using LegStar     *
      * HTTP client C API (LSHTTAPI).                                 *
      * The HTTP body contains a formatted LegStarMessage.            *
      * It is expected that the receiver will use LegStar COBOL       *
      * binding to convert payload to a Java object.                  *
      *                                                               *
      * Program generated by LegStar Mainframe to Jaxws generator.    *
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
      *  Constants                                                    *
      *---------------------------------------------------------------*
      * Address of service provider.
      *    
       77  W00-SERVICE-URI PIC X(55) VALUE 
           'http://192.168.0.112:8080/c2ws-MSNSearch/MSNSearchProxy'.

      *    
      * Service credentials.
      *    
       77  W00-USERID                    PIC X(8) VALUE SPACES.
       77  W00-PASSWORD                  PIC X(8) VALUE SPACES.
      *    
      * Service requested.
      *    
       77  W00-SERVICE-NAME PIC X(9) VALUE 'MSNSearch'.

           
      *---------------------------------------------------------------*
      *  CICS API parameters                                          *
      *---------------------------------------------------------------*
       01  W03-RESP                      PIC S9(9) BINARY VALUE 0.
           88  OK-CODE            VALUE 0.
           88  ERROR-CODE         VALUE -1.
       01  W03-RESP2                     PIC S9(9) BINARY VALUE 0.

      *---------------------------------------------------------------*
      * LSHTTAPI API parameters                                       *
      * C Structures are aligned on natural storage boundaries so we  *
      * need to specify SYNCHRONIZED.                                 *
      * The last character of each string is reserved to hold a       *
      * C string delimiter.                                           *
      *---------------------------------------------------------------*
       01  LAPI-TRACE-PARMS SYNCHRONIZED.
           05 LAPI-TRACE-ID              PIC X(17) VALUE SPACES.
           05 LAPI-TRACE-MODE            PIC S9(8) BINARY VALUE 1.
              88 TRACES-OFF       VALUE 0.
              88 TRACES-ON        VALUE 1.
           05 LAPI-ERROR-MESSAGE         PIC X(266) VALUE SPACES.

       01  LAPI-INVOKE-PARMS SYNCHRONIZED.
           05  LAPI-URI                  PIC X(513) VALUE SPACES.
           05  LAPI-SERVICE-NAME         PIC X(33) VALUE SPACES.
           05  LAPI-REQUEST-DATA         POINTER VALUE NULL.
           05  LAPI-REQUEST-DATA-LEN     PIC S9(8) BINARY VALUE ZERO.
           05  LAPI-REPLY-DATA           POINTER VALUE NULL.
           05  LAPI-REPLY-DATA-LEN       PIC S9(8) BINARY VALUE ZERO.
           05  LAPI-OPTIONS.
               10  LAPI-CONNECT-TIMEOUT  PIC 9(9) BINARY VALUE 3.
               10  LAPI-RECV-TIMEOUT     PIC 9(9) BINARY VALUE 10.
               10  LAPI-PROXY-URI        PIC X(513) VALUE SPACES.
               10  LAPI-USERID           PIC X(33) VALUE SPACES.
               10  LAPI-PASSWORD         PIC X(33) VALUE SPACES.
           
      *---------------------------------------------------------------*
      *  Work variables                                               *
      *---------------------------------------------------------------*
       01  ERROR-MESSAGE          PIC X(78) VALUE SPACES.
           88 NO-ERROR-MESSAGE VALUE SPACES.

      *---------------------------------------------------------------*
      *  Request parameters expected by target web service            *
      *---------------------------------------------------------------*
       01 COM-REQUEST.
           02  R-Search.
             03  Flags--C PIC 9(9) BINARY.
             03  SortBy--C PIC 9(9) BINARY.
             03  ResultFields--C PIC 9(9) BINARY.
             03  R-string--C PIC 9(9) BINARY.
             03  SourceRequest--C PIC 9(9) BINARY.
             03  Request.
               04  AppID PIC X(40) DISPLAY.
               04  Query PIC X(128) DISPLAY.
               04  CultureInfo PIC X(32) DISPLAY.
               04  SafeSearch PIC X(32) DISPLAY.
               04  Flags OCCURS 1 TO 10 DEPENDING ON Flags--C PIC X(32) 
                   DISPLAY.
               04  Location.
                 05  Latitude COMP-2.
                 05  Longitude COMP-2.
                 05  Radius COMP-2.
               04  Requests.
                 05  SourceRequest OCCURS 0 TO 10 DEPENDING ON 
                     SourceRequest--C.
                   06  R-Source PIC X(32) DISPLAY.
                   06  Offset PIC 9(9) COMP-5.
                   06  R-Count PIC 9(9) COMP-5.
                   06  FileType PIC X(32) DISPLAY.
                   06  SortBy OCCURS 1 TO 10 DEPENDING ON SortBy--C PIC 
                       X(32) DISPLAY.
                   06  ResultFields OCCURS 1 TO 10 DEPENDING ON 
                       ResultFields--C PIC X(32) DISPLAY.
                   06  SearchTagFilters.
                     07  R-string OCCURS 0 TO 10 DEPENDING ON 
                         R-string--C PIC X(32) DISPLAY.

       
      *****************************************************************
      *            L I N K A G E       S E C T I O N                  *
      *****************************************************************
       LINKAGE SECTION.

      *---------------------------------------------------------------*
      *  Reply parameters as returned by target web service           *
      *---------------------------------------------------------------*
       01 COM-REPLY.
           02  SearchResponse.
             03  SearchTag--C PIC 9(9) BINARY.
             03  Result--C PIC 9(9) BINARY.
             03  SourceResponse--C PIC 9(9) BINARY.
             03  Response.
               04  Responses.
                 05  SourceResponse OCCURS 0 TO 10 DEPENDING ON 
                     SourceResponse--C.
                   06  R-Source PIC X(32) DISPLAY.
                   06  Offset PIC 9(9) COMP-5.
                   06  Total PIC 9(9) COMP-5.
                   06  RecourseQuery PIC X(32) DISPLAY.
                   06  Results.
                     07  Result OCCURS 0 TO 10 DEPENDING ON Result--C.
                       08  R-Title PIC X(32) DISPLAY.
                       08  Description PIC X(256) DISPLAY.
                       08  Url PIC X(32) DISPLAY.
                       08  DisplayUrl PIC X(32) DISPLAY.
                       08  CacheUrl PIC X(32) DISPLAY.
                       08  R-Source0 PIC X(32) DISPLAY.
                       08  SearchTags PIC X(32) DISPLAY.
                       08  Phone PIC X(32) DISPLAY.
                       08  DateTime.
                         09  Year PIC 9(9) COMP-5.
                         09  Month PIC 9(9) COMP-5.
                         09  R-Day PIC 9(9) COMP-5.
                         09  Hour PIC 9(9) COMP-5.
                         09  Minute PIC 9(9) COMP-5.
                         09  Second PIC 9(9) COMP-5.
                       08  R-Address.
                         09  AddressLine PIC X(32) DISPLAY.
                         09  PrimaryCity PIC X(32) DISPLAY.
                         09  SecondaryCity PIC X(32) DISPLAY.
                         09  Subdivision PIC X(32) DISPLAY.
                         09  PostalCode PIC X(32) DISPLAY.
                         09  CountryRegion PIC X(32) DISPLAY.
                         09  FormattedAddress PIC X(32) DISPLAY.
                       08  Location.
                         09  Latitude COMP-2.
                         09  Longitude COMP-2.
                         09  Radius COMP-2.
                       08  SearchTagsArray.
                         09  SearchTag OCCURS 0 TO 10 DEPENDING ON 
                             SearchTag--C.
                           10  Name PIC X(32) DISPLAY.
                           10  R-Value PIC X(32) DISPLAY.
                         08  Summary PIC X(32) DISPLAY.
                         08  ResultType PIC X(32) DISPLAY.
                         08  Image.
                           09  ImageURL PIC X(32) DISPLAY.
                           09  ImageWidth PIC 9(9) COMP-5.
                           09  ImageHeight PIC 9(9) COMP-5.
                           09  ImageFileSize PIC 9(9) COMP-5.
                           09  ThumbnailURL PIC X(32) DISPLAY.
                           09  ThumbnailWidth PIC 9(9) COMP-5.
                           09  ThumbnailHeight PIC 9(9) COMP-5.
                           09  ThumbnailFileSize PIC 9(9) COMP-5.
                         08  Video.
                           09  PlayUrl PIC X(32) DISPLAY.
                           09  SourceTitle PIC X(32) DISPLAY.
                           09  Format PIC X(32) DISPLAY.
                           09  RunTime PIC 9(9) COMP-5.
                           09  Width PIC 9(9) COMP-5.
                           09  Height PIC 9(9) COMP-5.
                           09  FileSize PIC 9(9) COMP-5.
                           09  StaticThumbnail.
                             10  URL PIC X(32) DISPLAY.
                             10  Format0 PIC X(32) DISPLAY.
                             10  Width0 PIC 9(9) COMP-5.
                             10  Height0 PIC 9(9) COMP-5.
                             10  FileSize0 PIC 9(9) COMP-5.
                           09  MotionThumbnail.
                             10  URL0 PIC X(32) DISPLAY.
                             10  Format1 PIC X(32) DISPLAY.
                             10  RunTime0 PIC 9(9) COMP-5.
                             10  Width1 PIC 9(9) COMP-5.
                             10  Height1 PIC 9(9) COMP-5.
                             10  FileSize1 PIC 9(9) COMP-5.

               
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
           MOVE 'MSNSEARC' TO LAPI-TRACE-ID.
           
           CALL 'init' USING dfheiblk LAPI-TRACE-PARMS
                       RETURNING W03-RESP.
           IF (NOT OK-CODE)
               MOVE 'INITIALIZE-LSHTTAPI failed' TO ERROR-MESSAGE
               PERFORM ABORT-PROGRAM THRU
                   END-ABORT-PROGRAM
           END-IF.

      *
      * Setup invoke parameters
      *    
           MOVE W00-SERVICE-URI   TO LAPI-URI.
           MOVE W00-USERID        TO LAPI-USERID.
           MOVE W00-PASSWORD      TO LAPI-PASSWORD.
           MOVE W00-SERVICE-NAME  TO LAPI-SERVICE-NAME.

           PERFORM SET-REQUEST THRU
               END-SET-REQUEST.

           SET LAPI-REQUEST-DATA     TO ADDRESS OF COM-REQUEST.
           MOVE LENGTH OF COM-REQUEST TO LAPI-REQUEST-DATA-LEN.
           
           DISPLAY 'PROLOG ENDED'.
           
       END-PROLOG.   EXIT.
      
      *---------------------------------------------------------------*
      *  Populate the request parameters                              *
      *---------------------------------------------------------------*
       SET-REQUEST.

           DISPLAY 'SET-REQUEST STARTED'.

      *  TODO set input values in COM-REQUEST                         *
           
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
           CALL 'invoke' USING LAPI-INVOKE-PARMS
                         RETURNING W03-RESP.
           IF (NOT OK-CODE)
               MOVE LAPI-ERROR-MESSAGE TO ERROR-MESSAGE
               PERFORM ABORT-PROGRAM THRU
                   END-ABORT-PROGRAM
           END-IF.
           
           SET ADDRESS OF COM-REPLY TO LAPI-REPLY-DATA.

           PERFORM PRINT-RESULTS THRU
               END-PRINT-RESULTS.

           DISPLAY 'INVOKE-SERVICE SUCCESS'.
           
       END-INVOKE-SERVICE.   EXIT.
      
      *---------------------------------------------------------------*
      *  Display results returned from target web service             *
      *---------------------------------------------------------------*
       PRINT-RESULTS.
       
      *  TODO do something useful with data returned in  COM-REPLY    *
           
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
           DISPLAY '* COMPLETION CODE : ', W03-RESP.
           DISPLAY '* REASON CODE     : ', W03-RESP2.
           DISPLAY '************************************************'.

       END-DISPLAY-ERROR-MESSAGE.   EXIT.

       END PROGRAM MSNSEARC.
