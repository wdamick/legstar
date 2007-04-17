#pragma XOPTS(CICS)
#pragma longname   
/**********************************************************************/
/*                                                                    */
/*           Copyright (C) 2007 by Fady Moussallam.                   */
/*                                                                    */
/* This program is free software; you can redistribute it and/or      */
/* modify it under the terms of the GNU Lesser General Public License */
/* as published by the Free Software Foundation; either version 2.1   */
/* of the License, or (at your option) any later version.             */
/*                                                                    */
/* This library is distributed in the hope that it will be useful,    */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of     */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU   */
/* Lesser General Public License for more details.                    */
/*                                                                    */
/* You should have received a copy of the GNU Lesser General Public   */
/* License along with this library; if not, write to the Free         */
/* Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  */
/* 02111-1307 USA                                                     */
/*                                                                    */
/*   Author     - Fady Moussallam  (fady@legsem.com)                  */
/*   Purpose    - CICS Web aware program for simple binary DPL link   */
/*   Language   - IBM C/370                                           */
/*   System     - Tested on CICS TS 2.3                               */
/*   History    - 04 May 2006  - Original Implementation              */
/*   Notes      - This program provides HTTP clients with the         */
/*                capability to invoke a CICS program passing         */
/*                commarea data as a binary Http body.                */
/*                                                                    */
/*                Client has the responsibility to create the request */
/*                in the format expected by the linked program.       */
/*                This means that characters must be EBCDIC encoded   */
/*                and numeric data must be encoded in characters,     */
/*                binary, or packed-decimal.                          */
/*                                                                    */
/*                The protocol is implemented as a set of HTTP        */
/*                header extensions:                                  */
/*                                                                    */
/*                Request:                                            */
/*                  CICSProgram      : CICS program name to link to   */
/*                  CICSLength       : Total size of the commarea     */
/*                  CICSDataLength   : Size of the input data         */
/*                  CICSSysID        : Remote CICS system             */
/*                  CICSSyncOnReturn : Commit on each invoke          */
/*                  CICSTransID      : Remote mirror transaction ID   */
/*                                                                    */
/*                Reply:                                              */
/*                  CICSError        : Error text if something wrong  */
/*                                                                    */
/*                Traces can be turned on by passing the "trace"      */
/*                keyword in the URL query string                     */
/*                                                                    */
/**********************************************************************/
/*--------------------------------------------------------------------*/
/*  Constants                                                         */
/*--------------------------------------------------------------------*/
#define MAX_METHOD_LEN 25        /* HTTP method maximum length        */
#define MAX_VERSION_LEN 25       /* HTTP version maximum length       */
#define MAX_PATH_LEN 257         /* HTTP path maximum length          */
#define MAX_QUERY_STRING_LEN 257 /* HTTP query string maximum length  */
#define MAX_HEADER_LABEL_LEN 64  /* HTTP header name maximum length   */
#define MAX_HEADER_VALUE_LEN 128 /* HTTP header value maximum length  */
#define MAX_CLIENT_ADDR 257      /* TCPIP client address max length   */
#define MSG_DOCUMENT \                                      
   "<!doctype html public '-//W3C//DTD HTML 3.2//EN'> \  
   <html><head> \
   <title>LSWEBBIN - Http binary link interface - </title>\
   </head><body> \
   <h1>Welcome from LSWEBBIN</h1> \                     
   <p>Try to POST binary content. \
   <p>You can specify the following HTTP headers: \
   <p>CICSProgram      : CICS program name to link to \
   <br>CICSLength       : Total size of the commarea \
   <br>CICSDataLength   : Size of the input data \
   <br>CICSSysID        : Remote CICS system \
   <br>CICSSyncOnReturn : Commit on each invoke \
   <br>CICSTransID      : Remote mirror transaction ID \
   </body></html>"

#define SEVERE_CODE 8            /* Serious error occurred            */
#define BROWSER_CLIENT 4         /* Recoverable error occurred        */
#define OK_CODE 0                /* No errors or warnings             */
#define TRUE_CODE 1              /* Boolean true value                */
#define FALSE_CODE 0             /* Boolean false value               */
#define MAX_TRACES_BYTES 500     /* Flood prevention for large data   */

#define CONTENT_LENGTH_LABEL "Content-Length" /* Definition of the
                                    HTTP content length header        */
#define CONTENT_TYPE_LABEL "Content-Type" /* Definition of the
                                    HTTP content type header          */
#define CONTENT_TYPE_BINARY_LABEL "binary" /* Part of binary content
                                    types. ex: binary/octet-stream    */
#define CICS_PROGRAM "CICSProgram" /* Program to invoke               */
#define CICS_LENGTH "CICSLength"  /* Size of the commarea             */
#define CICS_DATALENGTH "CICSDataLength"  /* Size of the input data   */
#define CICS_SYSID "CICSSysID"    /* Remote CICS system               */
#define CICS_SYNCONRETURN "CICSSyncOnReturn" /* Commit on each invoke */
#define CICS_TRANSID "CICSTransID" /* Remote mirror transaction ID    */
#define CICS_ERROR "CICSError"     /* Error message returned 2 client */

/*--------------------------------------------------------------------*/
/*  Headers included                                                  */
/*--------------------------------------------------------------------*/
#include <stdio.h>               /* get the standard I/O library      */
#include <stdlib.h>              /* get the standard library          */
#include <string.h>              /* support string operations         */
/*--------------------------------------------------------------------*/
/* Type definitions                                                   */
/*--------------------------------------------------------------------*/
/*--------------------------------------------------------------------*/
/* Commarea pointer and length                                        */ 
/*--------------------------------------------------------------------*/
void* ca_ptr;            
short ca_len;

/*--------------------------------------------------------------------*/
/* Global variables                                                   */
/*--------------------------------------------------------------------*/
char g_httpMethod[MAX_METHOD_LEN];         /*HTTP method GET, POST... */
long g_httpMethodLen = MAX_METHOD_LEN - 1; /*HTTP method length       */
char g_httpVersion[MAX_VERSION_LEN];       /*HTTP version 1.0 1.1 ... */
long g_httpVersionLen = MAX_VERSION_LEN - 1;/*HTTP version length     */
char g_httpPath[MAX_PATH_LEN];             /*HTTP path from URL       */
long g_httpPathLen = MAX_PATH_LEN - 1;     /*HTTP path length         */
char g_queryString[MAX_QUERY_STRING_LEN];  /*HTTP query str from URL  */
long g_queryStringLen = MAX_QUERY_STRING_LEN - 1; /*HTTP query length */
long g_requestType = 0;                    /*HTTP request type        */
long g_contentLength = 0;                  /*HTTP body length         */
char g_errorMessage[512];                  /* error description       */
char g_CICSProgram[9];                     /* Program to invoke       */
long g_CICSLength;                         /* Size of the commarea    */
long g_CICSDataLength;                     /* Size of the input data  */
char g_CICSSysID[5];                       /* Remote CICS system      */
long g_CICSSyncOnReturn;                   /* Commit on each invoke   */
char g_CICSTransID[5];             /* Remote mirror transaction ID    */
signed long g_cicsResp = 0;                /*Last CICS command resp   */
signed long g_cicsResp2 = 0;               /*Last CICS command resp2  */
long g_trace = FALSE_CODE;                 /*Are traces activated     */
char g_clientID[MAX_CLIENT_ADDR];          /*Client identifier        */

/*--------------------------------------------------------------------*/
/* Function prototypes                                                */
/*--------------------------------------------------------------------*/
void initVariables();             /* Initialize all variables         */
long getClientID();               /* Get a unique client identifier   */
long checkRequest();              /* Verify this is a valid request   */
long invokeProgram();             /* Link to backend program          */
long sendReply(char* commarea, long commareaLength); /* Send reply from
                                     linked program back to client    */
long getRequestProperties();      /* Determine the request type       */
long getHTTPHeaders();            /* Get HTTP headers of interest     */
long getHTTPHeader(char* headerLabel,
                   long headerLabelLength,
                   char* headerValue); /* Get a single HTTP header    */
long sendWelcomeMessage();        /* Send friendly message to browser */
long logCicsError(char* errorCommand,
              signed long resp,
              signed long resp2 ); /* Log errors returned from CICS   */
long logError(char* errorMessage); /* Log errors from this module     */
long traceParameter(char* parameterName,
                    void* parameterValue,
                    int parameterType ); /* format trace for a single
                                      parameter                       */
long traceMessage(char* traceMessage ); /* Trace a message            */
long traceData(char* data,
               long dataLength );  /* Dump a buffer of data           */
long sendErrorMessage();           /* Notify client that error occured*/

/*====================================================================*/
/*  Main section                                                      */
/*====================================================================*/
void main()  
{                                 
    long rc = OK_CODE;            /* general purpose return code      */
    initVariables();              /* make sure storage is initialized */
        
    /* Verify that this is a request we can process                   */
    rc = checkRequest();

    /*  If request is from a browser, send back a friendly message    */
    if (BROWSER_CLIENT == rc) {
       sendWelcomeMessage();
       EXEC CICS RETURN;
    }
    
    /* Given a valid request, attempt to invoke backend program       */
    if (OK_CODE == rc) {
       rc = invokeProgram();
    }
    
    /* If an error has been detected, attempt notifying client        */
    if (OK_CODE != rc) {
       sendErrorMessage();
       EXEC CICS RETURN;
    }
    
    /* end of processing                                              */
    EXEC CICS RETURN;
}                                 

/*====================================================================*/
/* Initialize variables to a safe value                               */
/*                                                                    */
/* Output :  none                    None                             */
/*                                                                    */
/*====================================================================*/
void initVariables() 
{
  g_CICSProgram[0] = '\0';                 /* Program to invoke       */
  g_CICSLength = 0;                        /* Size of the commarea    */
  g_CICSDataLength = 0;                    /* Size of the input data  */
  g_CICSSysID[0] = '\0';                   /* Remote CICS system      */
  g_CICSSyncOnReturn = FALSE_CODE;         /* Commit on each invoke   */
  g_CICSTransID[0] = '\0';         /* Remote mirror transaction ID    */
  
  /* Determine a unique ID to identify a particular client            */
  getClientID();
}


/*====================================================================*/
/* Perform validation on the request received                         */
/*                                                                    */
/* Output :  rc                    Return code                        */
/*                                                                    */
/*====================================================================*/
long checkRequest()
{
    long rc = OK_CODE;            /* general purpose return code      */
    
    /* Get commarea address and EIB bloc address                      */
    EXEC CICS ADDRESS
              COMMAREA(ca_ptr)
              EIB(dfheiptr)
              RESP(g_cicsResp) RESP2(g_cicsResp2);                   
                                                
    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError("ADDRESS",g_cicsResp,g_cicsResp2);
       return g_cicsResp;
    }
       
    /* Extract request properties                                     */
    rc = getRequestProperties();
    if (OK_CODE!=rc)  return rc;
    
    /* It must be an HTTP client                                      */
    if (g_requestType != DFHVALUE(HTTPYES) ) {
       logError("Not an HTTP client");
       return SEVERE_CODE;
    }

    /* There must be a valid HTTP method                              */
    if (g_httpMethodLen < 3 ) {
       logError("Invalid HTTP method");
       return SEVERE_CODE;
    }
    
    /*  If request is from a browser, return immediatly               */
    if (strcmp("GET",g_httpMethod)==0) {
       return BROWSER_CLIENT;
    }
       
    /*  Starting from now, only POST is accepted                      */
    if (strcmp("POST",g_httpMethod)!=0) {
       logError("Invalid HTTP method");
       return SEVERE_CODE;
    }

    /*  Extract HTTP headers of interest                              */
    rc = getHTTPHeaders();
    if (OK_CODE!=rc)  return rc;

    /*  Make sure we have a program name we can link to               */
    if (strlen(g_CICSProgram) == 0) {
       logError("No CICSProgram was specified");
       return SEVERE_CODE;
    }

    /*  Make sure requested sizes make sense                          */
    if (g_CICSDataLength > g_CICSLength) {
       logError("CICSDataLength cannot be larger than CICSLength");
       return SEVERE_CODE;
    }

    return rc;
}

/*====================================================================*/
/* Link to the backend program                                        */
/*                                                                    */
/* Input  :  g_contentLength       Length of the data posted          */
/*           g_CICSLength          Size of the commarea requested     */
/*           g_CICSProgram         CICS Program to invoke             */
/*           g_CICSLength          Requested commarea size            */
/*           g_CICSDataLength      Incoming data size                 */
/*           g_CICSSysID           Remote system ID                   */
/*           g_CICSSyncOnReturn    Commit on return from remote system*/
/*           g_CICSTransID         Remote transaction ID              */
/*                                                                    */
/* Output :  rc                    Return code                        */
/*                                                                    */
/*====================================================================*/
long invokeProgram() 
{
    void* commarea = NULL;     /* Commarea for linked program         */
    long commareaLength = 0;   /* Size of the commarea                */
    long receivedLength = 0;   /* Data actually received from client  */

    /*  The commarea must be large enough to accomodate the incoming
        data but might have to be larger if client is asking so.      */
    if ( g_CICSLength >  g_contentLength)
        commareaLength = g_CICSLength;
    else
        commareaLength = g_contentLength;

    /*  Although its relatively unusual, program might have no
        commarea at all. Otherwise get memory for it                  */
    if (commareaLength > 0)  {
        EXEC CICS GETMAIN
                  SET(commarea)
                  FLENGTH(commareaLength)
                  RESP(g_cicsResp) RESP2(g_cicsResp2);

        if (g_cicsResp != DFHRESP(NORMAL)) {
           logCicsError("GETMAIN",g_cicsResp,g_cicsResp2);
           return g_cicsResp;
        }
    }

    /*  Receive the incoming data making sure no translation occur    */
    if (commareaLength > 0)  {
        EXEC CICS WEB RECEIVE
                  INTO(commarea)
                  LENGTH(receivedLength)
                  MAXLENGTH(commareaLength)
                  RESP(g_cicsResp) RESP2(g_cicsResp2);

        if (g_cicsResp != DFHRESP(NORMAL)) {
           logCicsError("WEB RECEIVE",g_cicsResp,g_cicsResp2);
           return g_cicsResp;
        }
        
        /* if traces are on display the request data                  */
        if (TRUE_CODE == g_trace) {
          traceMessage("");
          traceMessage("Request data:");
          traceMessage("-------------");
          traceData(commarea, receivedLength);
        }
    }

    /*  Adjust the incoming data length depending on client request  */
    if (receivedLength < g_CICSDataLength)
        receivedLength = g_CICSDataLength;
    
    /*  Now link to CICS program and check for errors                */
    if (FALSE_CODE == g_CICSSyncOnReturn)
        if (strlen(g_CICSSysID) == 0) {
          EXEC CICS LINK
                    PROGRAM(g_CICSProgram)
                    COMMAREA(commarea)
                    LENGTH(commareaLength)
                    DATALENGTH(receivedLength)
                    RESP(g_cicsResp) RESP2(g_cicsResp2);
        }else {
          EXEC CICS LINK
                    PROGRAM(g_CICSProgram)
                    COMMAREA(commarea)
                    LENGTH(commareaLength)
                    DATALENGTH(receivedLength)
                    SYSID(g_CICSSysID)
                    TRANSID(g_CICSTransID)
                    RESP(g_cicsResp) RESP2(g_cicsResp2);
        }
     else
        if (strlen(g_CICSSysID) == 0) {
          EXEC CICS LINK
                    PROGRAM(g_CICSProgram)
                    COMMAREA(commarea)
                    LENGTH(commareaLength)
                    DATALENGTH(receivedLength)
                    SYNCONRETURN
                    RESP(g_cicsResp) RESP2(g_cicsResp2);
        }else {
          EXEC CICS LINK
                    PROGRAM(g_CICSProgram)
                    COMMAREA(commarea)
                    LENGTH(commareaLength)
                    DATALENGTH(receivedLength)
                    SYSID(g_CICSSysID)
                    SYNCONRETURN
                    TRANSID(g_CICSTransID)
                    RESP(g_cicsResp) RESP2(g_cicsResp2);
        }

     if (g_cicsResp != DFHRESP(NORMAL)) {
        logCicsError("LINK",g_cicsResp,g_cicsResp2);
        return g_cicsResp;
     }
     
     return sendReply(commarea, commareaLength);
}

/*====================================================================*/
/* Get a client unique ID                                             */
/*                                                                    */
/* Output :  g_clientID            String uniquely identifying client */
/*                                                                    */
/*====================================================================*/
long getClientID()
{
    long clientIDLength = MAX_CLIENT_ADDR;
    
    /* extract the client IP address      */
    EXEC CICS EXTRACT TCPIP
              CLIENTADDR(g_clientID)
              CADDRLENGTH(clientIDLength);
     
    return OK_CODE;
}
/*====================================================================*/
/* Determine the request type                                         */
/*                                                                    */
/* Output :  rc                    Return code                        */
/*           g_httpMethod          HTTP method GET, POST...           */
/*           g_httpMethodLen       HTTP method length                 */
/*           g_httpVersion         HTTP version 1.0 1.1 ...           */
/*           g_httpVersionLen      HTTP version length                */
/*           g_httpPath            HTTP path from URL                 */
/*           g_httpPathLen         HTTP path length                   */
/*           g_queryString         HTTP query str from URL            */
/*           g_queryStringLen      HTTP query length                  */
/*           g_requestType         HTTP request type                  */
/*           g_trace               Trace mode on or off               */
/*                                                                    */
/*====================================================================*/
long getRequestProperties()
{
    EXEC CICS WEB EXTRACT
              HTTPMETHOD(g_httpMethod  ) METHODLENGTH(g_httpMethodLen)
              HTTPVERSION(g_httpVersion) VERSIONLEN(g_httpVersionLen)
              PATH(g_httpPath) PATHLENGTH(g_httpPathLen)
              QUERYSTRING(g_queryString) QUERYSTRLEN(g_queryStringLen)
              REQUESTTYPE(g_requestType)
              RESP(g_cicsResp) RESP2(g_cicsResp2) ;

    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError("WEB EXTRACT",g_cicsResp,g_cicsResp2);
       return g_cicsResp;
    }
    
    /* see if traces are requested                                    */
    if (g_queryStringLen > 4 &&
       (strncmp("trace",g_queryString,5) == 0))
       g_trace = TRUE_CODE;
       
    /* if traces are on display request parameters                    */
    if (TRUE_CODE == g_trace) {
      traceMessage("");
      traceMessage("=================================================");
      traceMessage("STARTED in trace mode                           =");
      traceMessage("=================================================");
      traceMessage("Parameters returned from WEB EXTRACT:");
      traceMessage("-------------------------------------");
      traceParameter("httpMethod",g_httpMethod, 1);
      traceParameter("httpMethodLen",&g_httpMethodLen, 0);
      traceParameter("httpVersion",g_httpVersion, 1);
      traceParameter("httpVersionLen",&g_httpVersionLen, 0);
      traceParameter("httpPath",g_httpPath, 1);
      traceParameter("httpPathLen",&g_httpPathLen, 0);
      traceParameter("queryString",g_queryString, 1);
      traceParameter("queryStringLen",&g_queryStringLen, 0);
      traceParameter("requestType",&g_requestType, 0);
    }
    return OK_CODE;
}

/*====================================================================*/
/* Extract HTTP headers of interest (a POST is assumed)               */
/*                                                                    */
/* Output :  rc                    Return code                        */
/*           g_contentLength       Size of the incoming HTTP body     */
/*           g_CICSProgram         CICS Program to invoke             */
/*           g_CICSLength          Requested commarea size            */
/*           g_CICSDataLength      Incoming data size                 */
/*           g_CICSSysID           Remote system ID                   */
/*           g_CICSSyncOnReturn    Commit on return from remote system*/
/*           g_CICSTransID         Remote transaction ID              */
/*                                                                    */
/*====================================================================*/
long getHTTPHeaders()
{
    char headerLabel[MAX_HEADER_LABEL_LEN]; /* Generic header name    */
    char headerValue[MAX_HEADER_VALUE_LEN]; /* Generic header value   */
    
    /* get the content length  (It is mandatory on a POST)            */
    strcpy(headerLabel, CONTENT_LENGTH_LABEL);
    g_cicsResp = getHTTPHeader(headerLabel,
                               strlen(headerLabel), headerValue);
    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError("WEB READ content-length",g_cicsResp,g_cicsResp2);
       return g_cicsResp;
    }
    
    /* now convert the numeric string representation to an int        */
    g_contentLength = atoi(headerValue);
    
    /* get the content type  (It is mandatory on a POST)              */
    strcpy(headerLabel, CONTENT_TYPE_LABEL);
    g_cicsResp = getHTTPHeader(headerLabel,
                               strlen(headerLabel), headerValue);
    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError("WEB READ content-type",g_cicsResp,g_cicsResp2);
       return g_cicsResp;
    }
    /* the content must be binary                                     */
    if (NULL == strstr(headerValue,CONTENT_TYPE_BINARY_LABEL)) {
       logError("Content type is not binary");
       return SEVERE_CODE;
    }

    /* get the program name (optional.It can be in query string)      */
    strcpy(headerLabel, CICS_PROGRAM);
    g_cicsResp = getHTTPHeader(headerLabel,
                               strlen(headerLabel), headerValue);
    if (g_cicsResp == DFHRESP(NORMAL)) {
       strcpy(g_CICSProgram,headerValue);
    }
    
    /* get the commarea length (optional. It can be in query string)  */
    strcpy(headerLabel, CICS_LENGTH);
    g_cicsResp = getHTTPHeader(headerLabel,
                               strlen(headerLabel), headerValue);
    if (g_cicsResp == DFHRESP(NORMAL)) {
       g_CICSLength = atoi(headerValue);
    }
    
    /* get the data length (optional, ca default to commarea length)  */
    strcpy(headerLabel, CICS_DATALENGTH);
    g_cicsResp = getHTTPHeader(headerLabel,
                               strlen(headerLabel), headerValue);
    if (g_cicsResp == DFHRESP(NORMAL)) {
       g_CICSDataLength = atoi(headerValue);
    }

    /* get the remote system id (optional)                            */
    strcpy(headerLabel, CICS_SYSID);
    g_cicsResp = getHTTPHeader(headerLabel,
                               strlen(headerLabel), headerValue);
    if (g_cicsResp == DFHRESP(NORMAL)) {
       strcpy(g_CICSSysID,headerValue);
    }

    /* get the commit on return indicator (optional)                  */
    strcpy(headerLabel, CICS_SYNCONRETURN);
    g_cicsResp = getHTTPHeader(headerLabel,
                               strlen(headerLabel), headerValue);
    if (g_cicsResp == DFHRESP(NORMAL)) {
       if((strlen(headerValue) > 0) &&
          (headerValue[0] == 'y' || headerValue[0] == 'Y'))
          g_CICSSyncOnReturn = TRUE_CODE;
    }

    /* get the remote transaction id (optional)                       */
    strcpy(headerLabel, CICS_TRANSID);
    g_cicsResp = getHTTPHeader(headerLabel,
                               strlen(headerLabel), headerValue);
    if (g_cicsResp == DFHRESP(NORMAL)) {
       strcpy(g_CICSTransID,headerValue);
    }

    /* if traces are on display request parameters                    */
    if (TRUE_CODE == g_trace) {
      traceMessage("");
      traceMessage("Parameters extracted from HTTP header:");
      traceMessage("--------------------------------------");
      traceParameter("CICSProgram",g_CICSProgram, 1);
      traceParameter("CICSLength",&g_CICSLength, 0);
      traceParameter("CICSDataLength",&g_CICSDataLength, 0);
      traceParameter("CICSSysID",g_CICSSysID, 1);
      traceParameter("CICSSyncOnReturn",&g_CICSSyncOnReturn, 0);
      traceParameter("CICSTransID",g_CICSTransID, 1);
      traceParameter("contentLength",&g_contentLength, 0);
    }

    return OK_CODE;
}

/*====================================================================*/
/* Get a single HTTP header                                           */
/*                                                                    */
/* Input  :  headerLabel           Name of the header to extract      */
/*                                                                    */
/* Output :  rc                    Return code                        */
/*           headerValue           Value of header if extract ok      */
/*                                                                    */
/*====================================================================*/
long getHTTPHeader(char* headerLabel,
                   long headerLabelLength,
                   char* headerValue)
{
    long headerValueLength = MAX_HEADER_VALUE_LEN;
    
    /* get the header                                                 */
    EXEC CICS WEB READ
              HTTPHEADER    (headerLabel)
              NAMELENGTH    (headerLabelLength)
              VALUE         (headerValue)
              VALUELENGTH   (headerValueLength)
              RESP          (g_cicsResp)
              RESP2         (g_cicsResp2);

    /* make sure returned value is a valid C string                   */
    if (g_cicsResp == DFHRESP(NORMAL)) {
      headerValue[headerValueLength] = '\0';
    }
    
    /* This function does not require headers to exist                */
    return g_cicsResp;
}

/*====================================================================*/
/* Send a the reply data back                                         */
/*                                                                    */
/* Input  :  commarea              updated by the backend program     */
/*           commareaLength        size of the data to send back      */
/*                                                                    */
/* Output :  rc                    Return code                        */
/*                                                                    */
/*====================================================================*/
long sendReply(char* commarea, long commareaLength)
{
    char docToken[16];            /* document identifier              */
    long docSize = 0;             /* size of document                 */
    
    /* if traces are on display the reply data                        */
    if (TRUE_CODE == g_trace) {
      traceMessage("");
      traceMessage("Reply data:");
      traceMessage("-----------");
      traceData(commarea, commareaLength);
    }

    /* Create a document from the output data (no symbols)            */
    EXEC CICS DOCUMENT CREATE DOCTOKEN(docToken)               
                       BINARY(commarea)              
                       LENGTH(commareaLength)  
                       DOCSIZE(docSize)
              RESP(g_cicsResp) RESP2(g_cicsResp2) ;
    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError("DOCUMENT CREATE",g_cicsResp,g_cicsResp2);
       return g_cicsResp;
    }
       
    /* Send the reply back requesting no translation                  */
    EXEC CICS WEB SEND DOCTOKEN(docToken)
              RESP(g_cicsResp) RESP2(g_cicsResp2) ;                                           
    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError("WEB SEND",g_cicsResp,g_cicsResp2);
       return g_cicsResp;
    }

    /* reply successfully sent back                                   */
    if (TRUE_CODE == g_trace) {
      traceMessage("STOPPED in success mode                         =");
      traceMessage("=================================================");
    }
    
    return OK_CODE;
}

/*====================================================================*/
/* Send a friendly message to a browser                               */
/*                                                                    */
/* Output :  rc                    Return code                        */
/*                                                                    */
/*====================================================================*/
long sendWelcomeMessage()
{
    char docToken[16];            /* document identifier              */
    long docSize = 0;             /* size of document                 */
    char welcomeMessage[1024];    /* will hold the html text sent back*/
    
    /* Create a document from the message text (no symbols)           */
    sprintf(welcomeMessage,"%s", MSG_DOCUMENT);
    EXEC CICS DOCUMENT CREATE DOCTOKEN(docToken)               
                       TEXT(welcomeMessage)              
                       LENGTH(strlen(welcomeMessage))  
                       DOCSIZE(docSize)
              RESP(g_cicsResp) RESP2(g_cicsResp2) ;
    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError("DOCUMENT CREATE",g_cicsResp,g_cicsResp2);
       return g_cicsResp;
    }
       
    /* Send the html document back assuming client understands latin-1*/
    EXEC CICS WEB SEND DOCTOKEN(docToken) CLNTCODEPAGE("iso-8859-1")
              RESP(g_cicsResp) RESP2(g_cicsResp2) ;                                           
    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError("WEB SEND",g_cicsResp,g_cicsResp2);
       return g_cicsResp;
    }
    
    return OK_CODE;
}

/*====================================================================*/
/* An error was encountered, this will translate into:                */
/* An HTTP Status code of 500 Internal Server error                   */
/* An iso-8859-1 message describing the error in the HTTP body        */
/*                                                                    */
/*                                                                    */
/* Output :  rc                    Return code                        */
/*                                                                    */
/*====================================================================*/
long sendErrorMessage()
{
    char docToken[16];            /* document identifier              */
    long docSize = 0;             /* size of document                 */
    char errorMessage[512];       /* will hold the html text sent back*/
    short statusCode = 500;       /* standard error on server side    */
    char statusText[22] = "Internal server error"; /* standard test   */
    char cicsErrorHeader[] = CICS_ERROR; /* http header signaling pb  */
     
    /* Send back an HTTP header holding the error text                */
    EXEC CICS WEB WRITE HTTPHEADER(cicsErrorHeader)               
                        NAMELENGTH(strlen(cicsErrorHeader))              
                        VALUE(g_errorMessage)
                        VALUELENGTH(strlen(g_errorMessage))
              RESP(g_cicsResp) RESP2(g_cicsResp2) ;
    if (g_cicsResp != DFHRESP(NORMAL)) {
       return g_cicsResp;
    }

    /* Create an empty document                                       */
    EXEC CICS DOCUMENT CREATE DOCTOKEN(docToken)               
                       DOCSIZE(docSize)
              RESP(g_cicsResp) RESP2(g_cicsResp2) ;
    if (g_cicsResp != DFHRESP(NORMAL)) {
       return g_cicsResp;
    }

    /* Send the empty document back                                   */
    EXEC CICS WEB SEND DOCTOKEN(docToken) CLNTCODEPAGE("iso-8859-1")
              STATUSCODE(statusCode)
              STATUSTEXT(statusText) Length(strlen(statusText))
              RESP(g_cicsResp) RESP2(g_cicsResp2) ;                                           
    if (g_cicsResp != DFHRESP(NORMAL)) {
       return g_cicsResp;
    }
    
    return OK_CODE;
}

/*====================================================================*/
/* Log any CICS error return code                                     */
/*                                                                    */
/* Input  :  errorCommand          the failed CICS command            */
/*           resp                  response code                      */
/*           resp2                 reason code                        */
/*                                                                    */
/* Output :                        None                               */
/*                                                                    */
/*====================================================================*/
long logCicsError(char* errorCommand,
                  signed long resp, signed long resp2 )
{
    char cicsErrorMessage[257];   /* complete message for CICS errors */
    char respText[12];            /* human readable resp code         */
    
    /* Attempt to get a user friendly resturn code                    */
    switch (resp) {
      case (DFHRESP(INVREQ)):
        strcpy(respText,"INVREQ");
        break; 
      case (DFHRESP(NOTFND)):
        strcpy(respText,"NOTFND");
        break;
      case (DFHRESP(LENGERR)):
        strcpy(respText,"LENGERR");
        break;
      case (DFHRESP(NOSTG)):
        strcpy(respText,"NOSTG");
        break;
      case (DFHRESP(NOTAUTH)):
        strcpy(respText,"NOTAUTH");
        break;
      case (DFHRESP(PGMIDERR)):
        strcpy(respText,"PGMIDERR");
        break;
      case (DFHRESP(RESUNAVAIL)):
        strcpy(respText,"RESUNAVAIL");
        break;
      case (DFHRESP(ROLLEDBACK)):
        strcpy(respText,"ROLLEDBACK");
        break;
      case (DFHRESP(SYSIDERR)):
        strcpy(respText,"SYSIDERR");
        break;
      case (DFHRESP(TERMERR)):
        strcpy(respText,"TERMERR");
        break;
      case (DFHRESP(SYMBOLERR)):
        strcpy(respText,"SYMBOLERR");
        break;
      case (DFHRESP(TEMPLATERR)):
        strcpy(respText,"TEMPLATERR");
        break;
      default: 
        sprintf(respText,"%d",resp);
    }
    sprintf(cicsErrorMessage,
            "CICS command=%s failed, resp=%s, resp2=%d",
            errorCommand, respText, resp2);
    logError(cicsErrorMessage);
}

/*====================================================================*/
/* Log general errors                                                 */
/*                                                                    */
/* Input  :  errorMessage          message describing error           */
/*                                                                    */
/* Output :                        None                               */
/*                                                                    */
/*====================================================================*/
long logError(char* errorMessage )
{
    char absTime[8];   /* current absolute time, ms since 01/01/1900  */
    unsigned char curtime[9];   /* human readable current time        */
    unsigned char curdate[11];  /* human readable current date        */
    signed long cicsResp = 0;   /* local resp to preserve global one  */
    signed long cicsResp2 = 0;  /* local resp2 to preserve global one */
    
    /* get the current time/date                                      */
    EXEC CICS ASKTIME ABSTIME(absTime)
                      RESP(cicsResp)
                      RESP2(cicsResp2);
    if (cicsResp != DFHRESP(NORMAL)) {
      strcpy(curtime,"Unknown");
      strcpy(curdate,"Unknown");
    }
    else {
      /* make the date and time human readable                        */
      curtime[8] = '\0';
      curdate[10] = '\0';
      EXEC CICS FORMATTIME ABSTIME(absTime)
                           YYYYMMDD(curdate)
                           TIME(curtime)
                           TIMESEP
                           DATESEP;
    }

    /* save the message so that it can be sent back to client        */
    sprintf(g_errorMessage,
            "LSWEBBIN : Client=%s : Time=%s @ %s : %s",
                   g_clientID, curdate, curtime, errorMessage);

    /* stderr, when used in an LE environment which is the case for
       CICS TS, uses the CESE transient data queue. This queue
       directs to CEEMSG by default.                                 */
    fprintf(stderr,"%s\n",g_errorMessage);
    /* also print message to CEEOUT in case someone is monitoring
       stdout                                                        */
    if (TRUE_CODE == g_trace) {
      traceMessage(errorMessage);
      traceMessage("STOPPED in error mode                           =");
      traceMessage("=================================================");
    }
}

/*====================================================================*/
/* Trace a parameter value                                            */
/*                                                                    */
/* Input  :  parameterName         Name of the parameter to trace     */
/*           parameterValue        Pointer to parameter value         */
/*           parameterType         0 for a long, otherwise string     */
/*                                                                    */
/* Output :                        None                               */
/*                                                                    */
/*====================================================================*/
long traceParameter(char* parameterName,
                    void* parameterValue,
                    int parameterType )
{
    char trace[512];
    switch (parameterType) {
      case 0:
        sprintf(trace,"%s = %d",
                parameterName, *(long*)parameterValue );
        break;
      default:  
        sprintf(trace,"%s = %s",
                parameterName, (char*)parameterValue );
        break;
    }
    return traceMessage(trace);  
}

/*====================================================================*/
/* Trace the content of a buffer                                      */
/*                                                                    */
/* Input  :  data                 `Pointer to buffer data             */
/*           dataLength            data size                          */
/*                                                                    */
/* Output :                        None                               */
/*                                                                    */
/*====================================================================*/
long traceData(char* data,
               long dataLength )
{
    int i;
    char dumpLine[128];
    char dumpChar[5];
    char dumpString[17];
    
    dumpLine[0]='\0';
    dumpString[0]='\0';
    for (i = 0; i < dataLength && i < MAX_TRACES_BYTES; i++) {
       /* print every 16 byte on a different line */
       sprintf(dumpChar,"%.2X ",data[i]);
       strcat(dumpLine,dumpChar);
       sprintf(dumpChar,"%c",data[i]);
       strcat(dumpString,dumpChar);
       if (i % 16 == 15 || i == dataLength - 1) {
          strcat(dumpLine," -- ");
          strcat(dumpLine,dumpString);
          traceMessage(dumpLine);
          dumpString[0]='\0';
          dumpLine[0]='\0';
       }
    }
    
    if (dataLength > MAX_TRACES_BYTES) {
        sprintf(dumpLine,"...data was truncated at %d bytes",
                MAX_TRACES_BYTES);
        traceMessage(dumpLine);
    }
    return OK_CODE;
}

/*====================================================================*/
/* Trace a message                                                    */
/*                                                                    */
/* Input  :  g_clientID            client identifier                  */
/*           traceMessage          message to trace                   */
/*                                                                    */
/* Output :                        None                               */
/*                                                                    */
/*====================================================================*/
long traceMessage(char* traceMessage )
{
    /* stdout, when used in an LE environment which is the case for
       CICS TS, uses the CESO transient data queue. This queue
       directs to CEEOUT by default.                                 */
    fprintf(stdout,"LSWEBBIN : Client=%s : %s\n",
            g_clientID, traceMessage);
            
    return OK_CODE;
}

