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
/*   History    - 01 Jan 2006  - Original Implementation              */
/*                04 May 2006  - Support for multiple parts           */
/*   Notes      - This program provides HTTP clients with the         */
/*                capability to invoke a CICS program passing         */
/*                data as a binary Http body.                         */
/*                                                                    */
/*                Client has the responsibility to create the request */
/*                in the format expected by the linked program.       */
/*                This means that characters must be EBCDIC encoded   */
/*                and numeric data must be encoded in characters,     */
/*                binary, or packed-decimal.                          */
/*                                                                    */
/*                A request is entirely described by the binary       */
/*                HTTP payload. That payload is devided into parts.   */
/*                Each part has 3 components                          */
/*                                                                    */
/*                  ID     : 16 bytes: Identifies the part            */
/*                  Length : 04 bytes: Gives the content length       */
/*                  Content: variable: The part content               */
/*                                                                    */
/*                Furthermore, the first part is known as the header  */
/*                part and its content has the following layout:      */
/*                                                                    */
/*                  partsn: 04 bytes: Number of input data parts      */
/*                  jsonl : 04 bytes: Length of following JSON string */
/*                  jsons : variable: A JSON string describing the    */
/*                                    CICS program to run.            */
/*                                                                    */
/*                Traces can be turned on by adding a "CICSTraceMode" */
/*                HTTP header with value "true"                       */
/*                Traces can use a client-provided correlation ID via */
/*                the "CICSRequestID" HTTP header                     */
/*                                                                    */
/*                If the request fails, an error message will         */
/*                complement the 500 status in the CICSError HTTP     */
/*                header on the response.                             */
/*                                                                    */
/*                If request succeeds, the response is formatted      */
/*                exactly like the request.                           */
/*                                                                    */
/**********************************************************************/
/*--------------------------------------------------------------------*/
/*  Constants                                                         */
/*--------------------------------------------------------------------*/
#define MODULE_NAME   "LSWEBBIN" /* used to correlate traces          */
#define MAX_CLIENT_ADDR 257      /* TCPIP client address max length   */
#define CONTENT_LENGTH_HHDR "Content-Length" /* Definition of the
                                    HTTP content length header        */
#define CONTENT_TYPE_HHDR "Content-Type" /* Definition of the
                                    HTTP content type header          */
#define CONTENT_TYPE_BINARY         "binary" /* Part of binary content
                                    types. ex: binary/octet-stream    */
#define REQUEST_TRACE_MODE_HHDR "CICSTraceMode" /* HTTP header for
                                    requested host trace mode (yes/no)*/
#define REQUEST_ID_HHDR "CICSRequestID" /* HTTP header for request
                                     identifier to be used as a 
                                     correlation ID                   */
#define CICS_ERROR_HHDR "CICSError"   /* HTTP header signaling errors
                                    back                              */
#define MAX_METHOD_LEN 25        /* HTTP method maximum length        */
#define MAX_VERSION_LEN 25       /* HTTP version maximum length       */
#define MAX_PATH_LEN 257         /* HTTP path maximum length          */
#define MAX_QUERY_STRING_LEN 257 /* HTTP query string maximum length  */
#define MAX_HEADER_LABEL_LEN 64  /* HTTP header name maximum length   */
#define MAX_HEADER_VALUE_LEN 128 /* HTTP header value maximum length  */
#define MSG_DOCUMENT \                                      
   "<!doctype html public '-//W3C//DTD HTML 3.2//EN'> \  
   <html><head> \
   <title>LSWEBBIN - Http binary link interface - </title>\
   </head><body> \
   <h1>Welcome from LSWEBBIN</h1> \                     
   <p>This program is expecting a POSTed binary content.</p> \
   <p>HTTP headers supported on request are:</p> \
   <br>CICSTraceMode    : true to perform tracing \
   <br>CICSRequestID    : A correlation ID for this request \
   <p>HTTP headers on response are:</p> \
   <br>CICSError        : Only present if an error occured \
   </body></html>"

#define BROWSER_CLIENT 4         /* Recoverable error occurred        */

/*--------------------------------------------------------------------*/
/*  Headers included                                                  */
/*--------------------------------------------------------------------*/
#include <stdio.h>               /* get the standard I/O library      */
#include <stdlib.h>              /* get the standard library          */
#include <string.h>              /* support string operations         */
#include "lscomdec.h"            /* legstar common declarations       */

/*--------------------------------------------------------------------*/
/* Global variables                                                   */
/*--------------------------------------------------------------------*/
TraceParms g_traceParms;             /* Set of trace parameters       */
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
char g_contentType[MAX_HEADER_VALUE_LEN];  /*HTTP body content type   */
char* g_pContent = NULL;                   /*HTTP body                */
char g_clientID[MAX_CLIENT_ADDR];          /*Client identifier        */

/*====================================================================*/
/*  Main section                                                      */
/*====================================================================*/
void main() {                                 
    int rc = OK_CODE;             /* general purpose return code      */
    init();                       /* make sure storage is initialized */
    /* Verify that this is a request we can process                   */
    rc = checkRequest();

    /*  If request is from a browser, send back a friendly message    */
    if (BROWSER_CLIENT == rc) {
       sendWelcomeMessage();
       EXEC CICS RETURN;
    }
    
    /* Given a valid request, process it                              */
    if (OK_CODE == rc) {
       rc = processRequest();
    }
    
    /* If an error has been detected, undo any updates and attempt to
     * notify client                                                  */
    if (OK_CODE != rc) {
       Rollback();
       sendErrorMessage();
       EXEC CICS RETURN;
    }

    if (g_traceParms.traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "Main ended");
        traceMessage(MODULE_NAME,
        "==============================================");
    }
    
    /* end of processing                                              */
    EXEC CICS RETURN;
}                                 

/*====================================================================*/
/* Initialize global variables to a safe value                        */
/*====================================================================*/
int init() {
    long clientIDLength = MAX_CLIENT_ADDR;
    
    memset(g_contentType, 0, sizeof(g_contentType));
    memset(g_traceMessage, 0, sizeof(g_traceMessage));
    
    /* Initialize tracing variables */
    memset(g_traceParms.CxID, 0, sizeof(g_traceParms.CxID));
    g_traceParms.traceMode = FALSE_CODE;
    memset(g_traceParms.formattedErrorMessage, 0,
           sizeof(g_traceParms.formattedErrorMessage));
  
    /* Get commarea address and EIB bloc address                      */
    EXEC CICS ADDRESS
              COMMAREA(ca_ptr)
              EIB(dfheiptr)
              RESP(g_cicsResp) RESP2(g_cicsResp2);                  
                                               
    if (g_cicsResp != DFHRESP(NORMAL)) {
       return abortServer(CICS_ABEND_CODE);
    }
    
    /* Determine a unique ID to identify a particular client          */
    EXEC CICS EXTRACT TCPIP
              CLIENTADDR(g_clientID)
              CADDRLENGTH(clientIDLength);
              
    initLog(dfheiptr, &g_traceParms);
    return OK_CODE;
}

/*====================================================================*/
/* Perform validation on the request received                         */
/*====================================================================*/
int checkRequest() {
    int rc = OK_CODE;             /* general purpose return code      */
    
    /* Extract request properties                                     */
    rc = getRequestProperties();
    if (ERROR_CODE == rc) {
         return rc;
    }
    
    /* It must be an HTTP client                                      */
    if (g_requestType != DFHVALUE(HTTPYES) ) {
       logError(MODULE_NAME, "Not an HTTP client");
       return ERROR_CODE;
    }

    /* There must be a valid HTTP method                              */
    if (g_httpMethodLen < 3 ) {
       logError(MODULE_NAME, "Invalid HTTP method");
       return ERROR_CODE;
    }
    
    /*  If request is from a browser, return immediatly               */
    if (strcmp("GET",g_httpMethod)==0) {
       return BROWSER_CLIENT;
    }
       
    /*  Starting from now, only POST is accepted                      */
    if (strcmp("POST",g_httpMethod)!=0) {
       logError(MODULE_NAME, "Invalid HTTP method");
       return ERROR_CODE;
    }

    /*  Extract HTTP headers of interest                              */
    rc = getHTTPHeaders();
    if (ERROR_CODE == rc) {
        return rc;
    }

    /* incoming data should at least contain a header so it cannot
     * be empty.                                                      */
    if (g_contentLength < strlen(HEADER_PART_ID)) {
       logError(MODULE_NAME,
           "Http body should at least contain a header part");
       return ERROR_CODE;
    }
 
    if (g_traceParms.traceMode == TRUE_CODE) {
       reportInputParameters();
    }
 
    return rc;
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
/*                                                                    */
/*====================================================================*/
int getRequestProperties() {
    EXEC CICS WEB EXTRACT
              HTTPMETHOD(g_httpMethod  ) METHODLENGTH(g_httpMethodLen)
              HTTPVERSION(g_httpVersion) VERSIONLEN(g_httpVersionLen)
              PATH(g_httpPath) PATHLENGTH(g_httpPathLen)
              QUERYSTRING(g_queryString) QUERYSTRLEN(g_queryStringLen)
              REQUESTTYPE(g_requestType)
              RESP(g_cicsResp) RESP2(g_cicsResp2) ;

    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError(MODULE_NAME, "WEB EXTRACT",g_cicsResp,g_cicsResp2);
       return abortServer(CICS_ABEND_CODE);
    }
    
    return OK_CODE;
}

/*====================================================================*/
/* Extract HTTP headers of interest (a POST is assumed)               */
/*                                                                    */
/* Output :  rc                     Return code                       */
/*           g_contentLength        Size of the incoming HTTP body    */
/*           g_contentType          Content MIME type                 */
/*           g_traceParms.traceMode Tracing mode                      */
/*           g_traceParms.CxID      Correlation ID                    */
/*                                                                    */
/*====================================================================*/
int getHTTPHeaders() {
    int rc = OK_CODE;
    char headerLabel[MAX_HEADER_LABEL_LEN]; /* Generic header name    */
    char headerValue[MAX_HEADER_VALUE_LEN]; /* Generic header value   */
    
    /* get the content length  (It is mandatory on a POST)            */
    getHTTPHeader(CONTENT_LENGTH_HHDR,
                  strlen(CONTENT_LENGTH_HHDR), headerValue);
    if (strlen(headerValue) == 0) {
       logError(MODULE_NAME, "Missing content-length http header");
       return ERROR_CODE;
    }
    
    /* now convert the numeric string representation to an int        */
    g_contentLength = atoi(headerValue);
    
    /* get the content type  (It is mandatory on a POST)              */
    getHTTPHeader(CONTENT_TYPE_HHDR,
                 strlen(CONTENT_TYPE_HHDR), headerValue);
    if (strlen(headerValue) == 0) {
       logError(MODULE_NAME, "Missing content-type http header");
       return ERROR_CODE;
    }
    /* the content must be binary                                     */
    if (NULL == strstr(headerValue,CONTENT_TYPE_BINARY)) {
       logError(MODULE_NAME, "Content type is not binary");
       return ERROR_CODE;
    }
    strcpy(g_contentType, headerValue);

    /* see if client is requesting traces      */
    getHTTPHeader(REQUEST_TRACE_MODE_HHDR,
                  strlen(REQUEST_TRACE_MODE_HHDR), headerValue);
    if ((strlen(headerValue) > 0)
        && (headerValue[0] == 't' || headerValue[0] == 'T')) {
       g_traceParms.traceMode = TRUE_CODE;
    } else {
       g_traceParms.traceMode = FALSE_CODE;
    }
    
    /* see if client is sending a request ID. If yes, use it as the
     * trace correlation ID otherwise use the client IP address       */
    getHTTPHeader(REQUEST_ID_HHDR,
                  strlen(REQUEST_ID_HHDR), headerValue);
    if (strlen(headerValue) > 0) {
        strcpy(g_traceParms.CxID, headerValue);
    } else {
        strcpy(g_traceParms.CxID, g_clientID);
    }
    
    return OK_CODE;
}

/*====================================================================*/
/* Report all parameters received.                                    */
/*====================================================================*/
int reportInputParameters() {
    traceMessage(MODULE_NAME,
        "==============================================");
    traceMessage(MODULE_NAME, "Main started");
    traceMessage(MODULE_NAME, "");
    traceMessage(MODULE_NAME, "Parameters returned from WEB EXTRACT:");
    traceMessage(MODULE_NAME, "-------------------------------------");
    traceParameter("httpMethod",g_httpMethod, 1);
    traceParameter("httpMethodLen",&g_httpMethodLen, 0);
    traceParameter("httpVersion",g_httpVersion, 1);
    traceParameter("httpVersionLen",&g_httpVersionLen, 0);
    traceParameter("httpPath",g_httpPath, 1);
    traceParameter("httpPathLen",&g_httpPathLen, 0);
    traceParameter("queryString",g_queryString, 1);
    traceParameter("queryStringLen",&g_queryStringLen, 0);
    traceParameter("requestType",&g_requestType, 0);
    traceMessage(MODULE_NAME, "");
    traceMessage(MODULE_NAME, "Parameters extracted from HTTP header:");
    traceMessage(MODULE_NAME, "--------------------------------------");
    traceParameter("contentLength",&g_contentLength, 0);
    traceParameter("Content type", &g_contentType, 1);
    traceParameter("Trace mode",&g_traceParms.traceMode, 0);
    traceParameter("Correlation ID", g_traceParms.CxID, 1);
    traceMessage(MODULE_NAME, "");
}

/*====================================================================*/
/* Get a single HTTP header. If not found, return an empty value.     */
/*====================================================================*/
int getHTTPHeader(char* headerLabel,
                  long headerLabelLength,
                  char* headerValue) {
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
    } else {
        headerValue[0] = '\0';
    }
    
    return OK_CODE;
}

/*====================================================================*/
/* Send a friendly message to a browser                               */
/*====================================================================*/
int sendWelcomeMessage() {
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
       logCicsError(MODULE_NAME,
            "DOCUMENT CREATE", g_cicsResp, g_cicsResp2);
       return ERROR_CODE;
    }
       
    /* Send the html document back assuming client understands latin-1*/
    EXEC CICS WEB SEND DOCTOKEN(docToken) CLNTCODEPAGE("iso-8859-1")
              RESP(g_cicsResp) RESP2(g_cicsResp2) ;
    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError(MODULE_NAME,
            "WEB SEND", g_cicsResp, g_cicsResp2);
       return ERROR_CODE;
    }
    
    return OK_CODE;
}

/*====================================================================*/
/* An error was encountered, this will translate into:                */
/* An HTTP Status code of 500 Internal Server error                   */
/* An iso-8859-1 message describing the error in the HTTP body        */
/*====================================================================*/
int sendErrorMessage() {
    char docToken[16];            /* document identifier              */
    long docSize = 0;             /* size of document                 */
    short statusCode = 500;       /* standard error on server side    */
    char statusText[22] = "Internal server error"; /* standard test   */
    char cicsErrorHeader[] = CICS_ERROR_HHDR; /* http header signaling pb  */
     
    /* Send back an HTTP header holding the error text                */
    EXEC CICS WEB WRITE HTTPHEADER(cicsErrorHeader)               
                        NAMELENGTH(strlen(cicsErrorHeader))              
                        VALUE(g_traceParms.formattedErrorMessage)
                        VALUELENGTH(
                           strlen(g_traceParms.formattedErrorMessage))
              RESP(g_cicsResp) RESP2(g_cicsResp2) ;
    if (g_cicsResp != DFHRESP(NORMAL)) {
       return ERROR_CODE;
    }

    /* Create an empty document                                       */
    EXEC CICS DOCUMENT CREATE DOCTOKEN(docToken)               
                       DOCSIZE(docSize)
              RESP(g_cicsResp) RESP2(g_cicsResp2) ;
    if (g_cicsResp != DFHRESP(NORMAL)) {
       return ERROR_CODE;
    }

    /* Send the empty document back                                   */
    EXEC CICS WEB SEND DOCTOKEN(docToken) CLNTCODEPAGE("iso-8859-1")
              STATUSCODE(statusCode)
              STATUSTEXT(statusText) Length(strlen(statusText))
              RESP(g_cicsResp) RESP2(g_cicsResp2) ;
    if (g_cicsResp != DFHRESP(NORMAL)) {
       return ERROR_CODE;
    }
    
    return OK_CODE;
}

/*====================================================================*/
/* If we can't communicate with the client, the only way to signal    */
/* an error is to abend with a meaningful error message in the CICS   */
/* log.                                                               */
/*====================================================================*/
int abortServer(char* abendCode) {
    EXEC CICS ABEND
              ABCODE(abendCode)
              NODUMP;
    return ERROR_CODE;
}
/*====================================================================*/
/* This will undo all updates in the current unit of work.            */
/*====================================================================*/
int Rollback() {
    EXEC CICS SYNCPOINT ROLLBACK
              RESP(g_cicsResp) RESP2(g_cicsResp2);

    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError(MODULE_NAME, "SYNCPOINT ROLLBACK",
                     g_cicsResp, g_cicsResp2);
       return ERROR_CODE;
    }
    return OK_CODE;
}

/*====================================================================*/
/* Trace a parameter value                                            */
/*====================================================================*/
int traceParameter(char* parameterName,
                    void* parameterValue,
                    int parameterType ) {
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
    return traceMessage(MODULE_NAME, trace);  
}

/*====================================================================*/
/*  Process a client request                                          */
/*====================================================================*/
int processRequest() {
    
    int rcSave;
    int rc = OK_CODE;
    CICSProgramDesc programDesc;
    char savedFormattedErrorMessage[MAX_FORM_MSG_LEN]; /* Original 
                                           error message save area.   */
    MessagePart requestHeaderPart;         /* Request header part     */
    MessagePart inParts[MAX_IN_MSG_PARTS]; /* Input message parts     */
    MessagePart responseHeaderPart;        /* Response header part    */
    MessagePart outParts[MAX_OUT_MSG_PARTS]; /* Output message parts  */
    Message requestMessage;                /* Complete request        */
    Message responseMessage;               /* Complete response       */
    
    if (g_traceParms.traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "Entering process request");
    }
    
    /* Initialize message structures */
    memset(&requestHeaderPart, '\0', sizeof(requestHeaderPart));
    memset(inParts, '\0', sizeof(inParts));
    memset(&responseHeaderPart, '\0', sizeof(responseHeaderPart));
    memset(outParts, '\0', sizeof(outParts));
    requestMessage.pHeaderPart = &requestHeaderPart;
    requestMessage.pParts = inParts;
    responseMessage.pHeaderPart = &responseHeaderPart;
    responseMessage.pParts = outParts;
    
    rc = recvRequestMessage(&requestMessage);
    if (OK_CODE == rc) {
        rc = invokeProgram(dfheiptr,
                           &g_traceParms,
                           &programDesc,
                           &requestMessage,
                           &responseMessage);
        if (OK_CODE == rc) {
            rc = sendResponseMessage(&responseMessage);
        }
    }
    
    /* Whatever the status, reclaim any memory acquired to service
     * the request. In case of error, preserve error code and
     * related message so we can accurately report the original
     * error rather than a potential subsequent free errors. */
    rcSave = rc;
    if (rcSave != OK_CODE) {
        strcpy(savedFormattedErrorMessage,
               g_traceParms.formattedErrorMessage);
    }
    rc = freeProgram(dfheiptr,
                     &g_traceParms,
                     &programDesc,
                     &requestMessage,
                     &responseMessage);
    
    /* Report free errors only if there was no previous error */
    if (rcSave != OK_CODE) {
        rc = rcSave;
        strcpy(g_traceParms.formattedErrorMessage,
               savedFormattedErrorMessage);
    }
    if (g_traceParms.traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "Process request ended");
    }
    return rc;
}

/*====================================================================*/
/* Receive the request message which will contain a header part       */
/* followed by any number of message parts. The exact number of input */
/* message parts is given by the header part.                         */
/*====================================================================*/
int recvRequestMessage(Message* pRequestMessage) {
    
    int i = 0;
    int rc = OK_CODE;
    char* pReceivedData = NULL;  /* Pointer to data received          */
    long receivedLength = 0;   /* Data actually received from client  */
    int pos = 0;           /* Current position with the received data */
    char headerID[9];      /* Request header identifier               */
    int* inPartsNum = 0;   /* Number of input parts                   */
    HeaderPartContent* pHeaderPartContent;
    MessagePart* pHeaderPart = pRequestMessage->pHeaderPart;
 
    if (g_traceParms.traceMode == TRUE_CODE) {
      traceMessage(MODULE_NAME, "About to receive request message");
    }
 
    EXEC CICS WEB RECEIVE
              SET       (pReceivedData)
              LENGTH    (receivedLength)
              RESP(g_cicsResp) RESP2(g_cicsResp2);

    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError(MODULE_NAME,"WEB RECEIVE",g_cicsResp,g_cicsResp2);
       return ERROR_CODE;
    }
    
    /* We should have received the announced length not less not more */
    if (receivedLength != g_contentLength) {
       logError(MODULE_NAME,
           "Web receive did not return the expected length");
       return ERROR_CODE;
    }
    
    /* The header is itself a message part */
    rc = recvMessagePart(pReceivedData, &pos, pHeaderPart);
    if (rc == ERROR_CODE) {
        return ERROR_CODE;
    }
    
    /* Check that this message part is indeed a header part.
     * If not, consider the client is out of sync or not talking
     * our protocol. */
    memset(headerID, '\0', sizeof(headerID));
    memcpy(headerID, pHeaderPart->ID, sizeof(HEADER_PART_ID) - 1);
    if (strcmp(headerID, HEADER_PART_ID) != 0) {
        logError(MODULE_NAME,
              "A request should start with a header part. Aborting.");
        return ERROR_CODE;
    }

    /* Analyze header part to determine the number of input parts to
     * receive.  */
    pHeaderPartContent = (HeaderPartContent*) pHeaderPart->content;
    inPartsNum = pHeaderPartContent->partsNumber.as_int;
   
    if (inPartsNum > MAX_IN_MSG_PARTS) {
        logError(MODULE_NAME, "Too many input message parts.");
        return ERROR_CODE;
    }
    
    if (g_traceParms.traceMode == TRUE_CODE) {
        sprintf(g_traceMessage,
            "Program header received, input parts=%d keyValuesSize=%d",
                   inPartsNum,
                   pHeaderPartContent->keyValuesSize.as_int);
        traceMessage(MODULE_NAME, g_traceMessage);
    }
   
    /* Now receive the input message parts */
    for (i = 0; i < inPartsNum && rc == OK_CODE; i++) {
        rc = recvMessagePart(
           pReceivedData, &pos, pRequestMessage->pParts + i);
    }
   
    if (g_traceParms.traceMode == TRUE_CODE) {
        if (OK_CODE == rc) {
            traceMessage(MODULE_NAME, "Request message received:");
            traceMessage(MODULE_NAME, "-------------------------");
            dumpMessage(MODULE_NAME, pRequestMessage);
        } else {
            traceMessage(MODULE_NAME,
                 "Request message receive failed");
        }
    }
    
    return rc;
}

/*====================================================================*/
/* This routine receives a message part.                              */
/* A message part starts with a message part header (MPH) formed by:  */
/* - 16 bytes giving the message part identifier                      */
/* - 4 bytes giving the content size                                  */
/* The message content follows the MPH immediatly.                    */
/*                                                                    */
/* Output :  The updated pointer to the message part data             */
/*           The updated position in the received data                */
/*                                                                    */
/*====================================================================*/
int recvMessagePart(
        char* pReceivedData, int* pPos, MessagePart* pMessagePart) {
 
    int rc = OK_CODE;
    int sLeft;

    if (g_traceParms.traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "About to receive message part");
    }
    
    /* If there is not enough bytes left to fill a part header,
     * there is a problem. */
    sLeft = g_contentLength - *pPos;
    if (sLeft < sizeof(pMessagePart->ID)
                            + sizeof(pMessagePart->size.as_bytes)) {
        logError(MODULE_NAME, "No message parts found");
        return ERROR_CODE;
    }

    /* First receive the message part header (16 bytes 
       identifier and 4 bytes giving the part content size. */
    memcpy(pMessagePart->ID, pReceivedData + *pPos,
                                sizeof(pMessagePart->ID));
    *pPos += sizeof(pMessagePart->ID);
    memcpy(pMessagePart->size.as_bytes, pReceivedData + *pPos,
                                sizeof(pMessagePart->size.as_bytes));
    *pPos += sizeof(pMessagePart->size.as_bytes);
     
   
    /* Sanity check the size received */
    if ((pMessagePart->size.as_int < 0)
       || (pMessagePart->size.as_int > MSGPART_MAX_LEN)) {
       logError(MODULE_NAME, "Invalid message part length.");
       return ERROR_CODE;
    }
    sLeft = g_contentLength - *pPos;
    if (sLeft < pMessagePart->size.as_int) {
        logError(MODULE_NAME, "No message part content found");
        return ERROR_CODE;
    }
 
    /* Empty content is perfectly valid */
    if (pMessagePart->size.as_int == 0) {
       if (g_traceParms.traceMode == TRUE_CODE) {
         sprintf(g_traceMessage,
                "Message part received, id=%s size=%d",
                 pMessagePart->ID,
                 pMessagePart->size.as_int);
         traceMessage(MODULE_NAME, g_traceMessage);
       }
       pMessagePart->content = NULL;
       return OK_CODE;
    }
 
    /* Acquire storage for the message part content. An additional
     * byte guaranteed to contain a binary zero is acquired so
     * that contents can be treated as C strings when necessary.  */
    EXEC CICS GETMAIN
              SET     (pMessagePart->content)
              INITIMG ('\0')
              FLENGTH (pMessagePart->size.as_int + 1)
              RESP    (g_cicsResp) RESP2(g_cicsResp2);

    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError(MODULE_NAME, "GETMAIN", g_cicsResp, g_cicsResp2);
       return ERROR_CODE;
    }
    if (g_traceParms.traceMode == TRUE_CODE) {
       sprintf(g_traceMessage,
        "Request message part content allocated at %#x size=%d",
        (int)pMessagePart->content, pMessagePart->size.as_int + 1);
        traceMessage(MODULE_NAME, g_traceMessage);
    }
    
    /* Receive the message content */
    memcpy(pMessagePart->content, pReceivedData + *pPos,
                                     pMessagePart->size.as_int);
    *pPos += pMessagePart->size.as_int;
 
    if (g_traceParms.traceMode == TRUE_CODE) {
       sprintf(g_traceMessage,
              "Message part received, id=%s size=%d",
               pMessagePart->ID,
               pMessagePart->size.as_int);
       traceMessage(MODULE_NAME, g_traceMessage);
    }
   
    return OK_CODE;
}

/*====================================================================*/
/* Send output message parts back to client. All parts are serialized */
/* in a single HTTP reply payload.                                    */
/*====================================================================*/
int sendResponseMessage(Message* pResponseMessage) {
  
    int rc;
    int i = 0;
    char docToken[16];            /* document identifier              */
    long docSize = 0;             /* size of document                 */
    int pos = 0;            /* Current position with the sent data    */
    int sendBufferLen = 0;   /* Total length of data to send          */
    char* pSendBuffer = NULL;  /* Pointer to data to send             */
    HeaderPartContent* pHeaderPartContent =
          (HeaderPartContent*)pResponseMessage->pHeaderPart->content;
    int outPartsNum = pHeaderPartContent->partsNumber.as_int;

    if (g_traceParms.traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "About to send response message");
    }
    
    /* Create a memory block large enough to hold all parts           */
    sendBufferLen = (outPartsNum + 1) *
             (sizeof(pResponseMessage->pHeaderPart->ID)
              + sizeof(pResponseMessage->pHeaderPart->size.as_bytes));
    sendBufferLen += pResponseMessage->pHeaderPart->size.as_int;
    for (i = 0; i < outPartsNum; i++) {
        sendBufferLen += (pResponseMessage->pParts + i)->size.as_int;
    }
    EXEC CICS GETMAIN
              SET    (pSendBuffer)
              FLENGTH(sendBufferLen)
              RESP   (g_cicsResp) RESP2(g_cicsResp2);

    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError(MODULE_NAME,"GETMAIN",g_cicsResp,g_cicsResp2);
       return ERROR_CODE;
    }
    if (g_traceParms.traceMode == TRUE_CODE) {
       sprintf(g_traceMessage,
        "Response buffer allocated at %#x size=%d",
        (int)pSendBuffer, sendBufferLen);
        traceMessage(MODULE_NAME, g_traceMessage);
    }
                  
    rc = sendMessagePart(
            pSendBuffer, &pos, pResponseMessage->pHeaderPart);
  
    /* Send message parts */
    for (i = 0; i < outPartsNum && rc == OK_CODE; i++) {
        rc = sendMessagePart(
                pSendBuffer, &pos, pResponseMessage->pParts + i);
    }
    
    /* Create a document from the output data (no symbols)            */
    EXEC CICS DOCUMENT CREATE DOCTOKEN(docToken)               
                       BINARY  (pSendBuffer)              
                       LENGTH  (sendBufferLen)  
                       DOCSIZE (docSize)
              RESP(g_cicsResp) RESP2(g_cicsResp2) ;
    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError(MODULE_NAME,
             "DOCUMENT CREATE",g_cicsResp,g_cicsResp2);
       return ERROR_CODE;
    }
       
    /* The content type needs to be binary (same as request)          */
    rc = setHTTPHeader(CONTENT_TYPE_HHDR, strlen(CONTENT_TYPE_HHDR),
                 g_contentType, strlen(g_contentType));
    if (rc != OK_CODE) {
        return rc;
    }
    
    /* Send the reply back requesting no translation                  */
    EXEC CICS WEB SEND DOCTOKEN(docToken)
              RESP(g_cicsResp) RESP2(g_cicsResp2) ;                     
    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError(MODULE_NAME, "WEB SEND",g_cicsResp,g_cicsResp2);
       return ERROR_CODE;
    }
    
    if (g_traceParms.traceMode == TRUE_CODE) {
       if (OK_CODE == rc) {
          traceMessage(MODULE_NAME, "Response message sent:");
          traceMessage(MODULE_NAME, "-------------------------");
          dumpMessage(MODULE_NAME, pResponseMessage);
       } else {
          traceMessage(MODULE_NAME, "Response message send failed");
       }
    }
    
    return rc;
}

/*====================================================================*/
/* Send a message part back to client.                                */
/*====================================================================*/
int sendMessagePart(char* pSendBuffer, int* pPos, MessagePart* pPart) {
    
    int contentLen = pPart->size.as_int;

    if (g_traceParms.traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "About to send message part");
    }
    /* Send the message part ID*/
    memcpy(pSendBuffer + *pPos, pPart->ID, MSG_ID_LEN);
    *pPos += MSG_ID_LEN;
    /* Send the message content size */
    memcpy(pSendBuffer + *pPos, pPart->size.as_bytes,
           MSG_CONTENT_SIZE_LEN);
    *pPos += MSG_CONTENT_SIZE_LEN;
    
    if (contentLen > 0 && pPart->content != NULL) {
        memcpy(pSendBuffer + *pPos, pPart->content,
               contentLen);
        *pPos += contentLen;
    }
    
    if (g_traceParms.traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "Message part sent");
    }
    
    return OK_CODE;
}

/*====================================================================*/
/* Create an HTTP Header                                              */
/*====================================================================*/
int setHTTPHeader(char* headerLabel,
                  long headerLabelLength,
                  char* headerValue,
                  long headerValueLength) {
    
    /* set the header                                                 */
    EXEC CICS WEB WRITE
              HTTPHEADER    (headerLabel)
              NAMELENGTH    (headerLabelLength)
              VALUE         (headerValue)
              VALUELENGTH   (headerValueLength)
              RESP          (g_cicsResp)
              RESP2         (g_cicsResp2);

    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError(MODULE_NAME, "WEB WRITE",g_cicsResp,g_cicsResp2);
       return ERROR_CODE;
    }
    
    return OK_CODE;
}

