#pragma XOPTS(CICS)
#pragma longname 
#define __CICS_IPV6
/**********************************************************************/
/*                                                                    */
/*                     Copyright (C) 2007 LegSem.                     */
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
/*  Author     - Fady Moussallam  (fady@legsem.com)                   */
/*  Purpose    - CICS Socket program for simple binary DPL link       */
/*  Language   - IBM C/370                                            */
/*  System     - Tested on CICS TS 2.3                                */
/*  History    - 23 Feb 2007  - Original Implementation               */
/*  Notes      - This program provides Socket clients with the        */
/*               capability to invoke a CICS program passing          */
/*               data as a binary payload.                            */
/*                                                                    */
/*               Client has the responsibility to create the request  */
/*               in the format expected by the linked program.        */
/*               This means that characters must be EBCDIC encoded    */
/*               and numeric data must be encoded in characters,      */
/*               binary, or packed-decimal.                           */
/*                                                                    */
/*               This server is expected to be launched by the IBM    */
/*               CICS Listener (see IBM IP CICS Sockets Guide).       */
/*                                                                    */
/*               The initial client message at connection time is     */
/*               expected to have the following format (EBCDIC):      */
/*                 Data item      Length Offset Purpose               */
/*                 -------------- ------ ------ -----------------     */
/*                 User ID           8     0    Security exit         */
/*                 Password          8     8    Security exit         */
/*                 Connection ID    16    16    Tracing correlation   */
/*                 Trace             1    32    1=Trace 0=No trace    */
/*                 Eye Catcher       2    33    Integrity checking    */
/*                                                                    */
/*               After the initial connection dialog, the client is   */
/*               expected to send data formatted like so:             */
/*                 Data item      Length Offset Purpose               */
/*                 -------------- ------ ------ -----------------     */
/*                 Message type      8     0    Type of service       */
/*                 String delimiter  1     8    Binary zero           */
/*                 Message           x     9    Variable size         */
/*                                                                    */
/*               Acceptable values for message types are:             */
/*                 LSOKEXEC :A request to execute a program.          */
/*                 LSOKDATA :Input data for a request.                */
/*                 LSOKUOWC :A unit of work command.                  */
/*                 LSOKPROB :A probe used to check if server is alive.*/
/*                                                                    */
/*               Each execution request starts with a header part     */
/*               followed by one or more message parts (EBCDIC):      */
/*                 Data item      Length Offset Purpose               */
/*                 -------------- ------ ------ -----------------     */
/*                 Message ID       16     0    Describes message     */
/*                 Content length    4    16    Content bytes size    */
/*                 Content           x    20    Variable size         */
/*                                                                    */
/*               A header part is itself a special Message part       */
/*               where the content is structured like this (EBCDIC):  */
/*                 Data item      Length Offset Purpose               */
/*                 -------------- ------ ------ -----------------     */
/*                 Msg parts num     4     0    Nbr of message parts  */
/*                 Key/Values size   4     4    key/values bytes size */
/*                 Key/Values        x     8    Downstream protocol   */
/*                                                                    */
/*               Key/Values are opaque to LSSOKBIN but used down the  */
/*               line by the LSLNKBIN sub-program.                    */
/*                                                                    */
/*               Replies flowing from this server to the client are   */
/*               structured like this:                                */
/*                 Data item      Length Offset Purpose               */
/*                 -------------- ------ ------ -----------------     */
/*                 Message type      8     0    Type of reply         */
/*                 String delimiter  1     8    Binary zero           */
/*                 Message           x     9    Variable size         */
/*                                                                    */
/*               There are 3 different types of replies this program  */
/*               might generate:                                      */
/*                 LSOKACK0 :Acknowledges data sent by client.        */
/*                 LSOKERR0 :An error has happened. Following is a    */
/*                           fixed size error message.                */
/*                 LSOKDATA :Normal reply. Following data is          */
/*                           structured as a Message part (see input).*/
/*                                                                    */
/*               In a symetrical fashion with the request, reply data */
/*               starts with a header part followed by one or more    */
/*               message parts (The header part gives the number of   */
/*               following message parts).                            */
/*                                                                    */
/**********************************************************************/
/*--------------------------------------------------------------------*/
/*  Headers included                                                  */
/*--------------------------------------------------------------------*/
#include <stdio.h>               /* get the standard I/O library      */
#include <stdlib.h>              /* get the standard library          */
#include <string.h>              /* support string operations         */
#include <cmanifes.h>            /* manifest for sockets (re-entrant) */
#include <fcntl.h>               /* fcntl socket API call parameters  */
#include <errno.h>               /* error codes                       */
#include <tcperrno.h>            /* socket related error codes        */
#include <socket.h>              /* socket API                        */
#include <ezacictm.h>            /* task input message from listener  */
#include <decimal.h>             /* zOs extension for packed decimals */
#include "lscomdec.h"            /* legstar common declarations       */

/*--------------------------------------------------------------------*/
/*  Constants                                                         */
/*--------------------------------------------------------------------*/
#define MODULE_NAME   "LSSOKBIN" /* used to correlate traces          */
#define READ_OPERATION 1         /* Code for receive operations       */
#define WRITE_OPERATION 2        /* Code for send operations          */
#define CICS_APPLID_LEN 8        /* CICS application ID length        */
#define CICS_LANG_LEN 3          /* CICS language in use length       */
#define CICS_USERID_LEN 8        /* CICS User ID in use length        */
#define CICS_TASKN_LEN 4         /* CICS server Task number length    */
#define CICS_ABSTIME_LEN 8       /* CICS absolute time length         */
#define CICS_CURRTIME_LEN 8      /* CICS human readable time length   */
#define CICS_CURRDATE_LEN 10     /* CICS human readable date length   */
#define CICS_TRNID_LEN 4         /* CICS server Transaction ID        */
#define OUT_BUFFER_LEN 16384     /* Send buffer length (Identical to
                                    default value for TCPSENDBFRSIZE
                                    in TCPCONFIG).                    */

/*--------------------------------------------------------------------*/
/* Global internal variables                                          */
/*--------------------------------------------------------------------*/
struct clientid g_listenClID;    /* Listener clientid                 */
unsigned long g_socket = 0;      /* Active socket                     */
int g_waiting4Requests = TRUE_CODE; /* True when waiting for client
                                    requests. False when processing has
                                    started for a client request.     */

char g_CICSApplid[CICS_APPLID_LEN + 1]; /* CICS region where this 
                                    server is executing.              */
char g_CICSLang[CICS_LANG_LEN + 1]; /* CICS region national language. */
char g_CICSUserID[CICS_USERID_LEN + 1]; /* User ID running this server*/
char g_CICSTaskn[2 * CICS_TASKN_LEN]; /* Task number for this server
                                         in human readable format.    */
char g_CICSAbsTime[CICS_ABSTIME_LEN]; /* Server absolute start time,
                                         ms since 01/01/1900          */
char g_CICSCurtime[CICS_CURRTIME_LEN + 1]; /* human readable server
                                              start time              */
char g_CICSCurdate[CICS_CURRDATE_LEN + 1]; /* human readable server
                                              start date              */
char g_CICSTrnID[CICS_TRNID_LEN + 1]; /* Transaction ID for this
                                         server                       */
char g_SendBuffer[OUT_BUFFER_LEN];   /* Used to buffer send operations*/
int  g_BufferedDataLen;              /* Current buffered data length  */
TraceParms g_traceParms;             /* Set of trace parameters       */

/*====================================================================*/
/*  Main section                                                      */
/*====================================================================*/
void main() 
{
    int rc = OK_CODE;
    /* Initialize and get parameters from IBM CICS Listener */
    init();
    if (g_traceParms.traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME,
        "==============================================");
       traceMessage(MODULE_NAME, "Main started");
    }
   
    /* Acquire a socket and acknowledge the client initial message.
     * No need to check for error code. If anything goes wrong at
     * this early stage, the server is immediatly aborted.    */
    doTakeSocket();
   
    /* Process client requests (there could be many). There is no
     * attempt to recover from failures. This server is stopped on
     * the first error encountered while processing a request.   */
    while (OK_CODE == rc) {
        rc = recvRequest();
    }
   
    /* If a request ended in error but we still have connectivity
     * with the client, try to send en error notification back. */
    if (ERROR_CODE == rc) {
        if (errno != ECONNCLOSED) {
          Rollback();
        }
        if (errno != ECONNCLOSED
            && errno != ECONNRESET
            && errno != EPIPE
            && errno != ETIMEDOUT) {
            sendErrorReport();
        }
    }
    
    /* Close the socket   */
    Close();
   
    if (g_traceParms.traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "Main ended");
        traceMessage(MODULE_NAME,
        "==============================================");
    }

    /* end of processing                                              */
    EXEC CICS RETURN;
}

/*====================================================================*/
/* Perform initialization of global variables and get an EIB block    */
/*====================================================================*/
int init() {
    union {
        char as_bytes[CICS_TASKN_LEN];
        decimal(7,0) as_packed;
    } eibtaskn;
    
    memset(&g_listenClID, 0, sizeof(g_listenClID));
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
    initLog(dfheiptr, &g_traceParms);
    
    /* Store characteristics of this server transaction */
    memset(g_CICSApplid, 0, sizeof(g_CICSApplid));
    memset(g_CICSLang, 0, sizeof(g_CICSLang));
    memset(g_CICSUserID, 0, sizeof(g_CICSUserID));
    memset(g_CICSCurtime, 0, sizeof(g_CICSCurtime));
    memset(g_CICSCurdate, 0, sizeof(g_CICSCurdate));
    memset(g_CICSTrnID, 0, sizeof(g_CICSTrnID));
    memset(g_CICSTaskn, 0, sizeof(g_CICSTaskn));
     
    EXEC CICS ASSIGN
        APPLID(g_CICSApplid)
        LANGINUSE(g_CICSLang)
        USERID(g_CICSUserID);
        
    /* get the current time/date                                      */
    EXEC CICS ASKTIME ABSTIME(g_CICSAbsTime);
    EXEC CICS FORMATTIME ABSTIME(g_CICSAbsTime)
                         YYYYMMDD(g_CICSCurdate)
                         TIME(g_CICSCurtime)
                         TIMESEP
                         DATESEP;
    
    memcpy(g_CICSTrnID, dfheiptr->eibtrnid, CICS_TRNID_LEN);
    memcpy(eibtaskn.as_bytes, dfheiptr->eibtaskn, CICS_TASKN_LEN);
    sprintf(g_CICSTaskn, "%D(7,0)", eibtaskn.as_packed);
    
    return getListenerParms();
}

/*====================================================================*/
/* Retrieves the caller (normally a Listener) parameters. The         */
/* clientID identifies an address space to the TCPIP region. The      */
/* caller clientID is needed for the takesocket API.                  */
/*                                                                    */
/* Output :  The listener clientID                                    */
/*           The remote caller connection ID                          */
/*           The socket to be taken                                   */
/*           Trace mode expected by client                            */
/*                                                                    */
/*====================================================================*/
int getListenerParms() {
 
    struct sock_enhanced_tim* tim;
    int timLen = 0;
    char traceMode;
    char eyeCatcher[MAX_TIM_EC_LEN + 1];
    memset(eyeCatcher, 0, sizeof(eyeCatcher));
   
    EXEC CICS RETRIEVE
              SET    (tim)
              LENGTH (timLen)
              RESP(g_cicsResp) RESP2(g_cicsResp2);

    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError(MODULE_NAME, "RETRIEVE", g_cicsResp, g_cicsResp2);
       return abortServer(CICS_ABEND_CODE);
    }
   
    /* The last 2 characters of the client data must contain the 
     * expectedeye catcher. Otherwise this is an unknown client 
     * type.        */
    memcpy(eyeCatcher, tim->client_in_data + 33, 2);
    if (strcmp(EYE_CATCHER, eyeCatcher) != 0) {
       logError(MODULE_NAME, "Unsupported client type.");
       return abortServer(PROT_ABEND_CODE);
    }
    
    /* Construct a socket clientid for the listener, based on the
     * parameters passed in the initial message. Here we assume the
     * domain is AF_INET instead of AF_INET6                         */
    g_listenClID.domain = AF_INET;       
    memcpy(g_listenClID.name, tim->listen_name, 8);
    memcpy(g_listenClID.subtaskname, tim->listen_taskid, 8);
          
    /* Get the remote caller connection ID from the user data passed
     * in the initial message.                                        */
    memcpy(g_traceParms.CxID, tim->client_in_data + 16,
           MAX_CXID_LEN);
    
     /* Get the socket to be taken */
    g_socket = tim->give_take_socket;
   
     /* Get the trace mode set by the client */
    memcpy(&traceMode, tim->client_in_data + 32, 1);
    if (traceMode == '1') {
       g_traceParms.traceMode = TRUE_CODE;
    } else {
       g_traceParms.traceMode = FALSE_CODE;
    }
    
    return OK_CODE;
}

/*====================================================================*/
/* Take the socket that was given by the listener. This is necessary  */
/* because this transaction is different from the listener one and    */
/* might run in a totally different CICS region.                      */
/*                                                                    */
/* Output :  The active socket                                        */
/*                                                                    */
/*====================================================================*/
int doTakeSocket() {
    int rc;
    unsigned long temps = g_socket;
   
    if (g_traceParms.traceMode == TRUE_CODE) {
      traceMessage(MODULE_NAME, "About to take socket");
    }
   
    rc = takesocket(&g_listenClID, temps);
    if (ERROR_CODE == rc) {
        logSocketError("takesocket");
        return abortServer(SOK_ABEND_CODE);
    }
    g_socket = rc;
   
    /* Set the socket options. */
    rc = doSetSockOpt();
    if (ERROR_CODE == rc) {
         return abortServer(SOK_ABEND_CODE);
    }
   
    /* Client is waiting for an ack to his initial message. */
    rc = sendConnectionAck();
    if (ERROR_CODE == rc) {
         return abortServer(SOK_ABEND_CODE);
    }
   
    if (g_traceParms.traceMode == TRUE_CODE) {
        sprintf(g_traceMessage,"Take socket %d success", g_socket);
        traceMessage(MODULE_NAME, g_traceMessage);
    }
    return OK_CODE;
}

/*====================================================================*/
/* The socket options need to be tweaked for performance.             */
/*====================================================================*/
int doSetSockOpt() {
    int rc;
    int enabled = 1;
    
    /* Set the socket to non-blocking mode in order to control time
     * outs on receive operations. */
    rc = fcntl(g_socket, F_SETFL, FNDELAY);
    if (ERROR_CODE == rc) {
        logSocketError("fcntl");
        return ERROR_CODE;
    }
    
    /* In order to optimize memory allocation, this server program
     * sends message parts back to client in 2 different sends. If
     * we don t disable Nagle, there is an unacceptable delay in
     * the client acknowldgement of the first send. */
    rc = setsockopt(g_socket, IPPROTO_TCP, TCP_NODELAY,
                    enabled, sizeof(enabled) );
    if (ERROR_CODE == rc) {
        logSocketError("setsockopt");
        return ERROR_CODE;
    }
    return OK_CODE;
}

/*====================================================================*/
/* Receives requests from client and dispatches processing depending  */
/* on the request type. This method is meant to be used in a loop     */
/* until client closes the connection or an error condition arises.   */
/*====================================================================*/
int recvRequest() {
    int i = 0;
    int rc = 0;
    char requestType[REQUEST_TYPE_LEN + 1];
 
    if (g_traceParms.traceMode == TRUE_CODE) {
      traceMessage(MODULE_NAME, "About to receive request");
    }
 
    /* Waiting for requests from clients. Client is authorized to
     * close the socket */
    g_waiting4Requests = TRUE_CODE;
    
    /* This receive will block until data is available or timeout. */
    rc = doReceive(requestType, sizeof(requestType));
    if (rc == ERROR_CODE) {
        return ERROR_CODE;
    }
    /* Dont trust the last byte to be a C string delimiter. */
    memset(&requestType[REQUEST_TYPE_LEN], '\0', 1);
    
    if (g_traceParms.traceMode == TRUE_CODE) {
      sprintf(g_traceMessage,
          "Request type:%s, received.", requestType);
        traceMessage(MODULE_NAME, g_traceMessage);
    }
    
    /* If this is an execution request, proceed with execution steps.*/
    if (strcmp(requestType, EXEC_REQUEST_EC) == 0) {
        return processRequest();
    }
    
     /* If this is unit of work command, proceed with uow steps.*/
    if (strcmp(requestType, UOW_REQUEST_EC) == 0) {
        return processUOWCommand();
    }
    
    /* If this is a probe, send back an ack to prove we are alive.*/
    if (strcmp(requestType, PROBE_REQUEST_EC) == 0) {
        if (g_traceParms.traceMode == TRUE_CODE) {
            traceMessage(MODULE_NAME, "Ack reply to probe request");
        }
        return sendAck();
    }
    
    /* At this stage, consider the client is out of sync or not talking
     * our protocol. */
    sprintf(g_traceMessage,
        "Unknown request type:%s, received.", requestType);
    logError(MODULE_NAME, g_traceMessage);
    return ERROR_CODE;

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
    
    /* From now on, client should send data and wait for a reply.
     * Any close request should be interpreted as an error. */
    g_waiting4Requests = FALSE_CODE;
      
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
    int rc = 0;
    int inPartsNum = 0;
    char headerID[9];
    HeaderPartContent* pHeaderPartContent;
    MessagePart* pHeaderPart = pRequestMessage->pHeaderPart;
 
    if (g_traceParms.traceMode == TRUE_CODE) {
      traceMessage(MODULE_NAME, "About to receive request message");
    }
 
    /* The header is itself a message part */
    rc = recvMessagePart(pHeaderPart);
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
        rc = recvMessagePart(pRequestMessage->pParts + i);
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
/*                                                                    */
/*====================================================================*/
int recvMessagePart(MessagePart* pMessagePart) {
 
    int rc = 0;

    if (g_traceParms.traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "About to receive message part");
    }
   
    /* First receive the message part header (16 bytes 
       identifier and 4 bytes giving the part content size. */
    rc = doReceive(pMessagePart, sizeof(pMessagePart->ID)
                            + sizeof(pMessagePart->size.as_bytes));
    if (rc == ERROR_CODE) {
        return ERROR_CODE;
    }
 
    /* Sanity check the size received */
    if ((pMessagePart->size.as_int < 0)
       || (pMessagePart->size.as_int > MSGPART_MAX_LEN)) {
       logError(MODULE_NAME, "Invalid message part length.");
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
              SET(pMessagePart->content)
              INITIMG('\0')
              FLENGTH(pMessagePart->size.as_int + 1)
              RESP(g_cicsResp) RESP2(g_cicsResp2);

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
    rc = doReceive(pMessagePart->content, pMessagePart->size.as_int);
    if (rc == ERROR_CODE) {
        return ERROR_CODE;
    }
 
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
/* Receive a predefined number of bytes. Since stream data might get  */
/* chunked due to network activity, a loop is necessary to collect    */
/* all the data expected.                                             */
/*                                                                    */
/* Output :  The data received                                        */
/*                                                                    */
/*====================================================================*/
int doReceive(char* buffer, int nBytes) {
    int recvbytes = 0;
    int rc = 0;
    unsigned long temps = g_socket;

    if (g_traceParms.traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "About to doReceive");
    }
   
    /* Receive all chunks till we have the requested byte count */
    while (recvbytes < nBytes) {
      rc = doReceiveWait(temps, buffer + recvbytes,
                         nBytes - recvbytes, RECV_TIME_OUT);
      /* If receive failed, percolate the error up the stack */
      if (rc == ERROR_CODE) {
          return ERROR_CODE;
      }
       /* Prepare to receive the next chunk */
      recvbytes += rc;
    }
   
    if (g_traceParms.traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "doReceive done");
    }
   
    return OK_CODE;
}

/*====================================================================*/
/* In non-blocking mode, recv usually returns immediatly with an      */
/* EWOULDBLOCK return code. This happens because the client might not */
/* have sent data yet or data is still in the network. This method    */
/* will wait for a maximum time for incoming data.                    */
/*                                                                    */
/* Output :  The data received                                        */
/*                                                                    */
/*====================================================================*/
int doReceiveWait(unsigned long socket,
    char* buffer, int nBytes, int timeLimit) {
     
    int rc = 0;
 
    if (g_traceParms.traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "About to doReceiveWait");
    }
    rc = recv(socket, buffer, nBytes, 0);
   
    if (g_traceParms.traceMode == TRUE_CODE) {
       if (rc > 0) {
            traceMessage(MODULE_NAME, "Data received");
       } else {
           if (rc == 0) {
              traceMessage(MODULE_NAME,
                           "Orderly close request received");
           } else {
                if (errno == ECONNRESET) {
                   traceMessage(MODULE_NAME, "Connection reset");
                } else {
                   traceMessage(MODULE_NAME, "Receive failed");
                }
           }
       }     
    }
    /* Receive operation actually worked (meaning data was received) */
    if (rc > 0) {
       return rc;
    }
    
    /* Client might have disconnected. Behavior varies 
     * depending on whether a close is expected or not. */
    if (rc == 0 || errno == ECONNRESET) {
      if (g_waiting4Requests == FALSE_CODE) {
          logError(MODULE_NAME,
                   "Client unexpectedly closed connection.");
      }
      errno = ECONNCLOSED;
      return ERROR_CODE;
    }
   
    /* Receive operation might have failed */
    if (rc < 0 && errno != EWOULDBLOCK) {
       logSocketError("recv");
       return ERROR_CODE;
    }
    
    /* This is the EWOULDBLOCK situation. Give the client a chance to
     * send his data within a time limit */
    rc = doWait(socket, READ_OPERATION, timeLimit);
    if (rc == ERROR_CODE) {
        return ERROR_CODE;
    }
   
     /* We now are sure there is data waiting to be received, so we
     * can try again */
    return doReceiveWait(socket, buffer, nBytes, timeLimit);
 
 }
 
/*====================================================================*/
/* This method uses the socket select method to wait for readability  */
/* (data is available to read) or writability (socket is ready to     */
/* serve write requests. The timeout is expressed in seconds.         */
/*                                                                    */
/* Output :  The data received                                        */
/*                                                                    */
/*====================================================================*/
 int doWait(unsigned long socket, int operation, int timeLimit) {
  
    struct timeval timeout;
    fd_set mask;
    int rc = 0;
    
    if (g_traceParms.traceMode == TRUE_CODE) {
      traceMessage(MODULE_NAME, "About to select");
    }
    timeout.tv_sec  = timeLimit;
    timeout.tv_usec = 0;
    FD_ZERO(&mask);                                        
    FD_SET(socket, &mask);   
    if (operation == READ_OPERATION) {
        rc = select(socket+1, &mask, NULL, NULL, &timeout);
    }
    if (operation == WRITE_OPERATION) {
        rc = select(socket+1, NULL, &mask, NULL, &timeout);
    }
    if (rc == 0) {
       errno = ETIMEDOUT;
       logSocketError("select");
       return ERROR_CODE;
    }
    if (rc < 0) {
       logSocketError("select");
       return ERROR_CODE;
    }
   
    if (g_traceParms.traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Select returned");
    }
    
    return OK_CODE;
 }

/*====================================================================*/
/* Send output message parts back to client.                          */
/*                                                                    */
/*                                                                    */
/*====================================================================*/
int sendResponseMessage(Message* pResponseMessage) {
  
    int rc;
    int i = 0;
    HeaderPartContent* pHeaderPartContent =
          (HeaderPartContent*)pResponseMessage->pHeaderPart->content;
    int outPartsNum = pHeaderPartContent->partsNumber.as_int;

    if (g_traceParms.traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "About to send response message");
    }
    /* Prepare for send buffering */
    g_BufferedDataLen = 0;
    
    /* Send the message type followed by the serialized message */
    rc = sendMessageType();
    if (rc == OK_CODE) {
        rc = sendMessagePart(pResponseMessage->pHeaderPart);
      
        /* Send message parts */
        for (i = 0; i < outPartsNum && rc == OK_CODE; i++) {
            rc = sendMessagePart(pResponseMessage->pParts + i);
        }
    }
     
    /* Flush any remaining buffered data */
    if (rc == OK_CODE) {
        rc = doFlushSendBuffer();
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
int sendMessagePart(MessagePart* pPart) {
    int rc;
    int i = 0;
    struct {
        char ID[MSG_ID_LEN];
        char SAB[MSG_CONTENT_SIZE_LEN];
    } MPH;
    int contentLen = pPart->size.as_int;

    if (g_traceParms.traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "About to send message part");
    }
    /* Send the message part ID*/
    memcpy(MPH.ID, pPart->ID, MSG_ID_LEN);
    /* Send the message content size */
    memcpy(MPH.SAB, pPart->size.as_bytes, MSG_CONTENT_SIZE_LEN);
    rc = doSendBuffered(&MPH, sizeof(MPH));
    
    if (OK_CODE == rc && contentLen > 0 && pPart->content != NULL) {
        /* Send the message content */
        rc = doSendBuffered(pPart->content, contentLen);
    }
    
    if (g_traceParms.traceMode == TRUE_CODE) {
      if (OK_CODE == rc) {
          traceMessage(MODULE_NAME, "Message part sent");
      } else {
          traceMessage(MODULE_NAME, "Message part send failed");
      }
    }
    
    return rc;
}

/*====================================================================*/
/* When server transaction sucessfully started, send back ack message */
/* along with attributes of the server transaction. These attributes  */
/* can help client correlate his request with events generated on     */
/* host.                                                              */
/*====================================================================*/
int sendConnectionAck() {
    int rc;
    char ackMsg[MAX_FORM_MSG_LEN];
    sprintf(ackMsg,"%s {\"CICSApplid\":\"%s\","\
                   "\"CICSUserID\":\"%s\","\
                   "\"CICSLang\":\"%s\","\
                   "\"CICSTrnID\":\"%s\","\
                   "\"CICSTaskn\":\"%s\","\
                   "\"CICSCurdate\":\"%s\","\
                   "\"CICSCurtime\":\"%s\"}",
            REPLY_ACK_EC,
            g_CICSApplid, g_CICSUserID, g_CICSLang, g_CICSTrnID,
            g_CICSTaskn, g_CICSCurdate, g_CICSCurtime);
    rc = doSend(ackMsg, strlen(ackMsg));
    if (rc == ERROR_CODE) {
        return ERROR_CODE;
    }
    if (g_traceParms.traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Connection Ack sent");
    }
    return OK_CODE;
}

/*====================================================================*/
/* A valid reply must start with a specific message type.             */
/*====================================================================*/
int sendMessageType() {
    int rc;
    rc = doSendBuffered(DATA_MSG_EC, sizeof(DATA_MSG_EC));
    if (rc == ERROR_CODE) {
        return ERROR_CODE;
    }
    if (g_traceParms.traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Message type sent");
    }
    return OK_CODE;
}

/*====================================================================*/
/* Acknowledge messages as valid. The client should wait for these    */
/* messages before proceding with more messages.                      */
/*                                                                    */
/*                                                                    */
/*====================================================================*/
int sendAck() {
    int rc;
    rc = doSend(REPLY_ACK_EC, sizeof(REPLY_ACK_EC));
    if (rc == ERROR_CODE) {
        return ERROR_CODE;
    }
    if (g_traceParms.traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Ack sent");
    }
    return OK_CODE;
}

/*====================================================================*/
/* When a valid socket exist, try to sendback a description of any    */
/* error that might have been encountered.                            */
/*                                                                    */
/* Input  :  formattedErrorMessage A formatted error description      */
/*                                                                    */
/*====================================================================*/
int sendErrorReport() {
    int rc;
    if (g_traceParms.traceMode == TRUE_CODE) {
       sprintf(g_traceMessage, "About to send error report :%s",
               g_traceParms.formattedErrorMessage);
       traceMessage(MODULE_NAME,g_traceMessage);
    }
    rc = doSend(g_traceParms.formattedErrorMessage,
              sizeof(g_traceParms.formattedErrorMessage));
    if (rc == ERROR_CODE) {
       return ERROR_CODE;
    }
    if (g_traceParms.traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Error report sent");
    }
    return OK_CODE;
}

/*====================================================================*/
/* Send a given number of bytes. Since stream data might get chunked  */
/* due to network activity, a loop is necessary to send all the data  */
/* expected.                                                          */
/*                                                                    */
/* Output :                                                           */
/*                                                                    */
/*====================================================================*/
int doSend(char* buffer, int nBytes) {
    int sendbytes = 0;
    int rc = 0;
    unsigned long temps = g_socket;

    if (g_traceParms.traceMode == TRUE_CODE) {
      traceMessage(MODULE_NAME, "About to doSend");
    }
   
    /* Send all chunks till we sent the requested byte count */
    while (sendbytes < nBytes) {
      rc = doSendWait(temps, buffer + sendbytes,
                         nBytes - sendbytes, SEND_TIME_OUT);
      /* If send failed, percolate the error up the stack */
      if (rc == ERROR_CODE) {
          return ERROR_CODE;
      }
      /* Prepare to send the next chunk */
      sendbytes += rc;
    }
   
    if (g_traceParms.traceMode == TRUE_CODE) {
        sprintf(g_traceMessage, "doSend %d bytes done", sendbytes);
        traceMessage(MODULE_NAME, g_traceMessage);
    }
   
    return OK_CODE;
}
/*====================================================================*/
/* A send operation (particularly on large volumes) might fail with a */
/* EWOULDBLOCK return code. This happens because the TCPIP stack is   */
/* busy sending buffers and is not ready to service more sends. This  */
/* routine gives the socket a chance to become writable within a      */
/* reasonable time limit.                                             */
/*                                                                    */
/* Output :  The number of bytes sent                                 */
/*                                                                    */
/*====================================================================*/
int doSendWait(unsigned long socket,
    char* buffer, int nBytes, int timeLimit) {
     
    int rc = 0;
 
    if (g_traceParms.traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "About to doSendWait");
    }
    rc = send(socket, buffer, nBytes, 0);
   
    /* Send operation actually worked (meaning data was sent) */
    if (rc > 0) {
        if (g_traceParms.traceMode == TRUE_CODE) {
           traceMessage(MODULE_NAME, "Data sent");
        }
       return rc;
    }
    
    /* Send operation might have failed */
    if (rc < 0 && errno != EWOULDBLOCK) {
       logSocketError("send");
       return ERROR_CODE;
    }
    
    /* This is the EWOULDBLOCK situation. Give the socket a chance to
     * become writable within a time limit */
    rc = doWait(socket, WRITE_OPERATION, timeLimit);
    if (rc == ERROR_CODE) {
        return ERROR_CODE;
    }
   
     /* We now are sure the socket is writable, so we can try again */
    return doSendWait(socket, buffer, nBytes, timeLimit);
 
 }
 
/*====================================================================*/
/* When several send operations occur in sequence, it is more         */
/* efficient to buffer them and send them over with a single socket   */
/* send.                                                              */
/*====================================================================*/
 int doSendBuffered(char* buffer, int nBytes) {
  
    int rc = OK_CODE;
    /* If the requested data is larger than the send buffer, we just
     * ignore the buffering request. Since data is expected to be
     * sent in order, we first flush any pending buffered data */
     if (nBytes > OUT_BUFFER_LEN) {
        if (g_traceParms.traceMode == TRUE_CODE) {
           traceMessage(MODULE_NAME,
            "Outbound data too large for buffering");
        }
        rc = doFlushSendBuffer();
        if (rc == OK_CODE) {
        	return doSend(buffer, nBytes);
        } else {
        	return rc;
        }
     }
   
    /* If the data to send is larger than the free space in the send
     * buffer, flush the send buffer first to make room. */
     if (nBytes > (OUT_BUFFER_LEN - g_BufferedDataLen)) {
        if (g_traceParms.traceMode == TRUE_CODE) {
           traceMessage(MODULE_NAME,
            "Outbound buffer overflow. flushing.");
        }
        rc = doFlushSendBuffer();
        if (rc == OK_CODE) {
        	rc = doSendBuffered(buffer, nBytes);;
        }
        if (rc == ERROR_CODE) {
        	return ERROR_CODE;
        }
     }
   
     /* At this stage, the data fits into the buffer so just copy it  */
     memcpy(g_SendBuffer + g_BufferedDataLen, buffer, nBytes);
     g_BufferedDataLen += nBytes;
     if (g_traceParms.traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "Outbound data buffered");
     }
 
     return OK_CODE;
 }
 
/*====================================================================*/
/* Flush the send buffer                                              */
/*====================================================================*/
 int doFlushSendBuffer() {
     int rc = OK_CODE;
     if (g_BufferedDataLen > 0) {
        rc = doSend(g_SendBuffer, g_BufferedDataLen);
        g_BufferedDataLen = 0;
     }
     if (g_traceParms.traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "Outbound data flushed");
     }
     return rc;
 }

/*====================================================================*/
/* Client needs to confirm that he received the results successfully. */
/* In addition, we need instruction on how to proceed with the current*/
/* unit of work (UOW).                                                */
/*====================================================================*/
int processUOWCommand() {
    int rc;
    char command[UOW_COMMAND_LEN + 1];
    
    if (g_traceParms.traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Processing UOW command");
    }
    
    rc = doReceive(command, sizeof(command));
    if (rc == ERROR_CODE) {
       return ERROR_CODE;
    }

    if (g_traceParms.traceMode == TRUE_CODE) {
        sprintf(g_traceMessage, "Processing %s command", command);
       traceMessage(MODULE_NAME, g_traceMessage);
    }
    
    /* Process the requested UOW command */
    if (strncmp(UOW_COMMIT_CMD, command,
                sizeof(UOW_COMMIT_CMD)) == 0) {
       rc = Commit();
    } else {
        if (strncmp(UOW_ROLLBACK_CMD, command,
                    sizeof(UOW_ROLLBACK_CMD)) == 0) {
           rc = Rollback();
        } else {
            if (strncmp(UOW_KEEP_CMD, command,
                        sizeof(UOW_KEEP_CMD)) == 0) {
                rc = OK_CODE;
            } else {
               logError(MODULE_NAME,
                  "Unrecognized UOW processing command.");
               return ERROR_CODE;
            }
        }
    }
    
    /* Send an ack back to client signaling UOW was processed. */
    if (OK_CODE == rc) {
        rc = sendAck();
    }
    
    return rc;
}

/*====================================================================*/
/* This will confirm all updates in the current unit of work.         */
/*====================================================================*/
int Commit() {
    EXEC CICS SYNCPOINT
              RESP(g_cicsResp) RESP2(g_cicsResp2);

    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError(MODULE_NAME, "SYNCPOINT",
                     g_cicsResp, g_cicsResp2);
       return ERROR_CODE;
    }
    return OK_CODE;
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
/* Terminate all communications ovedr the socket and close it.        */
/*====================================================================*/
int Close() {
    int rc = OK_CODE;
    
    /* Shut down both sides of the communication. If shutdowb fails
     * proceed with closing anyway. */
    rc = shutdown(g_socket, SOK_HOW);
    if (rc == ERROR_CODE) {
        logSocketError("shutdown");
    }
    rc = close(g_socket);
    return rc;
}

/*====================================================================*/
/* If we can't communicate with the client, the only way to signal    */
/* an error is to abend with a meaningful error message in the CICS   */
/* log.                                                               */
/*                                                                    */
/* Input  :  abendCode             The CICS abend code to raise       */
/*                                                                    */
/*====================================================================*/
int abortServer(char* abendCode) {
    EXEC CICS ABEND
              ABCODE(abendCode)
              NODUMP;
    return ERROR_CODE;
}

/*====================================================================*/
/* Log any Socket error return code                                   */
/*                                                                    */
/* Input  :  errorCommand          the failed socket API call         */
/*                                                                    */
/* Output :                        None                               */
/*                                                                    */
/*====================================================================*/
int logSocketError(char* errorCommand) {

    char sokErrorMessage[257];   /* complete message for socket error */
    char respText[200];          /* human readable return code       */
   
    /* Attempt to get a user friendly resturn code                    */
    switch (errno) {
      case (EACCES):
        strcpy(respText,"The other application did not give the socket "
        "to your application.");
        break;
      case (EBADF):
        strcpy(respText,"The socket is not valid or has already been "
        "taken.");
        break;
      case (EPIPE):
        strcpy(respText,"The connection is broken. For socket write/"
        "send, peer has shut down one or both directions.");
        break;
      case (EFAULT):
        strcpy(respText,"Using parameters as specified "
        "would result in an attempt to access storage outside the "
        "caller’s address space.");
        break;
      case (EINVAL):
        strcpy(respText,"One of the parameters to the socket call."
        "contains an invalid value.");
        break;
      case (EMFILE):
        strcpy(respText,"The socket descriptor table is already full.");
        break;
      case (ENOBUFS):
        strcpy(respText,"The operation cannot be performed because of "
        "the shortage of SCB or SKCB control blocks in the TCP/IP "
        "address space.");
        break;
      case (EPFNOSUPPORT):
        strcpy(respText,"The domain field of the clientid parameter is "
        "not AF_INET or AF_INET6.");
        break;
      case (EWOULDBLOCK):
        strcpy(respText,"socket is in nonblocking mode, and data is "
        "not available to read.");
        break;
      case (ECONNRESET):
        strcpy(respText,"Connection reset by peer.");
        break;
      case (EIBMINVSOCKET):
        strcpy(respText,"A connection token that is not valid was "
        "detected. No such socket exists.");
        break;
      case (ETIMEDOUT):
        strcpy(respText,"A socket operation timed out.");
        break;
      default:
        sprintf(respText,"errno=%d", errno);
    }
   
    sprintf(sokErrorMessage,
            "Socket call=%s on socket=%d failed. %s",
            errorCommand, g_socket, respText);
    logError(MODULE_NAME, sokErrorMessage);
}
