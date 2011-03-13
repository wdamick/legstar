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
/*   Purpose    - Handler for MQ requests for CICS DPL link           */
/*   Language   - IBM C/370                                           */
/*   System     - Tested on CICS TS 2.3                               */
/*   History    - 10 August 2007  - Original Implementation           */
/*   Notes      - This program provides MQ clients with the           */
/*                capability to invoke a CICS program passing         */
/*                data as a binary MQ message content.                */
/*                                                                    */
/*                Client has the responsibility to create the request */
/*                in the format expected by the linked program.       */
/*                This means that characters must be EBCDIC encoded   */
/*                and numeric data must be encoded in characters,     */
/*                binary, or packed-decimal.                          */
/*                                                                    */
/*                A request is entirely described by the binary       */
/*                MQ payload. That payload is devided into parts.     */
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
/*                Traces can be turned on by setting characters of    */
/*                MQ process user data to "true ", the next 16        */
/*                characters from user data will be used as the trace */
/*                ID. See sample MQDEFJCL for instructions on the     */
/*                expected user data format.                          */
/*                In addition, individual requests can turn traces on */
/*                by passing value "true " in the ApplIdentityData,   */
/*                part of the message context.                        */
/*                The trace ID used for requests is the correlation   */
/*                ID first 16 characters.                             */
/*                                                                    */
/*                If the request fails, the reply message will be an  */
/*                error message. A rollback is systematically issued. */
/*                As a result of the rollback, the faulty request     */
/*                stays in the request queue. You can control how     */
/*                many times the system should retry processing this  */
/*                request using the BOTHRESH request queue parameter. */
/*                When this threshold is reached, the faulty request  */
/*                is moved to the backout queue (BOQNAME) if you      */
/*                provide one.                                        */
/*                                                                    */
/*                If request succeeds, the response is formatted      */
/*                exactly like the request.                           */
/*                                                                    */
/**********************************************************************/
/*--------------------------------------------------------------------*/
/*  Constants                                                         */
/*--------------------------------------------------------------------*/
#define MODULE_NAME   "LSMQHBIN" /* used to correlate traces          */
#define GET_WAIT_TIMEOUT 1000 /* Maximum time (ms) to wait for message*/
#define POISONOUS_PROCESSED_CODE 17 /* Special return code when a
                                 poisonous message is requeued        */

/*--------------------------------------------------------------------*/
/*  Headers included                                                  */
/*--------------------------------------------------------------------*/
#include <stdio.h>               /* get the standard I/O library      */
#include <stdlib.h>              /* get the standard library          */
#include <string.h>              /* support string operations         */
#include <cmqc.h>                /* declarations for main MQI         */
#include "lscomdec.h"            /* legstar common declarations       */
#include "lsmqllib.h"            /* legstar common mq declarations    */
#include "lsmsglib.h"            /* legstar messaging include file    */

/*--------------------------------------------------------------------*/
/* Global variables                                                   */
/*--------------------------------------------------------------------*/
MQTM_Parms mqtmParms;              /* Trigger message parameters      */
MQLONG g_CodedCharSetId;                  /* character set id         */
char g_CorrelId[MQ_CORREL_ID_LENGTH + 1]; /* MQ correlation ID        */
char g_MsgId[MQ_MSG_ID_LENGTH + 1];       /* MQ message ID            */
char g_ReplyToQ[MQ_Q_NAME_LENGTH + 1];    /* queue to use for reply   */
char g_ReplyToQMgr[MQ_Q_MGR_NAME_LENGTH + 1]; /* queue manager 4 reply*/
char g_UserIdentifier[MQ_USER_ID_LENGTH + 1]; /* user identifier      */
char g_ApplIdentityData[MQ_APPL_IDENTITY_DATA_LENGTH + 1];
                                           /* Application data        */
char g_Format[MQ_FORMAT_LENGTH + 1];       /* MQ message format       */
int g_processingMsg = FALSE_CODE;          /* Indicates if a message
                                              is being processed.     */
MQLONG g_BackoutThreshold = 0; /* Maximum times a backouted message will
                     be reprocessed before it is considered poisonous */
MQLONG g_BackoutCount;                     /* Backout counter         */
MQLONG g_MsgDataLen = 0;          /* actual message data length       */
void *g_pMsgData = NULL;          /* pointer to actual message data   */

/*====================================================================*/
/*  Main section                                                      */
/*====================================================================*/
void main() {
                                    
    int rc = OK_CODE;             /* general purpose return code      */
    
    /* prepare environment to service requests */
    rc = prolog();
    
    /* process all requests on queue, including any poisonous
     * messages */
    while (OK_CODE == rc || POISONOUS_PROCESSED_CODE == rc) {
        
        rc = processRequest();
        
        /* If an error is encountered while processing a message, we
         * need to notify the client  */
        if ((OK_CODE != rc) && (g_processingMsg == TRUE_CODE)) {
            
            /* undo all updates that the linked program might have
             * performed. A consequence of the rollback is that the
             * mq request message is automatically requeued which
             * will probably trigger another instance of this handler */
            Rollback();
            
            /* do our best effort to notify client*/
            sendErrorMessage();
        }
        
        /* Commit will actually remove the mq message that was processed
         * here. Each request is a new UOW. */
        Commit();
    }
    
    epilog();
    
    if (g_traceParms.traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "Main ended");
        traceMessage(MODULE_NAME,
        "==============================================");
    }
    
    /* end of processing                                              */
    EXEC CICS RETURN;
}                                 

/*====================================================================*/
/* This transaction is triggered by the CICS adapter. During prolog,  */
/* the context passed by the adapter is recovered and various         */
/* initializations take place.                                        */
/*====================================================================*/
int prolog() {
    
    int rc = OK_CODE;             /* general purpose return code      */
    char StartCode[2];
    
    /* Initialize tracing and get eib addressability */
    init();

    /* This transaction should be started with data */
    EXEC CICS ASSIGN STARTCODE(&StartCode);
    if (strncmp(StartCode, "SD", sizeof(StartCode)) != 0) {
       logError(MODULE_NAME, "Transaction has invalid start code");
       return ERROR_CODE;
    }

    /* Extract trigger context data (includes request queue name)    */
    rc = getTriggerContext(
            &g_triggerMsg, &g_triggerMsgLen, &mqtmParms);
    if (ERROR_CODE == rc) {
         return rc;
    }
    strcpy(g_RequestQueue.Name, mqtmParms.RequestQueueName);
    strcpy(g_HandlerTransID, mqtmParms.HandlerTransID);
    g_WorkBufferLen = mqtmParms.WorkBufferLen;
    g_traceParms.traceMode = mqtmParms.TraceMode;
    strcpy(g_traceParms.CxID, mqtmParms.CxID);

    if (g_traceParms.traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME,
            "==============================================");
        traceMessage(MODULE_NAME, "Main started");
        reportTriggerContext(mqtmParms);
    }
    
    /* Get poisonous message parameters from the request queue */
    rc = inquireRequestQueue();
    if (ERROR_CODE == rc) {
         return rc;
    }
    
    /* Acquire work buffer memory (it will be automatically freed by
     * CICS when transaction terminates) */
    rc = allocHostBuffer(&g_pWorkBuffer, g_WorkBufferLen);
    if (ERROR_CODE == rc) {
         return rc;
    }
    
    return OK_CODE;
 
}

/*====================================================================*/
/* Initialize logging and get eib addressability                      */
/*====================================================================*/
int init() {
    
    memset(g_traceMessage, 0, sizeof(g_traceMessage));
    
    /* Initialize tracing variables */
    memset(g_traceParms.CxID, 0, sizeof(g_traceParms.CxID));
    g_traceParms.traceMode = FALSE_CODE;
    memset(g_traceParms.formattedErrorMessage, 0,
           sizeof(g_traceParms.formattedErrorMessage));
  
    /* Get commarea address and EIB bloc address                      */
    EXEC CICS ADDRESS
              COMMAREA (ca_ptr)
              EIB      (dfheiptr)
              RESP     (g_cicsResp) RESP2(g_cicsResp2);                  
                                               
    if (g_cicsResp != DFHRESP(NORMAL)) {
       return abortServer(CICS_ABEND_CODE);
    }
    
    /* Initialize logging module */
    initLog(dfheiptr, &g_traceParms);
    initLSMQLLIB(dfheiptr, &g_traceParms);
    initLSMSGLIB(dfheiptr, &g_traceParms);
    
    
    /* Initialize queue structures */
    initQueue(&g_RequestQueue);
    initQueue(&g_ResponseQueue);
    initQueue(&g_BackoutQueue);
    
    return OK_CODE;
}

/*====================================================================*/
/* This method processes a single request (if any).                   */
/*====================================================================*/
int processRequest() {
    
    int rc = OK_CODE;             /* general purpose return code      */
    int rcSave;                            /* Return code save area   */
    CICSProgramDesc programDesc;           /* Program description     */
    char savedFormattedErrorMessage[MAX_FORM_MSG_LEN]; /* Original 
                                           error message save area.   */
                                           
    LS_Message lsRequest;         /* Request message structure        */
    LS_Message lsResponse;        /* Response message structure       */
    
    if (g_traceParms.traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "Entering process request");
    }
    
    /* Initialize message structures */
    initLSMessage(&lsRequest);
    initLSMessage(&lsResponse);
    
    /* No message is being processed yet */
    g_processingMsg = FALSE_CODE;
    
    rc = readRequestQueue();
    if (OK_CODE != rc) {
         return rc;
    }

    /* From now on, we have a message we need to deal with */
    g_processingMsg = TRUE_CODE;
    
    rc = hostToMessage(g_pMsgData, g_MsgDataLen,
                             &lsRequest.message);
    if (OK_CODE == rc) {
        rc = invokeProgram(dfheiptr,
                           &g_traceParms,
                           &programDesc,
                           &lsRequest.message,
                           &lsResponse.message);
        if (OK_CODE == rc) {
           rc = messageToBuffer((char **)&g_pWorkBuffer,
                                (int *) &g_WorkBufferLen,
                                &lsResponse.message);
           if (rc > 0) {
                /* Save the total size of the formatted response */
                g_DataLen = rc;
                rc = writeResponseQueue();
           }
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
                     &lsRequest.message,
                     &lsResponse.message);
    
    /* Report free errors only if there was no previous errors */
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
/* Free resources acquired during processing. Best effort to free     */
/* all resources even if errors are encountered along the way.        */
/* There is no need to disconnect from the queue manager because this */
/* transaction runs under CICS.                                       */
/*====================================================================*/
int epilog() {

    closeQueue(g_RequestQueue);
    closeQueue(g_ResponseQueue);
    closeQueue(g_BackoutQueue);
    return OK_CODE;
}

/*====================================================================*/
/* Issue an MQINQ against the request queue.                          */
/* This will detect the Backound threshold and the Backout queue name */
/*====================================================================*/
int inquireRequestQueue() {
    
    int rc = OK_CODE;             /* general purpose return code      */
    MQLONG   openOptions;              /* MQ open options             */
    MQLONG   selectorCount = 2;
    MQLONG   selectors[2];
    MQLONG   intAttrCount  = 1;
    MQLONG   intAttrs[1];
    MQLONG   charAttrLen   = MQ_Q_NAME_LENGTH;
    MQCHAR   charAttrs[MQ_Q_NAME_LENGTH];
    
    if (g_traceParms.traceMode == TRUE_CODE) {
       sprintf(g_traceMessage, "About to inquire queue :%s",
             g_RequestQueue.Name);
       traceMessage(MODULE_NAME,g_traceMessage);
    }

    /* Open the request queue (if not already opened) */
    openOptions = MQOO_INPUT_SHARED +
                  MQOO_INQUIRE +   
                  MQOO_SAVE_ALL_CONTEXT +
                  MQOO_FAIL_IF_QUIESCING;
    
    rc = openQueue(&g_RequestQueue, openOptions);
    if (ERROR_CODE == rc) {
         return rc;
    }
    
    selectors[0] = MQIA_BACKOUT_THRESHOLD;
    selectors[1] = MQCA_BACKOUT_REQ_Q_NAME;
     
    MQINQ( g_HQueueManager,
        g_RequestQueue.HObj,
        selectorCount,
        selectors,
        intAttrCount,
        intAttrs,
        charAttrLen,
        charAttrs,
        &g_mqResp,                     
        &g_mqReason);

    if (g_mqResp != MQCC_OK) {
        sprintf(g_traceMessage, "MQINQ %s", g_RequestQueue.Name);
        logMQError(g_traceMessage);
        return ERROR_CODE;
    }
 

    g_BackoutThreshold = intAttrs[0];
    strncpy(g_BackoutQueue.Name, charAttrs, MQ_Q_NAME_LENGTH);
    
    if (g_traceParms.traceMode == TRUE_CODE) {
        sprintf(g_traceMessage, "Inquire on queue :%s:",
             g_RequestQueue.Name);
        traceMessage(MODULE_NAME,g_traceMessage);
        traceInquireQueueParameters();
    }

    return OK_CODE;
}

/*====================================================================*/
/* Issue an MQGET against the request queue.                          */
/* With the Syncpoint option, the message becomes unavailable for     */
/* other instances of this transaction but is deleted only when the   */
/* UOW is committed.                                                  */
/* This populates g_MsgDataLen and g_pMsgData global variables that   */
/* point to the actual LegStar data within the MQ message. This       */
/* accounts for RFH2 headers that may be present.                     */
/*====================================================================*/
int readRequestQueue() {
    
    int rc = OK_CODE;             /* general purpose return code      */
    MQMD msgDesc = { MQMD_DEFAULT };   /* message descriptor          */
    MQLONG   openOptions;              /* MQ open options             */
    PMQRFH2  pRfh2;               /* pointer to RFH2 header           */
    
    if (g_traceParms.traceMode == TRUE_CODE) {
       sprintf(g_traceMessage, "About to get from queue :%s",
             g_RequestQueue.Name);
       traceMessage(MODULE_NAME,g_traceMessage);
    }

    /* Open the request queue (if not already opened) */
    openOptions = MQOO_INPUT_SHARED +     
                  MQOO_SAVE_ALL_CONTEXT +
                  MQOO_FAIL_IF_QUIESCING;
    
    rc = openQueue(&g_RequestQueue, openOptions);
    if (ERROR_CODE == rc) {
         return rc;
    }
    
    /* MsgId and CorrelId are both input and output parms. A previous
     * get might have initialized them which would result in an unwanted
     * selective get here. */
    memcpy( msgDesc.MsgId, MQMI_NONE, sizeof( msgDesc.MsgId ) );
    memcpy( msgDesc.CorrelId, MQCI_NONE, sizeof( msgDesc.CorrelId ) );
    
    rc = readQueue(g_RequestQueue, &msgDesc, &g_WorkBufferLen,
                   &g_pWorkBuffer, &g_DataLen, GET_WAIT_TIMEOUT);
    if (OK_CODE != rc) {
        return rc;
    }
    
    storeMessageDescriptor(&msgDesc);

    /* Check if request is asking for traces */
    if (strncasecmp(g_ApplIdentityData, "true", 4) == 0) {
        g_traceParms.traceMode = TRUE_CODE;
        /* Starting from here, we use the next 16 characters of the mq
         * application identity data as the trace ID. */
        strncpy(g_traceParms.CxID, g_ApplIdentityData + 5, 16);
    }

    
    if (g_traceParms.traceMode == TRUE_CODE) {
        traceMQMessageDescriptor();
    }
    
    /* Check if this is a poisonous message */
    if (g_BackoutCount > g_BackoutThreshold) {
        return poisonousMsgHandling();
    }
    
    /* Make sure we have a response queue name */
    if (strlen(g_ReplyToQ) == 0) {
        logError(MODULE_NAME, "Request is missing ReplyToQ");
        return ERROR_CODE;
    }
    strcpy(g_ResponseQueue.Name, g_ReplyToQ);
    
    /* If incoming message has RFH header adjust pointer to data and
     * data length to point to the data itself (which follows RFH).
     */
    if (strncmp(g_Format, "MQHRF2", 6) == 0) {
        pRfh2 = g_pWorkBuffer;
        g_pMsgData = (char*)g_pWorkBuffer + pRfh2->StrucLength;
        g_MsgDataLen = g_DataLen - pRfh2->StrucLength;
    } else {
        g_pMsgData = g_pWorkBuffer;
        g_MsgDataLen = g_DataLen;
    }
    
    if (g_traceParms.traceMode == TRUE_CODE) {
        traceMQMessageContent();
    }

    return OK_CODE;
}

/*====================================================================*/
/* Store the response in the MQ response queue                        */
/* Response data is expected to come from g_pWorkBuffer and its size  */
/* given by g_DataLen.                                                */
/*====================================================================*/
int writeResponseQueue() {
    
    int rc = OK_CODE;             /* general purpose return code      */
    MQMD msgDesc = { MQMD_DEFAULT };    /* message descriptor         */
    MQLONG   openOptions;              /* MQ open options             */
    
    if (g_traceParms.traceMode == TRUE_CODE) {
       sprintf(g_traceMessage, "About to write queue :%s",
             g_ResponseQueue.Name);
       traceMessage(MODULE_NAME,g_traceMessage);
    }
    
    /* Open the response queue (if not already opened) */
    openOptions = MQOO_OUTPUT +     
                  MQOO_PASS_ALL_CONTEXT +
                  MQOO_FAIL_IF_QUIESCING; 
                                          
    rc = openQueue(&g_ResponseQueue, openOptions);
    if (OK_CODE != rc) {
        return rc;
    }
    
    msgDesc.MsgType = MQMT_REPLY;
    msgDesc.Report  = MQRO_NONE;
    memset( msgDesc.ReplyToQ,    ' ', sizeof( msgDesc.ReplyToQ ) );
    memset( msgDesc.ReplyToQMgr, ' ', sizeof( msgDesc.ReplyToQMgr ) );
    memcpy( msgDesc.MsgId, MQMI_NONE, sizeof( msgDesc.MsgId ) );

    /* Upon reply, if the request had a correlation ID, we propagate
     * it back. If there was no correlation ID, we propagate the 
     * request message ID. */
    if (strncmp(g_CorrelId, MQCI_NONE, MQ_CORREL_ID_LENGTH) == 0) {
        memcpy( msgDesc.CorrelId, g_MsgId, MQ_CORREL_ID_LENGTH );
    } else {
        memcpy( msgDesc.CorrelId, g_CorrelId, MQ_CORREL_ID_LENGTH );
    }

    rc = writeQueue(g_ResponseQueue, g_RequestQueue.HObj, msgDesc,
                    g_pWorkBuffer, g_DataLen);
    if (OK_CODE != rc) {
        return rc;
    }

    if (g_traceParms.traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "MQ Message written");
        storeMessageDescriptor(&msgDesc);
        traceMQMessageDescriptor();
        traceMQMessageContent();
    }

    return OK_CODE;
}

/*====================================================================*/
/* When a request message generated an error, a rollback results in   */
/* a requeue of the faulty message which will again trigger this      */
/* handler. Sometimes the error is transcient and will not happen the */
/* second time. But often the same request will again generate the    */
/* same error. To prevent an infinite loop we have special processing */
/* for these "poisonous" messages.                                    */
/*====================================================================*/
int poisonousMsgHandling() {

    int rc = OK_CODE;             /* general purpose return code      */
    
    sprintf(g_traceMessage,
        "Message with correlation id %s has been reprocessed more than"
        " %d times.", g_CorrelId, g_BackoutThreshold);
    logError(MODULE_NAME, g_traceMessage);
    if (strlen(g_BackoutQueue.Name) > 0
          && *g_BackoutQueue.Name != ' ') {
        sprintf(g_traceMessage,
            "It will be moved to backout queue %s.",
            g_BackoutQueue.Name);
        logError(MODULE_NAME, g_traceMessage);
        rc = saveErrorRequest();
        if (OK_CODE != rc) {
            logError(MODULE_NAME, "Unable to save faulty message."
            " It will be discarded");
            return rc;
        }
    } else {
        logError(MODULE_NAME, "It will be discarded");
    }
    
    return POISONOUS_PROCESSED_CODE;
}

/*====================================================================*/
/* An error was encountered, place a formatted error message in the   */
/* reply queue.                                                       */
/*====================================================================*/
int sendErrorMessage() {
    
    int rc = OK_CODE;             /* general purpose return code      */
    
    if (g_traceParms.traceMode == TRUE_CODE) {
       sprintf(g_traceMessage, "About to write error message :%s",
             g_traceParms.formattedErrorMessage);
       traceMessage(MODULE_NAME,g_traceMessage);
    }
    
    /* If there is no response queue, place message in CICS log
     * and return */
    if (strlen(g_ResponseQueue.Name) == 0) {
        return ERROR_CODE;
    }
    
    /* move the error message to io buffer. We assume the buffer is
     * always large enough to hold the error message  */
    g_DataLen = strlen(g_traceParms.formattedErrorMessage);
    strncpy(g_pWorkBuffer, g_traceParms.formattedErrorMessage,
        g_DataLen);
     
    /* try to send the reply, if unsuccesful abort this transaction */
    rc = writeResponseQueue();
    if (OK_CODE != rc) {
        return abortServer(CICS_ABEND_CODE);
    }

    return OK_CODE;
}

/*====================================================================*/
/* Saves a request message which generated errors in the backout      */
/* queue.                                                             */
/*====================================================================*/
int saveErrorRequest() {

    int rc = OK_CODE;             /* general purpose return code      */
    MQMD msgDesc = { MQMD_DEFAULT };   /* message descriptor          */
    MQLONG   openOptions;              /* MQ open options             */
    
    if (g_traceParms.traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "About to save error request");
    }

    /* Make sure this is the exact same message */
    memcpy( msgDesc.MsgId, MQMI_NONE, sizeof( msgDesc.MsgId ) );
    strncpy(msgDesc.MsgId, g_MsgId, strlen(g_MsgId));
    memcpy( msgDesc.CorrelId, MQCI_NONE, sizeof( msgDesc.CorrelId ) );
    strncpy(msgDesc.CorrelId, g_CorrelId, strlen(g_CorrelId));
    
    /* Open the backout queue (if not already opened) */
    openOptions = MQOO_OUTPUT +     
                  MQOO_PASS_ALL_CONTEXT +
                  MQOO_FAIL_IF_QUIESCING; 
                                          
    rc = openQueue(&g_BackoutQueue, openOptions);
    if (OK_CODE != rc) {
        return rc;
    }
    
    /* Store faulty request in backout queue */
    msgDesc.MsgType = MQMT_REPLY;
    msgDesc.Report  = MQRO_NONE;
    memset( msgDesc.ReplyToQ,    ' ', sizeof( msgDesc.ReplyToQ ) );
    memset( msgDesc.ReplyToQMgr, ' ',
         sizeof( msgDesc.ReplyToQMgr ) );

    rc = writeQueue(g_BackoutQueue, g_RequestQueue.HObj, msgDesc,
                    g_pWorkBuffer, g_DataLen);
    if (OK_CODE != rc) {
        return rc;
    }
 
    if (g_traceParms.traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "Error request saved");
        storeMessageDescriptor(&msgDesc);
        traceMQMessageDescriptor();
        traceMQMessageContent();
    }

    return OK_CODE;
}

/*====================================================================*/
/* This will undo all updates in the current unit of work.            */
/*====================================================================*/
int Rollback() {
    
    if (g_traceParms.traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "Issuing rollback");
    }

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
/* This will confirm all updates in the current unit of work.         */
/*====================================================================*/
int Commit() {

    if (g_traceParms.traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "Issuing commit");
    }
    
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
/* Store message descriptor attributes in global traceable variables  */
/*====================================================================*/
int storeMessageDescriptor(MQMD* pMsgDesc) {

    memset(g_CorrelId, 0, sizeof(g_CorrelId));
    memset(g_MsgId, 0, sizeof(g_MsgId));
    memset(g_ReplyToQ, 0, sizeof(g_ReplyToQ));
    memset(g_ReplyToQMgr, 0, sizeof(g_ReplyToQMgr));
    memset(g_UserIdentifier, 0, sizeof(g_UserIdentifier));
    memset(g_ApplIdentityData, 0, sizeof(g_ApplIdentityData));
    memset(g_Format, 0, sizeof(g_Format));

    g_CodedCharSetId = pMsgDesc->CodedCharSetId;
    trimcpy(g_CorrelId, pMsgDesc->CorrelId,
         MQ_CORREL_ID_LENGTH);
    trimcpy(g_MsgId, pMsgDesc->MsgId,
         MQ_MSG_ID_LENGTH);
    trimcpy(g_ReplyToQ, pMsgDesc->ReplyToQ,
         MQ_Q_NAME_LENGTH);
    trimcpy(g_ReplyToQMgr, pMsgDesc->ReplyToQMgr,
         MQ_Q_MGR_NAME_LENGTH);
    trimcpy(g_UserIdentifier, pMsgDesc->UserIdentifier,
         MQ_USER_ID_LENGTH);
    trimcpy(g_ApplIdentityData, pMsgDesc->ApplIdentityData,
         MQ_APPL_IDENTITY_DATA_LENGTH);
    trimcpy(g_Format, pMsgDesc->Format,
         MQ_FORMAT_LENGTH);
    g_BackoutCount = pMsgDesc->BackoutCount;
}

/*====================================================================*/
/* Trace inquire queue parameters                                     */
/*====================================================================*/
int traceInquireQueueParameters() {
    traceMessage(MODULE_NAME, "-------------------------------------");
    traceParameter("Backout threshold", &g_BackoutThreshold, 0);
    traceParameter("Backout queue name", g_BackoutQueue.Name, 1);
    traceMessage(MODULE_NAME, "-------------------------------------");
}

/*====================================================================*/
/* Trace an MQ Message descriptor                                     */
/*====================================================================*/
int traceMQMessageDescriptor() {
    traceMessage(MODULE_NAME, "MQ Message descriptor:");
    traceMessage(MODULE_NAME, "----------------------");
    traceParameter("Format", g_Format, 1);
    traceParameter("CodedCharSetId", g_CodedCharSetId, 0);
    traceMQID("MsgId", g_MsgId);
    traceMQID("CorrelId", g_CorrelId);
    traceParameter("ReplyToQ", g_ReplyToQ, 1);
    traceParameter("ReplyToQMgr", g_ReplyToQMgr, 1);
    traceParameter("UserIdentifier", g_UserIdentifier, 1);
    traceParameter("ApplIdentityData", g_ApplIdentityData, 1);
    traceParameter("BackoutCount", &g_BackoutCount, 0);
    traceMessage(MODULE_NAME,
         "-----------------------------------------------");
}

/*====================================================================*/
/* Trace an MQ Message content                                        */
/*====================================================================*/
int traceMQMessageContent() {
    traceMessage(MODULE_NAME, "MQ Message content:");
    traceMessage(MODULE_NAME, "-------------------");
    sprintf(g_traceMessage, "MQ Message data length :%d", g_DataLen);
    traceMessage(MODULE_NAME, g_traceMessage);
    traceMessage(MODULE_NAME, "MQ Message content:");
    traceData(MODULE_NAME, g_pWorkBuffer, g_DataLen);
    sprintf(g_traceMessage, "LegStar Message data length :%d",
        g_MsgDataLen);
    traceMessage(MODULE_NAME, g_traceMessage);
    traceMessage(MODULE_NAME, "LegStar Message content:");
    traceData(MODULE_NAME, g_pMsgData, g_MsgDataLen);
    traceMessage(MODULE_NAME,
        "-----------------------------------------------");
}

/*====================================================================*/
/* Message IDs and Correlation IDs are 24 bytes arrays                */
/*====================================================================*/
int traceMQID(char* idLabel, char* id) {
    char idHex[49];
    int i;
    char dumpChar[3];
    memset(idHex, 0, sizeof(idHex));
    for (i = 0; i < 24; i++) {
        sprintf(dumpChar,"%.2X",id[i] & 0xff);
        strcat(idHex, dumpChar);
    }
    traceParameter(idLabel, idHex, 1);
}


