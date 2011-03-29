#pragma XOPTS(CICS)
#pragma longname   
#pragma export(initLSMQLLIB)
#pragma export(getTriggerContext)
#pragma export(reportTriggerContext)
#pragma export(initQueue)
#pragma export(openQueue)
#pragma export(readQueue)
#pragma export(writeQueue)
#pragma export(closeQueue)
#pragma export(abortServer)
#pragma export(traceParameter)
#pragma export(logMQError)
#pragma export(trimcpy)
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
/*   Purpose    - Common MQ functions Library                         */
/*   Language   - IBM C/370                                           */
/*   System     - Tested on CICS TS 2.3                               */
/*   History    - 10 August 2007  - Original Implementation           */
/*   Notes      - This library is used to share code between the      */
/*                LegStar MQ Controller (LSMQCBIN) and Handler        */
/*                (LSMQHBIN).                                         */
/*                                                                    */
/*                                                                    */
/**********************************************************************/
/*--------------------------------------------------------------------*/
/*  Constants                                                         */
/*--------------------------------------------------------------------*/
#define MODULE_NAME   "LSMQLLIB" /* used to correlate traces          */

/*--------------------------------------------------------------------*/
/*  Headers included                                                  */
/*--------------------------------------------------------------------*/
#include <stdio.h>               /* get the standard I/O library      */
#include <stdlib.h>              /* get the standard library          */
#include <string.h>              /* support string operations         */
#include <cmqc.h>                /* declarations for main MQI         */
#include "lscomdec.h"            /* legstar common declarations       */
#include "lsmqllib.h"            /* legstar common mq declarations    */
/*--------------------------------------------------------------------*/
/* Global variables                                                   */
/*--------------------------------------------------------------------*/
/*====================================================================*/
/* Initialize logging and tracing variables.                          */
/*====================================================================*/
int initLSMQLLIB(inDfheiptr, inTraceParms)
    DFHEIBLK *inDfheiptr;                     /* Pointer to eib block */
    TraceParms* inTraceParms;          /* Pointer to trace parameters */
{
    dfheiptr = inDfheiptr;
    g_pTraceParms = inTraceParms;
    return OK_CODE;
}

/*====================================================================*/
/* Retrieve the trigger context. Interprete user data.                */
/*====================================================================*/
int getTriggerContext(pTriggerMsg, pTriggerMsgLen, pMQTMParms)
    MQTM *pTriggerMsg;       /* pointer to trigger message            */
    short *pTriggerMsgLen;   /* pointer to trigger message length     */
    MQTM_Parms *pMQTMParms;  /* pointer to trigger message parameters */
{
    
    Proc_User_Data *pUserData;
    char sWorkBufferLen[WORKBUF_LEN_DIGITS + 1];
    int nWorkBufferLen = 0;
       
    /* Set default values */
    strcpy(pMQTMParms->HandlerTransID, DEFAULT_HANDLER_TRANSID);
    pMQTMParms->WorkBufferLen = DEFAULT_WORKBUF_LEN;
    pMQTMParms->TraceMode = FALSE_CODE;
    strcpy(pMQTMParms->CxID, "");
     
    EXEC CICS RETRIEVE
              INTO   (pTriggerMsg)
              LENGTH( *pTriggerMsgLen )
              RESP(g_cicsResp) RESP2(g_cicsResp2) ;

    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError(MODULE_NAME, "RETRIEVE", g_cicsResp, g_cicsResp2);
       return abortServer(CICS_ABEND_CODE);
    }
    trimcpy(pMQTMParms->RequestQueueName, pTriggerMsg->QName,
        MQ_Q_NAME_LENGTH);
    trimcpy(pMQTMParms->ProcessName, pTriggerMsg->ProcessName,
        MQ_PROCESS_NAME_LENGTH);
    trimcpy(pMQTMParms->TriggerData, pTriggerMsg->TriggerData,
        MQ_TRIGGER_DATA_LENGTH);
    pMQTMParms->ApplType = pTriggerMsg->ApplType;
    trimcpy(pMQTMParms->ApplId, pTriggerMsg->ApplId,
        MQ_PROCESS_APPL_ID_LENGTH);
    trimcpy(pMQTMParms->EnvData, pTriggerMsg->EnvData,
        MQ_PROCESS_ENV_DATA_LENGTH);
    trimcpy(pMQTMParms->UserData, pTriggerMsg->UserData,
        MQ_PROCESS_USER_DATA_LENGTH);
        
    /* If user data is not empty, interprete the content */
    if (*pMQTMParms->UserData != ' ') {
        pUserData = (Proc_User_Data*) pMQTMParms->UserData;

        /* Get handler transaction ID*/
        strncpy(pMQTMParms->HandlerTransID, pUserData->HandlerTransID,
                TRANID_NAME_LEN);

        /* Administrator has the ability to change the initial work
         * buffer size for MQ puts and gets */
        strncpy(sWorkBufferLen, pUserData->WorkBufferLen,
             WORKBUF_LEN_DIGITS);
        nWorkBufferLen = atoi(sWorkBufferLen);
        if (nWorkBufferLen > 0) {
            pMQTMParms->WorkBufferLen = nWorkBufferLen;
        }
        
        /* traces may be turned on at the trigger level */
        if (strncasecmp(pUserData->TraceOn, "true", 4) == 0) {
            pMQTMParms->TraceMode = TRUE_CODE;
        }
        
        /* Because we are not servicing a particular request yet, a
         * default trace ID can be used here */
        strncpy(pMQTMParms->CxID, pUserData->CxID, MAX_CXID_LEN);
        
    }
   
    return OK_CODE;
}

/*====================================================================*/
/* Initialize a queue structure                                       */
/*====================================================================*/
int initQueue(queue)
    MQ_Queue *queue;              /* Queue structure to initialize    */
{
    strcpy(queue->Name, "");
    queue->OpenStatus = FALSE_CODE;
    queue->HObj = MQHO_UNUSABLE_HOBJ;
}

/*====================================================================*/
/* Opens an MQ queue                                                  */
/* There is no need to connect to the queue manager because this      */
/* transaction runs under CICS.                                       */
/*====================================================================*/
int openQueue(queue, openOptions)
    MQ_Queue *queue;              /* MQ Queue to open                 */
    MQLONG   openOptions;         /* MQ open options                  */
{
    MQOD objDesc = { MQOD_DEFAULT };   /* Object to be opened         */

    if (queue->OpenStatus == TRUE_CODE) {
        return OK_CODE;
    }
    
    objDesc.ObjectType = MQOT_Q;
    strncpy(objDesc.ObjectName, queue->Name, MQ_Q_NAME_LENGTH);
    
    if (g_pTraceParms->traceMode == TRUE_CODE) {
       sprintf(g_traceMessage, "About to open queue :%s",
         queue->Name);
       traceMessage(MODULE_NAME,g_traceMessage);
    }
    
    MQOPEN(g_HQueueManager,                         
           &objDesc,                      
           openOptions,                   
           &queue->HObj,                  
           &g_mqResp,                     
           &g_mqReason);

    if( (g_mqResp != MQCC_OK) ) {
        sprintf(g_traceMessage, "MQOPEN %s", queue->Name);
        logMQError(g_traceMessage);
        return ERROR_CODE;
    }

    queue->OpenStatus = TRUE_CODE;

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       sprintf(g_traceMessage, "Opened queue :%s", queue->Name);
       traceMessage(MODULE_NAME,g_traceMessage);
    }
    return OK_CODE;
}

/*====================================================================*/
/* Reads a message from an MQ queue                                   */
/* Content is stored in g_pWorkBuffer and size given by g_DataLen.    */
/* If message is too large for current buffer, buffer is reallocated. */
/*====================================================================*/
int readQueue(queue, pMsgDesc, pWorkBufferLen, pWorkBuffer,
              pDataLen, waitInterval)
    MQ_Queue queue;            /* MQ Queue to read                    */
    MQMD *pMsgDesc;            /* Pointer to message descriptor       */ 
    MQLONG *pWorkBufferLen;    /* Pointer to work buffer total size   */ 
    void **pWorkBuffer;        /* Pointer to work buffer to read into */ 
    MQLONG *pDataLen;          /* Actual size of data read            */ 
    MQLONG waitInterval;       /* Max time to wait for message(ms)    */ 
{
    int rc = OK_CODE;             /* general purpose return code      */
    MQGMO msgOpts = { MQGMO_DEFAULT }; /* get message options         */

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       sprintf(g_traceMessage, "About to read from queue :%s",
             queue.Name);
       traceMessage(MODULE_NAME, g_traceMessage);
    }

    msgOpts.Options = MQGMO_SYNCPOINT +
                      MQGMO_WAIT   +
                      MQGMO_FAIL_IF_QUIESCING;
    msgOpts.WaitInterval = waitInterval;

    MQGET( g_HQueueManager,
           queue.HObj,
           pMsgDesc,
           &msgOpts,
           *pWorkBufferLen,
           *pWorkBuffer,
           pDataLen,
           &g_mqResp,                     
           &g_mqReason);

    if (g_mqResp != MQCC_OK) {
        
        /* If message is too large for current buffer, reallocate 
         * and try again */
        if (g_mqReason == MQRC_TRUNCATED_MSG_FAILED) {
            rc = allocHostBuffer(pWorkBuffer, *pDataLen);
            if (ERROR_CODE == rc) {
                 return rc;
            }
            *pWorkBufferLen = *pDataLen;
            return readQueue(queue, pMsgDesc, pWorkBufferLen,
                             pWorkBuffer, pDataLen, waitInterval);
        }
        /* Do not consider unavailable message as an anomaly that 
         * needs to be logged */
        if (g_mqReason != MQRC_NO_MSG_AVAILABLE) {
            sprintf(g_traceMessage, "MQGET %s", queue.Name);
            logMQError(g_traceMessage);
        } else {
            if (g_pTraceParms->traceMode == TRUE_CODE) {
               sprintf(g_traceMessage,
                   "No messages available from queue :%s",
                    queue.Name);
               traceMessage(MODULE_NAME, g_traceMessage);
            }
        }
        
        return ERROR_CODE;
    }
    
    return OK_CODE;
}

/*====================================================================*/
/* Writes a message to an MQ queue.                                   */
/* Content is taken from g_pWorkBuffer and size given by g_DataLen.   */
/*====================================================================*/
int writeQueue(queue, context, msgDesc, workBuffer, dataLen)
    MQ_Queue queue;               /* MQ Queue to write                */
    MQHOBJ context;               /* The MQ context to propagate      */
    MQMD msgDesc;                 /* Message descriptor               */ 
    void *workBuffer;             /* Work buffer to read into         */ 
    MQLONG dataLen;             /* Actual size of data read         */ 
{
    MQPMO msgOpts = { MQPMO_DEFAULT };  /* put message options        */

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       sprintf(g_traceMessage, "About to write to queue :%s",
             queue.Name);
       traceMessage(MODULE_NAME, g_traceMessage);
    }

    /* Propagate the request context */
    msgOpts.Context = context;
    
    msgOpts.Options = MQPMO_SYNCPOINT +
                      MQPMO_PASS_ALL_CONTEXT +
                      MQPMO_FAIL_IF_QUIESCING;
    
    MQPUT( g_HQueueManager,
           queue.HObj,
           &msgDesc,
           &msgOpts,
           dataLen,
           workBuffer,
           &g_mqResp,                     
           &g_mqReason);

    if( (g_mqResp != MQCC_OK) ) {
        sprintf(g_traceMessage, "MQPUT %s", queue.Name);
        logMQError(g_traceMessage);
        return ERROR_CODE;
    }
    
    return OK_CODE;
}


/*====================================================================*/
/* Closes an MQ queue                                                 */
/*====================================================================*/
int closeQueue(queue)
    MQ_Queue queue;               /* MQ Queue to close                */
{
    if (queue.OpenStatus == TRUE_CODE) {
        MQCLOSE(g_HQueueManager,
                &queue.HObj,
                MQCO_NONE,
                &g_mqResp,
                &g_mqReason);
    
        if( (g_mqResp != MQCC_OK) ) {
            sprintf(g_traceMessage, "MQCLOSE %s", queue.Name);
            logMQError(g_traceMessage);
        }
        queue.OpenStatus = FALSE_CODE;
        
        if (g_pTraceParms->traceMode == TRUE_CODE) {
           sprintf(g_traceMessage, "Closed queue :%s", queue.Name);
           traceMessage(MODULE_NAME,g_traceMessage);
        }
    }

    return OK_CODE;
}

/*====================================================================*/
/* If we can't communicate with the client, the only way to signal    */
/* an error is to abend with a meaningful error message in the CICS   */
/* log.                                                               */
/*====================================================================*/
int abortServer(abendCode)
    char* abendCode;                           /* The CICS abend code */
{
    EXEC CICS ABEND
              ABCODE(abendCode)
              NODUMP;
    return ERROR_CODE;
}

/*====================================================================*/
/* Report all parameters received from CKTI                           */
/*====================================================================*/
int reportTriggerContext(parms)
    MQTM_Parms parms;                /* Trigger message parameter set */     
{
    traceMessage(MODULE_NAME, "MQ trigger message parameters:");
    traceMessage(MODULE_NAME, "-------------------------------------");
    traceParameter("Request queue name", parms.RequestQueueName, 1);
    traceParameter("Process name", parms.ProcessName, 1);
    traceParameter("Trigger Data", parms.TriggerData, 1);
    traceParameter("ApplType", &parms.ApplType, 0);
    traceParameter("ApplId", parms.ApplId, 1);
    traceParameter("EnvData", parms.EnvData, 1);
    traceParameter("UserData", parms.UserData, 1);
    traceParameter("Handler transaction ID", parms.HandlerTransID, 1);
    traceParameter("Work buffer length", &parms.WorkBufferLen, 0);
    traceMessage(MODULE_NAME, "-------------------------------------");
}

/*====================================================================*/
/* Trace a parameter value                                            */
/*====================================================================*/
int traceParameter(parameterName, parameterValue, parameterType )
    char* parameterName;    /* Name of parameter                      */
    void* parameterValue;   /* Parameter value                        */
    int parameterType;      /* Parameter type, 0=numeric, 1=String    */
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
    return traceMessage(MODULE_NAME, trace);  
}

/*====================================================================*/
/* Produce a human readble error message for MQ errors                */
/*====================================================================*/
int logMQError(errorCommand)
    char* errorCommand;               /* The MQ command that failed   */
{

    char mqErrorMessage[257];        /* complete message for MQ error */
    char respText[200];           /* human readable return code       */
   
    /* Attempt to get a user friendly return code                     */
    switch (g_mqReason) {
      case (MQRC_ALREADY_CONNECTED):
        strcpy(respText,"Application already connected.");
        break;
      case (MQRC_CLUSTER_EXIT_LOAD_ERROR):
        strcpy(respText,"Unable to load cluster workload exit.");
        break;
      case (MQRC_ADAPTER_CONN_LOAD_ERROR):
        strcpy(respText,"Unable to load adapter connection module.");
        break;
      case (MQRC_ADAPTER_DEFS_ERROR):
        strcpy(respText,"Adapter subsystem definition module not "
        "valid.");
        break;
      case (MQRC_ADAPTER_DEFS_LOAD_ERROR):
        strcpy(respText,"Unable to load adapter subsystem definition "
        "module.");
        break;
      case (MQRC_ADAPTER_NOT_AVAILABLE):
        strcpy(respText,"Adapter not available.");
        break;
      case (MQRC_ADAPTER_SERV_LOAD_ERROR):
        strcpy(respText,"Unable to load adapter service module.");
        break;
      case (MQRC_ADAPTER_STORAGE_SHORTAGE):
        strcpy(respText,"Insufficient storage for adapter.");
        break;
      case (MQRC_ANOTHER_Q_MGR_CONNECTED):
        strcpy(respText,"Another queue manager already connected.");
        break;
      case (MQRC_API_EXIT_ERROR):
        strcpy(respText,"API exit failed.");
        break;
      case (MQRC_API_EXIT_INIT_ERROR):
        strcpy(respText,"API exit initialization failed.");
        break;
      case (MQRC_API_EXIT_TERM_ERROR):
        strcpy(respText,"API exit termination failed.");
        break;
      case (MQRC_ASID_MISMATCH):
        strcpy(respText,"Primary and home ASIDs differ.");
        break;
      case (MQRC_BUFFER_LENGTH_ERROR):
        strcpy(respText,"Buffer length parameter not valid.");
        break;
      case (MQRC_CALL_IN_PROGRESS):
        strcpy(respText,"MQI call entered before previous call "
        "complete.");
        break;
      case (MQRC_CONN_ID_IN_USE):
        strcpy(respText,"Connection identifier already in use.");
        break;
      case (MQRC_CONNECTION_BROKEN):
        strcpy(respText,"Connection to queue manager lost.");
        break;
      case (MQRC_CONNECTION_ERROR):
        strcpy(respText,"Error processing MQCONN call.");
        break;
      case (MQRC_CONNECTION_QUIESCING):
        strcpy(respText,"Connection quiescing.");
        break;
      case (MQRC_CONNECTION_STOPPING):
        strcpy(respText,"Connection shutting down.");
        break;
      case (MQRC_CRYPTO_HARDWARE_ERROR):
        strcpy(respText,"Cryptographic hardware configuration error.");
        break;
      case (MQRC_DUPLICATE_RECOV_COORD):
        strcpy(respText,"Recovery coordinator already exists.");
        break;
      case (MQRC_ENVIRONMENT_ERROR):
        strcpy(respText,"Call not valid in environment.");
        break;
      case (MQRC_HCONN_ERROR):
        strcpy(respText,"Connection handle not valid.");
        break;
      case (MQRC_KEY_REPOSITORY_ERROR):
        strcpy(respText,"Key repository not valid.");
        break;
      case (MQRC_MAX_CONNS_LIMIT_REACHED):
        strcpy(respText,"Maximum number of connections reached.");
        break;
      case (MQRC_NOT_AUTHORIZED):
        strcpy(respText,"Not authorized for access.");
        break;
      case (MQRC_OPEN_FAILED):
        strcpy(respText,"Object not opened successfully.");
        break;
      case (MQRC_Q_MGR_NAME_ERROR):
        strcpy(respText,"Queue manager name not valid or not known.");
        break;
      case (MQRC_Q_MGR_NOT_AVAILABLE):
        strcpy(respText,"Queue manager not available for connection.");
        break;
      case (MQRC_Q_MGR_QUIESCING):
        strcpy(respText,"Queue manager quiescing.");
        break;
      case (MQRC_Q_MGR_STOPPING):
        strcpy(respText,"Queue manager shutting down.");
        break;
      case (MQRC_RESOURCE_PROBLEM):
        strcpy(respText,"Insufficient system resources available.");
        break;
      case (MQRC_SECURITY_ERROR):
        strcpy(respText,"Security error occurred.");
        break;
      case (MQRC_SSL_ALREADY_INITIALIZED):
        strcpy(respText,"SSL already initialized.");
        break;
      case (MQRC_SSL_INITIALIZATION_ERROR):
        strcpy(respText,"SSL initialization error.");
        break;
       case (MQRC_STORAGE_NOT_AVAILABLE):
        strcpy(respText,"Insufficient storage available.");
        break;
       case (MQRC_UNEXPECTED_ERROR):
        strcpy(respText,"Unexpected error occurred.");
        break;
       case (MQRC_HOBJ_ERROR):
        strcpy(respText,"Object handle not valid.");
        break;
       case (MQRC_FORMAT_ERROR):
        strcpy(respText,"Message format not valid.");
        break;
       case (MQRC_NOT_OPEN_FOR_INPUT):
        strcpy(respText,"Queue not open for input.");
        break;
       case (MQRC_BUFFER_ERROR):
        strcpy(respText,"Buffer parameter not valid.");
        break;
      default:
        sprintf(respText,"reason=%d", g_mqReason);
    }
   
    sprintf(mqErrorMessage,
            "MQ call to %s failed. %s", errorCommand, respText);
    logError(MODULE_NAME, mqErrorMessage);
}

/*====================================================================*/
/*  This routine copies a cobol-like string to a c-string. The cobol- */
/*  like string is searched for trailing spaces and the first space   */
/*  of the trailing space sequence is replaced with a null character. */
/*====================================================================*/
int trimcpy(char* dest, char* src, int size) {
    int i;
    
    /* Determine the position of the first space character of the
     * trailing space sequence */
    for(i = size; i > 0 && *(src + i - 1) == ' '; i--) {
    }
    
    /* Copy all characters before the trailing space sequence (if any)*/
    if (i > 0) {
        memcpy(dest, src, i);
    }
    memset(dest + i, '\0', 1);
    return OK_CODE;
}

