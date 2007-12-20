#pragma XOPTS(CICS,SP)
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
/*   Purpose    - Controller to regulate LegStar MQ Handlers activity */
/*   Language   - IBM C/370                                           */
/*   System     - Tested on CICS TS 2.3                               */
/*   History    - 10 August 2007  - Original Implementation           */
/*   Notes      - This program is inspired by the pattern developed   */
/*                by Vicente Suarez from IBM for an MQ CICS adapter.  */
/*                                                                    */
/*                When a request queue is defined with trigger type   */
/*                of FIRST, message handlers such as LSMQHBIN are     */
/*                single threaded. If the trigger type is EVERY,      */
/*                there is a risk for the CICS region to be flooded.  */
/*                                                                    */
/*                This program assumes a trigger type of FIRST but    */
/*                will actually start several LSMQHBIN handlers if    */
/*                the request queue depth justifies so.               */
/*                                                                    */
/*                The program limits the number of simultaneous       */
/*                handlers though in order to prevent flooding.       */
/*                The limit is set at the handler TRANCLASS level.    */
/*                This means you need to create a TRANCLASS for the   */
/*                handler transaction (usually LEGQ) and set an       */
/*                appropriate MAXACTIVE limit.                        */
/*                                                                    */
/*                                                                    */
/**********************************************************************/
/*--------------------------------------------------------------------*/
/*  Constants                                                         */
/*--------------------------------------------------------------------*/
#define MODULE_NAME   "LSMQCBIN" /* used to correlate traces          */

#define COOL_OFF_TIME 1          /* This is the time the controller 
                                    will wait before inquiring on 
                                    system activity again. (seconds)  */
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
MQTM_Parms mqtmParms;              /* Trigger message parameters      */
MQLONG g_GetInhibited = 0;         /* Are get operations inhibited    */
MQLONG g_OutstandingRequests = 0;  /* Requests waiting to be serviced */
MQLONG g_OpenInputCount = 0;       /* Number of opens issued          */
int g_requestsExist = FALSE_CODE;  /* Indicates if requests are to be
                                      serviced.                       */
char g_HandlerTranclass[TRANCLASS_NAME_LEN + 1]; /* Handler transaction
                                      class name                      */
long g_ActiveHandlers;             /* Number of active handlers       */
long g_MaxActiveHandlers;          /* Maximum active handlers         */
long g_QueuedHandlers;             /* Number of queued handlers       */

/*====================================================================*/
/*  Main section                                                      */
/*====================================================================*/
void main() {
                                    
    int rc = OK_CODE;             /* general purpose return code      */
    
    /* prepare environment to service requests */
    rc = prolog();
    
    /* process request that triggered this transaction */
    while ((OK_CODE == rc) && (g_requestsExist == TRUE_CODE)) {
        rc = processRequest();
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
    
    /* If we were triggered, this means there are outstanding
     *  requests */
     g_requestsExist = TRUE_CODE;
    
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
    
    /* Initialize queue structures */
    initQueue(&g_RequestQueue);
    
    return OK_CODE;
}

/*====================================================================*/
/* This method examines the request queue depth to determine if there */
/* are requests that were not being serviced yet. If there are, then  */
/* the program evaluates if more handlers are needed. A new handler   */
/* is started if the maximum number of handlers authorized is not     */
/* reached and handlers are not being queued (system is at maxtask).  */
/*====================================================================*/
int processRequest() {
    
    int rc = OK_CODE;             /* general purpose return code      */
    if (g_traceParms.traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "Entering process request");
    }
    
    /* Get the request queue parameters of interest */
    rc = inquireRequestQueue();
    if (ERROR_CODE == rc) {
         return rc;
    }
    
    /* If Gets are inhibited, signal problem and exit */
    if (g_GetInhibited != 0) {
       sprintf(g_traceMessage,
             "Get operations are inhibited on request queue %s",
             g_RequestQueue.Name);
       logError(MODULE_NAME, g_traceMessage);
       return ERROR_CODE;
    }
    
    /* Get the handler transaction class parameters (gives the number
     * of active handlers and the max allowed) */
    rc = inquireTranclass();
    if (ERROR_CODE == rc) {
         return rc;
    }
    
    /* If no outstanding requests, and no handlers are active then
     * there is no activity, just return */
    if (g_OutstandingRequests == 0 && g_ActiveHandlers == 0) {
        g_requestsExist = FALSE_CODE;
        return OK_CODE;
    }
    
    /* Don't start new handlers if system is overloaded.
     * A system is considered overloaded if:
     * 1. Handlers are being queued by CICS
     * 2. There are more active handlers than opens on the request 
     *    queue (This tends to indicate that handlers are stalled).
     * 3. The maximum number of active handlers is already reached. */
    if (g_QueuedHandlers == 0
            && (g_ActiveHandlers <= g_OpenInputCount)
            && (g_ActiveHandlers < g_MaxActiveHandlers)) {
        
        EXEC CICS START
             TRANSID     (g_HandlerTransID)
             FROM        (g_triggerMsg)
             LENGTH      (g_triggerMsgLen)
             RESP        (g_cicsResp) RESP2(g_cicsResp2);                  
                                               
        if (g_cicsResp != DFHRESP(NORMAL)) {
            logCicsError(MODULE_NAME, "START",g_cicsResp,g_cicsResp2);
            return ERROR_CODE;
        }
    } else {
        /* Reaching the maximum number of handlers allowed is normal
         * activity but if handlers are unproductive (not opening the
         * request queue) or being queued by CICS (maxtask) get out
         * of the way. */
         if (g_QueuedHandlers > 0 ||
             g_ActiveHandlers > g_OpenInputCount ) {
            logError(MODULE_NAME,
             "System overloaded. Stopping controller.");
            return ERROR_CODE;
         }
    }
    
    /* Give some time for actual work to occur */
    EXEC CICS DELAY FOR SECONDS(COOL_OFF_TIME);

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
    return OK_CODE;
}

/*====================================================================*/
/* Issue an MQINQ against the request queue.                          */
/* This will detect the current queue depth (how many requests are    */
/* waiting to be serviced).                                           */
/* This also determines if get operations are inhibited.              */
/*====================================================================*/
int inquireRequestQueue() {
    
    int rc = OK_CODE;             /* general purpose return code      */
    MQLONG   openOptions;              /* MQ open options             */
    MQLONG   selectors[3];
    MQLONG   intAttrs[3];
    MQCHAR   charAttrs;
    MQLONG   selectorCount = 3;
    MQLONG   intAttrCount  = 3;
    MQLONG   charAttrLen   = 0;
    
    if (g_traceParms.traceMode == TRUE_CODE) {
       sprintf(g_traceMessage, "About to inquire queue :%s",
             g_RequestQueue.Name);
       traceMessage(MODULE_NAME,g_traceMessage);
    }

    /* Open the request queue (if not already opened) */
    openOptions = MQOO_INQUIRE +     
                  MQOO_FAIL_IF_QUIESCING;
    
    rc = openQueue(&g_RequestQueue, openOptions);
    if (ERROR_CODE == rc) {
         return rc;
    }
    
    selectors[0] = MQIA_INHIBIT_GET ;
    selectors[1] = MQIA_CURRENT_Q_DEPTH;
    selectors[2] = MQIA_OPEN_INPUT_COUNT;
    
    MQINQ( g_HQueueManager,
        g_RequestQueue.HObj,
        selectorCount,
        selectors,
        intAttrCount,
        intAttrs,
        charAttrLen,
        &charAttrs,
        &g_mqResp,                     
        &g_mqReason);

    if (g_mqResp != MQCC_OK) {
        sprintf(g_traceMessage, "MQINQ %s", g_RequestQueue.Name);
        logMQError(g_traceMessage);
        return ERROR_CODE;
    }
    
    g_GetInhibited = intAttrs[0];
    g_OutstandingRequests = intAttrs[1];
    g_OpenInputCount = intAttrs[2];
    
    if (g_traceParms.traceMode == TRUE_CODE) {
        sprintf(g_traceMessage, "Inquire on queue :%s:",
             g_RequestQueue.Name);
        traceMessage(MODULE_NAME,g_traceMessage);
        traceInquireQueueParameters();
    }

    return OK_CODE;
}

/*====================================================================*/
/* Retrieve the handler transaction class.                            */
/* Determine how many such transactions are active, how many are      */
/* queued (an indication that there are already too many such         */
/* transactions) and the maximum active transactions allowed.         */
/*====================================================================*/
int inquireTranclass() {
    
    if (g_traceParms.traceMode == TRUE_CODE) {
       sprintf(g_traceMessage, "About to inquire on transaction :%s",
             g_HandlerTransID);
       traceMessage(MODULE_NAME,g_traceMessage);
    }

    EXEC CICS INQUIRE TRANSACTION(g_HandlerTransID)
         TRANCLASS   (g_HandlerTranclass)
         RESP        (g_cicsResp) RESP2(g_cicsResp2);                  
                                           
    if (g_cicsResp != DFHRESP(NORMAL)) {
        logCicsError(MODULE_NAME, "INQUIRE TRANSACTION", 
                     g_cicsResp, g_cicsResp2);
        return ERROR_CODE;
    }

    if (g_traceParms.traceMode == TRUE_CODE) {
       sprintf(g_traceMessage,
             "About to inquire on transaction class :%s",
             g_HandlerTranclass);
       traceMessage(MODULE_NAME,g_traceMessage);
    }

    EXEC CICS INQUIRE TRANCLASS(g_HandlerTranclass)
         ACTIVE      (g_ActiveHandlers)
         MAXACTIVE   (g_MaxActiveHandlers)
         QUEUED      (g_QueuedHandlers)
         RESP        (g_cicsResp) RESP2(g_cicsResp2);                  
                                           
    if (g_cicsResp != DFHRESP(NORMAL)) {
        logCicsError(MODULE_NAME, "INQUIRE TRANCLASS", 
                     g_cicsResp, g_cicsResp2);
        return ERROR_CODE;
    }

    if (g_traceParms.traceMode == TRUE_CODE) {
        sprintf(g_traceMessage, "Inquire on transaction :%s:",
             g_HandlerTransID);
        traceMessage(MODULE_NAME,g_traceMessage);
        traceInquireTransactionParameters();
    }

    return OK_CODE;
}

/*====================================================================*/
/* Trace inquire queue parameters                                     */
/*====================================================================*/
int traceInquireQueueParameters() {
    traceMessage(MODULE_NAME, "-------------------------------------");
    traceParameter("Get Inhibited", &g_GetInhibited, 0);
    traceParameter("Outstanding Requests", &g_OutstandingRequests, 0);
    traceParameter("Open input count", &g_OpenInputCount, 0);
    traceMessage(MODULE_NAME, "-------------------------------------");
}

/*====================================================================*/
/* Trace inquire transaction parameters                               */
/*====================================================================*/
int traceInquireTransactionParameters() {
    traceMessage(MODULE_NAME, "-------------------------------------");
    traceParameter("Handlers transaction class", g_HandlerTranclass, 1);
    traceParameter("Active Handlers", &g_ActiveHandlers, 0);
    traceParameter("Maximum active Handlers", &g_MaxActiveHandlers, 0);
    traceParameter("Queued Handlers", &g_QueuedHandlers, 0);
    traceMessage(MODULE_NAME, "-------------------------------------");
}

