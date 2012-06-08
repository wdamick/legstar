#pragma XOPTS(CICS)
#pragma longname
#pragma export(linkChannel)
#pragma export(freeChannel)
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
/*   Author     - Fady Moussallam  (fady@legsem.com)                  */
/*   Purpose    - LINK CHANNEL CONTAINER Driver                       */
/*   Language   - IBM C/370                                           */
/*   System     - Tested on CICS TS 3.1                               */
/*   History    - 09 May 2007  - Original Implementation              */
/*   Notes      - This sub-program (DLL) implements the capability to */
/*                invoke a CICS program passing data via CONTAINERS.  */
/*                This module supports multiple input and multiple    */
/*                output parts.                                       */
/*                                                                    */
/*                The output CONTAINER names are passed to this       */
/*                module as an array of fixed size strings.           */
/*                                                                    */
/*                Input data is received as an array of Message parts */
/*                                                                    */
/*                Reply:                                              */
/*                 Abnormal termination result in a -1 return code.   */
/*                 Further description of the error is found in the   */
/*                 tracing structure.                                 */
/*                                                                    */
/*                Output data is produced as an array of Message parts*/
/*                                                                    */
/**********************************************************************/
/*--------------------------------------------------------------------*/
/*  Headers included                                                  */
/*--------------------------------------------------------------------*/
#include <stdio.h>               /* get the standard I/O library      */
#include <stdlib.h>              /* get the standard library          */
#include <string.h>              /* support string operations         */
#include "lscomdec.h"            /* legstar common declarations       */
#include "lslnkdec.h"            /* legstar invoker declarations      */

/*--------------------------------------------------------------------*/
/*  Constants                                                         */
/*--------------------------------------------------------------------*/
#define MODULE_NAME   "LSLNKCTN" /* used to correlate traces          */

/*====================================================================*/
/*  Link with CHANNEL option.                                         */
/*  Program description must contain enough data to execute a LINK    */
/*  CHANNEL to the target program.                                    */
/*  The request message is multipart, each part corresponding to an   */
/*  input CONTAINER.                                                  */
/*  The response message must have been prepared by caller with       */
/*  multiple parts, each one corresponding to an anticipated output   */
/*  CONTAINER.                                                        */
/*====================================================================*/
int linkChannel(DFHEIBLK *inDfheiptr,
                TraceParms* inTraceParms,
                CICSProgramDesc* pProgramDesc,
                Message* pRequestMessage,
                Message* pResponseMessage) {
    dfheiptr = inDfheiptr;
    g_pTraceParms = inTraceParms;
    initLog(dfheiptr, inTraceParms);

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Entered linkChannel");
    }

    if (OK_CODE != checkChannel(pProgramDesc, pRequestMessage)) {
        return ERROR_CODE;
    }

    if (OK_CODE != createContainers(pProgramDesc, pRequestMessage)) {
        return ERROR_CODE;
    }

     /*  Now link to CICS program and check for errors                */
    if (strlen(pProgramDesc->CICSSysID) == 0) {
        if (FALSE_CODE == pProgramDesc->CICSSyncOnReturn) {
            if (strlen(pProgramDesc->CICSTransID) == 0) {
                EXEC CICS LINK
                          PROGRAM(pProgramDesc->CICSProgram)
                          CHANNEL(pProgramDesc->CICSChannel)
                          RESP(g_cicsResp) RESP2(g_cicsResp2);
            } else {
                EXEC CICS LINK
                          PROGRAM(pProgramDesc->CICSProgram)
                          CHANNEL(pProgramDesc->CICSChannel)
                          TRANSID(pProgramDesc->CICSTransID)
                          RESP(g_cicsResp) RESP2(g_cicsResp2);
            }
        } else {
            if (strlen(pProgramDesc->CICSTransID) == 0) {
                EXEC CICS LINK
                          PROGRAM(pProgramDesc->CICSProgram)
                          CHANNEL(pProgramDesc->CICSChannel)
                          SYNCONRETURN
                          RESP(g_cicsResp) RESP2(g_cicsResp2);
            } else {
                EXEC CICS LINK
                          PROGRAM(pProgramDesc->CICSProgram)
                          CHANNEL(pProgramDesc->CICSChannel)
                          TRANSID(pProgramDesc->CICSTransID)
                          SYNCONRETURN
                          RESP(g_cicsResp) RESP2(g_cicsResp2);
            }
        }
    } else {
        if (FALSE_CODE == pProgramDesc->CICSSyncOnReturn) {
            if (strlen(pProgramDesc->CICSTransID) == 0) {
                EXEC CICS LINK
                          PROGRAM(pProgramDesc->CICSProgram)
                          CHANNEL(pProgramDesc->CICSChannel)
                          SYSID(pProgramDesc->CICSSysID)
                          RESP(g_cicsResp) RESP2(g_cicsResp2);
            } else {
                EXEC CICS LINK
                          PROGRAM(pProgramDesc->CICSProgram)
                          CHANNEL(pProgramDesc->CICSChannel)
                          SYSID(pProgramDesc->CICSSysID)
                          TRANSID(pProgramDesc->CICSTransID)
                          RESP(g_cicsResp) RESP2(g_cicsResp2);
            }
        } else {
            if (strlen(pProgramDesc->CICSTransID) == 0) {
                EXEC CICS LINK
                          PROGRAM(pProgramDesc->CICSProgram)
                          CHANNEL(pProgramDesc->CICSChannel)
                          SYSID(pProgramDesc->CICSSysID)
                          SYNCONRETURN
                          RESP(g_cicsResp) RESP2(g_cicsResp2);
            } else {
                EXEC CICS LINK
                          PROGRAM(pProgramDesc->CICSProgram)
                          CHANNEL(pProgramDesc->CICSChannel)
                          SYSID(pProgramDesc->CICSSysID)
                          TRANSID(pProgramDesc->CICSTransID)
                          SYNCONRETURN
                          RESP(g_cicsResp) RESP2(g_cicsResp2);
            }
        }
    }

    if (g_cicsResp != DFHRESP(NORMAL)) {
        logCicsError(MODULE_NAME, "LINK CHANNEL",
                     g_cicsResp,g_cicsResp2);
        return ERROR_CODE;
    }

    if (OK_CODE != formatChannelResponse(
                           pProgramDesc, pResponseMessage)) {
        return ERROR_CODE;
    }

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Return from linkChannel");
    }
    return OK_CODE;
}

/*====================================================================*/
/*  Verifies LINK CHANNEL options.                                    */
/*====================================================================*/
int checkChannel(CICSProgramDesc* pProgramDesc,
                  Message* pRequestMessage) {

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Entered checkChannel");
    }
    /* We need a program name; We don't perform an inquire on the
     * program because that fails with autoinstall. */
    if (strlen(pProgramDesc->CICSProgram) == 0) {
        logError(MODULE_NAME, "No CICS program name was provided.");
        return ERROR_CODE;
    }

    /* There must be a channel name. */
    if (strlen(pProgramDesc->CICSChannel) == 0) {
        logError(MODULE_NAME, "No channel name specified.");
        return ERROR_CODE;
    }

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Return from checkChannel");
    }
    return OK_CODE;
}

/*====================================================================*/
/*  Create the input CONTAINERS expected by the target program.       */
/*  There will be one CONTAINER for each incoming message part.       */
/*====================================================================*/
int createContainers(CICSProgramDesc* pProgramDesc,
                  Message* pRequestMessage) {

    int i;
    MessagePart* pInputPart;

    HeaderPartContent* pRequestHeaderPartContent
         = (HeaderPartContent*) pRequestMessage->pHeaderPart->content;
    int inPartsNum = pRequestHeaderPartContent->partsNumber.as_int;

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Entered createContainers");
    }

    /* Now create a CONTAINER for each input message part */
    for (i = 0; i < inPartsNum; i++) {
        pInputPart = pRequestMessage->pParts + i;
        EXEC CICS PUT CONTAINER(pInputPart->ID)
             CHANNEL (pProgramDesc->CICSChannel)
             FROM    (pInputPart->content)
             FLENGTH (pInputPart->size.as_int)
             RESP    (g_cicsResp) RESP2(g_cicsResp2);
        if (g_cicsResp != DFHRESP(NORMAL)) {
            logCicsError(MODULE_NAME, "PUT CONTAINER",
                         g_cicsResp,g_cicsResp2);
            return ERROR_CODE;
        }
    }

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Return from createContainers");
    }
    return OK_CODE;
}

/*====================================================================*/
/*  Formats multipart response message from output CONTAINERS.        */
/*  Output containers might be present or not.                        */
/*====================================================================*/
int formatChannelResponse(CICSProgramDesc* pProgramDesc,
                          Message* pResponseMessage) {

    int outPartsNum =
        pProgramDesc->CICSOutputContainersCount;

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Entered formatChannelResponse");
    }

    if (OK_CODE != formatResponseHeaders(
                       outPartsNum, pResponseMessage)) {
        return ERROR_CODE;
    }

    if (outPartsNum > 0) {
       if (OK_CODE != formatResponseParts(
                        pProgramDesc, outPartsNum, pResponseMessage)) {
           return ERROR_CODE;
       }
    }

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Return from formatChannelResponse");
    }
    return OK_CODE;
}

/*====================================================================*/
/*  Create the response header specifying the number of output        */
/*  message parts and no key/values pairs.                            */
/*====================================================================*/
int formatResponseHeaders(
                 int outPartsNum,
                 Message* pResponseMessage) {

    /* Format header part, indicating  no key/values.  */
    int headerLength = PARTS_NUM_LEN + KEYVAL_SIZE_LEN;
    HeaderPartContent* pResponseHeaderPartContent;

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Entered formatResponseHeaders");
    }

    memset(pResponseMessage->pHeaderPart->ID, ' ', MSG_ID_LEN);
    memcpy(pResponseMessage->pHeaderPart->ID, HEADER_PART_ID,
           sizeof(HEADER_PART_ID) - 1);
    pResponseMessage->pHeaderPart->size.as_int = headerLength;

    EXEC CICS GETMAIN
              SET(pResponseMessage->pHeaderPart->content)
              INITIMG('\0')
              FLENGTH(headerLength)
              RESP(g_cicsResp) RESP2(g_cicsResp2);

    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError(MODULE_NAME, "GETMAIN",
                     g_cicsResp, g_cicsResp2);
       return ERROR_CODE;
    }
    if (g_pTraceParms->traceMode == TRUE_CODE) {
       sprintf(g_traceMessage,
        "Response header part content allocated at %#x size=%d",
        (int)pResponseMessage->pHeaderPart->content, headerLength);
        traceMessage(MODULE_NAME, g_traceMessage);
    }
    pResponseHeaderPartContent
         = (HeaderPartContent*) pResponseMessage->pHeaderPart->content;
    pResponseHeaderPartContent->partsNumber.as_int = outPartsNum;
    pResponseHeaderPartContent->keyValuesSize.as_int = 0;

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Return from formatResponseHeaders");
    }
    return OK_CODE;
}


/*====================================================================*/
/*  Create the response parts with content from corresponding         */
/*  CONTAINER. There will be one response part for each requested     */
/*  output CONTAINER. If a requested output CONTAINER was not created */
/*  by the linked program, the corresponding response part will have  */
/*  a NULL content.                                                   */
/*====================================================================*/
int formatResponseParts(
                 CICSProgramDesc* pProgramDesc,
                 int outPartsNum,
                 Message* pResponseMessage) {

    int i;
    MessagePart* pOutputPart;
    int pos = 0;

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Entered formatResponseParts");
    }

    for (i = 0; i < outPartsNum; i++) {
       pOutputPart = pResponseMessage->pParts + i;
       memset(pOutputPart->ID, '\0', MSG_ID_LEN);
       memcpy(pOutputPart->ID,
           pProgramDesc->CICSOutputContainers + pos,
           CONTAINER_NAME_LEN);
       EXEC CICS GET CONTAINER(pOutputPart->ID)
             CHANNEL (pProgramDesc->CICSChannel)
             SET     (pOutputPart->content)
             FLENGTH (pOutputPart->size.as_int)
             RESP    (g_cicsResp) RESP2(g_cicsResp2);
       if (g_cicsResp != DFHRESP(NORMAL) &&
            g_cicsResp != DFHRESP(CONTAINERERR)) {
           logCicsError(MODULE_NAME, "GET CONTAINER",
                         g_cicsResp, g_cicsResp2);
           return ERROR_CODE;
       }
       if (g_cicsResp == DFHRESP(CONTAINERERR)) {
           pOutputPart->content = NULL;
           pOutputPart->size.as_int = 0;
       }
       pos += (CONTAINER_NAME_LEN + 1);
    }

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Return from formatResponseParts");
    }
    return OK_CODE;
}

/*====================================================================*/
/*  When done with CONTAINER-driven program this routine explicitly   */
/*  deletes CONTAINERS. This is necessary because we might be called  */
/*  by a driver that processes CONTAINER-driven programs in a loop.   */
/*  Without an explicit delete, CONTAINERS would not go out of scope  */
/*  which would unnecessarily consume memory resources.               */
/*====================================================================*/
int freeChannel(DFHEIBLK *inDfheiptr,
                TraceParms* inTraceParms,
                CICSProgramDesc* pProgramDesc,
                Message* pRequestMessage,
                Message* pResponseMessage) {

    int rc = OK_CODE;

    dfheiptr = inDfheiptr;
    g_pTraceParms = inTraceParms;
    initLog(dfheiptr, inTraceParms);

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Entered freeChannel");
    }

    rc = freeMessages(TRUE_CODE, pProgramDesc, pRequestMessage);
    rc = freeMessages(FALSE_CODE, pProgramDesc, pResponseMessage);

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Return from freeChannel");
    }
    return rc;
}

/*====================================================================*/
/* Releases memory allocated for a message. Request messages are      */
/* treated differently because memory has been explicitly GETMAINed   */
/* by us and therefore it needs to be FREEMAIned. In the case of the  */
/* response, only message headers have been GETMAINed by us, the      */
/* CONTAINERS content has been allocated by CICS. Any attempt to      */
/* FREEMAIN such memory results in SEVERE CICS errors. We therefore   */
/* use DELETE CONTAINER on response messages so that CONTAINERS go    */
/* out of scope and their memory be reclamed by CICS itself.          */
/*====================================================================*/
int freeMessages(int request,
                CICSProgramDesc* pProgramDesc,
                Message* pMessage) {

    int i = 0;
    int partsNumber = 0;
    HeaderPartContent* pHeaderPartContent =
          (HeaderPartContent*)pMessage->pHeaderPart->content;
    MessagePart* part;

    if (pHeaderPartContent == NULL) {
      return OK_CODE;
    }

    partsNumber = pHeaderPartContent->partsNumber.as_int;

    /* Free the header part content */
    EXEC CICS FREEMAIN
              DATAPOINTER(pHeaderPartContent)
              RESP(g_cicsResp) RESP2(g_cicsResp2);
    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError(MODULE_NAME, "FREEMAIN",
                      g_cicsResp, g_cicsResp2);
       return ERROR_CODE;
    }
    if (g_pTraceParms->traceMode == TRUE_CODE) {
       sprintf(g_traceMessage,
        "Header part content freed from %#x",
        (int)pHeaderPartContent);
        traceMessage(MODULE_NAME, g_traceMessage);
    }

    /* Now free the message parts content and containers */
    for (i = 0; i < partsNumber && i < MAX_IN_MSG_PARTS; i++) {
        part = pMessage->pParts + i;
        EXEC CICS DELETE CONTAINER(part->ID)
            CHANNEL (pProgramDesc->CICSChannel)
            RESP(g_cicsResp) RESP2(g_cicsResp2);
        if (g_cicsResp != DFHRESP(NORMAL)
            && g_cicsResp != DFHRESP(CONTAINERERR)) {
            logCicsError(MODULE_NAME, "DELETE CONTAINER",
                        g_cicsResp, g_cicsResp2);
            return ERROR_CODE;
        }
        if (g_pTraceParms->traceMode == TRUE_CODE) {
           sprintf(g_traceMessage,
            "Container %s deleted from Channel %s",
            part->ID, pProgramDesc->CICSChannel);
            traceMessage(MODULE_NAME, g_traceMessage);
        }
        if (request == TRUE_CODE && part->content != NULL) {
            EXEC CICS FREEMAIN
                DATAPOINTER(part->content)
                RESP(g_cicsResp) RESP2(g_cicsResp2);
            if (g_cicsResp != DFHRESP(NORMAL)) {
                logCicsError(MODULE_NAME, "FREEMAIN",
                          g_cicsResp, g_cicsResp2);
                return ERROR_CODE;
            }
            if (g_pTraceParms->traceMode == TRUE_CODE) {
                sprintf(g_traceMessage,
                    "Message part content freed from %#x",
                    (int)part->content);
                traceMessage(MODULE_NAME, g_traceMessage);
            }
        }
    }
    return OK_CODE;
}
