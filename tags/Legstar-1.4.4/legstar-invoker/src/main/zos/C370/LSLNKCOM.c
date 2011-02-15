/* REMOVE SP OPTION UNLESS STORAGE DEBUGGING */
#pragma XOPTS(CICS)
#pragma longname
#pragma export(linkCommarea)
#pragma export(freeCommarea)
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
/*   Purpose    - LINK COMMAREA Driver                                */
/*   Language   - IBM C/370                                           */
/*   System     - Tested on CICS TS 3.1                               */
/*   History    - 23 Feb 2007  - Original Implementation              */
/*                06 Jan 2009  - Truncate trailing low values from    */
/*                               the reply payload.                   */
/*   Notes      - This sub-program (DLL) implements the capability to */
/*                invoke a CICS program passing data via COMMAREA.    */
/*                This module supports a single input and single      */
/*                output parts.                                       */
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
#define MODULE_NAME   "LSLNKCOM" /* used to correlate traces          */

/*====================================================================*/
/*  Link with COMMAREA option.                                        */
/*  Program header must contain enough data to execute an EXEC CICS   */
/*  LINK COMMAREA to the target program.                              */
/*  The request message has a single part which content is the        */
/*  COMMAREA input data.                                              */
/*  The response message must have been prepared by the caller, its   */
/*  single part content will hold the COMMAREA output data.           */
/*  In order to reduce the memory allocation/deallocation activity,   */
/*  it is possible for the request and response parts to share the    */
/*  same content memory location provided the request content was     */
/*  large enough to hold the response.                                */
/*====================================================================*/
int linkCommarea(DFHEIBLK *inDfheiptr,
                TraceParms* inTraceParms,
                CICSProgramDesc* pProgramDesc,
                Message* pRequestMessage,
                Message* pResponseMessage) {
    char* inputContent;
    dfheiptr = inDfheiptr;
    g_pTraceParms = inTraceParms;
    initLog(dfheiptr, inTraceParms);

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Entered linkCommarea");
    }

    if (OK_CODE != checkCommarea(pProgramDesc, pRequestMessage)) {
        return ERROR_CODE;
    }

    /* The commarea expected by the target program might be larger
     * than the content of the incoming message part. In this case,
     * we need to rebuild the commarea. */
    if (pProgramDesc->CICSLength >
              pRequestMessage->pParts->size.as_int) {
        reallocContent(pProgramDesc, pRequestMessage->pParts);
    }
    inputContent = pRequestMessage->pParts->content;

    /* COMMENT OUT UNLESS STORAGE DEBUGGING
    sprintf(g_traceMessage, "Storage before LINK PROGRAM(%s)",
          pProgramDesc->CICSProgram);
    traceMessage(MODULE_NAME, g_traceMessage);
    traceStorage(); */
    
     /*  Now link to CICS program and check for errors                */
    if (strlen(pProgramDesc->CICSSysID) == 0) {
        if (FALSE_CODE == pProgramDesc->CICSSyncOnReturn) {
            if (strlen(pProgramDesc->CICSTransID) == 0) {
                EXEC CICS LINK
                          PROGRAM(pProgramDesc->CICSProgram)
                          COMMAREA(inputContent)
                          LENGTH(pProgramDesc->CICSLength)
                          DATALENGTH(pProgramDesc->CICSDataLength)
                          RESP(g_cicsResp) RESP2(g_cicsResp2);
            } else {
                EXEC CICS LINK
                          PROGRAM(pProgramDesc->CICSProgram)
                          COMMAREA(inputContent)
                          LENGTH(pProgramDesc->CICSLength)
                          DATALENGTH(pProgramDesc->CICSDataLength)
                          TRANSID(pProgramDesc->CICSTransID)
                          RESP(g_cicsResp) RESP2(g_cicsResp2);
            }
        } else {
            if (strlen(pProgramDesc->CICSTransID) == 0) {
                EXEC CICS LINK
                          PROGRAM(pProgramDesc->CICSProgram)
                          COMMAREA(inputContent)
                          LENGTH(pProgramDesc->CICSLength)
                          DATALENGTH(pProgramDesc->CICSDataLength)
                          SYNCONRETURN
                          RESP(g_cicsResp) RESP2(g_cicsResp2);
            } else {
                EXEC CICS LINK
                          PROGRAM(pProgramDesc->CICSProgram)
                          COMMAREA(inputContent)
                          LENGTH(pProgramDesc->CICSLength)
                          DATALENGTH(pProgramDesc->CICSDataLength)
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
                          COMMAREA(inputContent)
                          LENGTH(pProgramDesc->CICSLength)
                          DATALENGTH(pProgramDesc->CICSDataLength)
                          SYSID(pProgramDesc->CICSSysID)
                          RESP(g_cicsResp) RESP2(g_cicsResp2);
            } else {
                EXEC CICS LINK
                          PROGRAM(pProgramDesc->CICSProgram)
                          COMMAREA(inputContent)
                          LENGTH(pProgramDesc->CICSLength)
                          DATALENGTH(pProgramDesc->CICSDataLength)
                          SYSID(pProgramDesc->CICSSysID)
                          TRANSID(pProgramDesc->CICSTransID)
                          RESP(g_cicsResp) RESP2(g_cicsResp2);
            }
        } else {
            if (strlen(pProgramDesc->CICSTransID) == 0) {
                EXEC CICS LINK
                          PROGRAM(pProgramDesc->CICSProgram)
                          COMMAREA(inputContent)
                          LENGTH(pProgramDesc->CICSLength)
                          DATALENGTH(pProgramDesc->CICSDataLength)
                          SYSID(pProgramDesc->CICSSysID)
                          SYNCONRETURN
                          RESP(g_cicsResp) RESP2(g_cicsResp2);
            } else {
                EXEC CICS LINK
                          PROGRAM(pProgramDesc->CICSProgram)
                          COMMAREA(inputContent)
                          LENGTH(pProgramDesc->CICSLength)
                          DATALENGTH(pProgramDesc->CICSDataLength)
                          SYSID(pProgramDesc->CICSSysID)
                          TRANSID(pProgramDesc->CICSTransID)
                          SYNCONRETURN
                          RESP(g_cicsResp) RESP2(g_cicsResp2);
            }
        }
    }

    if (g_cicsResp != DFHRESP(NORMAL)) {
        logCicsError(MODULE_NAME, "LINK COMMAREA",
                     g_cicsResp,g_cicsResp2);
        return ERROR_CODE;
    }

    /* COMMENT OUT UNLESS STORAGE DEBUGGING
    sprintf(g_traceMessage, "Storage after LINK PROGRAM(%s)",
          pProgramDesc->CICSProgram);
    traceMessage(MODULE_NAME, g_traceMessage);
    traceStorage(); */
    
    if (OK_CODE != formatCommareaResponse(
                   pProgramDesc, pRequestMessage, pResponseMessage)) {
        return ERROR_CODE;
    }

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Return from linkCommarea");
    }

    return OK_CODE;
}

/*====================================================================*/
/*  Verifies LINK COMMAREA options.                                   */
/*====================================================================*/
int checkCommarea(CICSProgramDesc* pProgramDesc,
                  Message* pRequestMessage) {

    HeaderPartContent* pRequestHeaderPartContent
         = (HeaderPartContent*) pRequestMessage->pHeaderPart->content;
    MessagePart* pInputPart = pRequestMessage->pParts;
    int inPartsNum = pRequestHeaderPartContent->partsNumber.as_int;

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Entered checkCommarea");
    }
    /* We need a program name; We don't perform an inquire on the
     * program because that fails with autoinstall. */
    if (strlen(pProgramDesc->CICSProgram) == 0) {
        logError(MODULE_NAME, "No CICS program name was provided.");
        return ERROR_CODE;
    }

    /* There must be one, and only one, input message part. */
    if (inPartsNum == 0) {
        logError(MODULE_NAME, "No input message part for commarea.");
        return ERROR_CODE;
    }
    if (inPartsNum > 1) {
        logError(MODULE_NAME, "Too many message parts for commarea.");
        return ERROR_CODE;
    }

    /* If no commarea length was specified, assume the size of the
     * incoming data. */
    if (pProgramDesc->CICSLength == 0) {
        pProgramDesc->CICSLength = pInputPart->size.as_int;
    }

    /* If DataLength is specified, it must be smaller than the
     * commarea length. */
    if (pProgramDesc->CICSDataLength > pProgramDesc->CICSLength) {
        logError(MODULE_NAME,
                   "Data length cannot exceed commarea length.");
        return ERROR_CODE;
    }

    /* If no data length was specified, assume the size of the
     * incoming data. */
    if (pProgramDesc->CICSDataLength == 0) {
        pProgramDesc->CICSDataLength = pInputPart->size.as_int;
    }

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Return from checkCommarea");
    }
    return OK_CODE;
}

/*====================================================================*/
/* Reallocate a content that is not large enough to hold the reply    */
/* from a commarea driven program.                                    */
/*====================================================================*/
int reallocContent(CICSProgramDesc* pProgramDesc,
                   MessagePart* pInputPart) {

    char* newContent;

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Reallocating content");
    }
    EXEC CICS GETMAIN
              SET(newContent)
              INITIMG('\0')
              FLENGTH(pProgramDesc->CICSLength + 1)
              RESP(g_cicsResp) RESP2(g_cicsResp2);

    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError(MODULE_NAME, "GETMAIN",
                     g_cicsResp, g_cicsResp2);
       return ERROR_CODE;
    }
    memcpy(newContent, pInputPart->content, pInputPart->size.as_int);
    if (pInputPart->content != NULL) {
        EXEC CICS FREEMAIN
                  DATAPOINTER(pInputPart->content)
                  RESP(g_cicsResp) RESP2(g_cicsResp2);
        if (g_cicsResp != DFHRESP(NORMAL)) {
           logCicsError(MODULE_NAME, "FREEMAIN",
                        g_cicsResp, g_cicsResp2);
           return ERROR_CODE;
        }
    }

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       sprintf(g_traceMessage,
        "Content reallocated from %#x size=%d to %#x size=%ld",
        (int)pInputPart->content,
        pInputPart->size.as_int,
        (int)newContent,
        pProgramDesc->CICSLength);
        traceMessage(MODULE_NAME, g_traceMessage);
    }
    /* Update the input part to reflect the new allocation */
    pInputPart->content = newContent;
    pInputPart->size.as_int = pProgramDesc->CICSLength;

    return OK_CODE;
}

/*====================================================================*/
/*  Formats single part response message from output COMMAREA.        */
/*====================================================================*/
int formatCommareaResponse(CICSProgramDesc* pProgramDesc,
                          Message* pRequestMessage,
                          Message* pResponseMessage) {


    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Entered formatCommareaResponse");
    }

    if (OK_CODE != formatResponseHeader(pResponseMessage)) {
        return ERROR_CODE;
    }

     if (OK_CODE != formatResponsePart(
                      pRequestMessage, pResponseMessage)) {
         return ERROR_CODE;
     }

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Return from formatCommareaResponse");
    }
    return OK_CODE;
}

/*====================================================================*/
/*  Create the response header specifying no key/values pairs.        */
/*====================================================================*/
int formatResponseHeader(
                 Message* pResponseMessage) {

    /* Format header part, indicating  no key/values.  */
    int headerLength = PARTS_NUM_LEN + KEYVAL_SIZE_LEN;
    HeaderPartContent* pResponseHeaderPartContent;

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Entered formatResponseHeader");
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
    pResponseHeaderPartContent->partsNumber.as_int = 1;
    pResponseHeaderPartContent->keyValuesSize.as_int = 0;

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Return from formatResponseHeader");
    }
    return OK_CODE;
}

/*====================================================================*/
/*  Create the single response part with content from corresponding   */
/*  COMMAREA.                                                         */
/*====================================================================*/
int formatResponsePart(
                 Message* pRequestMessage,
                 Message* pResponseMessage) {


    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Entered formatResponsePart");
    }
    /* Format the single output message part. It has the
     * content as the input message part since the linked program
     * updated the content.  */
    memset(pResponseMessage->pParts->ID, ' ', MSG_ID_LEN);
    memcpy(pResponseMessage->pParts->ID, COMMAREA_PART_ID,
           sizeof(COMMAREA_PART_ID) - 1);
    pResponseMessage->pParts->size.as_int =
           pRequestMessage->pParts->size.as_int;
    pResponseMessage->pParts->content =
           pRequestMessage->pParts->content;
    adjustPartSize(pResponseMessage->pParts);

    /* In order to avoid having 2 pointers on the same memory location
     * which would complicate freeing memory, we disconnect the
     * input message from its content. */
    pRequestMessage->pParts->size.as_int = 0;
    pRequestMessage->pParts->content = NULL;

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Return from formatResponsePart");
    }
    return OK_CODE;
}
/*====================================================================*/
/*  Detect the last non null character in a message part content and  */
/*  adjust the size of the message part content to prevent sending    */
/*  back trailing binary zeroes.                                      */
/*====================================================================*/
int adjustPartSize(MessagePart* pPart) {
    int i = 0;
    int lastNonNull = -1;
    for (i = 0; i < pPart->size.as_int; i++) {
        if (pPart->content[i] != '\0') {
            lastNonNull = i;
        }
    }
    if (g_pTraceParms->traceMode == TRUE_CODE) {
       sprintf(g_traceMessage,
        "Response content size reduced from %d to %d",
        pPart->size.as_int,
        lastNonNull + 1);
        traceMessage(MODULE_NAME, g_traceMessage);
    }
    pPart->size.as_int = lastNonNull + 1;
    return OK_CODE;
}

/*====================================================================*/
/*  When done with COMMAREA-driven program this routine explicitly    */
/*  releases memory allocated for COMMAREAS. This is necessary because*/
/*  we might be called by a driver that processes COMMAREA-driven     */
/*  programs in a loop. Without an explicit release, COMMAREAS memory */
/*  would not be reclamed over multiple calls.                        */
/*====================================================================*/
int freeCommarea(DFHEIBLK *inDfheiptr,
                TraceParms* inTraceParms,
                CICSProgramDesc* pProgramDesc,
                Message* pRequestMessage,
                Message* pResponseMessage) {

    int rc = OK_CODE;

    dfheiptr = inDfheiptr;
    g_pTraceParms = inTraceParms;
    initLog(dfheiptr, inTraceParms);

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Entered freeCommarea");
    }

    rc = freeMessage(pRequestMessage);
    rc = freeMessage(pResponseMessage);

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Return from freeCommarea");
    }
    return rc;
}

/*====================================================================*/
/* Releases memory allocated for a message.                           */
/*====================================================================*/
int freeMessage(Message* pMessage) {

    int i = 0;
    int partsNumber = 0;
    HeaderPartContent* pHeaderPartContent =
          (HeaderPartContent*)pMessage->pHeaderPart->content;
    MessagePart* part;

    if (pHeaderPartContent != NULL) {

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

      /* Now free the message parts content */
      for (i = 0; i < partsNumber && i < MAX_IN_MSG_PARTS; i++) {
          part = pMessage->pParts + i;
          if (part->content != NULL) {
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
    }

    return OK_CODE;
}

/*====================================================================*/
/* Function to convert packed field to string                     */
/*====================================================================*/
int ptos(char *s_buff,unsigned char *p_buff) {
    unsigned long  p_val;
    p_val=*(unsigned long *)p_buff;
    sprintf(s_buff,"%lx",p_val>>4);
    return OK_CODE;
}

/*====================================================================*/
/* Prints a detailed map of all memory blocks allocated by this task. */
/*====================================================================*/
/* COMMENT OUT UNLESS STORAGE DEBUGGING
int traceStorage() {
    int numelements;
    signed long resp;
    int* lengthlist;
    char** elementlist;
    int i;
    long taskTotal;
    long sysTotal;
    char cicsTaskn[20];

    memset(cicsTaskn, 0, sizeof(cicsTaskn));
    ptos(cicsTaskn,dfheiptr->eibtaskn);

    EXEC CICS INQUIRE SYSTEM
        EUDSASIZE(sysTotal)
        RESP(resp);

    if (resp != DFHRESP(NORMAL)) {
        sprintf(g_traceMessage,"Task no:%s Inquire system failed %ld",
            cicsTaskn, resp);
        traceMessage(MODULE_NAME, g_traceMessage);
        return ERROR_CODE;
    }
    sprintf(g_traceMessage,"Task no:%s System storage %ld",
        cicsTaskn, sysTotal);
    traceMessage(MODULE_NAME, g_traceMessage);

    EXEC CICS INQUIRE STORAGE
        DSANAME("EUDSA")
        ELEMENTLIST(elementlist)
        LENGTHLIST(lengthlist)
        NUMELEMENTS(numelements)
        RESP(resp);

    if (resp != DFHRESP(NORMAL)) {
        sprintf(g_traceMessage,"Task no:%s Inquire storage failed %ld",
            cicsTaskn, resp);
        traceMessage(MODULE_NAME, g_traceMessage);
        return ERROR_CODE;
    }

    sprintf(g_traceMessage,"Task no:%s Storage elements %d",
        cicsTaskn, numelements);
    traceMessage(MODULE_NAME, g_traceMessage);

    taskTotal=0;
    for (i=0;i < numelements;i++){
        sprintf(g_traceMessage,
            "Task no:%s Element at address %#x size %d",
            cicsTaskn, *(elementlist + i), *(lengthlist + i));
        traceMessage(MODULE_NAME, g_traceMessage);
        taskTotal += *(lengthlist + i);
    }

     sprintf(g_traceMessage,"Task no:%s Total task EUDSA %ld",
        cicsTaskn, taskTotal);
     traceMessage(MODULE_NAME, g_traceMessage);
     
     return OK_CODE;
}
 */
