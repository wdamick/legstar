#pragma XOPTS(CICS)
#pragma longname
#pragma export(initLSMSGLIB)
#pragma export(initLSMessage)
#pragma export(allocHostBuffer)
#pragma export(hostToMessage)
#pragma export(hostToMessagePart)
#pragma export(messageToBuffer)
#pragma export(messagePartToBuffer)
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
/*   Purpose    - LegStar Messaging common methods                    */
/*   Language   - IBM C/370                                           */
/*   System     - Tested on CICS TS 2.3                               */
/*   History    - 10 August 2007  - Original Implementation           */
/*   Notes      - This library provides utility methods that are      */
/*                useful when creating or reading LegStar messages.   */
/*                                                                    */
/*                LegStar messages are complex structures that allow  */
/*                requests and reply to carry multiple message parts. */
/*                Message parts fit nicely with CICS containers for   */
/*                instance.                                           */
/*                                                                    */
/**********************************************************************/
/*--------------------------------------------------------------------*/
/*  Constants                                                         */
/*--------------------------------------------------------------------*/
#define MODULE_NAME   "LSMSGLIB" /* used to correlate traces          */

/*--------------------------------------------------------------------*/
/*  Headers included                                                  */
/*--------------------------------------------------------------------*/
#include <stdio.h>               /* get the standard I/O library      */
#include <stdlib.h>              /* get the standard library          */
#include <string.h>              /* support string operations         */
#include "lscomdec.h"            /* legstar common declarations       */
#include "lsmsglib.h"            /* legstar messaging include file    */

/*--------------------------------------------------------------------*/
/* Global variables                                                   */
/*--------------------------------------------------------------------*/
/*====================================================================*/
/* Initialize logging and tracing variables.                          */
/*====================================================================*/
int initLSMSGLIB(
    DFHEIBLK *inDfheiptr,                     /* Pointer to eib block */
    TraceParms* inTraceParms)          /* Pointer to trace parameters */
{
    dfheiptr = inDfheiptr;
    g_pTraceParms = inTraceParms;
    return OK_CODE;
}

/*====================================================================*/
/* Initialize a LegStar message structure.                            */
/*====================================================================*/
int initLSMessage(
    LS_Message *pLsMessage)    /* Pointer to message to initialize    */
{
    memset(&pLsMessage->headerPart, '\0',
           sizeof(pLsMessage->headerPart));
    memset(pLsMessage->dataParts, '\0',
           sizeof(pLsMessage->dataParts));

    pLsMessage->message.pHeaderPart = &pLsMessage->headerPart;
    pLsMessage->message.pParts = pLsMessage->dataParts;
    return OK_CODE;
}

/*====================================================================*/
/* Allocate or re-allocate memory                                     */
/*====================================================================*/
int allocHostBuffer(pBuffer, bufferLen)
    void **pBuffer;          /* Pointer to the buffer to be allocated */
    long bufferLen;          /* Buffer size requested                 */
{
    int rc = OK_CODE;             /* general purpose return code      */

    /* Free any previously allocated buffer */
    rc = freeHostBuffer(*pBuffer);
    if (OK_CODE != rc) {
        return rc;
    }

    /* Acquire a  buffer */
    EXEC CICS GETMAIN
              SET     (*pBuffer)
              INITIMG ('\0')
              FLENGTH (bufferLen)
              RESP    (g_cicsResp) RESP2(g_cicsResp2);

    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError(MODULE_NAME, "GETMAIN", g_cicsResp, g_cicsResp2);
       return ERROR_CODE;
    }
    if (g_pTraceParms->traceMode == TRUE_CODE) {
       sprintf(g_traceMessage,
        "Host buffer allocated at %#x size=%ld",
        (int)*pBuffer, bufferLen);
        traceMessage(MODULE_NAME, g_traceMessage);
    }

    return OK_CODE;
}

/*====================================================================*/
/* Free memory                                                        */
/*====================================================================*/
int freeHostBuffer(
    void *buffer)                           /* Buffer to be freed     */
{
    if (buffer != NULL) {
        EXEC CICS FREEMAIN
                  DATAPOINTER(buffer)
                  RESP(g_cicsResp) RESP2(g_cicsResp2);
        if (g_cicsResp != DFHRESP(NORMAL)) {
           logCicsError(MODULE_NAME, "FREEMAIN",
                        g_cicsResp, g_cicsResp2);
           return ERROR_CODE;
        }
        if (g_pTraceParms->traceMode == TRUE_CODE) {
            sprintf(g_traceMessage,
            "Host buffer freed from %#x",
            (int)buffer);
            traceMessage(MODULE_NAME, g_traceMessage);
        }
    }

    return OK_CODE;
}

/*====================================================================*/
/* Formats a message structure which will contain a header part       */
/* followed by any number of message parts. The exact number of       */
/* message parts is given by the header part.                         */
/*====================================================================*/
int hostToMessage(
    char* hostBuffer,          /* Pointer to the raw in-memory data   */
    int  len,                  /* Total size of the raw data          */
    Message* pMessage)  /*  message being formatted     */
{

    int rc = OK_CODE;             /* general purpose return code      */
    int pos = 0;         /* Current position within the received data */
    int inPartsNum = 0;    /* Number of input parts                   */
    int i = 0;
    MessagePart* pHeaderPart = pMessage->pHeaderPart;
    HeaderPartContent* pHeaderPartContent;

    if (g_pTraceParms->traceMode == TRUE_CODE) {
      traceMessage(MODULE_NAME, "About to format message");
    }

    /* First format the header part. */
    rc = hostToMessagePart(hostBuffer, &pos, len, pHeaderPart);
    if (rc == ERROR_CODE) {
        return ERROR_CODE;
    }

    /* Check that this message part is indeed a header part.
     * If not, consider the client is out of sync or not talking
     * our protocol. */
    if (strncmp(pHeaderPart->ID, HEADER_PART_ID,
             sizeof(HEADER_PART_ID) - 1) != 0) {
        logError(MODULE_NAME,
              "A message should start with a header part. Aborting.");
        return ERROR_CODE;
    }

    /* Analyze header part to determine the number of input parts to
     * receive.  */
    pHeaderPartContent = (HeaderPartContent*) pHeaderPart->content;
    inPartsNum = pHeaderPartContent->partsNumber.as_int;

    if (inPartsNum > MAX_IN_MSG_PARTS) {
        logError(MODULE_NAME, "Too many message parts.");
        return ERROR_CODE;
    }

    if (g_pTraceParms->traceMode == TRUE_CODE) {
        sprintf(g_traceMessage,
            "Message header formatted,"
            " parts number=%d keyValuesSize=%d",
                   inPartsNum,
                   pHeaderPartContent->keyValuesSize.as_int);
        traceMessage(MODULE_NAME, g_traceMessage);
    }

    /* Now format the message parts */
    for (i = 0; i < inPartsNum && rc == OK_CODE; i++) {
        rc = hostToMessagePart(
           hostBuffer, &pos, len, pMessage->pParts + i);
    }

    if (g_pTraceParms->traceMode == TRUE_CODE) {
        if (OK_CODE == rc) {
            traceMessage(MODULE_NAME, "Message formatted:");
            traceMessage(MODULE_NAME,
                 "-----------------------------------------------");
            dumpMessage(MODULE_NAME, pMessage);
        } else {
            traceMessage(MODULE_NAME,
                 " message formatting failed");
        }
    }

    return rc;

}

/*====================================================================*/
/* This routine formats a message part structure.                     */
/* A message part starts with a message part header (MPH) formed by:  */
/* - 16 bytes giving the message part identifier                      */
/* - 4 bytes giving the content size                                  */
/* The message content follows the MPH immediatly.                    */
/*====================================================================*/
int hostToMessagePart(
    char* hostBuffer,        /* Pointer to the raw in-memory data     */
    int* pPos,               /* Current position within the raw data  */
    int  len,                /* Total size of the raw data            */
    MessagePart* pMessagePart) /* Message part to be formatted        */
{

    int rc = OK_CODE;             /* general purpose return code      */
    int sLeft;                    /* bytes available in host buffer   */

    if (g_pTraceParms->traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "About to format message part");
    }

    /* If there is not enough bytes left to fill a part header,
     * there is a problem. */
    sLeft = len - *pPos;
    if (sLeft < sizeof(pMessagePart->ID)
                            + sizeof(pMessagePart->size.as_bytes)) {
        logError(MODULE_NAME, "No message parts found");
        return ERROR_CODE;
    }

    /* First format the message part header (16 bytes
       identifier and 4 bytes giving the part content size. */
    memcpy(pMessagePart->ID, hostBuffer + *pPos,
                                sizeof(pMessagePart->ID));
    *pPos += sizeof(pMessagePart->ID);
    memcpy(pMessagePart->size.as_bytes, hostBuffer + *pPos,
                                sizeof(pMessagePart->size.as_bytes));
    *pPos += sizeof(pMessagePart->size.as_bytes);

    /* Sanity check the size received */
    if ((pMessagePart->size.as_int < 0)
       || (pMessagePart->size.as_int > MSGPART_MAX_LEN)) {
       logError(MODULE_NAME, "Invalid message part length.");
       return ERROR_CODE;
    }
    sLeft = len - *pPos;
    if (sLeft < pMessagePart->size.as_int) {
        logError(MODULE_NAME, "No message part content found");
        return ERROR_CODE;
    }

    pMessagePart->content = NULL;

    /* Empty content is perfectly valid */
    if (pMessagePart->size.as_int == 0) {
       if (g_pTraceParms->traceMode == TRUE_CODE) {
         sprintf(g_traceMessage,
                "Message part formatted, id=%s size=%d",
                 pMessagePart->ID,
                 pMessagePart->size.as_int);
         traceMessage(MODULE_NAME, g_traceMessage);
       }
       return OK_CODE;
    }

    /* Acquire storage for the message part content. An additional
     * byte guaranteed to contain a binary zero is acquired so
     * that contents can be treated as C strings when necessary.  */
     rc = allocHostBuffer(&pMessagePart->content,
                     pMessagePart->size.as_int + 1);
     if (OK_CODE != rc) {
        return rc;
     }

    /* Set the message content */
    memcpy(pMessagePart->content, hostBuffer + *pPos,
            pMessagePart->size.as_int);
    *pPos += pMessagePart->size.as_int;

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       sprintf(g_traceMessage,
              "Message part formatted, id=%s size=%d",
               pMessagePart->ID,
               pMessagePart->size.as_int);
       traceMessage(MODULE_NAME, g_traceMessage);
    }

    return OK_CODE;
}

/*====================================================================*/
/* Folds the various parts of the response message into a single      */
/* contiguous buffer in memory. The method attempts to reuse an       */
/* existing buffer and will reallocate it if it is not large enough.  */
/* When successful, this method returns the total number of bytes     */
/* formatted.                                                         */
/*====================================================================*/
int messageToBuffer(
    char** pHostBuffer,        /* Points to a buffer to format        */
    int*  pHostBufferLen,      /* Current size of the host buffer     */
    Message* pMessage)         /* The message to read from            */
 {

    int rc = OK_CODE;             /* general purpose return code      */
    int pos = 0;            /* Current position with the sent data    */
    int hostDataLen = 0;     /* Total length of data to send          */
    HeaderPartContent* pHeaderPartContent =
          (HeaderPartContent*)pMessage->pHeaderPart->content;
    int outPartsNum = pHeaderPartContent->partsNumber.as_int;
    int i = 0;

    if (g_pTraceParms->traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "About to format buffer");
    }

    /* Calculate how much memory we need and evaluate if passed
     * buffer is large enough. Reallocate otherwise.             */
    hostDataLen = (outPartsNum + 1) *
             (sizeof(pMessage->pHeaderPart->ID)
              + sizeof(pMessage->pHeaderPart->size.as_bytes));
    hostDataLen += pMessage->pHeaderPart->size.as_int;
    for (i = 0; i < outPartsNum; i++) {
        hostDataLen += (pMessage->pParts + i)->size.as_int;
    }
    if (hostDataLen > *pHostBufferLen) {
        rc = allocHostBuffer(pHostBuffer, hostDataLen);
        if (ERROR_CODE == rc) {
             return rc;
        }
    }

    /* format the header */
    rc = messagePartToBuffer(
            *pHostBuffer, &pos, pMessage->pHeaderPart);

    /* format message parts */
    for (i = 0; i < outPartsNum && rc == OK_CODE; i++) {
        rc = messagePartToBuffer(
                *pHostBuffer, &pos, pMessage->pParts + i);
    }

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       if (OK_CODE == rc) {
          traceMessage(MODULE_NAME, "Buffer formatted:");
            traceMessage(MODULE_NAME,
                 "-----------------------------------------------");
          traceData(MODULE_NAME, *pHostBuffer, hostDataLen);
       } else {
          traceMessage(MODULE_NAME,
              " buffer formatting failed");
       }
    }

    return hostDataLen;
}

/*====================================================================*/
/* Serialize a message part as a contiguuous buffer in memory         */
/*====================================================================*/
int messagePartToBuffer(
    char* hostBuffer,           /* Host buffer being formatted        */
    int* pPos,                  /* Current position within the buffer */
    MessagePart* pPart)         /* Message part to read from          */
{
    int contentLen = pPart->size.as_int;

    if (g_pTraceParms->traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME,
         "About to format buffer from message part");
    }
    /* Format the message part ID*/
    memcpy(hostBuffer + *pPos, pPart->ID, MSG_ID_LEN);
    *pPos += MSG_ID_LEN;
    /* Format the message content size */
    memcpy(hostBuffer + *pPos, pPart->size.as_bytes,
           MSG_CONTENT_SIZE_LEN);
    *pPos += MSG_CONTENT_SIZE_LEN;

    if (contentLen > 0 && pPart->content != NULL) {
        memcpy(hostBuffer + *pPos, pPart->content,
               contentLen);
        *pPos += contentLen;
    }

    if (g_pTraceParms->traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME,
           "Buffer formatted from message part");
    }

    return OK_CODE;
}

