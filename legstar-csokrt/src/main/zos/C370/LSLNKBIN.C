#pragma XOPTS(CICS)
#pragma longname
#pragma export(invokeProgram)
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
/*   Purpose    - CICS link program driver                            */
/*   Language   - IBM C/370                                           */
/*   System     - Tested on CICS TS 2.3                               */
/*   History    - 23 Feb 2007  - Original Implementation              */
/*   Notes      - This sub-program (DLL) implements the capability to */
/*                invoke a CICS program passing commarea data as a    */
/*                binary payload.                                     */
/*                                                                    */
/*                Each request is described by a program header which */
/*                contains key/value pairs. These can be:             */
/*                Request:                                            */
/*                  CICSProgram      : CICS program name to link to   */
/*                  CICSLength       : Total size of the commarea     */
/*                  CICSDataLength   : Size of the input data         */
/*                  CICSSysID        : Remote CICS system             */
/*                  CICSSyncOnReturn : Commit on each invoke          */
/*                  CICSTransID      : Remote mirror transaction ID   */
/*                  CICSChannel      : For future use                 */
/*                                                                    */
/*                There is provision to process multi-part messages   */
/*                in preparation to support CICS containers (not      */
/*                implemented yet).                                   */
/*                                                                    */
/*                Input data is received as an array of Message parts */
/*                                                                    */
/*                Reply:                                              */
/*                 Abnormal termination result in a -1 return code    */
/*                 from the invokeProgram entry. Further description  */
/*                 of the error is found in the tracing structure.    */
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

/*--------------------------------------------------------------------*/
/*  Constants                                                         */
/*--------------------------------------------------------------------*/
#define OBJECT_DELIM  '{'        /* start of an object in json        */
#define ARRAY_DELIM   '['        /* start of an array in json         */
#define KEYWORD_DELIM '"'        /* delimits a keyword in key/values  */
#define MAX_KEY_LEN   33         /* largest key string                */
#define MAX_VALUE_LEN 257        /* largest value string              */
#define KEYVALUES_EC  "keyvalues"    /* Start of key/value pairs      */
#define PROGRAM_KEY   "CICSProgram"  /* CICS program to invoke        */
#define LENGTH_KEY    "CICSLength"   /* Size of the commarea          */
#define DATALEN_KEY   "CICSDataLength"  /* Size of the input data     */
#define SYSID_KEY     "CICSSysID"    /* Remote CICS system            */
#define SYNCONRET_KEY "CICSSyncOnReturn" /* Commit on each invoke     */
#define TRANSID_KEY   "CICSTransID"  /* Remote mirror transaction ID  */
#define CHANNEL_KEY   "CICSChannel"  /* CICS channel to use           */

#define PROGNAME_LEN  8          /* CICS program name max length      */
#define SYSID_LEN  4             /* CICS system ID max length         */
#define TRANSID_LEN  4           /* CICS transaction ID max length    */
#define CHANNEL_LEN  16          /* CICS channel name max length      */

/*--------------------------------------------------------------------*/
/*  Prototypes                                                        */
/*--------------------------------------------------------------------*/
char* getToken(char* bufferString, char* token,
               int maxTokenSize, char delimiter);

/*--------------------------------------------------------------------*/
/*  Structure describing a CICS program                               */
/*--------------------------------------------------------------------*/
typedef struct {
    char CICSProgram[PROGNAME_LEN + 1]; /* Program to invoke          */
    long CICSLength;                 /* Size of the commarea          */
    long CICSDataLength;             /* Size of the input data        */
    char CICSSysID[SYSID_LEN + 1];   /* Remote CICS system            */
    long CICSSyncOnReturn;           /* Commit on each invoke         */
    char CICSTransID[TRANSID_LEN + 1]; /* Remote mirror transaction ID*/
    char CICSChannel[CHANNEL_LEN + 1]; /* Channel                     */
} CICSProgramDesc;

/*====================================================================*/
/*  Call a CICS program                                               */
/*====================================================================*/
int invokeProgram(Message* pRequestMessage,
                  Message* pResponseMessage,
                  TraceParms* pTraceParms) {
            
    int rc;
    CICSProgramDesc programDesc;
    HeaderPartContent* pRequestHeaderPartContent
         = (HeaderPartContent*) pRequestMessage->pHeaderPart->content;
    
    if (pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(pTraceParms, "About to invoke program");
    }
    
    rc = init(&programDesc, pTraceParms);
    
    /* Parse the request header key/value pairs */
    if (OK_CODE == rc) {
        rc = parseKeyValues(
                    pRequestHeaderPartContent->keyValuesSize.as_int,
                    pRequestHeaderPartContent + 1,
                    &programDesc);
    }
    
    /* Link to the target program */
    if (OK_CODE == rc) {
        if (strlen(programDesc.CICSChannel) > 0) {
            rc = linkChannel(&programDesc,
                             pRequestMessage,
                             pResponseMessage);
        } else {
            rc = linkCommarea(&programDesc,
                              pRequestMessage,
                              pResponseMessage);
        }
    }
    
    /* Feedback any error message to the caller */
    if (ERROR_CODE == rc) {
        if (g_traceParms.traceMode == TRUE_CODE) {
           traceMessage(pTraceParms, "Invoke program failed");
        }
        strcpy(pTraceParms->formattedErrorMessage,
               g_traceParms.formattedErrorMessage);
        return ERROR_CODE;
    }
    
    if (g_traceParms.traceMode == TRUE_CODE) {
       traceMessage(pTraceParms, "Return from invoke program");
    }
    
    return OK_CODE;
}

/*====================================================================*/
/* Perform initialization of global variables and get an EIB block    */
/*====================================================================*/
int init(CICSProgramDesc* pProgramDesc, TraceParms* pTraceParms) {
   
    /* Initialize global variables */
    memset(g_traceMessage, 0, sizeof(g_traceMessage));
    memset(pProgramDesc, 0, sizeof(*pProgramDesc));
   
    /* Initialize local tracing variables */
    strcpy(g_traceParms.module, "LSLNKBIN");
    strcpy(g_traceParms.CxID, pTraceParms->CxID);
    g_traceParms.traceMode = pTraceParms->traceMode;
    strcpy(g_traceParms.formattedErrorMessage,
           pTraceParms->formattedErrorMessage);
    initLog();
    
    /* Get commarea address and EIB bloc address                      */
    EXEC CICS ADDRESS
              COMMAREA(ca_ptr)
              EIB(dfheiptr)
              RESP(g_cicsResp) RESP2(g_cicsResp2);                  
                                               
    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError(&g_traceParms, "ADDRESS", g_cicsResp, g_cicsResp2);
       return ERROR_CODE;
    }
    return OK_CODE;
}

/*====================================================================*/
/*  Link with COMMAREA option.                                        */
/*  The request message should contain a single data part which       */
/*  content is to be used as the commarea.                            */
/*  Program header must contain enough data to execute an EXEC CICS   */
/*  LINK COMMAREA to the target program.                              */
/*====================================================================*/
int linkCommarea(CICSProgramDesc* pProgramDesc,
                 Message* pRequestMessage,
                 Message* pResponseMessage) {
                    
    char* inputContent;
    
    if (g_traceParms.traceMode == TRUE_CODE) {
       traceMessage(&g_traceParms, "Entered linkCommarea");
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
        if (g_traceParms.traceMode == TRUE_CODE) {
           traceMessage(&g_traceParms, "linkCommarea failed");
        }
        logCicsError(&g_traceParms, "LINK",g_cicsResp,g_cicsResp2);
        return ERROR_CODE;
    }
    
    /* Format the response message */
    if (OK_CODE != formatCommareaResponse(
                     pRequestMessage, pResponseMessage)) {
        return ERROR_CODE;
    }
    
    if (g_traceParms.traceMode == TRUE_CODE) {
       traceMessage(&g_traceParms, "Return from linkCommarea");
    }
    return OK_CODE;
}

/*====================================================================*/
/*  The response message for commarea-driven programs has a single    */
/*  data message part which content is identical to the request       */
/*  data message part. This is because a commarea is a single memory  */
/*  location used for both input and output.                          */
/*====================================================================*/
int formatCommareaResponse(
                 Message* pRequestMessage,
                 Message* pResponseMessage) {
 
    /* Format header part, indicating a single output message
     * part and no key/values.  */
    int headerLength = PARTS_NUM_LEN + KEYVAL_SIZE_LEN;
    HeaderPartContent* pResponseHeaderPartContent;
    
    if (g_traceParms.traceMode == TRUE_CODE) {
       traceMessage(&g_traceParms, "Formatting commarea response");
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
       logCicsError(&g_traceParms, "GETMAIN",
                     g_cicsResp, g_cicsResp2);
       return ERROR_CODE;
    }
    if (g_traceParms.traceMode == TRUE_CODE) {
       sprintf(g_traceMessage,
        "Response header part content allocated at %#x size=%d",
        (int)pResponseMessage->pHeaderPart->content, headerLength);
        traceMessage(&g_traceParms, g_traceMessage);
    }
    pResponseHeaderPartContent 
         = (HeaderPartContent*) pResponseMessage->pHeaderPart->content;
    pResponseHeaderPartContent->partsNumber.as_int = 1;
    pResponseHeaderPartContent->keyValuesSize.as_int = 0;
           
    
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
    
    /* In order to avoid having 2 pointers on the same memory location
     * which would complicate freeing memory, we disconnect the 
     * input message from its content. */
    pRequestMessage->pParts->size.as_int = 0;
    pRequestMessage->pParts->content = NULL;
    
}

/*====================================================================*/
/*  ***** NOT IMPLEMENTED ******                                      */
/*  Link with CHANNEL option.                                         */
/*  Program header must contain enough data to execute an EXEC CICS   */
/*  LINK CHANNEL to the target program.                               */
/*====================================================================*/
int linkChannel(CICSProgramDesc* pProgramDesc,
                Message* pRequestMessage,
            Message* pResponseMessage) {
    logError(&g_traceParms, "Link channel is not implemented.");
    return ERROR_CODE;
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
    
    if (g_traceParms.traceMode == TRUE_CODE) {
       traceMessage(&g_traceParms, "Entered checkCommarea");
    }
    /* We need a program name; We don't perform an inquire on the
     * program because that fails with autoinstall. */
    if (strlen(pProgramDesc->CICSProgram) == 0) {
        logError(&g_traceParms, "No CICS program name was provided.");
        return ERROR_CODE;
    }
 
    /* There must be one, and only one, input message part. */
    if (inPartsNum == 0) {
        logError(&g_traceParms, "No input message part for commarea.");
        return ERROR_CODE;
    }
    if (inPartsNum > 1) {
        logError(&g_traceParms, "Too many message parts for commarea.");
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
        logError(&g_traceParms,
                   "Data length cannot exceed commarea length.");
        return ERROR_CODE;
    }
     
    /* If no data length was specified, assume the size of the
     * incoming data. */
    if (pProgramDesc->CICSDataLength == 0) {
        pProgramDesc->CICSDataLength = pInputPart->size.as_int;
    }
    
    if (g_traceParms.traceMode == TRUE_CODE) {
       traceMessage(&g_traceParms, "Return from checkCommarea");
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
    
    if (g_traceParms.traceMode == TRUE_CODE) {
       traceMessage(&g_traceParms, "Reallocating content");
    }
    EXEC CICS GETMAIN
              SET(newContent)
              INITIMG('\0')
              FLENGTH(pProgramDesc->CICSLength + 1)
              RESP(g_cicsResp) RESP2(g_cicsResp2);

    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError(&g_traceParms, "GETMAIN",
                     g_cicsResp, g_cicsResp2);
       return ERROR_CODE;
    }
    memcpy(newContent, pInputPart->content, pInputPart->size.as_int);
    if (pInputPart->content != NULL) {
        EXEC CICS FREEMAIN
                  DATAPOINTER(pInputPart->content)
                  RESP(g_cicsResp) RESP2(g_cicsResp2);
        if (g_cicsResp != DFHRESP(NORMAL)) {
           logCicsError(&g_traceParms, "FREEMAIN",
                        g_cicsResp, g_cicsResp2);
           return ERROR_CODE;
        }
    }
    
    if (g_traceParms.traceMode == TRUE_CODE) {
       sprintf(g_traceMessage,
        "Content reallocated from %#x size=%d to %#x size=%d",
        (int)pInputPart->content,
        pInputPart->size.as_int,
        (int)newContent,
        pProgramDesc->CICSLength);
        traceMessage(&g_traceParms, g_traceMessage);
    }
    /* Update the input part to reflect the new allocation */
    pInputPart->content = newContent;
    pInputPart->size.as_int = pProgramDesc->CICSLength;

    return OK_CODE;
}
/*====================================================================*/
/* Extract key/value pairs from JSON string                           */
/*====================================================================*/
int parseKeyValues(int keyValuesSize,
                   char* keyValues,
                   CICSProgramDesc* pProgramDesc) {
    char* remaining;
    char key[MAX_KEY_LEN];
    char value[MAX_VALUE_LEN];
    
    if (g_traceParms.traceMode == TRUE_CODE) {
       traceMessage(&g_traceParms, "Entered parseKeyValues");
    }
    
    /* We expect a sequence of key/value pairs */
    remaining = getToken(
                    keyValues, key, MAX_KEY_LEN - 1, KEYWORD_DELIM);
    while (key != NULL && strlen(key) > 0
           && remaining != NULL && *remaining == ':') {
        remaining = getToken(remaining, value, MAX_VALUE_LEN - 1,
                             KEYWORD_DELIM);
        processKeyValue(key, value, pProgramDesc);
        remaining = getToken(remaining, key, MAX_KEY_LEN - 1,
                             KEYWORD_DELIM);
    }
    
    if (g_traceParms.traceMode == TRUE_CODE) {
        sprintf(g_traceMessage,
            "CICSProgram=%s", pProgramDesc->CICSProgram);
        traceMessage(&g_traceParms, g_traceMessage);
        sprintf(g_traceMessage,
            "CICSLength=%d", pProgramDesc->CICSLength);
        traceMessage(&g_traceParms, g_traceMessage);
        sprintf(g_traceMessage,
            "CICSDataLength=%d", pProgramDesc->CICSDataLength);
        traceMessage(&g_traceParms, g_traceMessage);
        sprintf(g_traceMessage,
            "CICSSysID=%s", pProgramDesc->CICSSysID);
        traceMessage(&g_traceParms, g_traceMessage);
        sprintf(g_traceMessage,
            "CICSSyncOnReturn=%d", pProgramDesc->CICSSyncOnReturn);
        traceMessage(&g_traceParms, g_traceMessage);
        sprintf(g_traceMessage,
            "CICSTransID=%s", pProgramDesc->CICSTransID);
        traceMessage(&g_traceParms, g_traceMessage);
        sprintf(g_traceMessage,
            "CICSChannel=%s", pProgramDesc->CICSChannel);
        traceMessage(&g_traceParms, g_traceMessage);
        
        traceMessage(&g_traceParms, "Ended parseKeyValues");
    }
    
    return OK_CODE;
}

/*====================================================================*/
/* Match keys with expected headers and store corresponding values    */
/*====================================================================*/
int processKeyValue(char* key,
                    char* value,
                    CICSProgramDesc* pProgramDesc) {
    
    if (g_traceParms.traceMode == TRUE_CODE) {
        sprintf(g_traceMessage,
            "processKeyValue key=%s value=%s", key, value);
        traceMessage(&g_traceParms, g_traceMessage);
    }
    
    if (0 == strcmp(PROGRAM_KEY, key)) {
        memset(pProgramDesc->CICSProgram, ' ', PROGNAME_LEN);
        memcpy(pProgramDesc->CICSProgram, value, strlen(value));
    } else {
    if (0 == strcmp(LENGTH_KEY, key)) {
        pProgramDesc->CICSLength = atol(value);
    } else {
    if (0 == strcmp(DATALEN_KEY, key)) {
        pProgramDesc->CICSDataLength = atol(value);
    } else {
    if (0 == strcmp(SYSID_KEY, key)) {
        memset(pProgramDesc->CICSSysID, ' ', SYSID_LEN);
        memcpy(pProgramDesc->CICSSysID, value, strlen(value));
    } else {
    if (0 == strcmp(SYNCONRET_KEY, key)) {
        pProgramDesc->CICSSyncOnReturn = atol(value);
    } else {
    if (0 == strcmp(TRANSID_KEY, key)) {
        memset(pProgramDesc->CICSTransID, ' ', TRANSID_LEN);
        memcpy(pProgramDesc->CICSTransID, value, strlen(value));
    } else {
     if (0 == strcmp(CHANNEL_KEY, key)) {
        memset(pProgramDesc->CICSChannel, ' ', CHANNEL_LEN);
        memcpy(pProgramDesc->CICSChannel, value, strlen(value));
    }}}}}}}
    return OK_CODE;
}

/*====================================================================*/
/* Extract keywords between a given delimiter. Return the position    */
/* following the ending delimiter so that this routine can be called  */
/* iteratively.                                                       */
/*====================================================================*/
char* getToken(char* bufferString,
               char* token,
               int maxTokenSize,
               char delimiter) {
                
    char* startPos = NULL;
    char* endPos = NULL;
    int tokenSize;
    
    memset(token, '\0', maxTokenSize + 1);
    startPos = strchr(bufferString, (int) delimiter);
    
    /* No more keywords in the buffer string */
    if (startPos == NULL) {
        return NULL;
    }
    endPos = strchr(startPos + 1, delimiter);
    /* Unbalanced delimiters */
    if (endPos == NULL) {
         return NULL;
    }
    
    tokenSize = endPos - (startPos + 1);
    /* make sure token fits */
    if (tokenSize > maxTokenSize) {
        return NULL;
    }
    strncpy(token, startPos + 1, tokenSize);
    
    return endPos + 1; 
    
}

                         