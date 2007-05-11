#pragma XOPTS(CICS)
#pragma longname
#pragma export(invokeProgram)
#pragma export(freeProgram)
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
/*                invoke a CICS program passing data as a binary      */
/*                 payload.                                           */
/*                                                                    */
/*                Each request is described by a program header which */
/*                contains key/value pairs. These can be:             */
/*                Request:                                            */
/*                  CICSProgram       : CICS program name to link to  */
/*                  CICSLength        : Total size of the commarea    */
/*                  CICSDataLength    : Size of the input data        */
/*                  CICSSysID         : Remote CICS system            */
/*                  CICSSyncOnReturn  : Commit on each invoke         */
/*                  CICSTransID       : Remote mirror transaction ID  */
/*                  CICSChannel       : CICS Channel name             */
/*                  CICSOutContainers : List of output containers     */
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
#define MODULE_NAME   "LSLNKBIN" /* used to correlate traces          */
#define OBJECT_DELIM  '{'        /* start of an object in json        */
#define ARRAY_START_DELIM '['    /* start of an array in json         */
#define ARRAY_END_DELIM   ']'    /* start of an array in json         */
#define KEYWORD_DELIM '"'        /* delimits a keyword in key/values  */
#define VALUE_DELIM   '"'        /* delimits a value in key/values    */
#define KEYVAL_DELIM  ':'        /* separates keys from values        */
#define SEP_DELIM     ','        /* separates items                   */
#define MAX_KEY_LEN   33         /* largest key string                */
#define MAX_VALUE_LEN 513        /* largest value string              */
#define KEYVALUES_EC  "keyvalues"    /* Start of key/value pairs      */
#define PROGRAM_KEY   "CICSProgram"  /* CICS program to invoke        */
#define LENGTH_KEY    "CICSLength"   /* Size of the commarea          */
#define DATALEN_KEY   "CICSDataLength"  /* Size of the input data     */
#define SYSID_KEY     "CICSSysID"    /* Remote CICS system            */
#define SYNCONRET_KEY "CICSSyncOnReturn" /* Commit on each invoke     */
#define TRANSID_KEY   "CICSTransID"  /* Remote mirror transaction ID  */
#define CHANNEL_KEY   "CICSChannel"  /* CICS channel to use           */
#define OUTCONTAINERS_KEY  "CICSOutContainers"  /* Output containers  */

/*--------------------------------------------------------------------*/
/*  Prototypes                                                        */
/*--------------------------------------------------------------------*/
char* getToken(char* bufferString, char* token,
               int maxTokenSize, char startDelim, char endDelim);
char* getValueStart(char* bufferString) ;

/*====================================================================*/
/*  Call a CICS program                                               */
/*====================================================================*/
int invokeProgram(DFHEIBLK *inDfheiptr,
                  TraceParms* inTraceParms,
                  CICSProgramDesc* pProgramDesc,
                  Message* pRequestMessage,
                  Message* pResponseMessage) {
            
    int rc;
    HeaderPartContent* pRequestHeaderPartContent
         = (HeaderPartContent*) pRequestMessage->pHeaderPart->content;
    
    dfheiptr = inDfheiptr;
    g_pTraceParms = inTraceParms;
    initLog(dfheiptr, inTraceParms);
    initProgramDesc(pProgramDesc);
    
    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "About to invoke program");
    }
    
    /* Parse the request header key/value pairs */
    rc = parseKeyValues(
                pRequestHeaderPartContent->keyValuesSize.as_int,
                pRequestHeaderPartContent + 1,
                pProgramDesc);
    /* Link to the target program */
    if (OK_CODE == rc) {
        if (strlen(pProgramDesc->CICSChannel) > 0
            && pProgramDesc->CICSChannel[0] != ' ') {
            rc = linkChannel(dfheiptr,
                             g_pTraceParms,
                             pProgramDesc,
                             pRequestMessage,
                             pResponseMessage);
        } else {
            rc = linkCommarea(dfheiptr,
                              g_pTraceParms,
                              pProgramDesc,
                              pRequestMessage,
                              pResponseMessage);
        }
    }
    
    if (g_pTraceParms->traceMode == TRUE_CODE) {
       if (ERROR_CODE == rc) {
           traceMessage(MODULE_NAME, "Invoke program failed");
       } else {
           traceMessage(MODULE_NAME, "Return from invoke program");
       }
    }
    
    return rc;
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
    int rc = OK_CODE;
    
    if (g_pTraceParms->traceMode == TRUE_CODE) {
       traceMessage(MODULE_NAME, "Entered parseKeyValues");
    }
    
    /* We expect the first token encountered to be a key */
    remaining = getToken(
                    keyValues, key, MAX_KEY_LEN - 1,
                    KEYWORD_DELIM, KEYWORD_DELIM);
    while (rc == OK_CODE
           && key != NULL
           && strlen(key) > 0
           && remaining != NULL) {
      
        /*  Values are separated from keys by a column char */
        remaining = getValueStart(remaining);
        if (remaining != NULL) {
            switch(remaining[0]) {
               /* Case of a simple value "key":"value" */
               case VALUE_DELIM:
                  remaining = getToken(
                                 remaining, value, MAX_VALUE_LEN - 1,
                                 VALUE_DELIM, VALUE_DELIM);
                  rc = processKeyValue(key, value, pProgramDesc);
                  break;
               /* Case of an array "key":["value1",.,"valuen"]*/
               case ARRAY_START_DELIM:
                  remaining = getToken(
                                 remaining, value, MAX_VALUE_LEN - 1,
                                 ARRAY_START_DELIM, ARRAY_END_DELIM);
                  rc = processKeyArray(key, value, pProgramDesc);
                  break;
            }
            /* Look for the next key */
            remaining = getToken(remaining, key, MAX_KEY_LEN - 1,
                                 KEYWORD_DELIM, KEYWORD_DELIM);
        }
    }
    
    if (g_pTraceParms->traceMode == TRUE_CODE) {
        traceProgramDesc(pProgramDesc);
        traceMessage(MODULE_NAME, "Ended parseKeyValues");
    }
    
    return rc;
}

/*====================================================================*/
/* Make sure all parameters have default values.                      */
/*====================================================================*/
int initProgramDesc(CICSProgramDesc* pProgramDesc) {
    memset(pProgramDesc->CICSProgram, '\0', PROGNAME_LEN + 1);
    pProgramDesc->CICSLength = 0;
    pProgramDesc->CICSDataLength = 0;
    memset(pProgramDesc->CICSSysID, '\0', SYSID_LEN + 1);
    pProgramDesc->CICSSyncOnReturn = FALSE_CODE;
    memset(pProgramDesc->CICSTransID, '\0', TRANSID_LEN + 1);
    memset(pProgramDesc->CICSChannel, '\0', CHANNEL_NAME_LEN + 1);
    pProgramDesc->CICSOutputContainersCount = 0;
    memset(pProgramDesc->CICSOutputContainers, ' ',
       MAX_OUT_CONTAINERS * (CONTAINER_NAME_LEN + 1));
    return OK_CODE;
}

/*====================================================================*/
/* Trace parameter set extracted from JSON string                     */
/*====================================================================*/
int traceProgramDesc(CICSProgramDesc* pProgramDesc) {
    int i;
    char outContainerName[CONTAINER_NAME_LEN + 1];
    int pos = 0;
    
    sprintf(g_traceMessage,
        "CICSProgram=%s", pProgramDesc->CICSProgram);
    traceMessage(MODULE_NAME, g_traceMessage);
    sprintf(g_traceMessage,
        "CICSLength=%d", pProgramDesc->CICSLength);
    traceMessage(MODULE_NAME, g_traceMessage);
    sprintf(g_traceMessage,
        "CICSDataLength=%d", pProgramDesc->CICSDataLength);
    traceMessage(MODULE_NAME, g_traceMessage);
    sprintf(g_traceMessage,
        "CICSSysID=%s", pProgramDesc->CICSSysID);
    traceMessage(MODULE_NAME, g_traceMessage);
    sprintf(g_traceMessage,
        "CICSSyncOnReturn=%d", pProgramDesc->CICSSyncOnReturn);
    traceMessage(MODULE_NAME, g_traceMessage);
    sprintf(g_traceMessage,
        "CICSTransID=%s", pProgramDesc->CICSTransID);
    traceMessage(MODULE_NAME, g_traceMessage);
    sprintf(g_traceMessage,
        "CICSChannel=%s", pProgramDesc->CICSChannel);
    traceMessage(MODULE_NAME, g_traceMessage);
    sprintf(g_traceMessage,
        "CICSOutputContainersCount=%d",
         pProgramDesc->CICSOutputContainersCount);
    traceMessage(MODULE_NAME, g_traceMessage);
    
    for (i=0; i < pProgramDesc->CICSOutputContainersCount; i++) {
        memset(outContainerName, '\0', CONTAINER_NAME_LEN + 1);
        memcpy(outContainerName,
           pProgramDesc->CICSOutputContainers + pos,
           CONTAINER_NAME_LEN);
        sprintf(g_traceMessage,
            "CICSOutContainer=%s", outContainerName);
        traceMessage(MODULE_NAME, g_traceMessage);
        pos += (CONTAINER_NAME_LEN + 1);
    }
    return OK_CODE;
}

/*====================================================================*/
/* Match keys with expected headers and store corresponding values    */
/*====================================================================*/
int processKeyValue(char* key,
                    char* value,
                    CICSProgramDesc* pProgramDesc) {
    
    if (value == NULL || strlen(value) == 0) {
        logError(MODULE_NAME, "Key found with no associated value.");
        return ERROR_CODE;
    }
    
    if (g_pTraceParms->traceMode == TRUE_CODE) {
        sprintf(g_traceMessage,
            "processKeyValue key=%s value=%s", key, value);
        traceMessage(MODULE_NAME, g_traceMessage);
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
        memset(pProgramDesc->CICSChannel, ' ', CHANNEL_NAME_LEN);
        memcpy(pProgramDesc->CICSChannel, value, strlen(value));
    }}}}}}}
    return OK_CODE;
}

/*====================================================================*/
/* Match keys with expected headers and store corresponding values    */
/*====================================================================*/
int processKeyArray(char* key,
                    char* value,
                    CICSProgramDesc* pProgramDesc) {
    
    char itemValue[MAX_VALUE_LEN];
    char* remaining = value;
    int pos = 0;
    int maxSize = MAX_OUT_CONTAINERS * (CONTAINER_NAME_LEN + 1);
    
    if (value == NULL || strlen(value) == 0) {
        logError(MODULE_NAME, "Key found with no associated value.");
        return ERROR_CODE;
    }
    
    if (g_pTraceParms->traceMode == TRUE_CODE) {
        sprintf(g_traceMessage,
            "processKeyArray key=%s value=%s", key, value);
        traceMessage(MODULE_NAME, g_traceMessage);
    }
    
    if (0 == strcmp(OUTCONTAINERS_KEY, key)) {
        /* Get the first item value */
        remaining = getToken(
                       remaining, itemValue, MAX_VALUE_LEN - 1,
                       VALUE_DELIM, VALUE_DELIM);
        while(itemValue != NULL
                && strlen(itemValue) > 0 && pos < maxSize) {
            memcpy(pProgramDesc->CICSOutputContainers + pos,
                   itemValue, strlen(itemValue));
            pProgramDesc->CICSOutputContainersCount += 1;
            pos += (CONTAINER_NAME_LEN + 1);
            remaining = getToken(
                           remaining, itemValue, MAX_VALUE_LEN - 1,
                           VALUE_DELIM, VALUE_DELIM);
        }
    }
    
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
               char startdelim,
               char endDelim) {
                
    char* startPos = NULL;
    char* endPos = NULL;
    int tokenSize;
    
    memset(token, '\0', maxTokenSize + 1);
    if (bufferString == NULL || strlen(bufferString) == 0) {
        return NULL;
    }
    
    startPos = strchr(bufferString, (int) startdelim);
    
    /* No more keywords in the buffer string */
    if (startPos == NULL || strlen(startPos) < 2) {
        return NULL;
    }
    
    /* Look for the ending delimiter */
    endPos = strchr(startPos + 1, endDelim);
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

/*====================================================================*/
/* When a key has been found, the buffer is left on the first         */
/* character following the closing ". This routine will look for      */
/* the expected column that separates keys from values and then       */
/* take position on the value delimiter that could be a " for simple  */
/* values, [ for arrays and { for objects.                            */
/*====================================================================*/
char* getValueStart(char* bufferString) {
    int i = 0;
    
    /* skip any leading spaces */
    while (i < strlen(bufferString) && bufferString[i] == ' ') {
       i++;
    }
    
    /* this is all spaces */
    if (i == strlen(bufferString)) {
       return NULL;
    }
    
    /* this must be a column */
    if (bufferString[i] != KEYVAL_DELIM) {
       return NULL;
    }
    /* skip the column */
    i++;
    
    /* skip any following spaces */
    while (i < strlen(bufferString) && bufferString[i] == ' ') {
       i++;
    }
    
    /* no values are provided */
    if (i >= strlen(bufferString)) {
       return NULL;
    }

    return bufferString + i;
    
}

/*====================================================================*/
/*  Release resources allocated to service this program               */
/*====================================================================*/
int freeProgram(DFHEIBLK *inDfheiptr,
                TraceParms* inTraceParms,
                CICSProgramDesc* pProgramDesc,
                Message* pRequestMessage,
                Message* pResponseMessage) {
    
    int rc = OK_CODE;
    dfheiptr = inDfheiptr;
    g_pTraceParms = inTraceParms;
    initLog(dfheiptr, inTraceParms);
    
    if (strlen(pProgramDesc->CICSChannel) > 0
        && pProgramDesc->CICSChannel[0] != ' ') {
        rc = freeChannel(dfheiptr,
                         g_pTraceParms,
                         pProgramDesc,
                         pRequestMessage,
                         pResponseMessage);
    } else {
        rc = freeCommarea(dfheiptr,
                          g_pTraceParms,
                          pProgramDesc,
                          pRequestMessage,
                          pResponseMessage);
    }
    return rc;
}

                         