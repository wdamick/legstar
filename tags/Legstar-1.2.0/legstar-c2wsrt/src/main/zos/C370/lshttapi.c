#pragma XOPTS(CICS)
#pragma longname   
#pragma export(init)
#pragma export(invoke)
#pragma export(strCob2C)
#pragma export(strC2Cob)
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
/*  Purpose    - HTTP Client library                                  */
/*  Language   - IBM C/370                                            */
/*  System     - Tested on CICS TS 2.3                                */
/*  History    - 08 Jul 2007  - Original Implementation               */
/*  Notes      - This API provides the caller with a high level       */
/*               interface to the legstar RPC protocol over HTTP.     */
/*               It is expected that on the other end of the line,    */
/*               an RPC server will process requests emitted by this  */
/*               API.                                                 */
/*               See LSSOKBIN in legstar-csokrt for a description of  */
/*               the RPC protocol.                                    */
/*                                                                    */
/**********************************************************************/
/*--------------------------------------------------------------------*/
/*  Constants                                                         */
/*--------------------------------------------------------------------*/
#define MODULE_NAME   "LSHTTAPI" /* used to correlate traces          */
#define JSON_REQ_FORMAT "{\"ServiceName\":\"%s\"}" /* Format of the
                          JSON request for a service   */
#define MAX_JSON_STR_LEN 256  /* Maximum size of JSON string          */ 
#define MAX_URL_STR_LEN 512   /* Maximum size of a URL string         */ 
#define MAX_SERVICE_NAME_LEN 32   /* Maximum size of service name     */ 

/*--------------------------------------------------------------------*/
/*  Headers included                                                  */
/*--------------------------------------------------------------------*/
#include <stdio.h>               /* get the standard I/O library      */
#include <stdlib.h>              /* get the standard library          */
#include <string.h>              /* support string operations         */
#include "lscomdec.h"            /* legstar common declarations       */
#include "lshttlib.h"            /* http library                      */

/*--------------------------------------------------------------------*/
/*  Invoke parameters structure                                       */
/*--------------------------------------------------------------------*/
typedef struct {
    int connect_timeout;
    int recv_timeout;
    char proxy_url[MAX_URL_STR_LEN + 1];
    Http_basic_credentials credentials;
} InvokeOptions;

typedef struct {
    char url[MAX_URL_STR_LEN + 1];
    char service_name[MAX_SERVICE_NAME_LEN + 1];
    char* request_data;
    int request_data_len;
    char* reply_data;
    int reply_data_len; 
    InvokeOptions options;   
} InvokeParms;

/*====================================================================*/
/* Initialize logging and tracing variables.                          */
/*====================================================================*/
int init(DFHEIBLK *inDfheiptr, TraceParms* trace_parms)
{
    int rc;
    dfheiptr = inDfheiptr;
    g_pTraceParms = trace_parms;
    rc = strCDelimit(trace_parms->CxID, MAX_CXID_LEN);
    if (rc != OK_CODE) {
        logError(MODULE_NAME, "Invalid trace parameters");
        return rc;
    }
    
    initLog(dfheiptr, trace_parms);
    memset(g_traceMessage, 0, sizeof(g_traceMessage));
    
    return OK_CODE;
}

/*====================================================================*/
/*  Issue a stateless HTTP POST request                               */
/*  This is a single request message/single response message          */
/*  exchange pattern.                                                 */
/*  There is no FREEMAIN of the memory acquired in this module. It is */
/*  assumed this will be used in a CICS pseudo-conversational         */
/*  transaction and that memory will be automatically reclaimed at    */
/*  transaction end.                                                  */
/*====================================================================*/
int invoke(InvokeParms * parms) {
    int rc;
    Http_options options;
    Http_content http_request;
    Http_content http_reply;
    Message requestMessage;
    Message responseMessage;
    MessagePart headerPart;
    MessagePart dataPart;
    MessagePart responseHeaderPart;
    MessagePart responseDataPart;

    rc = checkInvokeParms(parms);
    if (rc != OK_CODE) {
        logError(MODULE_NAME, "Invalid invoke parameters");
        return rc;
    }
    if (g_pTraceParms->traceMode == TRUE_CODE) {
        traceInvokeParms(parms);
    }
      
    /* Create options structure to the httplib format  */
    if (parms->options.connect_timeout == 0) {
        options.connect_timeout = DEFAULT_CONNECT_TIMEOUT;
    } else {
        options.connect_timeout = parms->options.connect_timeout;
    }
    if (parms->options.recv_timeout == 0) {
        options.recv_timeout = DEFAULT_RECV_TIMEOUT;
    } else {
        options.recv_timeout = parms->options.recv_timeout;
    }
    options.proxy_url = parms->options.proxy_url;
    options.credentials = &(parms->options.credentials);

    /* Create request and prepare response structure  */
    http_reply.content = NULL;
    requestMessage.pHeaderPart = &headerPart;
    requestMessage.pParts = &dataPart;
    responseMessage.pHeaderPart = &responseHeaderPart;
    responseMessage.pParts = &responseDataPart;
    
    rc = formatRequest(parms->service_name,
                       parms->request_data,
                       parms->request_data_len,
                       &requestMessage);
    if (rc != OK_CODE) {
        logError(MODULE_NAME, "Request formatting failed");
        return rc;
    }
    http_request.content_len = serializeMessage(&requestMessage,
                                           &http_request.content);
    if (http_request.content_len < 0) {
        logError(MODULE_NAME, "Serializing request failed");
        return ERROR_CODE;
    }
    
    strcpy(http_request.content_type, "binary/octet-stream");
    
    /* Post request to server */
    rc = http_invoke("POST", parms->url, &http_request, &http_reply,
                      &options, g_pTraceParms);
    if (rc != OK_CODE) {
        sprintf(g_traceMessage,
                "HTTP invoke failed. %s",  
                g_pTraceParms->formattedErrorMessage);
        logError(MODULE_NAME, g_traceMessage);
        return rc;
    }

    if (http_reply.content != NULL) {
        rc = deserializeMessage(&responseMessage,
                         http_reply.content, http_reply.content_len);
        free(http_reply.content);
        if (rc != OK_CODE) {
            logError(MODULE_NAME, "Response deserialization failed");
            return rc;
        }
        if (g_pTraceParms->traceMode == TRUE_CODE) {
            dumpMessage(MODULE_NAME, &responseMessage);
        }
        
        /* Only the first data part is of interest */
        parms->reply_data = responseDataPart.content;
        parms->reply_data_len = responseDataPart.size.as_int;

    } else {
        parms->reply_data = NULL;
        parms->reply_data_len = 0;
    }
    
    if (g_pTraceParms->traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "Return from invoke");
        traceMessage(MODULE_NAME,
         "===============================================");
    }
    return OK_CODE;
}

/*====================================================================*/
/*  Make sure parameters are useable. When caller is cobol, fields    */
/*  will have to be null terminated.                                  */
/*====================================================================*/
int checkInvokeParms(InvokeParms * parms) {
    int rc;
    char *debugPtr;
    
    rc = strCDelimit(parms->url, MAX_URL_STR_LEN);
    if (rc != OK_CODE) {
        return rc;
    }
    rc = strCDelimit(parms->service_name, MAX_SERVICE_NAME_LEN);
    if (rc != OK_CODE) {
        return rc;
    }
    rc = strCDelimit(parms->options.proxy_url, MAX_URL_STR_LEN);
    if (rc != OK_CODE) {
        return rc;
    }
    rc = strCDelimit(parms->options.credentials.user, USERID_MAXLEN);
    if (rc != OK_CODE) {
        return rc;
    }
    rc = strCDelimit(parms->options.credentials.password,
                     PASSWORD_MAXLEN);
    if (rc != OK_CODE) {
        return rc;
    }
}

/*====================================================================*/
/*  Trace the invoke parameters                                       */
/*====================================================================*/
int traceInvokeParms(InvokeParms * parms) {
    traceMessage(MODULE_NAME,
     "===============================================");
    traceMessage(MODULE_NAME, "Entered invoke with parameters:");
    sprintf(g_traceMessage, "  URI: %s", parms->url);
    traceMessage(MODULE_NAME, g_traceMessage);
    sprintf(g_traceMessage, "  ServiceName: %s",
                    parms->service_name);
    traceMessage(MODULE_NAME, g_traceMessage);
    sprintf(g_traceMessage, "  Request Data length: %d",
                    parms->request_data_len);
    traceMessage(MODULE_NAME, g_traceMessage);
    traceMessage(MODULE_NAME, "  Request data:");
    traceMessage(MODULE_NAME,
     "-----------------------------------------------");
    traceData(MODULE_NAME, parms->request_data,
                    parms->request_data_len);
    traceMessage(MODULE_NAME,
     "-----------------------------------------------");
    sprintf(g_traceMessage, "  Connect timeout: %d seconds",
                            parms->options.connect_timeout);
    traceMessage(MODULE_NAME, g_traceMessage);
    sprintf(g_traceMessage, "  Receive timeout: %d seconds",
                            parms->options.recv_timeout);
    traceMessage(MODULE_NAME, g_traceMessage);
    sprintf(g_traceMessage, "  Proxy URL: %s",
                            parms->options.proxy_url);
    traceMessage(MODULE_NAME, g_traceMessage);
    sprintf(g_traceMessage, "  User: %s",
            parms->options.credentials.user);
    traceMessage(MODULE_NAME, g_traceMessage);
}

/*====================================================================*/
/*  Formats a request compliant with the LSSOKBIN protocol            */
/*  This code is for a single message request                         */
/*====================================================================*/
int formatRequest(char* serviceName,
                  char* request_data,
                  int request_data_len,
                  Message* requestMessage) {
    int rc;
    
    if (g_pTraceParms->traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "About to format request");
    }

    /* This is a single message request for now */
    rc = formatRequestHeader(serviceName, requestMessage->pHeaderPart,
                             1);
    if (rc != OK_CODE) {
        return rc;
    }
    
    rc = formatMessageData(requestMessage->pParts, request_data,
                           request_data_len);
    if (rc != OK_CODE) {
        return rc;
    }
 
    if (g_pTraceParms->traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "Request formatted");
        dumpMessage(MODULE_NAME, requestMessage);
    }
    return OK_CODE;
}

/*====================================================================*/
/*  Formats a request header compliant with the LSSOKBIN protocol     */
/*  The header contains the number of message part an a JSON string   */
/*  that further describes the request.                               */
/*====================================================================*/
int formatRequestHeader(char* serviceName,
                        MessagePart* requestHeaderPart,
                        int partsNum) {
    int rc;
    int headerContentLen = PARTS_NUM_LEN + KEYVAL_SIZE_LEN;
    char jsonString[MAX_JSON_STR_LEN];
    HeaderPartContent* headerContent;

    if (g_pTraceParms->traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "About to format request header");
    }
    
    memset(requestHeaderPart->ID, ' ', MSG_ID_LEN);
    memcpy(requestHeaderPart->ID, HEADER_PART_ID,
           sizeof(HEADER_PART_ID) - 1);
           
    sprintf(jsonString, JSON_REQ_FORMAT, serviceName);
    headerContentLen += strlen(jsonString);
    requestHeaderPart->size.as_int = headerContentLen;
           
    rc = allocMem(headerContentLen, &requestHeaderPart->content,
                  "Request header part content");
    if (rc != OK_CODE) {
        return rc;
    }

    headerContent = (HeaderPartContent*) requestHeaderPart->content;
    headerContent->partsNumber.as_int = partsNum;
    headerContent->keyValuesSize.as_int = strlen(jsonString);
    memcpy(requestHeaderPart->content + PARTS_NUM_LEN + KEYVAL_SIZE_LEN,
           jsonString, strlen(jsonString));

    if (g_pTraceParms->traceMode == TRUE_CODE) {
       sprintf(g_traceMessage,
              "Request header formatted, parts num=%d json=%s",
               headerContent->partsNumber.as_int,
               jsonString);
       traceMessage(MODULE_NAME, g_traceMessage);
    }
    return OK_CODE;
}

/*====================================================================*/
/*  Formats a message data part                                       */
/*====================================================================*/
int formatMessageData(MessagePart* messagePart,
                      char* data, int data_len) {
                        
    if (g_pTraceParms->traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "About to format data part");
    }
    
    memset(messagePart->ID, ' ', MSG_ID_LEN);
    memcpy(messagePart->ID, COMMAREA_PART_ID,
           sizeof(COMMAREA_PART_ID) - 1);
    messagePart->size.as_int = data_len;
    messagePart->content = data;
    
    if (g_pTraceParms->traceMode == TRUE_CODE) {
       sprintf(g_traceMessage,
              "Data part formatted, content size=%d",
               messagePart->size.as_int);
       traceMessage(MODULE_NAME, g_traceMessage);
    }
    return OK_CODE;
}

/*====================================================================*/
/*  Allocate memory                                                   */
/*====================================================================*/
int allocMem(int len, char **pointer, char* nature) {

    if (g_pTraceParms->traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "About to allocate memory");
    }
    
    EXEC CICS GETMAIN
              SET      (*pointer)
              INITIMG  ('\0')
              FLENGTH  (len)
              RESP     (g_cicsResp) RESP2(g_cicsResp2);

    if (g_cicsResp != DFHRESP(NORMAL)) {
       logCicsError(MODULE_NAME, "GETMAIN", g_cicsResp, g_cicsResp2);
       return ERROR_CODE;
    }
    
    if (g_pTraceParms->traceMode == TRUE_CODE) {
       sprintf(g_traceMessage,
        "%s allocated at %#x size=%d", nature,
        (int)*pointer, len);
        traceMessage(MODULE_NAME, g_traceMessage);
    }
    return OK_CODE;
}

/*====================================================================*/
/*  Creates an in-memory buffer holding the serialized content        */
/*  Returns the total serialized length                               */
/*====================================================================*/
int serializeMessage(Message *message, char** out) {
    int len, rc, pos, i;
    HeaderPartContent* headerContent;
    
    if (g_pTraceParms->traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "About to serialize message");
    }

    /* First allocate a buffer large enough to hold the message */
    len = getMessageLen(message);
    rc = allocMem(len, out, "Output HTTP body");
    if (rc != OK_CODE) {
        return ERROR_CODE;
    }

     /* Serialize the header */
    pos = 0;
    rc = serializeMessagePart(*out, &pos, message->pHeaderPart);
    if (rc != OK_CODE) {
        return ERROR_CODE;
    }
    headerContent = (HeaderPartContent*) message->pHeaderPart->content;

     /* Serialize the data message parts */
    for (i = 0; i < headerContent->partsNumber.as_int; i++) {
        rc = serializeMessagePart(*out, &pos, message->pParts + i);
        if (rc != OK_CODE) {
            return ERROR_CODE;
        }
    }
    
    if (g_pTraceParms->traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "Message serialized");
    }
    return len;
}

/*====================================================================*/
/*  Serialize a message part at a memory location                     */
/*====================================================================*/
int serializeMessagePart(char* out, int* pPos, MessagePart* pPart) {
    
    int contentLen = pPart->size.as_int;

    if (g_pTraceParms->traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "About to serialize message part");
    }
    /* Serialize the message part ID*/
    memcpy(out + *pPos, pPart->ID, MSG_ID_LEN);
    *pPos += MSG_ID_LEN;
    
    /* Serialize the message content size */
    memcpy(out + *pPos, pPart->size.as_bytes,
           MSG_CONTENT_SIZE_LEN);
    *pPos += MSG_CONTENT_SIZE_LEN;
    
    /* Serialize the part content */
    if (contentLen > 0 && pPart->content != NULL) {
        memcpy(out + *pPos, pPart->content,  contentLen);
        *pPos += contentLen;
    }
    
    if (g_pTraceParms->traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "Message part serialized");
    }
    
    return OK_CODE;
}

/*====================================================================*/
/*  Deserialize a message from an in-memory buffer                    */
/*====================================================================*/
int deserializeMessage(Message *message, char* in, int inLen) {
    int rc, pos, i;
    HeaderPartContent* headerContent;
    
    if (g_pTraceParms->traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "About to deserialize message");
    }
    
    /* First deserialize the header */
    pos = 0;
    rc = deserializeMessagePart(in, &pos, inLen, message->pHeaderPart);
    if (rc != OK_CODE) {
        return ERROR_CODE;
    }
    headerContent = (HeaderPartContent*) message->pHeaderPart->content;

     /* Deserialize the data message parts */
    for (i = 0; i < headerContent->partsNumber.as_int; i++) {
        rc = deserializeMessagePart(in, &pos, inLen,
                                    message->pParts + i);
        if (rc != OK_CODE) {
            return ERROR_CODE;
        }
    }

    if (g_pTraceParms->traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "Message deserialized");
    }
}

/*====================================================================*/
/*  Deserialize a message part from a memory location                 */
/*====================================================================*/
int deserializeMessagePart(char* in, int* pPos, int inLen,
                           MessagePart* pPart) {
    
    int rc = OK_CODE;
    int len;

    if (g_pTraceParms->traceMode == TRUE_CODE) {
        traceMessage(MODULE_NAME, "About to deserialize message part");
    }
    
    /* First receive the message part header (16 bytes 
       identifier and 4 bytes giving the part content size. */
    if (inLen < *pPos + MSG_ID_LEN + MSG_CONTENT_SIZE_LEN) {
       logError(MODULE_NAME, "Attempt to read past buffer end");
       return ERROR_CODE;
    }
    
    memcpy(pPart->ID, in + *pPos, MSG_ID_LEN);
    *pPos += MSG_ID_LEN;
    memcpy(pPart->size.as_bytes, in + *pPos, MSG_CONTENT_SIZE_LEN);
    *pPos += MSG_CONTENT_SIZE_LEN;
     
    /* Sanity check the size received */
    len = pPart->size.as_int;
    if ((len < 0) || (len > MSGPART_MAX_LEN)) {
       logError(MODULE_NAME, "Invalid message part length");
       return ERROR_CODE;
    }
 
    if (len > 0) {
        /* Acquire storage for the message part content. An additional
         * byte guaranteed to contain a binary zero is acquired so
         * that contents can be treated as C strings when necessary.  */
        rc = allocMem(len + 1, &pPart->content, "Message part content");
        if (rc != OK_CODE) {
            return ERROR_CODE;
        }
        
        /* Receive the message content */
        if (inLen < *pPos + len) {
            logError(MODULE_NAME, "Attempt to read past buffer end");
            return ERROR_CODE;
        }
        memcpy(pPart->content, in + *pPos, len);
        *pPos += len;
    } else {
        pPart->content = NULL;
    }
 
    if (g_pTraceParms->traceMode == TRUE_CODE) {
       sprintf(g_traceMessage,
              "Message part deserialized, id=%s size=%d",
               pPart->ID,
               pPart->size.as_int);
       traceMessage(MODULE_NAME, g_traceMessage);
    }
    return OK_CODE;
}

/*====================================================================*/
/*  Calculates the total length of a message                          */
/*====================================================================*/
int getMessageLen(Message *message) {
    int len = 0;
    int i;
    HeaderPartContent* headerContent;
    
    /* First account for the header length */
    len += MSG_ID_LEN + MSG_CONTENT_SIZE_LEN
          + message->pHeaderPart->size.as_int;
    
    /* Then add each of the data message length */
    headerContent = (HeaderPartContent*) message->pHeaderPart->content;
    for (i = 0; i < headerContent->partsNumber.as_int; i++) {
        len += MSG_ID_LEN + MSG_CONTENT_SIZE_LEN
         + message->pParts[i].size.as_int;
    }
    return len;
}

/*====================================================================*/
/*  Convert a COBOL String to a C String                              */
/*  Assumes that the C string has been allocated large enough to      */
/*  receive the largest cobol characters + 1.                         */
/*====================================================================*/
int strCob2C(char* cobol, char* c, int cobSize) {
    int i;
    int lastChar = -1;
    /* Detect the last non-blank character */
    for (i = 0; i < cobSize; i++) {
        if (*(cobol + i) != ' ') {
            lastChar = i;
        }
    }
    memcpy(c, cobol, lastChar + 1);
    memset(c + lastChar + 1, '\0', 1);
    return OK_CODE;
}

/*====================================================================*/
/*  Convert a C String to a COBOL String                              */
/*  This truncates C Strings if they are larger than the target COBOL */
/*====================================================================*/
int strC2Cob(char* c, char* cobol, int cobSize) {
    int i;
    int lastChar = -1;
    memset(cobol, ' ', cobSize);
    /* Detect the last non-blank character */
    for (i = 0; i < cobSize; i++) {
        if (*(c + i) == '\0') {
            break;
        }
    }
    memcpy(cobol, c, i);
    return OK_CODE;
}

/*====================================================================*/
/*  Inserts a C delimiter after the last non-blank character. This    */
/*  transform a Cobol string into a C string.                         */
/*====================================================================*/
int strCDelimit(char* cobol, int cobSize) {
    int i;
    char c;
    
    for(i = cobSize; i > -1; i--) {
        c = *(cobol + i);
        /* If this is already a zero-delimited string just return */
        if (c == '\0') {
            return OK_CODE;
        }
        /* If the last non-blank character occupies the last position
         * we have no room for the delimiter. */
        if (c != ' ') {
            if (i == cobSize) {
                memset(cobol + i, '\0', 1);
            } else {
                memset(cobol + i + 1, '\0', 1);
            }
            return OK_CODE;
        } else {
            /* If the first character is blank, than we have an empty
             * C string*/
             if (i == 0) {
                memset(cobol, '\0', 1);
             }
        }
    }
    return OK_CODE;
}
