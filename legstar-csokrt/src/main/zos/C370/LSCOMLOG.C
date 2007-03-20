#pragma XOPTS(CICS)
#pragma longname 
#pragma export(initLog)
#pragma export(logCicsError)
#pragma export(logError)
#pragma export(traceMessage)
#pragma export(traceData)
#pragma export(dumpMessage)
#pragma export(dumpMessagePart)
/**********************************************************************/
/*                                                                    */
/*           Copyright (C) 2007 LegSem            .                   */
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
/*   Purpose    - Tracing a logging DLL                               */
/*   Language   - IBM C/370                                           */
/*   System     - Tested on CICS TS 2.3                               */
/*   History    - 23 Feb 2007  - Original Implementation              */
/*   Notes      - This sub-program (DLL) provides formatting and      */
/*                tracing in a CICS region for other LegStar programs */
/*                                                                    */
/*                                                                    */
/**********************************************************************/
/*--------------------------------------------------------------------*/
/*  Headers included                                                  */
/*--------------------------------------------------------------------*/
#include <stdio.h>               /* get the standard I/O library      */
#include <stdlib.h>              /* get the standard library          */
#include <string.h>              /* support string operations         */
#include "lscomdec.h"            /* legstar common declarations       */

/*====================================================================*/
/* Initialize logging and tracing variables.                          */
/*====================================================================*/
int initLog()
{
    /* Get commarea address and EIB bloc address                      */
    EXEC CICS ADDRESS
              COMMAREA(ca_ptr)
              EIB(dfheiptr)
              RESP(g_cicsResp) RESP2(g_cicsResp2);                  
                                               
    if (g_cicsResp != DFHRESP(NORMAL)) {
        /* Since there is no way to log this error, just blow up*/
        EXEC CICS ABEND
                  ABCODE(CICS_ABEND_CODE)
                  NODUMP;
       return ERROR_CODE;
    }
    return OK_CODE;
}

/*====================================================================*/
/* Log any CICS error return code                                     */
/*                                                                    */
/* Input  :  traceParms            trace options                      */
/*           errorCommand          the failed CICS command            */
/*           resp                  response code                      */
/*           resp2                 reason code                        */
/*                                                                    */
/* Output :  formattedErrorMessage                                    */
/*                                                                    */
/*====================================================================*/
int logCicsError(TraceParms* pTraceParms,
                 char* errorCommand,
                 signed long resp, signed long resp2)
{
    char cicsErrorMessage[257];   /* complete message for CICS errors */
    char respText[12];            /* human readable resp code         */
   
    /* Attempt to get a user friendly resturn code                    */
    switch (resp) {
      case (DFHRESP(ENVDEFERR)):
        strcpy(respText,"ENVDEFERR");
        break;
      case (DFHRESP(IOERR)):
        strcpy(respText,"IOERR");
        break;
      case (DFHRESP(INVREQ)):
        strcpy(respText,"INVREQ");
        break;
      case (DFHRESP(NOTFND)):
        strcpy(respText,"NOTFND");
        break;
      case (DFHRESP(LENGERR)):
        strcpy(respText,"LENGERR");
        break;
      case (DFHRESP(NOSTG)):
        strcpy(respText,"NOSTG");
        break;
      case (DFHRESP(NOTAUTH)):
        strcpy(respText,"NOTAUTH");
        break;
      case (DFHRESP(PGMIDERR)):
        strcpy(respText,"PGMIDERR");
        break;
      case (DFHRESP(RESUNAVAIL)):
        strcpy(respText,"RESUNAVAIL");
        break;
      case (DFHRESP(ROLLEDBACK)):
        strcpy(respText,"ROLLEDBACK");
        break;
      case (DFHRESP(SYSIDERR)):
        strcpy(respText,"SYSIDERR");
        break;
      case (DFHRESP(TERMERR)):
        strcpy(respText,"TERMERR");
        break;
      case (DFHRESP(SYMBOLERR)):
        strcpy(respText,"SYMBOLERR");
        break;
      case (DFHRESP(TEMPLATERR)):
        strcpy(respText,"TEMPLATERR");
        break;
      default:
        sprintf(respText,"%d", resp);
    }
    sprintf(cicsErrorMessage,
            "CICS command=%s failed, resp=%s, resp2=%d",
            errorCommand, respText, resp2);
    logError(pTraceParms, cicsErrorMessage);
    return OK_CODE;
}

/*====================================================================*/
/* Log general errors                                                 */
/*                                                                    */
/* Input  :  traceParms            trace options                      */
/*           errorMessage          message describing error           */
/*                                                                    */
/* Output :  formattedErrorMessage formatted error message            */
/*                                                                    */
/*====================================================================*/
int logError(TraceParms* pTraceParms,
             char* errorMessage)
{
    char errorReport[512];
    char absTime[8];   /* current absolute time, ms since 01/01/1900  */
    unsigned char curtime[9];   /* human readable current time        */
    unsigned char curdate[11];  /* human readable current date        */
    signed long cicsResp = 0;   /* local resp to preserve global one  */
    signed long cicsResp2 = 0;  /* local resp2 to preserve global one */
   
    /* get the current time/date                                      */
    EXEC CICS ASKTIME ABSTIME(absTime)
                      RESP(cicsResp)
                      RESP2(cicsResp2);
    if (cicsResp != DFHRESP(NORMAL)) {
        strcpy(curtime,"Unknown");
        strcpy(curdate,"Unknown");
    }
    else {
        /* make the date and time human readable                      */
        curtime[8] = '\0';
        curdate[10] = '\0';
        EXEC CICS FORMATTIME ABSTIME(absTime)
                             YYYYMMDD(curdate)
                             TIME(curtime)
                             TIMESEP
                             DATESEP;
    }

    /* save the message so that it can be sent back to client        */
    sprintf(pTraceParms->formattedErrorMessage, "%s %s",
            REPLY_ERROR_EC, errorMessage);

    /* format an error report                                        */
    sprintf(errorReport,
            "%s : Connection ID=%s : Time=%s @ %s : %s",
            REPLY_ERROR_EC, pTraceParms->CxID, curdate, curtime,
            errorMessage);

    /* stderr, when used in an LE environment which is the case for
       CICS TS, uses the CESE transient data queue. This queue
       directs to CEEMSG by default.                                 */
    fprintf(stderr,"%s\n",errorReport);
    /* also print message to CEEOUT in case someone is monitoring
       stdout                                                        */
    if (TRUE_CODE == pTraceParms->traceMode) {
        traceMessage(pTraceParms, errorMessage);
    }
    return OK_CODE;
}
                         
/*====================================================================*/
/* Trace a message                                                    */
/*                                                                    */
/* Input  :  traceParms            trace options                      */
/*           traceMessage          message to trace                   */
/*                                                                    */
/* Output :                        None                               */
/*                                                                    */
/*====================================================================*/
int traceMessage(TraceParms* pTraceParms, char* traceMessage )
{
    /* stdout, when used in an LE environment which is the case for
       CICS TS, uses the CESO transient data queue. This queue
       directs to CEEOUT by default.                                 */
    fprintf(stdout,"%s : CxID=%s : %s\n",
            pTraceParms->module, pTraceParms->CxID, traceMessage);
           
    return OK_CODE;
}

/*====================================================================*/
/* Trace the content of a buffer                                      */
/*                                                                    */
/* Input  :  traceParms            trace options                      */
/*           data                 `Pointer to buffer data             */
/*           dataLength            data size                          */
/*                                                                    */
/* Output :                        None                               */
/*                                                                    */
/*====================================================================*/
int traceData(TraceParms* pTraceParms,
              char* data,
              long dataLength )
{
    int i;
    char dumpLine[128];
    char dumpChar[5];
    char dumpString[17];
   
    dumpLine[0]='\0';
    dumpString[0]='\0';
    for (i = 0; i < dataLength && i < MAX_TRACES_BYTES; i++) {
       /* print every 16 byte on a different line */
       sprintf(dumpChar,"%.2X ",data[i]);
       strcat(dumpLine,dumpChar);
       sprintf(dumpChar,"%c",data[i]);
       if (strlen(dumpChar) > 0) {
            strcat(dumpString,dumpChar);
       } else {
            strcat(dumpString," ");
       }
       if (i % 16 == 15 || i == dataLength - 1) {
          while (i % 16 < 15) {
            strcat(dumpLine,"   ");
            i++;
          }
          strcat(dumpLine," -- ");
          strcat(dumpLine,dumpString);
          traceMessage(pTraceParms, dumpLine);
          dumpString[0]='\0';
          dumpLine[0]='\0';
       }
    }
   
    if (dataLength > MAX_TRACES_BYTES) {
        sprintf(dumpLine,"...data was truncated at %d bytes",
                MAX_TRACES_BYTES);
        traceMessage(pTraceParms, dumpLine);
    }
    return OK_CODE;
}

/*====================================================================*/
/* Trace the content of a structured message                          */
/*                                                                    */
/* Input  :  traceParms            trace options                      */
/*           message              `Pointer to the message             */
/*                                                                    */
/* Output :                        None                               */
/*                                                                    */
/*====================================================================*/
int dumpMessage(TraceParms* pTraceParms, Message* message) {
    int i;
    int nParts;
    char dumpLine[128];
    HeaderPartContent* pHeaderPartContent;
    
    pHeaderPartContent =
       (HeaderPartContent*)message->pHeaderPart->content;

    traceMessage(pTraceParms, "Header message part:");
    dumpMessagePart(pTraceParms, message->pHeaderPart);
    nParts = pHeaderPartContent->partsNumber.as_int;
    sprintf(dumpLine, "Data message parts number=%d", nParts);
    traceMessage(pTraceParms, dumpLine);
    for (i=0; i < nParts; i++) {
	    sprintf(dumpLine, "Data message part %d:", i + 1);
        traceMessage(pTraceParms, dumpLine);
	    dumpMessagePart(pTraceParms, message->pParts + i);
    }
}

/*====================================================================*/
/* Trace the content of a message part                                */
/*                                                                    */
/* Input  :  traceParms            trace options                      */
/*           messagePart          `Pointer to the message part        */
/*                                                                    */
/* Output :                        None                               */
/*                                                                    */
/*====================================================================*/
int dumpMessagePart(TraceParms* pTraceParms, MessagePart* messagePart) {
    char dumpLine[128];
    char dumpID[MSG_ID_LEN + 1];
    
    memset(dumpID, '\0', MSG_ID_LEN + 1);
    memcpy(dumpID, messagePart->ID, MSG_ID_LEN);
    sprintf(dumpLine, "Message part: ID=%s Content-length=%d",
             dumpID, messagePart->size.as_int);
    traceMessage(pTraceParms, dumpLine);
    traceMessage(pTraceParms, "Message part content:");
    traceData(pTraceParms, messagePart->content,
    	 messagePart->size.as_int);
}


                         