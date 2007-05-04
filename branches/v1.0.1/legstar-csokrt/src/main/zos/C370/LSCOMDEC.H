#ifndef _H_LSCOMDEC
#define _H_LSCOMDEC
/**********************************************************************/
/*                                                                    */
/*                  Copyright (C) 2007 LegSem .                       */
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
/*   Purpose    - Common LegStar declarations include file            */
/*   Language   - IBM C/370                                           */
/*   System     - Tested on CICS TS 2.3                               */
/*   History    - 23 Feb 2007  - Original Implementation              */
/*   Notes      - Shared declarations for LegStar CICS programs.      */
/*                                                                    */
/*                                                                    */
/**********************************************************************/
/*--------------------------------------------------------------------*/
/*  Constants                                                         */
/*--------------------------------------------------------------------*/
#define OK_CODE 0                /* No errors or warnings             */
#define ERROR_CODE -1            /* Error code                        */
#define TRUE_CODE 1              /* Boolean true value                */
#define FALSE_CODE 0             /* Boolean false value               */

#define EYE_CATCHER "SK"         /* Protocol signature in client data */
#define EXEC_REQUEST_EC  "LSOKEXEC" /* Identifies execution requests  */
#define PROBE_REQUEST_EC "LSOKPROB" /* Identifies probes (is alive )  */
#define UOW_REQUEST_EC   "LSOKUOWC" /* Eye catcher for UOW command    */
#define REPLY_ACK_EC   "LSOKACK0" /* Eye catcher for acknowledgements */
#define DATA_MSG_EC    "LSOKDATA" /* Eye catcher data messages        */
#define REPLY_ERROR_EC "LSOKERR0" /* Eye catcher for error replies    */
#define HEADER_PART_ID "LSOKHEAD" /* Identifier for a header part     */
#define COMMAREA_PART_ID "LSOKCOMMAREA" /* Commarea part identifier   */

#define CICS_ABEND_CODE "LCIC"   /* Abend code for CICS faults        */
#define SOK_ABEND_CODE "LSOK"    /* Abend code for socket faults      */
#define PROT_ABEND_CODE "LPRO"   /* Abend code for protocol errors    */

#define RECV_TIME_OUT 2          /* Wait time for incoming data (sec) */
#define SEND_TIME_OUT 2          /* Wait time for outbound data (sec) */

#define ECONNCLOSED      7001     /* Client closed the connection. This 
                                    is not a standard errno.          */
#define SOK_HOW  2               /* Shutdown both sides option        */
#define MAX_TIM_EC_LEN 2         /* Transaction init msg eye catcher  */
#define MAX_CXID_LEN 17          /* Connection ID max length          */
#define MSGPART_MAX_LEN 16777216 /* Largest tolerated message part    */
#define MAX_TRACES_BYTES 500     /* Flood prevention for large data   */
#define MAX_IN_MSG_PARTS 10       /* Maximum number of input parts    */
#define MAX_OUT_MSG_PARTS 10      /* Maximum number of input parts    */
#define REQUEST_TYPE_LEN 8       /* Service request type length       */
#define MSG_ID_LEN 16            /* Message identifier max length     */
#define MSG_CONTENT_SIZE_LEN 4   /* Message identifier max length     */
#define PARTS_NUM_LEN 4          /* Size of field giving parts number */
#define KEYVAL_SIZE_LEN 4    /* Size of field giving Key/Value length */
#define MODULE_LEN 8         /* Module name producing traces          */
#define MAX_FORM_MSG_LEN 266 /* Formatted error message max length    */
#define MAX_TRACE_MSG_LEN 256 /* Tracing/logging message max length   */

#define UOW_COMMAND_LEN 8    /* UOW command length                    */
#define UOW_COMMIT_CMD  "Commit"   /* Commit command (confirm updates)*/
#define UOW_ROLLBACK_CMD "Rollback" /* Rollback command (undo updates)*/
#define UOW_KEEP_CMD    "Keep"     /* Keep command (continue same UOW)*/

/*--------------------------------------------------------------------*/
/* Message part structure                                             */
/*--------------------------------------------------------------------*/
typedef struct {
  char ID[MSG_ID_LEN];
  union {
     char as_bytes[MSG_CONTENT_SIZE_LEN];
     int as_int;
  } size;
  char* content;
} MessagePart;

/*--------------------------------------------------------------------*/
/* Content layout for a header message part                           */
/*--------------------------------------------------------------------*/
typedef struct {
  union {
     char as_bytes[PARTS_NUM_LEN];
     int as_int;
  } partsNumber;
  union {
     char as_bytes[KEYVAL_SIZE_LEN];
     int as_int;
  } keyValuesSize;
} HeaderPartContent;

/*--------------------------------------------------------------------*/
/* Message structure                                                  */
/*--------------------------------------------------------------------*/
typedef struct {
  MessagePart* pHeaderPart;
  MessagePart* pParts;
} Message;

/*--------------------------------------------------------------------*/
/* Tracing parameters                                                 */
/*--------------------------------------------------------------------*/
typedef struct {
    char module[MODULE_LEN + 1];
    char CxID[MAX_CXID_LEN];
    int traceMode;
    char formattedErrorMessage[MAX_FORM_MSG_LEN];
} TraceParms;

/*--------------------------------------------------------------------*/
/* Global variables                                                   */
/*--------------------------------------------------------------------*/
signed long g_cicsResp = 0;                /* Last CICS command resp  */
signed long g_cicsResp2 = 0;               /* Last CICS command resp2 */
char g_traceMessage[MAX_TRACE_MSG_LEN];    /* Tracing messages        */
TraceParms g_traceParms;                   /* trace parameters        */

/*--------------------------------------------------------------------*/
/* Commarea pointer and length                                        */
/*--------------------------------------------------------------------*/
void* ca_ptr;           
short ca_len;
#endif                        