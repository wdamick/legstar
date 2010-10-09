#ifndef _H_LSMQLLIB
#define _H_LSMQLLIB
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
/*   Purpose    - Common MQ header file                               */
/*   Language   - IBM C/370                                           */
/*   System     - Tested on CICS TS 2.3                               */
/*   History    - 10 August 2007  - Original Implementation           */
/*   Notes      - Shared declarations for LegStar MQ programs.        */
/*                                                                    */
/*                                                                    */
/**********************************************************************/
/*--------------------------------------------------------------------*/
/*  Constants                                                         */
/*--------------------------------------------------------------------*/
#define DEFAULT_WORKBUF_LEN 65536 /* buffer length used to get & put  */
#define WORKBUF_LEN_DIGITS 8 /* Fixed number of digits for buffer len */
#define TRANID_NAME_LEN 4    /* CICS transaction name length          */
#define TRANCLASS_NAME_LEN 8 /* CICS transaction classes name length  */
#define DEFAULT_HANDLER_TRANSID "LEGQ" /* Default CICS transaction ID */

/*--------------------------------------------------------------------*/
/* MQ API related variables                                           */
/*--------------------------------------------------------------------*/
MQLONG g_mqResp;                   /* Last MQ command completion code */  
MQLONG g_mqReason;                 /* Last MQ command reason code     */
MQHCONN  g_HQueueManager = MQHC_DEF_HCONN; /* Connection to Q manager */

/*--------------------------------------------------------------------*/
/* Structure describing an MQ queue status                            */
/*--------------------------------------------------------------------*/
typedef struct tagMQQueue {
    char Name[MQ_Q_NAME_LENGTH + 1]; /* Queue name                    */
    int OpenStatus;                  /* TRUE_CODE if queue is opened  */
    MQHOBJ HObj;                     /* Handle on active connection   */
} MQ_Queue;

/*--------------------------------------------------------------------*/
/* Structure describing the process user data parameters              */
/*--------------------------------------------------------------------*/
typedef struct tagProcUserData {
    char HandlerTransID[TRANID_NAME_LEN];
    char WorkBufferLen[WORKBUF_LEN_DIGITS];
    char TraceOn[5];
    char CxID[MAX_CXID_LEN];
} Proc_User_Data;

/*--------------------------------------------------------------------*/
/* Formatted parameter set retrieved from the trigger message data    */
/*--------------------------------------------------------------------*/
typedef struct tagMQTMParms {
    char RequestQueueName[MQ_Q_NAME_LENGTH + 1]; /* Request queue name*/
    char ProcessName[MQ_PROCESS_NAME_LENGTH + 1]; /* Process name     */
    char TriggerData[MQ_TRIGGER_DATA_LENGTH + 1]; /* Trigger data     */
    long ApplType;                            /* Application type     */
    char ApplId[MQ_PROCESS_APPL_ID_LENGTH + 1];/* Application ID      */
    char EnvData[MQ_PROCESS_ENV_DATA_LENGTH + 1]; /* Environment data */
    char UserData[MQ_PROCESS_USER_DATA_LENGTH + 1]; /* User data      */
    char HandlerTransID[TRANID_NAME_LEN + 1];/* Handler transaction ID*/
    MQLONG WorkBufferLen;                    /* Work buffer size      */
    int TraceMode;                           /* Trace requested       */
    char CxID[MAX_CXID_LEN + 1];             /* Trace ID              */
} MQTM_Parms;

/*--------------------------------------------------------------------*/
/* Global variables                                                   */
/*--------------------------------------------------------------------*/
MQ_Queue g_RequestQueue;           /* Request queue description       */
MQ_Queue g_ResponseQueue;          /* Response queue description      */
MQ_Queue g_BackoutQueue;           /* Poisonous messages queue        */

MQTM g_triggerMsg;                 /* Trigger message                 */
short g_triggerMsgLen  = sizeof(MQTM);  /* Trigger message length     */
char g_HandlerTransID[5];                  /* Handler transaction ID  */
MQLONG g_WorkBufferLen = DEFAULT_WORKBUF_LEN; /* put & get buffer len */
void *g_pWorkBuffer = NULL;        /* pointer to put and get buffer   */
MQLONG g_DataLen;                         /* last MQ message data len */

TraceParms g_traceParms;                   /* Set of trace parameters */
#endif

