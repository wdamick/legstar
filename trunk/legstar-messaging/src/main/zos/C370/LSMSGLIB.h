#ifndef _H_LSMSGLIB
#define _H_LSMSGLIB
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
/*   Purpose    - LegStar messaging include file                      */
/*   Language   - IBM C/370                                           */
/*   System     - Tested on CICS TS 2.3                               */
/*   History    - 12 Aug 2007  - Original Implementation              */
/*   Notes      - Describes structures used by LegStar messaging.     */
/*                                                                    */
/*                                                                    */
/**********************************************************************/
/*--------------------------------------------------------------------*/
/*  Constants                                                         */
/*--------------------------------------------------------------------*/
#define MAX_MSG_PARTS 10          /* Maximum number of parts    */

/*--------------------------------------------------------------------*/
/* Structure describing complete LegStar messages                     */
/* It turns out that the original Message structures are not always   */
/* practical to use. Since it is unlikely that we will get more than  */
/* 10 parts in a message this is a convenience structure that result  */
/* in much less getmains.                                             */
/*--------------------------------------------------------------------*/
typedef struct tagLSMessage {
    Message message;                      /* message pointing to parts*/
    MessagePart headerPart;               /* header part              */
    MessagePart dataParts[MAX_MSG_PARTS]; /* data parts               */
} LS_Message;

/*--------------------------------------------------------------------*/
/*  Prototypes                                                        */
/*--------------------------------------------------------------------*/
int freeHostBuffer(void *buffer);
int hostToMessagePart(
    char* hostBuffer,        /* Pointer to the raw in-memory data     */
    int* pPos,               /* Current position within the raw data  */
    int  len,                /* Total size of the raw data            */
    MessagePart* pMessagePart  /* Message part to be formatted        */
    );
int messagePartToBuffer(
    char* hostBuffer,           /* Host buffer being formatted        */
    int* pPos,                  /* Current position within the buffer */
    MessagePart* pPart          /* Message part to read from          */
    );

#endif
