#ifndef _H_LSSOKDEC
#define _H_LSSOKDEC
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
/*   Purpose    - LegStar socket include file                         */
/*   Language   - IBM C/370                                           */
/*   System     - Tested on CICS TS 2.3                               */
/*   History    - 12 Aug 2007  - Original Implementation              */
/*   Notes      - Describes structures used by LegStar sockets.       */
/*                                                                    */
/*                                                                    */
/**********************************************************************/
/*--------------------------------------------------------------------*/
/*  Prototypes                                                        */
/*--------------------------------------------------------------------*/
int init();
int doTakeSocket();
int recvRequest();
int rollback();
int commit();
int sendErrorReport();
int doCloseSocket();
int abortServer(char* abendCode);
int getListenerParms();
int logSocketError(char* errorCommand);
int doSetSockOpt();
int doReceive(void* buffer, int nBytes);
int doReceiveWait(unsigned long socket,
    char* buffer, int nBytes, int timeLimit);
int doWait(unsigned long socket, int operation, int timeLimit);
int processRequest();
int recvRequestMessage(Message* pRequestMessage);
int recvMessagePart(MessagePart* pMessagePart);
int sendResponseMessage(Message* pResponseMessage);
int sendMessageType();
int sendMessagePart(MessagePart* pPart);
int sendConnectionAck();
int processUOWCommand();
int sendAck();
int doSendBuffered(void* buffer, int nBytes);
int doFlushSendBuffer();
int doSend(char* buffer, int nBytes);
int doSendWait(unsigned long socket,
    char* buffer, int nBytes, int timeLimit);
void ptos(char *s_buff,unsigned char *p_buff);

#endif
