#ifndef _H_LSLNKDEC
#define _H_LSLNKDEC
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
/*   Purpose    - LegStar invoker include file                        */
/*   Language   - IBM C/370                                           */
/*   System     - Tested on CICS TS 2.3                               */
/*   History    - 12 Aug 2007  - Original Implementation              */
/*   Notes      - Describes structures used by LegStar invoker.       */
/*                                                                    */
/*                                                                    */
/**********************************************************************/
/*--------------------------------------------------------------------*/
/*  Prototypes                                                        */
/*--------------------------------------------------------------------*/
int freeProgram(DFHEIBLK *inDfheiptr,
                TraceParms* inTraceParms,
                CICSProgramDesc* pProgramDesc,
                Message* pRequestMessage,
                Message* pResponseMessage);
int invokeProgram(DFHEIBLK *inDfheiptr,
                  TraceParms* inTraceParms,
                  CICSProgramDesc* pProgramDesc,
                  Message* pRequestMessage,
                  Message* pResponseMessage);


int initProgramDesc(CICSProgramDesc* pProgramDesc);
int parseKeyValues(int keyValuesSize,
                   void* keyValues,
                   CICSProgramDesc* pProgramDesc);
int processKeyValue(char* key,
                    char* value,
                    CICSProgramDesc* pProgramDesc);
int processKeyArray(char* key,
                    char* value,
                    CICSProgramDesc* pProgramDesc);
int traceProgramDesc(CICSProgramDesc* pProgramDesc);

int linkChannel(DFHEIBLK *inDfheiptr,
                TraceParms* inTraceParms,
                CICSProgramDesc* pProgramDesc,
                Message* pRequestMessage,
                Message* pResponseMessage);
int checkChannel(CICSProgramDesc* pProgramDesc,
                  Message* pRequestMessage);
int createContainers(CICSProgramDesc* pProgramDesc,
                  Message* pRequestMessage);
int formatChannelResponse(CICSProgramDesc* pProgramDesc,
                          Message* pResponseMessage);
int formatResponseHeaders(
                 int outPartsNum,
                 Message* pResponseMessage);
int formatResponseParts(
                 CICSProgramDesc* pProgramDesc,
                 int outPartsNum,
                 Message* pResponseMessage);
int freeChannel(DFHEIBLK *inDfheiptr,
                TraceParms* inTraceParms,
                CICSProgramDesc* pProgramDesc,
                Message* pRequestMessage,
                Message* pResponseMessage);
int freeMessages(int request,
                CICSProgramDesc* pProgramDesc,
                Message* pMessage);

int linkCommarea(DFHEIBLK *inDfheiptr,
                TraceParms* inTraceParms,
                CICSProgramDesc* pProgramDesc,
                Message* pRequestMessage,
                Message* pResponseMessage);
int checkCommarea(CICSProgramDesc* pProgramDesc,
                  Message* pRequestMessage);
int reallocContent(CICSProgramDesc* pProgramDesc,
                   MessagePart* pInputPart);
int formatCommareaResponse(CICSProgramDesc* pProgramDesc,
                          Message* pRequestMessage,
                          Message* pResponseMessage);
int formatResponseHeader(
                 Message* pResponseMessage);
int formatResponsePart(
                 Message* pRequestMessage,
                 Message* pResponseMessage);
int adjustPartSize(MessagePart* pPart);
int freeCommarea(DFHEIBLK *inDfheiptr,
                TraceParms* inTraceParms,
                CICSProgramDesc* pProgramDesc,
                Message* pRequestMessage,
                Message* pResponseMessage);
int freeMessage(Message* pMessage);

char* getToken(char* bufferString, char* token,
               int maxTokenSize, char startDelim, char endDelim);
char* getValueStart(char* bufferString) ;


#endif
