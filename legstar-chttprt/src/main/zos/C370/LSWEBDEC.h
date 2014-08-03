#ifndef _H_LSWEBDEC
#define _H_LSWEBDEC
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
/*   Purpose    - LegStar HTTP include file                           */
/*   Language   - IBM C/370                                           */
/*   System     - Tested on CICS TS 2.3                               */
/*   History    - 12 Aug 2007  - Original Implementation              */
/*   Notes      - Describes structures used by LegStar HTTP.          */
/*                                                                    */
/*                                                                    */
/**********************************************************************/
/*--------------------------------------------------------------------*/
/*  Prototypes                                                        */
/*--------------------------------------------------------------------*/
int init();
int checkRequest();
int sendWelcomeMessage();
int processRequest();
int rollback();
int sendErrorMessage();
int abortServer(char* abendCode);
int getRequestProperties();
int getHTTPHeaders();
int reportInputParameters();
int getHTTPHeader(char* headerLabel,
                  long headerLabelLength,
                  char* headerValue);
int traceParameter(char* parameterName,
                    void* parameterValue,
                    int parameterType );
int recvRequestContent();
int sendResponse();
int traceHTTPContent();
int setHTTPHeader(char* headerLabel,
                  long headerLabelLength,
                  char* headerValue,
                  long headerValueLength);


#endif
