#ifndef _H_LSHTTLIB
#define _H_LSHTTLIB
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
/*  Language   - C and IBM C/370                                      */
/*  System     - Tested on CICS TS 2.3                                */
/*  History    - 08 Jul 2007  - Original Implementation               */
/*  Notes      - This library provides HTTP 1.0 connectivity with a   */
/*               server. It will compile on MVS with the CICS Sockets.*/
/*                                                                    */
/*                                                                    */
/*                                                                    */
/*                                                                    */
/*                                                                    */
/**********************************************************************/
/*--------------------------------------------------------------------*/
/*  Constants                                                         */
/*--------------------------------------------------------------------*/
#ifndef _H_LSCOMDEC
#define OK_CODE 0                /* No errors or warnings             */
#define ERROR_CODE -1            /* Error code                        */
#define TRUE_CODE 1              /* Boolean true value                */
#define MAX_CXID_LEN 16          /* Connection ID max length          */
#define MAX_TRACES_BYTES 1500    /* Flood prevention for large data   */
#endif

#define SOCKET int
#define socket_error errno
#define DEFAULT_HTTP_PORT 80       /* Server port number              */
#define DEFAULT_CONNECT_TIMEOUT 3  /* Connection timeout in seconds   */
#define DEFAULT_RECV_TIMEOUT 10    /* Receive timeout in seconds      */

#define CRLF "\r\n"
#define ASCII_CR 0x0D 
#define ASCII_LF 0x0A
#define ASCII_EQUAL 0x3D
#define SUPPORTED_SCHEME "http://"

/* How many bytes it will take to store LEN bytes in base64.  */
#define BASE64_LENGTH(len) (4 * (((len) + 2) / 3))

/*--------------------------------------------------------------------*/
/* Maximum sizes for strings                                          */
/*--------------------------------------------------------------------*/
#define URI_HOST_MAXLEN 128
#define URI_PATH_MAXLEN 128
#define URI_QUERY_MAXLEN 128
#define HEADER_MAXLEN 512
#define LOG_MSG_MAXLEN 128
#define CONTENT_TYPE_MAXLEN 128
#define USER_PASSWORD_MAXLEN 64
#define B64_USER_PASSWORD_MAXLEN 128
#define USERID_MAXLEN 32
#define PASSWORD_MAXLEN 32
#define MAX_ERROR_MSG_LEN 266

/*--------------------------------------------------------------------*/
/* Http headers formats                                               */
/*--------------------------------------------------------------------*/
#define HTTP_USER_AGENT "LegStar httplib/1.0"
#define IN_STATUS_LINE_FORMAT "HTTP/1.%*d %03d"
#define IN_CONTENT_LENGTH_FORMAT "content-length: %d"
#define IN_CONTENT_TYPE_FORMAT "content-type: %s"
#define REQUEST_LINE_PROXY_FORMAT \
    "%s http://%.128s:%d/%.256s HTTP/1.0\r\n"
#define REQUEST_LINE_FORMAT "%s /%.256s HTTP/1.0\r\n"
#define USER_AGENT_FORMAT "User-Agent: %s\r\n"
#define CONTENT_LENGTH_FORMAT "Content-length: %d\r\n"
#define CONTENT_TYPE_FORMAT "Content-type: %s\r\n"
#define AUTHORIZATION_PART1 "Authorization: Basic "
#define CORRELATION_ID_FORMAT "Correlation-id: %s\r\n"

/*--------------------------------------------------------------------*/
/* Error messages                                                     */
/*--------------------------------------------------------------------*/
#define URL_SCHEME_ERRMSG "Invalid url %s - must start with 'http://'"
#define INVALID_PORT_ERRMSG "Invalid port in url %s"
#define DNS_LOOKUP_ERRMSG "Cannot resolve host name =%s"
#define SOK_CREATE_ERRMSG "Socket creation error: %s"
#define SOK_CONNECT_ERRMSG "Connection error: %s"
#define SOK_RECV_ERRMSG "Receive error: %s"
#define SOK_CONN_ABORT_ERRMSG "Server closed the connection"
#define SOK_NOHEAD_ERRMSG "Data received is not a valid HTTP header"
#define SOK_SEND_ERRMSG "Send error: %s"
#define NULL_DATA_ERRMSG "Missing address or length for reply"
#define INVALID_REPLY_ERRMSG "Invalid reply %s"
#define USERP_TOOLONG_ERRMSG "User password combination too long"
#define MEM_ALLOC_ERRMSG "Out of memory"

/*--------------------------------------------------------------------*/
/* return codes                                                       */
/*--------------------------------------------------------------------*/
#define DNS_LOOKUP_ERRCODE -1                         /* No such host */
#define SOK_CREATE_ERRCODE -2                  /* Can't create socket */
#define SOK_CONNECT_ERRCODE -3               /* Can't connect to host */
#define SOK_CONN_ABORT_ERRCODE -4           /* Connection was aborted */
#define SOK_SEND_ERRCODE -5               /* Error while sending data */
#define READ_HEADER_ERRCODE -6          /* Error while reading header */
#define INVALID_REPLY_ERRCODE -7         /* Invalid reply from server */
#define NULL_DATA_ERRCODE -8          /* Null reply pointer or length */
#define SOK_NOHEAD_ERRCODE -9         /* Invalid http header received */
#define MEM_ALLOC_ERRCODE -10                /* Can't allocate memory */
#define SOK_RECV_ERRCODE -11         /* Read error while reading data */
#define INVALID_URL_ERRCODE -12   /* Url did not start with 'http://' */
#define INVALID_PORT_ERRCODE -13               /* Invalid port in url */
#define USERP_TOOLONG_ERRCODE -14           /* User password too long */
#define TIMEOUT_ERRCODE -15                    /* Operation timed out */

/*--------------------------------------------------------------------*/
/* Tracing parameters  (compatible with LSCOMDEC)                     */
/*--------------------------------------------------------------------*/
typedef struct {
    char cxid[MAX_CXID_LEN + 1];
    int on;
    char error_message[MAX_ERROR_MSG_LEN];
} Trace_parm;

/*--------------------------------------------------------------------*/
/* URI components                                                     */
/*--------------------------------------------------------------------*/
typedef struct {
  char host[URI_HOST_MAXLEN + 1];
  int  port;
  char path[URI_PATH_MAXLEN + 1];
  char query[URI_QUERY_MAXLEN + 1];
} Uri_t;

/*--------------------------------------------------------------------*/
/* structure used to describe outbound or inbound data exchanged      */
/* with an HTTP server                                                */
/*--------------------------------------------------------------------*/
typedef struct {
  char* content;
  int content_len;
  char content_type[CONTENT_TYPE_MAXLEN + 1];
} Http_content;

/*--------------------------------------------------------------------*/
/* Describes user credentials for basic authentication                */
/*--------------------------------------------------------------------*/
typedef struct {
  char user[USERID_MAXLEN + 1];
  char password[PASSWORD_MAXLEN + 1];
} Http_basic_credentials;

/*--------------------------------------------------------------------*/
/* Groups options that can be set to influence the http library       */
/* behavior.                                                          */
/*--------------------------------------------------------------------*/
typedef struct {
  int connect_timeout;
  int recv_timeout;
  char *proxy_url;
  Http_basic_credentials *credentials;
} Http_options;

#endif 
