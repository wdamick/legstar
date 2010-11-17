#pragma longname
#pragma export(http_invoke)
#pragma export(http_parse_url)
#pragma export(http_open)
#pragma export(http_close)
#pragma export(http_query)
#pragma export(http_get_reply)
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
/*  Notes      - This library provides HTTP 1.0 connectivity with a   */
/*               server. It will compile on MVS with CICS Sockets.    */
/*                                                                    */
/*                                                                    */
/*                                                                    */
/*                                                                    */
/*                                                                    */
/**********************************************************************/
/*--------------------------------------------------------------------*/
/*  Headers included                                                  */
/*--------------------------------------------------------------------*/
#include <string.h>              /* support string operations         */
#include <stdlib.h>              /* get the standard library          */
#include <stdio.h>               /* get the standard I/O library      */
#include <cmanifes.h>            /* manifest for sockets (re-entrant) */
#include <in.h>                  /* socket address structures         */
#include <socket.h>              /* socket API                        */
#include <tcperrno.h>            /* socket related error codes        */
#include <netdb.h>               /* needed for DNS resolution         */
#include <ioctl.h>               /* ioctl socket API call parameters  */
#include <unistd.h>              /* ascii to ebcdic conversion        */
#include "lshttlib.h"

/*--------------------------------------------------------------------*/
/*  Constants                                                         */
/*--------------------------------------------------------------------*/
#define MODULE_NAME   "LSHTTLIB" /* This module trace identifier      */
#define READ_OPERATION 1         /* Code for receive operations       */
#define WRITE_OPERATION 2        /* Code for send operations          */
#define BLOCKING_MODE 0          /* Set a socket to blocking mode     */
#define NON_BLOCKING_MODE 1      /* Set a socket to non-blocking mode */
#define MAX_SEND_BUF_LEN 4096    /* Used to buffer sends              */
#define MAX_RECV_BUF_LEN 4096    /* Used to buffer reads              */

/*--------------------------------------------------------------------*/
/*  Global variables                                                  */
/*--------------------------------------------------------------------*/
char g_log_message[LOG_MSG_MAXLEN + 1];
char g_error_message[LOG_MSG_MAXLEN + 1];

int g_sendMark = 0;
char g_sendBuffer[MAX_SEND_BUF_LEN];

int g_recvMark = 0;
int g_recvBufferLen = 0;
char g_recvBuffer[MAX_RECV_BUF_LEN];

/*--------------------------------------------------------------------*/
/*  Functions prototypes                                              */
/*--------------------------------------------------------------------*/
char* get_socket_message(int errcode);

/*====================================================================*/
/* Invoke a command on an HTTP server                                 */
/*                                                                    */
/* This function is generic for both GET and POST commands.           */
/* The requested URL gives the target host and resource.              */
/* Data to post is passed in request (size given by requestlen).      */
/* Caller can provide credentials if basic authentication is required.*/
/* Caller should pass an address for the reply. The actual buffer will*/
/* be allocated by this code. Caller is responsible for freeing       */
/* memory.                                                            */
/* returns a negative error code or a positive code from the server   */
/*====================================================================*/
int http_invoke(command, url, request, reply, options, trace)
    char *command;          /* command to send                        */
    char *url;              /* target host and resource               */
    Http_content* request;  /* data to be sent to the server          */
    Http_content* reply;    /* data returned from the server. This
                               cannot be NULL.                        */
    Http_options* options;  /* optional parameters                    */
    Trace_parm* trace;      /* trace parameters                       */
{
    int rc;
    SOCKET s;
    Uri_t uri_server;
    Uri_t uri_proxy;

    if (trace->on == TRUE_CODE) {
        sprintf(g_log_message, "http_invoke for url:%s", url);
        log(trace->cxid, g_log_message);
    }

    /* Extract the target URL content */
    rc = http_parse_url(url, &uri_server, trace);
    if (rc != OK_CODE) {
        return rc;
    }
    
    /* Prepare the proxy (if any) */
    if (options->proxy_url != NULL && strlen(options->proxy_url) > 0) {
        rc = http_parse_url(options->proxy_url, &uri_proxy, trace);
        if (rc != OK_CODE) {
            return rc;
        }
    } else {
        memset(uri_proxy.host, '\0', sizeof(uri_proxy.host));
        uri_proxy.port = 0;
    }

    /* Open a connection to server or proxy */
    rc = http_open(&s, &uri_server, &uri_proxy, options, trace);
    if (rc != OK_CODE) {
        return rc;
    }

    /* Execute the requested command */
    rc = http_query(s, command, &uri_server, &uri_proxy,
                    request, options, trace);
    if (rc != OK_CODE) {
        http_close(s, trace);
        return rc;
    }

    /* Retrieve the reply data */
    rc = http_get_reply(s, reply, options, trace);
    if (rc != OK_CODE) {
        http_close(s, trace);
        return rc;
    }
  
    if (trace->on == TRUE_CODE) {
        sprintf(g_log_message, "http_invoke ended");
        log(trace->cxid, g_log_message);
    }
    return http_close(s, trace);
}

/*====================================================================*/
/* Extract host, port and path from a URL with the following format:  */
/* http_URL       = "http:" "//" host [ ":" port ] [ abs_path ]       */
/* Does not support userinfo.                                         */
/*====================================================================*/
int http_parse_url(url, uri, trace)
    char *url;            /* the url to parse                         */
    Uri_t *uri;           /* structure describing server url          */
    Trace_parm* trace;    /* trace parameters                         */
{
    char *pc, c;
  
    if (trace->on == TRUE_CODE) {
        sprintf(g_log_message,
               "http_parse_url for url:%s", url);
        log(trace->cxid, g_log_message);
    }
    /* Initialize */
    memset(uri->host, '\0', sizeof(uri->host));
    uri->port = DEFAULT_HTTP_PORT;
    memset(uri->path, '\0', sizeof(uri->path));

    /* Make sure this is a supported scheme */
    if (strncasecmp(SUPPORTED_SCHEME, url, strlen(SUPPORTED_SCHEME))) {
        sprintf(trace->error_message, URL_SCHEME_ERRMSG, url );
        return INVALID_URL_ERRCODE;
    }
    url += strlen(SUPPORTED_SCHEME);

    /* Extract the host */
    for (pc = url, c = *pc; (c && c!=':' && c!='/');) {
        c = *pc++;
    }
    strncpy(uri->host, url, (c) ? pc - url - 1 : pc - url);

    /* Extract the port number (if any) */
    if (c == ':') {
        if (sscanf(pc, "%d", &uri->port) !=1 ) {
            sprintf(trace->error_message, INVALID_PORT_ERRMSG, url );
            return INVALID_PORT_ERRCODE;
        }
        for (pc++; (*pc && *pc != '/') ; pc++) ;
        if (*pc) {
            pc++;
        }
    }

    /* Everything past the host:port is part of the path for now */
    strcpy(uri->path, c ? pc : "");

    if (trace->on == TRUE_CODE) {
        sprintf(g_log_message,
            "http_parse_url ended. Host=%s port=%d path=%s", 
            uri->host, uri->port, uri->path);
        log(trace->cxid, g_log_message);
    }
    return OK_CODE;
}

/*====================================================================*/
/* Open a connection and return a writable and readable streams       */
/* This is using IPV4 only.                                           */
/*====================================================================*/
int http_open(ps, uri_server, uri_proxy, options, trace)
    SOCKET *ps;             /* a pointer to the new opened socket     */
    Uri_t *uri_server;      /* structure describing server uri        */
    Uri_t *uri_proxy;       /* structure describing proxy uri         */
    Http_options* options;  /* optional parameters                    */
    Trace_parm* trace;      /* trace parameters                       */
{
    struct  hostent *hp;
    struct  sockaddr_in saddr;
    int  proxy = (uri_proxy->host != NULL && uri_proxy->port != 0);
    int  port = proxy ? uri_proxy->port : uri_server->port ;
    char*  host = proxy ? uri_proxy->host : uri_server->host;
    
    if (trace->on == TRUE_CODE) {
        sprintf(g_log_message, "http_open started to host %s", host);
        log(trace->cxid, g_log_message);
    }
    /* Prepare address structure */
    memset((char *) &saddr, 0 , sizeof(saddr));
    saddr.sin_family = AF_INET;
    saddr.sin_port = (unsigned short) htons( port );
    saddr.sin_addr.s_addr = inet_addr(host);

    /* if this is not a dotted decimal address, resolve name from dns */
    if (saddr.sin_addr.s_addr == INADDR_NONE) {
        if (hp = gethostbyname(host)) {
            memmove((char *)&saddr.sin_addr, hp->h_addr, hp->h_length);
            saddr.sin_family = hp->h_addrtype;
        } else {
            sprintf(trace->error_message, DNS_LOOKUP_ERRMSG, host);
            return DNS_LOOKUP_ERRCODE;
        }
    }

    /* create socket */
    if ((*ps = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
        sprintf(trace->error_message, SOK_CREATE_ERRMSG,
                get_socket_message(socket_error) );
        return SOK_CREATE_ERRCODE;
    }
    
    /* connect to server */
    if (do_connect_timeout(
              *ps, &saddr, options->connect_timeout, trace) < 0) {
        sprintf(trace->error_message, SOK_CONNECT_ERRMSG,
                get_socket_message(socket_error) );
        return SOK_CONNECT_ERRCODE;
    }

    /* initialize buffers */
    g_sendMark = 0;
    g_recvMark = 0;
    g_recvBufferLen = 0;
    
    if (trace->on == TRUE_CODE) {
        sprintf(g_log_message,
                "http_open ended. socket created:%d", *ps);
        log(trace->cxid, g_log_message);
    }
    return OK_CODE;
}

/*====================================================================*/
/* Emit a request and check for error replies.                        */
/*                                                                    */
/* send command and headers to the http server (optionally through the*/
/* proxy).                                                            */
/*                                                                    */
/* Limitations: the url is truncated to first 256 chars and the server*/
/* name to 128 in case of proxy request.                              */
/*====================================================================*/
int http_query(s, command, uri_server, uri_proxy, request, options,
               trace)
    SOCKET s;                /* an opened socket                      */
    char *command;           /* command to send                       */
    Uri_t *uri_server;       /* structure describing server uri       */
    Uri_t *uri_proxy;        /* structure describing proxy uri        */
    Http_content* request;  /* data to be sent to the server          */
    Http_options* options;  /* optional parameters                    */
    Trace_parm* trace;      /* trace parameters                       */
{
    char header[HEADER_MAXLEN + 1];
    int  proxy = (uri_proxy->host != NULL && uri_proxy->port != 0);
    int rc;
    
    if (trace->on == TRUE_CODE) {
        sprintf(g_log_message,
          "http_query started for command=%s resource=%s",
          command, uri_server->path);
        log(trace->cxid, g_log_message);
    }

    /* send request line http header */
    if (proxy) {
        sprintf(header, REQUEST_LINE_PROXY_FORMAT,
              command, uri_server->host, uri_server->port,
              uri_server->path);
        rc = http_send_header(s, header, strlen(header), trace);
        if (rc != OK_CODE) {
            return rc;
        }
    } else {
        sprintf(header, REQUEST_LINE_FORMAT, command,
                uri_server->path);
        rc = http_send_header(s, header, strlen(header), trace);
        if (rc != OK_CODE) {
            return rc;
        }
    }

    /* send user agent http header */
    sprintf(header, USER_AGENT_FORMAT, HTTP_USER_AGENT);
    rc = http_send_header(s, header, strlen(header), trace);
    if (rc != OK_CODE) {
        return rc;
    }

    /* If data posted, make sure there is a content-length */
    if (request && request->content_len > 0) {
        sprintf(header, CONTENT_LENGTH_FORMAT, request->content_len);
        rc = http_send_header(s, header, strlen(header), trace);
        if (rc != OK_CODE) {
            return rc;
        }
        if (strlen(request->content_type) > 0) {
            sprintf(header, CONTENT_TYPE_FORMAT, request->content_type);
            rc = http_send_header(s, header, strlen(header), trace);
            if (rc != OK_CODE) {
                return rc;
            }
        }
    }

    /* If credentials are provided create an appropriate header
     We use the raw send because the authentication header is already
     in ASCII. */
    if (options->credentials) {
        rc = basic_authheader(options->credentials->user,
                              options->credentials->password,
                              header, trace);
        if (rc != OK_CODE) {
            return rc;
        }
        rc = http_send(s, header, strlen(header), trace);
        if (rc != OK_CODE) {
            return rc;
        }
    }
    
    /* If a correlation id is provided, send it as part of a special
     * http header */
     if (trace->cxid != NULL && strlen(trace->cxid) > 0) {
        sprintf(header, CORRELATION_ID_FORMAT, trace->cxid);
        rc = http_send_header(s, header, strlen(header), trace);
        if (rc != OK_CODE) {
            return rc;
        }
     }

    /* Terminate headers with an extra CRLF */
    strcpy(header, CRLF);
    rc = http_send_header(s, header, strlen(header), trace);
    if (rc != OK_CODE) {
        return rc;
    }

    /* send raw data content */
    if (request && request->content_len > 0) {
        rc = http_send(
                s, request->content, request->content_len, trace);
        if (rc != OK_CODE) {
            return rc;
        }
    }
    
    /* flush any remaining buffered data */
    rc = flushb(s, trace);
    if (rc < 0) {
        return rc;
    }

    /* read result & check when data is received */
    rc = http_read_header(s, header, HEADER_MAXLEN - 1,
                          options->recv_timeout, trace);
    if (rc > 0)  {
        if (sscanf(header, IN_STATUS_LINE_FORMAT, &rc,
                   trace->error_message) != 1) {
            sprintf(trace->error_message, INVALID_REPLY_ERRMSG, header);
            rc = INVALID_REPLY_ERRCODE;
        } else {
            strcpy(trace->error_message, header + 13);
            if (rc != 200 && rc != 201) {
                /* turn the http status code into a negative response 
                 * code */
                rc = -1 * rc;
            } else {
                rc = OK_CODE;
            }
        }
    }
    
    if (trace->on == TRUE_CODE) {
        sprintf(g_log_message,
            "http_query ended. Return code=%d message=%s", 
            rc, trace->error_message);
        log(trace->cxid, g_log_message);
    }
    return rc;
}

/*====================================================================*/
/* Reads data returned from the server and collates http header data  */
/* and the reply data.                                                */
/*====================================================================*/
int http_get_reply(s, reply, options, trace)
    SOCKET s;               /* a socket with data waiting to be read  */
    Http_content* reply;    /* a reply structure to populate          */
    Http_options* options;  /* optional parameters                    */
    Trace_parm* trace;      /* trace parameters                       */
{
    char header[HEADER_MAXLEN + 1];
    int  n, rc;
    char *pc;
    
    if (trace->on == TRUE_CODE) {
        sprintf(g_log_message, "http_get_reply started");
        log(trace->cxid, g_log_message);
    }

    /* Check that we have valid locations to store the reply */
    if (!reply) {
        sprintf(trace->error_message, NULL_DATA_ERRMSG);
        return NULL_DATA_ERRCODE;
    }
    reply->content = NULL;
    reply->content_len = 0;
    reply->content_type[0] = '\0';

    /* Retrieve HTTP reply headers of interest. */
    while (1) {
        n = http_read_header(s, header, HEADER_MAXLEN-1,
                             options->recv_timeout, trace);
        if (n < 0) {
            http_close(s, trace);
            return n;
        }
        /* empty line ? (=> end of headers) */
        if (n == 0) {
            break;
        }
        /* convert to lower case 'till a : is found or end of string */
        for (pc=header; (*pc!=':' && *pc) ; pc++) {
            *pc=tolower(*pc);
        }
        sscanf(header, IN_CONTENT_LENGTH_FORMAT, &reply->content_len);
        sscanf(header, IN_CONTENT_TYPE_FORMAT, reply->content_type);
    }

    if (reply->content_len > 0) {
        /* Allocate memory for reply buffer and populate */
        if (!(reply->content = malloc(reply->content_len))) {
            sprintf(trace->error_message, MEM_ALLOC_ERRMSG);
            return MEM_ALLOC_ERRCODE;
        }
        return http_read(s, reply->content, reply->content_len,
                         options->recv_timeout, trace);
    }
    
    /* reply with no content */
    if (trace->on == TRUE_CODE) {
        sprintf(g_log_message, "http_get_reply ended. no reply data");
        log(trace->cxid, g_log_message);
    }
    return OK_CODE;
}

/*====================================================================*/
/* Connect with timeout                                               */
/*====================================================================*/
int do_connect_timeout(s, saddr, connect_timeout, trace)
    SOCKET s;        /* an newly created socket                       */
    struct sockaddr_in *saddr; /* the socket address                  */
    int connect_timeout;    /* maximum time to wait for connection (s)*/
    Trace_parm* trace;      /* trace parameters                       */
{
    int rc;
    int mode = NON_BLOCKING_MODE;
    int enabled = 1;
    
    /* put the socket in non-blocking mode in order to control
     * timeouts */
    rc = ioctl(s, FIONBIO, (char *)&mode);
    if (rc < 0) {
        return rc;
    }
    
    /* disable the Naggle algorithm */
    rc = setsockopt(s, IPPROTO_TCP, TCP_NODELAY,
                    (char *) &enabled, sizeof(enabled) );
    if (rc < 0) {
        return rc;
    }
    
    /* first attempt should normally fail with EINPROGRESS which
     * means that the connection did not complete yet */
    rc = connect(s, (struct sockaddr *)saddr, sizeof(*saddr));
    if (trace->on == TRUE_CODE) {
        sprintf(g_log_message, "Initial connection attempt:%s",
             get_socket_message(socket_error));
        log(trace->cxid, g_log_message);
    }
    if (rc == 0) {
        return OK_CODE;
    }
    if (socket_error != EINPROGRESS) {
        return rc;
    }
    
    /* Wait till socket becomes writeable */
    rc = do_wait(s, WRITE_OPERATION, connect_timeout, trace);
    if (rc < 0) {
        return rc;
    }
    
    return OK_CODE;
}

/*====================================================================*/
/* Release resources                                                  */
/*====================================================================*/
int http_close(s, trace)
    SOCKET s;        /* an opened socket                             */
    Trace_parm* trace;     /* trace parameters                       */
{
    int rc;
    if (trace->on == TRUE_CODE) {
        log(trace->cxid, "http_close started.");
    }
    rc = close(s);
    if (trace->on == TRUE_CODE) {
        log(trace->cxid, "http_close ended.");
    }
    return rc;
}

/*====================================================================*/
/* Send an http header to an ASCII based server.                      */
/*====================================================================*/
int http_send_header(s, header, headerLen, trace)
    SOCKET s;             /* an opened socket                         */
    char *header;         /* header data                              */
    int headerLen;        /* number of bytes to send                  */
    Trace_parm* trace;      /* trace parameters                       */
{
    __etoa(header);
    return http_send(s, header, headerLen, trace);
}

/*====================================================================*/
/* Send a requested amount of bytes.                                  */
/*====================================================================*/
int http_send(s, data, dataLen, trace)
    SOCKET s;             /* an opened socket                         */
    char *data;           /* data buffer                              */
    int dataLen;          /* number of bytes to send                  */
    Trace_parm* trace;      /* trace parameters                       */
{
    int rc;
    
    if (trace->on == TRUE_CODE) {
        sprintf(g_log_message, "http_send started on socket:%d", s);
        log(trace->cxid, g_log_message);
    }
    
    rc = sendb(s, data, dataLen, trace);
    if (rc < 0) {
        return rc;
    }
   
    if (trace->on == TRUE_CODE) {
        log(trace->cxid, "http_send ended. Content:");
        traceData(trace->cxid, data, rc);
    }
    return OK_CODE;
}

/*====================================================================*/
/* read an http header from an opened socket. a header ends with a    */
/* carriage return/line feed.                                         */
/* returns the number of bytes read. negative if a read error occured */
/* before the end of line or the max.                                 */
/*====================================================================*/
int http_read_header (s, buffer, max, recv_timeout, trace)
    SOCKET s;             /* an opened socket                         */
    char *buffer;         /* placeholder for data                     */
    int max;              /* max number of bytes to read              */
    int recv_timeout;     /* maximum time to wait for data (seconds)  */
    Trace_parm* trace;      /* trace parameters                       */
{
    int n = 0; /* total bytes already read */
    int rc = 0; 

    if (trace->on == TRUE_CODE) {
        sprintf(g_log_message,
                "http_read_header started on socket:%d", s);
        log(trace->cxid, g_log_message);
    }

    while(n < max) {
        rc = do_recv_timeout(s, buffer, 1, recv_timeout, trace);
        if (rc == 0) {
            sprintf(trace->error_message, SOK_CONN_ABORT_ERRMSG);
            return SOK_CONN_ABORT_ERRCODE;
        }
        if (rc != 1) {
            sprintf(trace->error_message, SOK_RECV_ERRMSG,
                        get_socket_message(socket_error));
            return SOK_RECV_ERRCODE;
        }
        n++;
        if (*buffer == ASCII_CR) continue; /* ignore CR */
        if (*buffer == ASCII_LF) break;    /* LF is the separator */
        buffer++;
    }

    /* If something was received, make sure it is a valid header and
     adjust size to account for the header without ending crlf */
    if (n > 0) {
        if (*buffer != ASCII_LF || n == 1) {
            sprintf(trace->error_message, SOK_NOHEAD_ERRMSG);
            return SOK_NOHEAD_ERRCODE;
        } else {
            n -= 2;
        }
    }

    *buffer=0;
    __atoe(buffer - n);
    if (trace->on == TRUE_CODE) {
        log(trace->cxid, "http_read_header ended. Content:");
        traceData(trace->cxid, buffer - n, n);
    }
    return n;
}

/*====================================================================*/
/* read data from opened socket                                       */
/* retries reading until the number of bytes requested is read.       */
/* returns an error indicator if a read error (EOF) occurs            */
/* before the requested length was received.                          */
/*====================================================================*/
int http_read(s, buffer, length, recv_timeout, trace)
    SOCKET s;             /* an opened socket                         */
    char *buffer;         /* placeholder for data                     */
    int length;           /* number of bytes to read                  */
    int recv_timeout;     /* maximum time to wait for data (seconds)  */
    Trace_parm* trace;      /* trace parameters                       */
{
    int n = 0; /* total bytes already read */
    int rc = 0;

    if (trace->on == TRUE_CODE) {
        sprintf(g_log_message,
                "http_read started on socket:%d", s);
        log(trace->cxid, g_log_message);
    }

    for (n = 0; n < length; n += rc) {
        rc = do_recv_timeout(s, buffer, length - n,
                             recv_timeout, trace);
        if ( rc <= 0) {
            sprintf(trace->error_message, SOK_RECV_ERRMSG,
                        get_socket_message(socket_error));
            return SOK_RECV_ERRCODE;
        }
        buffer += rc;
    }

    if (trace->on == TRUE_CODE) {
        log(trace->cxid, "http_read ended. Content:");
        traceData(trace->cxid, buffer - n, n);
    }

    return OK_CODE;
}

/*====================================================================*/
/* Read on a non-blocling socket with a timeout                       */
/*====================================================================*/
int do_recv_timeout(s, buffer, length, recv_timeout, trace)
    SOCKET s;             /* an opened socket                         */
    char *buffer;         /* placeholder for data                     */
    int length;           /* number of bytes to read                  */
    int recv_timeout;     /* maximum time to wait for data (seconds)  */
    Trace_parm* trace;    /* trace parameters                         */
{
    int rc;
       
    /* Attempt to receive, if success, no need to wait */
    rc = recvb(s, buffer, length);
    if (rc > -1) {
        return rc;
    }
      
    /* If this is not a blocking situation, we have a serious problem */
    if (socket_error != EWOULDBLOCK) {
        return rc;
    }
    
    /* Wait till data becomes available */
    rc = do_wait(s, READ_OPERATION, recv_timeout, trace);
    if (rc < 0) {
        return rc;
    }
    return recvb(s, buffer, length);
}

/*====================================================================*/
/* Wait for a socket event with timeout                               */
/*====================================================================*/
 int do_wait(s, operation, timeLimit, trace)
    SOCKET s;            /* an opened socket                          */
    int operation;       /* either read or write operation            */
    int timeLimit;       /* the maximum time we are prepared to wait  */
    Trace_parm* trace;      /* trace parameters                       */
{
  
    struct timeval timeout;
    fd_set mask;
    int rc = 0;
    
    if (trace->on == TRUE_CODE) {
        sprintf(g_log_message,
            "Select for %s operation",
            (operation == READ_OPERATION)? "read" : "write");
        log(trace->cxid, g_log_message);
    }
    timeout.tv_sec  = timeLimit;
    timeout.tv_usec = 0;
    FD_ZERO(&mask);                                        
    FD_SET(s, &mask);   
    if (operation == READ_OPERATION) {
        rc = select(s+1, &mask, NULL, NULL, &timeout);
    }
    if (operation == WRITE_OPERATION) {
        rc = select(s+1, NULL, &mask, NULL, &timeout);
    }
    if (rc == 0) {
        sprintf(g_log_message,
            "Timed out on %s operation",
            (operation == READ_OPERATION)? "read" : "write");
        log(trace->cxid, g_log_message);
        socket_error = ETIMEDOUT;
        return TIMEOUT_ERRCODE;
    }
    if (rc < 0) {
       return rc;
    }
    
    return OK_CODE;
 }
 
/*====================================================================*/
/* Buffered read reduces the requests to sockets. The internal mark   */
/* keeps track of the current position within the buffer. When a      */
/* buffer fault is detected, a socket read is issued.                 */
/*====================================================================*/
int recvb(s, buffer, length)
     SOCKET s;            /* an opened socket                         */
     char *buffer;        /* placeholder for data                     */
     int length;          /* number of bytes to read                  */
{
    int len, rc;
    /* Check if the amount of data requested is available from the 
       current buffer */
    if (g_recvMark + length < g_recvBufferLen) {
        len = length;
    } else {
        /* The data requested is larger than the content on hand. we
           send back the remainder if any */
        if (g_recvMark < g_recvBufferLen) {
            len = g_recvBufferLen - g_recvMark;
        } else {
            /* Buffer is exhausted, try a socket read using the
               maximum buffer capacity */
            g_recvMark = g_recvBufferLen = 0;
            /* If the amount of data requested is larger than our
               buffer capacity, fall back to a regular receive */
            if (length > MAX_RECV_BUF_LEN) {
                return recv(s, buffer, length, 0);
            }
            rc = recv(s, g_recvBuffer, MAX_RECV_BUF_LEN, 0);
            if (rc <= 0) {
                return rc;
            } else {
                g_recvBufferLen = rc;
                len = length;
            }
        }
    }
    memcpy(buffer, g_recvBuffer + g_recvMark, len);
    g_recvMark += len;
    return len;
}

/*====================================================================*/
/* Buffered sends reduces the requests to sockets. If the data to     */
/* send fits in the buffer, it is added to the buffer. If it does not */
/* fit (either because the buffer is filled up or data is too large), */
/* the buffer is flushed on the socket.                               */
/*====================================================================*/
int sendb(s, buffer, length, trace)
    SOCKET s;             /* an opened socket                         */
    char *buffer;         /* placeholder for data                     */
    int length;           /* number of bytes to send                  */
    Trace_parm* trace;      /* trace parameters                       */
{
    int len, rc;
    
    /* Check if the amount of data requested fits in the buffer */
    if (g_sendMark + length < MAX_SEND_BUF_LEN) {
        len = length;
    } else {
        /* Data does not fit so flush the send buffer */
        rc = flushb(s, trace);
        if (rc < 0) {
            return rc;
        }
        /* if data fits in the cleared up buffer, buffer it*/
        if (length < MAX_SEND_BUF_LEN) {
            len = length;
        } else {
            /* No buffering if buffer too small */
            return send_chunks(s, buffer, length, trace);
        }
    }
    
    memcpy(g_sendBuffer + g_sendMark, buffer, len);
    g_sendMark += len;
    return len;
}

/*====================================================================*/
/* Actually sends the buffered data on the socket                     */
/* Returns the number of bytes flushed                                */
/*====================================================================*/
int flushb(s, trace)
    SOCKET s;             /* an opened socket                         */
    Trace_parm* trace;      /* trace parameters                       */
{    
    int rc;
    
    rc = send_chunks(s, g_sendBuffer, g_sendMark, trace);
    if (rc < 0) {
        return rc;
    }

    /* buffer is now empty */
    g_sendMark = 0;
    return rc;
}

/*====================================================================*/
/* Sends the requested amount of data on a socket taking into account */
/* potential chunking by the lower transport protocol.                */
/* Returns the number of bytes sent                                   */
/*====================================================================*/
int send_chunks(s, buffer, length, trace)
    SOCKET s;             /* an opened socket                         */
    char *buffer;         /* the data to send                         */
    int length;           /* number of bytes to send                  */
    Trace_parm* trace;      /* trace parameters                       */
{
    int n = 0;            /* total bytes already sent                 */
    int rc;
    
    while (n < length) {
        rc = send(s, buffer + n, length - n, 0);
        /* If send failed, percolate the error up the stack */
        if (rc < 0) {
            sprintf(trace->error_message, SOK_SEND_ERRMSG,
                    get_socket_message(socket_error));
            return SOK_SEND_ERRCODE;
        }
        /* Prepare to send the next chunk */
        n += rc;
    }
    return n;
}

/*====================================================================*/
/* Create a basic authentication header.                              */
/* This assumes the header is large enough to accomodate the result.  */
/* On MVS, conversion to ASCII must occur before encoding is done     */
/*====================================================================*/
int basic_authheader (user, password, header, trace)
    char *user;             /* User ID                                */
    char *password;         /* Password                               */
    char *header;           /* result authentication header           */
    Trace_parm* trace;      /* trace parameters                       */
{
    char up[USER_PASSWORD_MAXLEN + 1];
    char up64[B64_USER_PASSWORD_MAXLEN + 1];
    int len = strlen (user) + 1 + strlen (password);
    int len64 = BASE64_LENGTH (len);
    char *header_part1 = AUTHORIZATION_PART1;
    char *header_part3 = CRLF;
    
    if (trace->on == TRUE_CODE) {
        log(trace->cxid, "basic_authheader started.");
    }
    if (len > USER_PASSWORD_MAXLEN ||
        len64 > B64_USER_PASSWORD_MAXLEN) {
        sprintf(trace->error_message, USERP_TOOLONG_ERRMSG);
        return USERP_TOOLONG_ERRCODE;
    }
    sprintf (up, "%s:%s", user, password);
    base64_encode (up, len, up64);
    
    __etoa(header_part1);
    __etoa(header_part3);
    sprintf(header,"%s%s%s", header_part1, up64, header_part3);

    if (trace->on == TRUE_CODE) {
        log(trace->cxid, "basic_authheader ended.");
    }
    return OK_CODE;
}

/*====================================================================*/
/* Convert a string to base64                                         */
/*====================================================================*/
int base64_encode (str, length, b64store)
  char *str;            /* String to convert                        */
  int length;           /* Number of bytes to convert               */
  char *b64store;       /* result                                   */
{
    char tbl[64] = {
        'A','B','C','D','E','F','G','H',
        'I','J','K','L','M','N','O','P',
        'Q','R','S','T','U','V','W','X',
        'Y','Z','a','b','c','d','e','f',
        'g','h','i','j','k','l','m','n',
        'o','p','q','r','s','t','u','v',
        'w','x','y','z','0','1','2','3',
        '4','5','6','7','8','9','+','/'
    };
    int i;
    char *s = str;
    char *p = b64store;
    __etoa_l(tbl, 64);
    __etoa(s);

    for (i = 0; i < length; i += 3)
    {
        *p++ = tbl[s[0] >> 2];
        *p++ = tbl[((s[0] & 3) << 4) + (s[1] >> 4)];
        *p++ = tbl[((s[1] & 0xf) << 2) + (s[2] >> 6)];
        *p++ = tbl[s[2] & 0x3f];
        s += 3;
    }

    if (i == length + 1)
        *(p - 1) = ASCII_EQUAL;
    else if (i == length + 2)
        *(p - 1) = *(p - 2) = ASCII_EQUAL;

    *p = '\0';

    return p - b64store;
}

/*====================================================================*/
/* Produce a formatted trace entry for a simple message               */
/*====================================================================*/
int log(char* cxid, char *message) {
    fprintf(stdout, "%s : CxID=%s : %s\n", MODULE_NAME, cxid, message);
    return OK_CODE;
}

/*====================================================================*/
/* Returns a human readable message for socket error codes            */
/*====================================================================*/
char* get_socket_message(int errcode) {
    
    switch (errcode) {
      case (EACCES):
        return "The other application did not give the socket "
        "to your application.";
      case (EBADF):
        return "The socket is not valid or has already been "
        "taken.";
      case (EPIPE):
        return "The connection is broken. For socket write/"
        "send, peer has shut down one or both directions.";
      case (EFAULT):
        return "Using parameters as specified "
        "would result in an attempt to access storage outside the "
        "caller’s address space.";
      case (EINVAL):
        return "One of the parameters to the socket call."
        "contains an invalid value.";
      case (EMFILE):
        return "The socket descriptor table is already full.";
      case (ENOBUFS):
        return "The operation cannot be performed because of "
        "the shortage of SCB or SKCB control blocks in the TCP/IP "
        "address space.";
      case (EPFNOSUPPORT):
        return "The domain field of the clientid parameter is "
        "not AF_INET or AF_INET6.";
      case (EWOULDBLOCK):
        return "socket is in nonblocking mode, and data is "
        "not available to read.";
      case (EINPROGRESS):
        return "socket is in nonblocking mode, connection "
        "attempt is pending.";
      case (ECONNRESET):
        return "Connection reset by peer.";
      case (ECONNREFUSED):
        return "The target host rejected the connection"
        " attempt.";
      case (EIBMINVSOCKET):
        return "A connection token that is not valid was "
        "detected. No such socket exists.";
      case (ETIMEDOUT):
        return "A socket operation timed out.";
      default:
        if (errcode == 0) {
            sprintf(g_error_message, "success");
        } else {
            sprintf(g_error_message, "socket error code=%d", errcode);
        }
        return g_error_message;
    }
}

/*====================================================================*/
/* Produce a dump-like report of a data buffer content                */
/*====================================================================*/
int traceData(char* cxid, char* data, long dataLength )
{
    int i, j;
    char dumpLine[128];
    char dumpChar[5];
    char dumpString[17];
   
    dumpLine[0]='\0';
    dumpString[0]='\0';
    for (i = 0; i < dataLength && i < MAX_TRACES_BYTES; i++) {
       /* print every 16 byte on a different line */
       sprintf(dumpChar,"%02.2X ", data[i] & 0xff);
       j = strlen(dumpChar);
       strcat(dumpLine, dumpChar);
       sprintf(dumpChar,"%c",data[i]);
       if (strlen(dumpChar) > 0) {
            strcat(dumpString, dumpChar);
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
          log(cxid, dumpLine);
          dumpString[0]='\0';
          dumpLine[0]='\0';
       }
    }
   
    if (dataLength > MAX_TRACES_BYTES) {
        sprintf(dumpLine,"...data was truncated at %d bytes",
                MAX_TRACES_BYTES);
        log(cxid, dumpLine);
    }
    
    return OK_CODE;
}


