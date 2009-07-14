/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.http.client;

import java.io.IOException;
import java.io.InputStream;

import org.apache.commons.httpclient.DefaultHttpMethodRetryHandler;
import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HostConfiguration;
import org.apache.commons.httpclient.params.HttpConnectionParams;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpState;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.httpclient.UsernamePasswordCredentials;
import org.apache.commons.httpclient.auth.AuthScope;
import org.apache.commons.httpclient.methods.InputStreamRequestEntity;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.params.HttpClientParams;
import org.apache.commons.httpclient.params.HttpMethodParams;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.messaging.HostMessageFormatException;
import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.HostReceiveException;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;

/**
 * Client side CICS HTTP connectivity. This class provides the core
 * methods to connect to CICS over http, send requests, receive 
 * results, etc...
 * This implementation of an HTTP Client does not use an Apache
 * MultithreadConnection manager because it is meant to be usable
 * with the LegStar engine which comes with its own connection pooling.
 *
 */
public class CicsHttp implements LegStarConnection  {

    /**
     * Starting from now it will be the preferred content type. 
     * This is an extract from RFC2046:
     * The "application" media type is to be used for discrete data which do
     * not fit in any of the other categories, and particularly for data to
     * be processed by some type of application program.  This is
     * information which must be processed by an application before it is
     * viewable or usable by a user.
     */
    private static final String APPLICATION_CONTENT_TYPE = "application/octet-stream";

    /** HTTP Header to request traces on host. */
    public static final String REQUEST_TRACE_MODE_HHDR = "CICSTraceMode";

    /** HTTP Header providing a correlation ID. */
    public static final String REQUEST_ID_HHDR = "CICSRequestID";

    /** HTTP Header signaling errors on response. */
    public static final String CICS_ERROR_HHDR = "CICSError";

    /** An identifier for this connection. */
    private String mConnectionID;

    /** Host CICS Http endpoint. */
    private CicsHttpEndpoint mCicsHttpEndpoint;

    /** Apache HTTP client. Connection persistence is implemented by default. */
    private HttpClient mHttpClient;

    /** Apache HTTP POST method (We use POST command with CICS Server).*/
    private PostMethod mPostMethod;

    /** Most recent status code returned by an HTTP method execution. */
    private int mStatusCode = 0;

    /** Logger. */
    private final Log _log = LogFactory.getLog(CicsHttp.class);

    /**
     * A CicsHttp instance exists for a target CICS region, a given CICS URL
     * path and a particular User ID to use for authentication and 
     * impersonation. Observe that no password is stored in this class
     * for security reasons.
     * 
     * @param connectionID an identifier for this connection
     * @param cicsHttpEndpoint CICS Http endpoint
     */
    public CicsHttp(
            final String connectionID,
            final CicsHttpEndpoint cicsHttpEndpoint) {
        mConnectionID = connectionID;
        mCicsHttpEndpoint = cicsHttpEndpoint;
        mHttpClient = createHttpClient(cicsHttpEndpoint);
    }

    /**
     * HTTPClient actually connects to the host on the first request and
     * will then try to persist the connection. Therefore there is not
     * much to be done here apart from initializing credentials.
     * 
     * @param cicsPassword credentials for basic authentication
     * @throws ConnectionException if connection fails
     */
    public void connect(
            final String cicsPassword) throws ConnectionException {

        if (_log.isDebugEnabled()) {
            _log.debug("Connection:" + getConnectionID()
                    + " Setup connection. Host:" 
                    + getCicsHttpEndpoint());
        }
        /* If a password is not passed, use the one from configuration */
        String password;
        if (cicsPassword == null || cicsPassword.length() == 0) {
            password = getCicsHttpEndpoint().getHostPassword();
        } else {
            password = cicsPassword;
        }

        /* Create a state using the passed credentials.
         * TODO add support for realm */
        getHttpClient().setState(createHttpState(
                getCicsHttpEndpoint().getHostIPAddress(),
                getCicsHttpEndpoint().getHostIPPort(),
                getCicsHttpEndpoint().getHostUserID(),
                password,
                null));

        if (_log.isDebugEnabled()) {
            _log.debug("Connection:" + getConnectionID() + " Connection setup.");
        }
    }

    /**
     * Client is requesting reuse of the HTTP Connection. This is happening
     * automatically with HTTPClient (If the server supports HTTP 1.1).
     * Therefore there is no particular processing here.
     * @param cicsPassword host password if it is not stored in configuration
     *  file
     *  @throws ConnectionException if connection fails
     * */
    public void connectReuse(
            final String cicsPassword) throws ConnectionException {
        if (!isOpen()) {
            connect(cicsPassword);
        }
    }

    /**
     * A request is serialized as HTTP Headers and a binary payload. 
     * A new post method is created on each send request.
     * 
     * @param request the request to be serviced
     * @throws RequestException if send fails
     */
    public void sendRequest(
            final LegStarRequest request) throws RequestException {

        if (_log.isDebugEnabled()) {
            try {
                _log.debug("Sending Request:" + request.getID()
                        + " on Connection:" + getConnectionID()
                        + " "
                        + request.getRequestMessage().getHeaderPart().
                        getJsonString()
                        + '.');
            } catch (HeaderPartException e) {
                throw new RequestException(e);
            }
        }
        try {
            mPostMethod = createPostMethod(request,
                    getCicsHttpEndpoint().getHostURLPath());
            /* Status code is not processed here because we need to
             * receive response whatever the status code is. If status code
             * shows an error, it will be handled while receiving response. */
            mStatusCode = getHttpClient().executeMethod(mPostMethod);
        } catch (HttpException e) {
            throw new RequestException(e);
        } catch (IOException e) {
            throw new RequestException(e);
        }

        if (_log.isDebugEnabled()) {
            _log.debug("Request:" + request.getID()
                    + " on Connection:" + getConnectionID()
                    + " message request sent.");
        }
    }

    /**
     * A response is serialized as a header message part followed by 
     * data message parts. This method creates a response message
     * for the request.
     * 
     * @param request the request being serviced
     * @throws RequestException if receive fails
     */
    public void recvResponse(
            final LegStarRequest request) throws RequestException {

        if (_log.isDebugEnabled()) {
            _log.debug("Receiving response for Request:" + request.getID()
                    + " on Connection:" + getConnectionID()
                    + '.');
        }

        /* First make sure we are not out of sync. */
        if (getPostMethod() == null) {
            throw new RequestException(
            "No prior send request. Nothing to receive.");
        }

        /* If the response was not successful, the content of the reply
         * will help the caller handle the error. */
        if (getStatusCode() != HttpStatus.SC_OK) {
            throwErrorResponse(getPostMethod());
            return;
        }

        /* At this stage, HTTP is not reporting and error. Try to get a
         * valid response message from the HTTP payload */
        InputStream respStream = null;
        try {
            respStream = getPostMethod().getResponseBodyAsStream();
            request.setResponseMessage(createResponseMessage(respStream));
        } catch (IOException e) {
            throw new RequestException(e);
        } catch (HostReceiveException e) {
            throw new RequestException(e);
        } finally {
            if (respStream != null) {
                try {
                    respStream.close();
                } catch (IOException e) {
                    _log.warn("Unable to close response stream", e);
                }
            }
            getPostMethod().releaseConnection();
        }

        if (_log.isDebugEnabled()) {
            _log.debug("Request:" + request.getID()
                    + " on Connection:" + getConnectionID()
                    + " response received.");
        }
    }

    /**
     * Terminates a connection with the host. This is invoked
     * when connections are not reused. The underlying connection manager
     * will try to keep the connection open if it is HTTP 1.1 compliant.
     * These pending connections cause TS31 to shoke so we force a
     * close here.
     * @throws RequestException if close fails
     */
    public void close() throws RequestException {
        if (getHttpClient() != null) {
            getHttpClient().getHttpConnectionManager().closeIdleConnections(0);
        }
    }

    /**
     * Create a reusable HTTP Client with a set of parameters that match
     * the remote CICS Server characteristics. At this stage, the HTTPClient is not
     * associated with a state and a method yet.
     * @param cicsHttpEndpoint the connection configuration
     * @return the new HTTP Client
     */
    public HttpClient createHttpClient(
            final CicsHttpEndpoint cicsHttpEndpoint) {

        if (_log.isDebugEnabled()) {
            _log.debug("enter createHttpClient()");
        }
        HttpClientParams params = new HttpClientParams();

        /* Consider that if server is failing to respond, we should never retry.
         * A system such as CICS should always be responsive unless something
         * bad is happening in which case, retry makes things worse. */
        DefaultHttpMethodRetryHandler retryhandler =
            new DefaultHttpMethodRetryHandler(0, false);
        params.setParameter(
                HttpMethodParams.RETRY_HANDLER, retryhandler);

        /* Preemptive authentication forces the first HTTP payload to hold
         * credentials. This bypasses the inefficient default mechanism that
         * expects a 401 reply on the first request and then re-issues the same
         * request again with credentials.*/
        params.setAuthenticationPreemptive(true);

        /* Set the receive time out. */
        params.setSoTimeout(cicsHttpEndpoint.getReceiveTimeout());

        /* Tell the connection manager how long we are prepared to wait
         * for a connection. */
        params.setIntParameter(
                HttpConnectionParams.CONNECTION_TIMEOUT, cicsHttpEndpoint.getConnectTimeout());

        /* Disable Nagle algorithm */
        params.setBooleanParameter(
                HttpConnectionParams.TCP_NODELAY, true);

        HttpClient httpClient = new HttpClient(params);

        httpClient.setHostConfiguration(createHostConfiguration(cicsHttpEndpoint));

        return httpClient;
    }

    /**
     * Create an http host configuration using the protocol/host/port triple.
     * @param cicsHttpEndpoint the connection configuration
     * @return a valid host configuration
     */
    public HostConfiguration createHostConfiguration(
            final CicsHttpEndpoint cicsHttpEndpoint) {
        HostConfiguration hostConfiguration = new HostConfiguration();
        hostConfiguration.setHost(
                cicsHttpEndpoint.getHostIPAddress(),
                cicsHttpEndpoint.getHostIPPort(),
                cicsHttpEndpoint.getHostURLProtocol());
        /* TODO add proxy handling */
        return hostConfiguration;
    }

    /**
     * Create a state with the given credentials. A state persists from
     * request to request.
     * @param host the host name
     * @param port the port number
     * @param userid the host user id
     * @param password the host password
     * @param realm the host realm
     * @return a new HTTP State
     */
    public HttpState createHttpState(
            final String host,
            final int port,
            final String userid,
            final String password,
            final String realm) {

        if (_log.isDebugEnabled()) {
            _log.debug("enter createHttpState");
        }

        HttpState httpState = new HttpState();

        /* If there are no credentials, assume the server might have
         * been setup without basic authentication. */
        if (password == null || password.length() == 0) {
            return httpState;
        }

        /* Username and password will be used as basic authentication
         *  credentials */
        UsernamePasswordCredentials upc =
            new UsernamePasswordCredentials(userid, password);
        httpState.setCredentials(
                new AuthScope(host, port,
                        (realm == null || realm.length() == 0) ? null : realm,
                                AuthScope.ANY_SCHEME), upc);
        return httpState;
    }

    /**
     * Create and populate an HTTP post method to send the execution request.
     * @param request the request to be serviced
     * @param hostURLPath the target host URL path
     * @return the new post method
     * @throws RequestException if post method cannot be created
     */
    public PostMethod createPostMethod(
            final LegStarRequest request,
            final String hostURLPath) throws RequestException {

        if (_log.isDebugEnabled()) {
            _log.debug("enter createPostMethod(request)");
        }

        PostMethod postMethod = new PostMethod();

        /* Point to URL under CICS Web Support */ 
        postMethod.setPath(hostURLPath);

        /* Pass on trace data to host via HTTP headers */
        postMethod.setRequestHeader(REQUEST_TRACE_MODE_HHDR,
                Boolean.toString(request.getAddress().isHostTraceMode()));
        postMethod.setRequestHeader(REQUEST_ID_HHDR, request.getID());

        /* Create the binary content */
        try {
            postMethod.setRequestEntity(
                    new InputStreamRequestEntity(
                            request.getRequestMessage().sendToHost(),
                            APPLICATION_CONTENT_TYPE));
        } catch (HostMessageFormatException e) {
            throw new RequestException(e);
        }

        return postMethod;

    }

    /**
     * Creates a response message from the HTTP reply back.
     * The HTTP payload should contain serailization of a header part 
     * followed by any number of data parts.
     * @param respStream the HTTP response data
     * @return a response message
     * @throws HostReceiveException if response cannot be mapped to a message
     */
    private LegStarMessage createResponseMessage(
            final InputStream respStream) throws HostReceiveException {

        if (_log.isDebugEnabled()) {
            _log.debug("enter createResponseMessage(respStream)");
        }

        LegStarMessage reponseMessage;
        try {
            reponseMessage = new LegStarMessage();
            reponseMessage.recvFromHost(respStream);
        } catch (HeaderPartException e) {
            throw new HostReceiveException(e);
        } catch (HostMessageFormatException e) {
            throw new HostReceiveException(e);
        }

        if (_log.isDebugEnabled()) {
            _log.debug("response message received");
        }
        return reponseMessage;
    }

    /**
     * When the response status code is not 200 (OK) we build the
     * most meaningful exception message.
     * @param postMethod the method that failed
     * @throws RequestException systematically thrown
     */
    private void throwErrorResponse(
            final PostMethod postMethod) throws RequestException {

        if (_log.isDebugEnabled()) {
            _log.debug("enter throwErrorResponse()");
        }

        StringBuilder errorMessage = new StringBuilder();
        errorMessage.append(postMethod.getStatusText());

        /* This might be an error reported by the LegStar layer on host */
        Header cicsError = postMethod.getResponseHeader(CICS_ERROR_HHDR);
        if (cicsError != null) {
            errorMessage.append(" ");
            errorMessage.append(cicsError.getValue());
        }
        /* Try to get the response content in all cases (this is 
         * specifically recommended by the HTTPClient documentation)  */
        try {
            errorMessage.append(" ");
            errorMessage.append(postMethod.getResponseBodyAsString());
            throw new RequestException(errorMessage.toString());
        } catch (IOException e) {
            /* Assume there are no error descriptions in the content. */
            throw new RequestException(errorMessage.toString());
        } finally {
            postMethod.releaseConnection();
        }
    }

    /** No-op for HTTP transport.
     * {@inheritDoc} */
    public void commitUOW() throws RequestException {
    }

    /** No-op for HTTP transport.
     * {@inheritDoc} */
    public void keepUOW() throws RequestException {
    }

    /** No-op for HTTP transport.
     * {@inheritDoc} */
    public void rollbackUOW() throws RequestException {
    }

    /**
     * @return the identifier for this connection
     */
    public String getConnectionID() {
        return mConnectionID;
    }

    /**
     * @param connectionID an identifier for this connection to set
     */
    public void setConnectionID(final String connectionID) {
        mConnectionID = connectionID;
    }

    /** (non-Javadoc).
     * @see com.legstar.messaging.LegStarConnection#getConnectTimeout()
     * {@inheritDoc}
     */
    public long getConnectTimeout() {
        return getHttpClient().getParams().getIntParameter(
                HttpConnectionParams.CONNECTION_TIMEOUT, -1);
    }

   /** (non-Javadoc).
     * @see com.legstar.messaging.LegStarConnection#getReceiveTimeout()
     * {@inheritDoc}
     */
    public long getReceiveTimeout() {
        return getHttpClient().getParams().getSoTimeout();
    }

    /**
     * @return the CICS Http endpoint
     */
    public CicsHttpEndpoint getCicsHttpEndpoint() {
        return mCicsHttpEndpoint;
    }

    /**
     * @return the HttpClient
     */
    public HttpClient getHttpClient() {
        return mHttpClient;
    }

    /**
     * @param httpClient the HttpClient
     */
    public void setHttpClient(final HttpClient httpClient) {
        mHttpClient = httpClient;
    }

    /**
     * @return the PostMethod
     */
    public PostMethod getPostMethod() {
        return mPostMethod;
    }

    /**
     * @return the StatusCode
     */
    public int getStatusCode() {
        return mStatusCode;
    }

    /** {@inheritDoc} */
    public boolean isOpen() {
        return getHttpClient().getHttpConnectionManager().getConnection(
                getHttpClient().getHostConfiguration()).isOpen();
    }

}
