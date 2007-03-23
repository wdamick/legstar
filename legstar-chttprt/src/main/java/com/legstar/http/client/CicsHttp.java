package com.legstar.http.client;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.httpclient.DefaultHttpMethodRetryHandler;
import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HostConfiguration;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpState;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.httpclient.UsernamePasswordCredentials;
import org.apache.commons.httpclient.auth.AuthScope;
import org.apache.commons.httpclient.methods.ByteArrayRequestEntity;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.params.HttpMethodParams;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.Connection;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.HeaderPart;
import com.legstar.messaging.Message;
import com.legstar.messaging.MessagePart;
import com.legstar.messaging.Request;
import com.legstar.messaging.RequestException;
import com.legstar.config.Constants;

/**
 * Client side CICS HTTP connectivity. This class provides the core
 * methods to connect to CICS over http, send requests, receive 
 * results, etc...
 * This implementation of an HTTP Client does not use an Apache HTTP Connection
 * manager because it is meant to be usable with the LegStar engine which comes
 * with its own connection pooling.
 *
 */
public class CicsHttp implements Connection  {

	/** Mime type of HTTP content. */
	private static final String BINARY_CONTENT_TYPE = "binary/octet-stream";

	/** An identifier for this connection. */
	private String mConnectionID;
	
	/** Host CICS Http endpoint. */
	private CicsHttpEndpoint mCicsHttpEndpoint;
	
	/** Apache HTTP client. Connection persistence is implemented by default. */
	private HttpClient mHttpClient;
	
	/** Apache HTTP host configuration (Remote host address, port, etc.). */
	private HostConfiguration mHostConfig;
	
	/** Apache HTTP state (holds credentials and cookies). */
	private HttpState mHttpState;
	
	/** Apache HTTP POST method (We use POST command with CICS Server).*/
	private PostMethod mPostMethod;
	
	/** Most recent status code returned by an HTTP method execution. */
	private int mStatusCode = 0;

	/** Maximum time (milliseconds) to wait for connection. */
	private int mConnectTimeout;
	
	/** Maximum time (milliseconds) to wait for host reply. */
	private int mReceiveTimeout;
	
	/** Logger. */
	private static final Log LOG = LogFactory.getLog(CicsHttp.class);
	
	/**
	 * A CicsHttp instance exists for a target CICS region, a given CICS URL
	 * path and a particular User ID to use for authentication and 
	 * impersonation. Observe that no password is stored in this class
	 * for security reasons.
	 * 
	 * @param connectionID an identifier for this connection
	 * @param cicsHttpEndpoint CICS Http endpoint
	 * @param connectionTimeout Maximum time (milliseconds) to wait for
	 *  connection
	 * @param receiveTimeout Maximum time (milliseconds) to wait for host reply
	 */
	public CicsHttp(
			final String connectionID,
			final CicsHttpEndpoint cicsHttpEndpoint,
			final int connectionTimeout,
			final int receiveTimeout) {
		mConnectionID = connectionID;
		mConnectTimeout = connectionTimeout;
		mReceiveTimeout = receiveTimeout;
		setCicsHttpEndpoint(cicsHttpEndpoint);
		mHttpClient = createHttpClient();
	}
	
	/**
	 * HTTPClient actually connects to the host on the first request and
	 * will then try to persist the connection. Therefore there is not
	 * much to be done here apart from initializing credentials.
	 * 
	 * @param cicsPassword credentials for basic authentication
	 * @throws ConnectionException if connection fails
	 */
	public final void connect(
			final String cicsPassword) throws ConnectionException {
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("Connection:" + mConnectionID
					 + " Setup connection. Host:" 
					 + mCicsHttpEndpoint.getReport());
		}
		/* Create a state using the passed credentials */
		mHttpState = createHttpState(cicsPassword);
		
		/* Tell the connection manager how long we are prepared to wait
		 * for a connection. */
		mHttpClient.getHttpConnectionManager().getParams().
			setConnectionTimeout(mConnectTimeout);
		
		/* There must be a new post method on each request*/
		mPostMethod = null;

		if (LOG.isDebugEnabled()) {
			LOG.debug("Connection:" + mConnectionID + " Connection setup.");
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
	public final void connectReuse(
			final String cicsPassword) throws ConnectionException {
		connect(cicsPassword);
	}

	/**
	 * A request is serialized as HTTP Headers and a binary payload. 
	 * A new post method is created on each send request.
	 * 
	 * @param request the request to be serviced
	 * @throws RequestException if send fails
	 */
	public final void sendRequest(
			final Request request) throws RequestException {
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("Sending Request:" + request.getID()
			   + " on Connection:" + mConnectionID
			   + " "
			   + request.getRequestMessage().getHeaderPart().
			   			getStringizedKeyValues()
			   + '.');
		}
		/* First make sure we are not out of sync. */
		if (mHttpState == null) {
			throw new RequestException(
					"No prior connect. Missing host credentials.");
		}
		mHttpClient.getParams().setSoTimeout(mReceiveTimeout);
		
		try {
			mPostMethod = createPostMethod(request);
			/* Status code is not processed here because we need to
			 * receive response whatever the status code is. If status code
			 * shows an error, it will be handled while receiving response. */
			mStatusCode = mHttpClient.executeMethod(
					mHostConfig, mPostMethod, mHttpState);
		} catch (HttpException e) {
			throw new RequestException(e);
		} catch (IOException e) {
			throw new RequestException(e);
		}

		if (LOG.isDebugEnabled()) {
			LOG.debug("Request:" + request.getID()
					   + " on Connection:" + mConnectionID
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
	public final void recvResponse(
			final Request request) throws RequestException {
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("Receiving response for Request:" + request.getID()
					   + " on Connection:" + mConnectionID
					   + '.');
		}
		
		/* First make sure we are not out of sync. */
		if (mPostMethod == null) {
			throw new RequestException(
					"No prior send request. Nothing to receive.");
		}

		/* If the response was not successful, the content of the reply
		 * will help the caller handle the error. */
		if (mStatusCode != HttpStatus.SC_OK) {
			throwErrorResponse();
			return;
		}
		
		/* Try to get the response content  */
		try {
			InputStream respStream = mPostMethod.getResponseBodyAsStream();
			request.setResponseMessage(createResponseMessage(respStream));
		} catch (IOException e) {
			throw new RequestException(e);
		} finally {
			mPostMethod.releaseConnection();
		}

		if (LOG.isDebugEnabled()) {
			LOG.debug("Request:" + request.getID()
					   + " on Connection:" + mConnectionID
					   + " response received.");
		}
	}
	
	/**
	 * Terminates a connection with the host. Closing is managed by the
	 * underlying Http connection manager dependant on the partner
	 * support for HTTP 1.1. We make no attempt to interfere here.
	 * @throws RequestException if a failure is detected
	 */
	public void close() throws RequestException {
	}
	
	/**
	 * Create a reusable HTTP Client with a set of parameters that match
	 * the remote CICS Server behavior. At this stage, the HTTPClient is not
	 * associated with a state and a method yet.
	 * @return the new HTTP Client
	 */
	private HttpClient createHttpClient() {
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("enter createHttpClient()");
		}
		HttpClient httpClient = new HttpClient();
		
		/* Consider that if server is failing to respond, we should never retry.
		 * A system such as CICS should always be responsive unless something
		 * bad is happening in which case, retry makes things worse. */
		DefaultHttpMethodRetryHandler retryhandler =
			new DefaultHttpMethodRetryHandler(0, false);
		httpClient.getParams().setParameter(
				HttpMethodParams.RETRY_HANDLER, retryhandler);
		
		/* Preemptive authentication forces the first HTTP payload to hold
		 * credentials. This bypasses the inefficient default mechanism that
		 * expects a 401 reply on the first request and then re-issues the same
		 * request again with credentials.*/
		httpClient.getParams().setAuthenticationPreemptive(true);
		
		return httpClient;
	}
	
	/**
	 * Create a state with the given credentials. A state persists from
	 * request to request.
	 * @param cicsPassword the passed host password
	 * @return a new HTTP State
	 */
	private HttpState createHttpState(final String cicsPassword) {

		if (LOG.isDebugEnabled()) {
			LOG.debug("enter createHttpState(cicsPassword)");
		}

		HttpState httpState = new HttpState();

		/* If a password is not passed, use the one from configuration */
		String password;
		if (cicsPassword == null || cicsPassword.length() == 0) {
			password = mCicsHttpEndpoint.getHostPassword();
		} else {
			password = cicsPassword;
		}
		
		/* If there are no credentials, assume the server might have
		 * been setup without basic authentication. */
		if (password == null || password.length() == 0) {
			return httpState;
		}
		
		/* Username and password will be used as basic authentication
		 *  credentials */
		UsernamePasswordCredentials upc =
			new UsernamePasswordCredentials(
					mCicsHttpEndpoint.getHostUserID(),
					password);
		httpState.setCredentials(
				new AuthScope(mCicsHttpEndpoint.getHostIPAddress(),
						mCicsHttpEndpoint.getHostIPPort(),
						AuthScope.ANY_HOST), upc);
		return httpState;
	}

	/**
	 * Create and populate an HTTP post method to send the execution request.
	 * @param request the request to be serviced
	 * @return the new post method
	 * @throws RequestException if post method cannot be created
	 */
	public final PostMethod createPostMethod(
			final Request request) throws RequestException {
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("enter createPostMethod(request)");
		}
		
		PostMethod postMethod = new PostMethod();

		/* Point to URL under CICS Web Support */ 
		postMethod.setPath(mCicsHttpEndpoint.getHostURLPath());

		/* The CICS Web Server is expecting an HTTP header including the
		 * execution context parameters. */ 
		Map < String, String > keyValues =
			request.getRequestMessage().getHeaderPart().getKeyValues();
		addHttpHeader(postMethod, Constants.CICS_PROGRAM_KEY, keyValues);
		addHttpHeader(postMethod, Constants.CICS_LENGTH_KEY, keyValues);
		addHttpHeader(postMethod, Constants.CICS_DATALEN_KEY, keyValues);
		addHttpHeader(postMethod, Constants.CICS_SYSID_KEY, keyValues);
		addHttpHeader(postMethod, Constants.CICS_SYNCONRET_KEY, keyValues);
		addHttpHeader(postMethod, Constants.CICS_TRANSID_KEY, keyValues);
		
		/* If there are no messages, this will be a no-content HTTP request */
		if (request.getRequestMessage().getHeaderPart().
				getDataPartsNumber() == 0) {
			return postMethod;
		}
		
		/* In this release, we only send a single message part to the
		 * host. In a future release, we will support MultipartRequestEntity */
		if (request.getRequestMessage().getHeaderPart().
				getDataPartsNumber() > 1) {
			throw new RequestException("No support for multi-part messages.");
		}

		/* Create the binary content */
		postMethod.setRequestEntity(
				new ByteArrayRequestEntity(
						request.getRequestMessage().getDataParts().get(0).
						getContent(),
						BINARY_CONTENT_TYPE));

		return postMethod;

	}
	
	/**
	 * Add a new HTTP header for a key.
	 * @param postMethod to populate
	 * @param key the keyword to be used as HTTP header
	 * @param keyValues a map of all key/values
	 */
	private void addHttpHeader(final PostMethod postMethod,
			final String key,
			final Map < String, String > keyValues) {
		String value = keyValues.get(key);
		if (value != null && value.length() > 0) {
			postMethod.setRequestHeader(key, value);
			if (LOG.isDebugEnabled()) {
				LOG.debug("Adding HTTP header=" + key + " value=" + value);
			}
		}
	}


	/**
	 * Creates a response message from the HTTP reply back.
	 * This implementation does not support multi-part messages yet.
	 * @param respStream the HTTP response data
	 * @return a response message
	 * @throws RequestException if response cannot be mapped to a message
	 */
	private Message createResponseMessage(
			final InputStream respStream) throws RequestException {

		if (LOG.isDebugEnabled()) {
			LOG.debug("enter createResponseMessage(respStream)");
		}
		
		List < MessagePart > dataParts = new ArrayList < MessagePart >();
		
		/* The host is not actually sending back a header part with the
		 * HTTP transport. So we fake one. */
		HeaderPart headerPart = new HeaderPart();
		
		/* Nothing in the HTTP payload */
		if (mPostMethod.getResponseContentLength() == 0) {
			return new Message(headerPart, dataParts);
		}
		
		/* Since the protocol is not ready to handle anthing else than
		 * a commarea, assume the message back is a commarea. In the
		 * future, the server will be sending back a multi-part mime
		 * message. */
		CommareaPart commarea = new CommareaPart(
				getHttpResponseContent(respStream));
		dataParts.add(commarea);
		headerPart.setDataPartsNumber(1);
		
		return new Message(headerPart, dataParts);
	}
	
	/**
	 * Creates a byte buffer holding the entire HTTP post reply content.
	 * @param respStream the HTTP response data
	 * @return a byte buffer
	 * @throws RequestException if fail to receive response
	 */
	private byte[] getHttpResponseContent(
			final InputStream respStream) throws RequestException {
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("enter getHttpResponseContent(respStream)"
					+ " " + mPostMethod.getResponseContentLength());
		}
		
		/* Sanity check the reply content length. */
		if (mPostMethod.getResponseContentLength() > Integer.MAX_VALUE) {
			throw (new RequestException("Response is larger than "
					+ Integer.MAX_VALUE));
		}

		/* For large payloads, reading from the stream will not deliver
		 * all the data in a single read. */
		try {
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			byte[] buffer = new byte[4096];
			int len = 0;
			while ((len = respStream.read(buffer)) >= 0) {
				baos.write(buffer, 0, len);
			}
			return baos.toByteArray();
		} catch (IOException e) {
			throw new RequestException(e);
		}

	}
	
	/**
	 * When the response status code is not 200 (OK) we build the
	 * most meaningful exception message.
	 * @throws RequestException systematically thrown
	 */
	private void throwErrorResponse() throws RequestException {
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("enter throwErrorResponse()");
		}
		
		StringBuilder errorMessage = new StringBuilder();
		errorMessage.append(mPostMethod.getStatusText());
		
		/* This might be an error reported by the LegStar layer on host */
		Header cicsError = mPostMethod.getResponseHeader("CICSError");
		if (cicsError != null) {
			errorMessage.append(" ");
			errorMessage.append(cicsError.getValue());
		}
		/* Try to get the response content in all cases (this is 
		 * specifically recommended by the HTTPClient documentation)  */
		try {
			errorMessage.append(" ");
			errorMessage.append(mPostMethod.getResponseBodyAsString());
			throw new RequestException(errorMessage.toString());
		} catch (IOException e) {
			/* Assume there are no error descriptions in the content. */
			throw new RequestException(errorMessage.toString());
		} finally {
			mPostMethod.releaseConnection();
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
	public final String getConnectionID() {
		return mConnectionID;
	}

	/**
	 * @param connectionID an identifier for this connection to set
	 */
	public final void setConnectionID(final String connectionID) {
		mConnectionID = connectionID;
	}

	/** (non-Javadoc).
	 * @see com.legstar.messaging.Connection#getConnectTimeout()
	 * {@inheritDoc}
	 */
	public final long getConnectTimeout() {
		return mConnectTimeout;
	}

	/** (non-Javadoc).
	 * @see com.legstar.messaging.Connection#setConnectTimeout(int)
	 * {@inheritDoc}
	 */
	public final void setConnectTimeout(final long timeout) {
		mConnectTimeout = new Long(timeout).intValue();
	}

	/** (non-Javadoc).
	 * @see com.legstar.messaging.Connection#getReceiveTimeout()
	 * {@inheritDoc}
	 */
	public final long getReceiveTimeout() {
		return mReceiveTimeout;
	}
	
	/** (non-Javadoc).
	 * @see com.legstar.messaging.Connection#setReceiveTimeout(int)
	 * {@inheritDoc}
	 */
	public final void setReceiveTimeout(final long timeout) {
		mReceiveTimeout = new Long(timeout).intValue();
	}

	/**
	 * @return the CICS Http endpoint
	 */
	public final CicsHttpEndpoint getCicsHttpEndpoint() {
		return mCicsHttpEndpoint;
	}

	/**
	 * Creates an HTTP Host configuration based on the HTTP parameters
	 * received.
	 * @param cicsHttpEndpoint the CICS Http endpoint to set
	 */
	public final void setCicsHttpEndpoint(
			final CicsHttpEndpoint cicsHttpEndpoint) {
		mCicsHttpEndpoint = cicsHttpEndpoint;
		mHostConfig = new HostConfiguration();
		mHostConfig.setHost(
				mCicsHttpEndpoint.getHostIPAddress(),
				mCicsHttpEndpoint.getHostIPPort());

	}

	/**
	 * @return the HostConfig
	 */
	public final HostConfiguration getHostConfig() {
		return mHostConfig;
	}

	/**
	 * @return the HttpClient
	 */
	public final HttpClient getHttpClient() {
		return mHttpClient;
	}

	/**
	 * @return the HttpState
	 */
	public final HttpState getHttpState() {
		return mHttpState;
	}

	/**
	 * @return the PostMethod
	 */
	public final PostMethod getPostMethod() {
		return mPostMethod;
	}

	/**
	 * @return the StatusCode
	 */
	public final int getStatusCode() {
		return mStatusCode;
	}

}
