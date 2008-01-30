/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.mq.client;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.util.Hashtable;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.ibm.mq.MQC;
import com.ibm.mq.MQException;
import com.ibm.mq.MQGetMessageOptions;
import com.ibm.mq.MQMessage;
import com.ibm.mq.MQPutMessageOptions;
import com.ibm.mq.MQQueue;
import com.ibm.mq.MQQueueManager;
import com.legstar.codec.HostCodec;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.HostReceiveException;
import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;

/**
 * Client side CICS MQ connectivity. This class provides the core
 * methods to connect to CICS over MQ, send requests, receive 
 * results, etc...
 *
 */
public class CicsMQ implements LegStarConnection  {

	/** An identifier for this connection. */
	private String mConnectionID;
	
	/** Host CICS Http endpoint. */
	private CicsMQEndpoint mCicsMQEndpoint;
	
	/** The target MQ Manager. */
	private MQQueueManager mMQManager;
	
	/** The request queue. */
	private MQQueue mRequestQueue;
	
	/** The response queue. */
	private MQQueue mResponseQueue;
	
	/** Maximum time (milliseconds) to wait for connection. */
	private int mConnectTimeout;
	
	/** Maximum time (milliseconds) to wait for host reply. */
	private int mReceiveTimeout;
	
	/** This host character set is used for the protocol elements
	 * which are checked by the LegStar host programs. Because these
	 * target host programs are compiled with a fixed charset, it
	 * might be different from the actual user data character set. */
	private String mHostProtocolCharset;
	
	/** Logger. */
	private static final Log LOG = LogFactory.getLog(CicsMQ.class);
	
	/**
	 * A CicsMQ instance exists for a target MQ Manager, a given MQ Request
	 * Queue and a particular User ID to use for authentication and 
	 * impersonation. Observe that no password is stored in this class
	 * for security reasons.
	 * 
	 * @param connectionID an identifier for this connection
	 * @param cicsMQEndpoint MQ endpoint
	 * @param connectionTimeout Maximum time (milliseconds) to wait for
	 *  connection
	 * @param receiveTimeout Maximum time (milliseconds) to wait for host reply
	 * @throws CicsMQConnectionException if instanciation fails
	 */
	public CicsMQ(
			final String connectionID,
			final CicsMQEndpoint cicsMQEndpoint,
			final int connectionTimeout,
			final int receiveTimeout) throws CicsMQConnectionException {
		mConnectionID = connectionID;
		mConnectTimeout = connectionTimeout;
		mReceiveTimeout = receiveTimeout;
		setCicsMQEndpoint(cicsMQEndpoint);
		mHostProtocolCharset = HostCodec.HEADER_CODE_PAGE;
	}
	
	/**
	 * Connect to a IBM MQ Manager passing credentials.
	 * 
	 * @param mqPassword credentials for security exits
	 * @throws ConnectionException if connection fails
	 */
	public final void connect(
			final String mqPassword) throws ConnectionException {

		if (LOG.isDebugEnabled()) {
			LOG.debug("Connection:" + mConnectionID
					 + " Attempting connection. Host:" 
					 + mCicsMQEndpoint.getReport());
		}

		/* If a password is not passed, use the one from configuration */
		String password;
		if (mqPassword == null || mqPassword.length() == 0) {
			password = mCicsMQEndpoint.getHostPassword();
		} else {
			password = mqPassword;
		}
		mMQManager = createMQManager(password);
		mRequestQueue = getRequestQueue(mMQManager);
		mResponseQueue = getResponseQueue(mMQManager);
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("Connection:" + mConnectionID + " Connected.");
		}
	}

	/**
	 * Create an MQ Manager instance.
	 * @param mqPassword credentials for security exits
	 * @return the new MQ manager
	 * @throws CicsMQConnectionException if manager cannot be created
	 */
	private MQQueueManager createMQManager(
			final String mqPassword) throws CicsMQConnectionException {
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("enter createMQManager()");
		}
		/*
		 * Let the MQ API understand that we are a client only. Without this
		 * the API fails with java.lang.UnsatisfiedLinkError: no mqjbnd05 in
		 * java.library.path because it assumes by default that there is a
		 * local queue manager.
		 */
		Hashtable < String, Object > props =
			new java.util.Hashtable < String, Object >();
		props.put(MQC.TRANSPORT_PROPERTY, MQC.TRANSPORT_MQSERIES_CLIENT);
		props.put(MQC.HOST_NAME_PROPERTY, mCicsMQEndpoint.getHostIPAddress());
		props.put(MQC.PORT_PROPERTY, mCicsMQEndpoint.getHostIPPort());
		props.put(MQC.CHANNEL_PROPERTY, mCicsMQEndpoint.getHostMQChannel());
		if (mCicsMQEndpoint.getHostUserID() != null) {
			props.put(MQC.USER_ID_PROPERTY, mCicsMQEndpoint.getHostUserID());
		}
		if (mqPassword != null) {
			props.put(MQC.PASSWORD_PROPERTY, mqPassword);
		}
		try {
			return new MQQueueManager(
					mCicsMQEndpoint.getHostMQManager(), props);
		} catch (MQException e) {
			throw new CicsMQConnectionException(e);
		}
	}
	
	/**
	 * Gets a handle on the request queue, ready to be written.
	 * @param mqManager the MQ manager
	 * @return the request queue
	 * @throws CicsMQConnectionException if queue cannot be created
	 */
	private MQQueue getRequestQueue(
			final MQQueueManager mqManager) throws CicsMQConnectionException {

		if (LOG.isDebugEnabled()) {
			LOG.debug("enter getRequestQueue()");
		}

		/* We are passing identity context data for tracing purposes */
		int openOptions = MQC.MQOO_OUTPUT
		                + MQC.MQOO_SET_IDENTITY_CONTEXT
		                + MQC.MQOO_FAIL_IF_QUIESCING;
		try {
			return mqManager.accessQueue(
					mCicsMQEndpoint.getHostMQRequestQueue(),
			        openOptions, null, null, null);
		} catch (MQException e) {
			throw new CicsMQConnectionException(e);
		}
	}
	
	/**
	 * Gets a handle on the response queue, ready to be read.
	 * @param mqManager the MQ manager
	 * @return the response queue
	 * @throws CicsMQConnectionException if queue cannot be created
	 */
	private MQQueue getResponseQueue(
			final MQQueueManager mqManager) throws CicsMQConnectionException {

		if (LOG.isDebugEnabled()) {
			LOG.debug("enter getResponseQueue()");
		}

		int openOptions = MQC.MQOO_INPUT_SHARED
		                + MQC.MQOO_SAVE_ALL_CONTEXT
		                + MQC.MQOO_FAIL_IF_QUIESCING;
		try {
			return mqManager.accessQueue(
					mCicsMQEndpoint.getHostMQResponseQueue(),
			        openOptions, null, null, null);
		} catch (MQException e) {
			throw new CicsMQConnectionException(e);
		}
	}
	
	/**
	 * Terminates a connection with the host.
	 * @throws RequestException if a failure is detected
	 */
	public final void close() throws RequestException {
		if (mRequestQueue != null && mRequestQueue.isOpen()) {
			try {
				mRequestQueue.close();
			} catch (MQException e) {
				throw new RequestException(e);
			}
		}
		if (mResponseQueue != null && mResponseQueue.isOpen()) {
			try {
				mResponseQueue.close();
			} catch (MQException e) {
				throw new RequestException(e);
			}
		}
		if (mMQManager != null && mMQManager.isConnected()) {
			try {
				mMQManager.disconnect();
			} catch (MQException e) {
				throw new RequestException(e);
			}
		}
	}

	/**
	 * This method simply checks that a valid MQ Manager and Request Queue
	 * are already available.
	 * @param mqPassword host password if it is not stored in configuration
	 *  file
	 *  @throws ConnectionException if connection fails
	 * */
	public final void connectReuse(
			final String mqPassword) throws ConnectionException {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Connection:" + mConnectionID + " Attempting reuse.");
		}

		if (mMQManager != null
				&& mRequestQueue != null && mResponseQueue != null) {
			if (LOG.isDebugEnabled()) {
				LOG.debug("Connection:" + mConnectionID
						+ " Connection will be reused.");
			}
			return;
		}
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("Connection:" + mConnectionID
					+ " Connection not reusable.");
		}
		/* Socket is not reusable, fallback to standard connect. */
		connect(mqPassword);
	}


	/**
	 * Creates and put an MQ message. 
	 * 
	 * @param request the request to be serviced
	 * @throws RequestException if send fails
	 */
	public final void sendRequest(
			final LegStarRequest request) throws RequestException {

		if (LOG.isDebugEnabled()) {
			try {
				LOG.debug("Sending Request:" + request.getID()
				   + " on Connection:" + mConnectionID
				   + " "
				   + request.getRequestMessage().getHeaderPart().
				   			getJsonString()
				   + '.');
				} catch (HeaderPartException e) {
					throw new RequestException(e);
				}
		}

		MQMessage mqMessage = createMQRequestMessage(request);
		try {
			MQPutMessageOptions pmo = new MQPutMessageOptions();
			pmo.options = MQC.MQPMO_NO_SYNCPOINT
							+ MQC.MQPMO_SET_IDENTITY_CONTEXT
							+ MQC.MQPMO_FAIL_IF_QUIESCING;
			mRequestQueue.put(mqMessage, pmo);
			/* Get the unique message ID that was generated by MQ.
			 * It will be needed to retrieve the correlated reply.
			 * So store it as an attachment to the request. */
			request.setAttachment(mqMessage.messageId);
		} catch (MQException e) {
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
			final LegStarRequest request) throws RequestException {

		if (LOG.isDebugEnabled()) {
			LOG.debug("Receiving response for Request:" + request.getID()
					   + " on Connection:" + mConnectionID
					   + '.');
		}
		
		MQMessage mqMessage = new MQMessage();
		try {
			mqMessage.messageId = MQC.MQMI_NONE;
			/* The reply is correlated to the request by means of the
			 * MQ message ID that was generated when we sent the
			 * request. That ID was attached to the request object. */
			mqMessage.correlationId = request.getAttachment();
			MQGetMessageOptions gmo = new MQGetMessageOptions();
			gmo.options =  MQC.MQPMO_NO_SYNCPOINT
							+ MQC.MQGMO_WAIT
							+ MQC.MQPMO_FAIL_IF_QUIESCING;
			gmo.waitInterval = mReceiveTimeout;
			mResponseQueue.get(mqMessage, gmo);
			InputStream respStream = q2pipe(mqMessage);
			request.setResponseMessage(createResponseMessage(respStream));
			
		} catch (MQException e) {
			throw new RequestException(e);
		} catch (UnsupportedEncodingException e) {
			throw new RequestException(e);
		} catch (IOException e) {
			throw new RequestException(e);
		} catch (HostReceiveException e) {
			throw new RequestException(e);
		}
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("Request:" + request.getID()
					   + " on Connection:" + mConnectionID
					   + " response received.");
		}
	}

	/**
	 * Creates an MQ request message with appropriate header data.
	 * A request is folded as MQ Headers and a binary payload. 
	 * @param request request description
	 * @return the MQ message
	 * @throws RequestException if formatting of mq message fails
	 */
	private MQMessage createMQRequestMessage(
			final LegStarRequest request) throws RequestException {
		MQMessage mqMessage = new MQMessage();
		
		/* Reply to queue name is where we expect the reply. We expect it to
		 * be managed by the same mq manager as the request queue. */
		mqMessage.replyToQueueName = mCicsMQEndpoint.getHostMQResponseQueue();
		mqMessage.replyToQueueManagerName = mCicsMQEndpoint.getHostMQManager();
		
		try {
			/* Use the request ID to identify the mq message and also to be 
			 * used as a correlation ID */
			mqMessage.messageId =
				request.getID().getBytes(mHostProtocolCharset);
			mqMessage.correlationId = mqMessage.messageId;

			/* Use the 5 first characters of the application identity data to
			 * specify if host traces are requested */
			mqMessage.applicationIdData = Boolean.toString(
					request.getAddress().isHostTraceMode());
			mqMessage.userId = mCicsMQEndpoint.getHostUserID();
			
			/* Finally create the mq message content */
			pipe2q(request.getRequestMessage().sendToHost(), mqMessage);
			
		} catch (UnsupportedEncodingException e) {
			throw new RequestException(e);
		} catch (IOException e) {
			throw new RequestException(e);
		}
		
		return mqMessage;
	}

	/**
	 * Creates a response message from the MQ reply back.
	 * The MQ payload should contain serailization of a header part 
	 * followed by any number of data parts.
	 * @param respStream the MQ response data
	 * @return a response message
	 * @throws HostReceiveException if response cannot be mapped to a message
	 */
	private LegStarMessage createResponseMessage(
			final InputStream respStream) throws HostReceiveException {

		if (LOG.isDebugEnabled()) {
			LOG.debug("enter createResponseMessage(respStream)");
		}
		
		LegStarMessage reponseMessage;
		try {
			reponseMessage = new LegStarMessage();
			reponseMessage.recvFromHost(respStream);
		} catch (HeaderPartException e) {
			throw new HostReceiveException(e);
		}
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("response message received");
		}
		return reponseMessage;
	}
	
	/**
	 * Simple piping using an intermediary buffer.
	 * @param in the input stream
	 * @param mqMessage the mq message
	 * @throws IOException if piping fails
	 */
	private void pipe2q(
			final InputStream in,
			final MQMessage mqMessage) throws IOException {
		byte[] buffer = new byte[1024];
		int r;
		while ((r = in.read(buffer)) > 0) {
			mqMessage.write(buffer, 0, r);
		}
	}
	
	/**
	 * Since there does not seem to be a stream interface to mq
	 * messages we have to create the stream here.
	 * @param mqMessage the MQ message
	 * @return a byte stream
	 * @throws IOException if piping fails
	 */
	private InputStream q2pipe(
			final MQMessage mqMessage) throws IOException {
		byte[] buffer = new byte[mqMessage.getDataLength()];
		mqMessage.readFully(buffer);
		ByteArrayInputStream stream = new ByteArrayInputStream(buffer);
		return stream;
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
	public final CicsMQEndpoint getCicsMQEndpoint() {
		return mCicsMQEndpoint;
	}

	/**
	 * Creates an MQ Host configuration based on the HTTP parameters
	 * received.
	 * @param cicsMQEndpoint the MQ endpoint to set
	 */
	public final void setCicsMQEndpoint(
			final CicsMQEndpoint cicsMQEndpoint) {
		mCicsMQEndpoint = cicsMQEndpoint;
	}

	/**
	 * @return the MQ Manager
	 */
	public final MQQueueManager getMQManager() {
		return mMQManager;
	}

	/**
	 * @return the Request Queue
	 */
	public final MQQueue getRequestQueue() {
		return mRequestQueue;
	}

}
