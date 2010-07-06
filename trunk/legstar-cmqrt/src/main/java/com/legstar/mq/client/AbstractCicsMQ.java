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
package com.legstar.mq.client;

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
import com.legstar.coxb.host.HostData;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.HostReceiveException;
import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;

/**
 * Client side MQ connectivity. This class provides the common
 * methods to connect to a mainframe over MQ, send requests, receive 
 * results, etc...
 *
 */
public abstract class AbstractCicsMQ implements LegStarConnection  {

    /** An identifier for this connection. */
    private String mConnectionID;

    /** Host CICS WMQ endpoint. */
    private CicsMQEndpoint mCicsMQEndpoint;

    /** The target MQ Manager. */
    private MQQueueManager mMQManager;

    /** The request queue. */
    private MQQueue mRequestQueue;

    /** The response queue. */
    private MQQueue mResponseQueue;

    /** true if connection opened. */
    private boolean _isOpen;
    
    /** last time this connection was used. */
    private long _lastUsedTime = -1;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * A CicsMQ instance exists for a target MQ Manager, a given MQ Request
     * Queue and a particular User ID to use for authentication and 
     * impersonation. Observe that no password is stored in this class
     * for security reasons.
     * 
     * @param connectionID an identifier for this connection
     * @param cicsMQEndpoint MQ endpoint
     * @throws CicsMQConnectionException if instanciation fails
     */
    public AbstractCicsMQ(
            final String connectionID,
            final CicsMQEndpoint cicsMQEndpoint) throws CicsMQConnectionException {
        mConnectionID = connectionID;
        setCicsMQEndpoint(cicsMQEndpoint);
    }

    /**
     * Connect to a IBM MQ Manager passing credentials.
     * 
     * @param mqPassword credentials for security exits
     * @throws ConnectionException if connection fails
     */
    public void connect(
            final String mqPassword) throws ConnectionException {

        if (_log.isDebugEnabled()) {
            _log.debug("Connection:" + mConnectionID
                    + " Attempting connection. Host:" 
                    + mCicsMQEndpoint.toString());
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

        _isOpen = true;
        _lastUsedTime = System.currentTimeMillis();
        if (_log.isDebugEnabled()) {
            _log.debug("Connection:" + mConnectionID + " Connected.");
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

        if (_log.isDebugEnabled()) {
            _log.debug("enter createMQManager()");
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

        if (_log.isDebugEnabled()) {
            _log.debug("enter getRequestQueue()");
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

        if (_log.isDebugEnabled()) {
            _log.debug("enter getResponseQueue()");
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
    public void close() throws RequestException {
        if (_log.isDebugEnabled()) {
            _log.debug("Connection:" + mConnectionID + " closing.");
        }
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
        mRequestQueue = null;
        mResponseQueue = null;
        mMQManager = null;
        _isOpen = false;
        _lastUsedTime = System.currentTimeMillis();
    }

    /**
     * This method simply checks that a valid MQ Manager and Request Queue
     * are already available.
     * @param mqPassword host password if it is not stored in configuration
     *  file
     *  @throws ConnectionException if connection fails
     * */
    public void connectReuse(
            final String mqPassword) throws ConnectionException {
        if (_log.isDebugEnabled()) {
            _log.debug("Connection:" + mConnectionID + " Attempting reuse.");
        }

        if (isOpen()) {
            if (_log.isDebugEnabled()) {
                _log.debug("Connection:" + mConnectionID
                        + " Connection will be reused.");
            }
            _lastUsedTime = System.currentTimeMillis();
            return;
        }

        if (_log.isDebugEnabled()) {
            _log.debug("Connection:" + mConnectionID
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
    public void sendRequest(
            final LegStarRequest request) throws RequestException {

        if (_log.isDebugEnabled()) {
            try {
                _log.debug("Sending Request:" + request.getID()
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
        /* Reply to queue name is where we expect the reply. We expect it to
         * be managed by the same mq manager as the request queue. */
        mqMessage.replyToQueueName = getCicsMQEndpoint().getHostMQResponseQueue();
        mqMessage.replyToQueueManagerName = getCicsMQEndpoint().getHostMQManager();

        try {
            MQPutMessageOptions pmo = new MQPutMessageOptions();
            pmo.options = MQC.MQPMO_NO_SYNCPOINT
            + MQC.MQPMO_SET_IDENTITY_CONTEXT
            + MQC.MQPMO_FAIL_IF_QUIESCING;
            mRequestQueue.put(mqMessage, pmo);
            /* Get the unique message ID that was generated by MQ.
             * It will be needed to retrieve the correlated reply. */
            request.setAttachment(mqMessage.messageId);
        } catch (MQException e) {
            throw new RequestException(e);
        }

        _lastUsedTime = System.currentTimeMillis();
        if (_log.isDebugEnabled()) {
            _log.debug("Sent Request:" + request.getID()
                    + " on Connection:" + mConnectionID
                    + ". Message ID:"
                    + HostData.toHexString(mqMessage.messageId));
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
                    + " on Connection:" + mConnectionID
                    + ". Correlation ID:"
                        + HostData.toHexString(request.getAttachment()));
        }

        MQMessage mqMessage = new MQMessage();
        try {
            /* The reply is correlated to the request by means of the
             * MQ message ID that was generated when we sent the
             * request. That ID was attached to the request object. */
            mqMessage.correlationId = request.getAttachment();
            MQGetMessageOptions gmo = new MQGetMessageOptions();
            gmo.matchOptions = MQC.MQMO_MATCH_CORREL_ID;
            gmo.options =  MQC.MQPMO_NO_SYNCPOINT
            + MQC.MQGMO_WAIT
            + MQC.MQPMO_FAIL_IF_QUIESCING;
            gmo.waitInterval = getCicsMQEndpoint().getReceiveTimeout();
            mResponseQueue.get(mqMessage, gmo);
            request.setResponseMessage(createResponseMessage(mqMessage));

            _lastUsedTime = System.currentTimeMillis();
            if (_log.isDebugEnabled()) {
                _log.debug("Received response for Request:" + request.getID()
                        + " on Connection:" + mConnectionID
                        + ". Correlation ID:"
                        + HostData.toHexString(mqMessage.correlationId));
            }

        } catch (MQException e) {
            throw new RequestException(e);
        } catch (HostReceiveException e) {
            throw new RequestException(e);
        }

    }

    /**
     * Creates an MQ request message with appropriate header data.
     * A request is folded as MQ Headers and a binary payload. 
     * @param request request description
     * @return the MQ message
     * @throws RequestException if formatting of mq message fails
     */
    public abstract MQMessage createMQRequestMessage(
            final LegStarRequest request) throws RequestException;

    /**
     * Creates a response message from the MQ reply back.
     * The MQ payload should contain serailization of a header part 
     * followed by any number of data parts.
     * @param mqMessage the MQ response message
     * @return a response message
     * @throws HostReceiveException if response cannot be mapped to a message
     */
    public abstract LegStarMessage createResponseMessage(
            final MQMessage mqMessage) throws HostReceiveException;
    
    /** No-op for WMQ transport.
     * {@inheritDoc} */
    public void commitUOW() throws RequestException {
    }

    /** No-op for WMQ transport.
     * {@inheritDoc} */
    public void keepUOW() throws RequestException {
    }

    /** No-op for WMQ transport.
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

    /**
     * @return the CICS WMQ endpoint
     */
    public CicsMQEndpoint getCicsMQEndpoint() {
        return mCicsMQEndpoint;
    }

    /**
     * Creates an MQ Host configuration based on the WMQ parameters
     * received.
     * @param cicsMQEndpoint the MQ endpoint to set
     */
    public void setCicsMQEndpoint(
            final CicsMQEndpoint cicsMQEndpoint) {
        mCicsMQEndpoint = cicsMQEndpoint;
    }

    /**
     * @return the MQ Manager
     */
    public MQQueueManager getMQManager() {
        return mMQManager;
    }

    /**
     * @return the Request Queue
     */
    public MQQueue getRequestQueue() {
        return mRequestQueue;
    }

    /** {@inheritDoc} */
    public long getConnectTimeout() {
        return getCicsMQEndpoint().getConnectTimeout();
    }

    /** {@inheritDoc} */
    public long getReceiveTimeout() {
        return getCicsMQEndpoint().getReceiveTimeout();
    }

    /** {@inheritDoc} */
    public boolean isOpen() {
        return _isOpen;
    }

    /** {@inheritDoc} */
    public long getLastUsedTime() {
        return _lastUsedTime;
    }
}
