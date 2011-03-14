/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.mq.client;

import java.util.Properties;

import javax.jms.BytesMessage;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.QueueConnection;
import javax.jms.QueueConnectionFactory;
import javax.jms.QueueSession;
import javax.jms.Session;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.HostReceiveException;
import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;

/**
 * Client side JMS/MQ connectivity. This class provides the common methods to
 * connect to a mainframe over JMS/MQ, send requests, receive results, etc...
 * 
 */
public abstract class AbstractCicsMQ implements LegStarConnection {

    /** An identifier for this connection. */
    private String _connectionID;

    /** Host CICS WMQ endpoint. */
    private CicsMQEndpoint _cicsMQEndpoint;

    /** The JNDI context. */
    private Context _jndiContext;

    /** The active JMS connection. */
    private QueueConnection _jmsConnection;

    /** The active JMS session. */
    private QueueSession _jmsQueueSession;

    /** The request queue. */
    private Destination _jmsRequestQueue;

    /** The reply queue. */
    private Destination _jmsReplyQueue;

    /** true if connection opened. */
    private boolean _isOpen;

    /** last time this connection was used. */
    private long _lastUsedTime = -1;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * A CicsMQ instance exists for a target MQ endpoint.
     * <p/>
     * An MQ endpoint is a set of JNDI parameters that actually point to the
     * target MQ system. We don't reference MQ directly. This decouples this
     * class from the MQ API per se.
     * 
     * @param connectionID an identifier for this connection
     * @param cicsMQEndpoint MQ endpoint
     * @throws CicsMQConnectionException if instantiation fails
     */
    public AbstractCicsMQ(final String connectionID,
            final CicsMQEndpoint cicsMQEndpoint)
            throws CicsMQConnectionException {
        _connectionID = connectionID;
        _cicsMQEndpoint = cicsMQEndpoint;
        _jndiContext = createJndiContext(cicsMQEndpoint);
    }

    /**
     * Given the endpoint parameters, setup a JNDI context to lookup JMS
     * resources.
     * 
     * @param cicsMQEndpoint the endpoint paramers
     * @return the JNDI context
     * @throws CicsMQConnectionException if JNDI context cannot be created
     */
    protected Context createJndiContext(final CicsMQEndpoint cicsMQEndpoint)
            throws CicsMQConnectionException {
        try {
            Properties env = new Properties();
            env.put(Context.INITIAL_CONTEXT_FACTORY,
                    cicsMQEndpoint.getInitialContextFactory());
            if (cicsMQEndpoint.getJndiProviderURL() != null
                    && cicsMQEndpoint.getJndiProviderURL().length() > 0) {
                env.put(Context.PROVIDER_URL,
                        cicsMQEndpoint.getJndiProviderURL());
            }
            if (cicsMQEndpoint.getJndiUrlPkgPrefixes() != null
                    && cicsMQEndpoint.getJndiUrlPkgPrefixes().length() > 0) {
                env.put(Context.URL_PKG_PREFIXES,
                        cicsMQEndpoint.getJndiUrlPkgPrefixes());
            }
            if (cicsMQEndpoint.getJndiProperties() != null) {
                env.putAll(getProperties(cicsMQEndpoint.getJndiProperties()));
            }
            return new InitialContext(env);
        } catch (NamingException e) {
            throw new CicsMQConnectionException(e);
        }
    }

    /**
     * Connect to a IBM MQ Manager passing credentials.
     * 
     * @param mqPassword credentials for security exits
     * @throws ConnectionException if connection fails
     */
    public void connect(final String mqPassword) throws ConnectionException {

        if (_log.isDebugEnabled()) {
            _log.debug("Connection:" + _connectionID
                    + " Attempting connection. Host:"
                    + getCicsMQEndpoint().toString());
        }

        /* If a password is not passed, use the one from configuration */
        String password;
        if (mqPassword == null || mqPassword.length() == 0) {
            password = getCicsMQEndpoint().getHostPassword();
        } else {
            password = mqPassword;
        }
        _jmsConnection = createQueueConnection(getCicsMQEndpoint()
                .getHostUserID(), password);
        _jmsQueueSession = createQueueSession();
        _jmsRequestQueue = createRequestQueue();
        _jmsReplyQueue = createReplyQueue();

        _isOpen = true;
        _lastUsedTime = System.currentTimeMillis();
        if (_log.isDebugEnabled()) {
            _log.debug("Connection:" + _connectionID + " Connected.");
        }
    }

    /**
     * Create a JMS queue connection.
     * <p/>
     * Starts the connection's delivery of incoming messages. (Messages will not
     * be received without this call).
     * 
     * @param userId for authentication
     * @param password for authentication
     * @return the new JMS queue connection
     * @throws CicsMQConnectionException if JMS queue connection cannot be
     *             created
     */
    protected QueueConnection createQueueConnection(final String userId,
            final String password) throws CicsMQConnectionException {

        if (_log.isDebugEnabled()) {
            _log.debug("enter createQueueConnection()");
        }
        try {
            QueueConnectionFactory factory = (QueueConnectionFactory) getJndiContext()
                    .lookup(getCicsMQEndpoint().getJndiConnectionFactoryName());
            if (factory == null) {
                throw new CicsMQConnectionException("JNDI lookup for "
                        + getCicsMQEndpoint().getJndiConnectionFactoryName()
                        + " failed");
            }
            QueueConnection connection = factory.createQueueConnection(userId,
                    password);
            connection.start();
            return connection;
        } catch (NamingException e) {
            throw new CicsMQConnectionException(e);
        } catch (JMSException e) {
            throw new CicsMQConnectionException(e);
        }
    }

    /**
     * Create a new JMS session.
     * <p/>
     * There is no support for transactions.
     * 
     * @return new JMS session
     * @throws CicsMQConnectionException if session cannot be created
     */
    protected QueueSession createQueueSession()
            throws CicsMQConnectionException {
        try {
            boolean transacted = false;
            QueueSession session = getJmsConnection().createQueueSession(
                    transacted, Session.AUTO_ACKNOWLEDGE);
            return session;
        } catch (JMSException e) {
            throw new CicsMQConnectionException(e);
        }
    }

    /**
     * Creates a queue for the request message.
     * 
     * @return the request queue
     * @throws CicsMQConnectionException if queue cannot be created
     */
    protected Destination createRequestQueue() throws CicsMQConnectionException {
        return createQueue(getCicsMQEndpoint().getJndiRequestQueueName());
    }

    /**
     * Creates a queue for the reply message.
     * 
     * @return the reply queue
     * @throws CicsMQConnectionException if queue cannot be created
     */
    protected Destination createReplyQueue() throws CicsMQConnectionException {
        return createQueue(getCicsMQEndpoint().getJndiReplyQueueName());
    }

    /**
     * Lookup a queue from JNDI.
     * 
     * @return the request queue
     * @throws CicsMQConnectionException if queue cannot be created
     */
    protected Destination createQueue(final String queueJndiName)
            throws CicsMQConnectionException {

        if (_log.isDebugEnabled()) {
            _log.debug("enter createQueue() for  " + queueJndiName);
        }

        try {
            Destination queue = (Destination) getJndiContext().lookup(
                    queueJndiName);
            if (queue == null) {
                throw new CicsMQConnectionException("JNDI lookup for "
                        + queueJndiName + " failed");
            }
            return queue;
        } catch (NamingException e) {
            throw new CicsMQConnectionException(e);
        }
    }

    /**
     * Terminates a connection with the host.
     * 
     * @throws RequestException if a failure is detected
     */
    public void close() throws RequestException {
        if (_log.isDebugEnabled()) {
            _log.debug("Connection:" + _connectionID + " closing.");
        }
        if (_jmsQueueSession != null) {
            try {
                _jmsQueueSession.close();
            } catch (JMSException e) {
                _log.error(e);
            }
        }
        if (_jmsConnection != null) {
            try {
                _jmsConnection.close();
            } catch (JMSException e) {
                _log.error(e);
            }
        }
        _jmsRequestQueue = null;
        _jmsReplyQueue = null;
        _jmsQueueSession = null;
        _jmsConnection = null;
        _isOpen = false;
        _lastUsedTime = System.currentTimeMillis();
    }

    /**
     * This method simply checks that a session is already opened.
     * 
     * @param mqPassword host password if it is not stored in configuration file
     * @throws ConnectionException if connection fails
     * */
    public void connectReuse(final String mqPassword)
            throws ConnectionException {
        if (_log.isDebugEnabled()) {
            _log.debug("Connection:" + _connectionID + " Attempting reuse.");
        }

        if (isOpen()) {
            if (_log.isDebugEnabled()) {
                _log.debug("Connection:" + _connectionID
                        + " Connection will be reused.");
            }
            _lastUsedTime = System.currentTimeMillis();
            return;
        }

        if (_log.isDebugEnabled()) {
            _log.debug("Connection:" + _connectionID
                    + " Connection not reusable.");
        }
        /* Socket is not reusable, fallback to standard connect. */
        connect(mqPassword);
    }

    /**
     * Creates and send a JMS message to the mainframe.
     * <p/>
     * Reply to queue name is where we expect the reply. We expect it to be
     * managed by the same mq manager as the request queue.
     * <p/>
     * Save the unique message ID that was generated by JMS. It will be needed
     * to retrieve the correlated reply.
     * 
     * @param request the request to be sent
     * @throws RequestException if send fails
     */
    public void sendRequest(final LegStarRequest request)
            throws RequestException {

        MessageProducer producer = null;
        try {
            if (_log.isDebugEnabled()) {
                _log.debug("Sending Request:"
                        + request.getID()
                        + " on Connection:"
                        + _connectionID
                        + " "
                        + request.getRequestMessage().getHeaderPart()
                                .getJsonString() + '.');
            }

            Message jmsMessage = createRequestMessage(request);
            jmsMessage.setJMSReplyTo(getJmsReplyQueue());
            producer = getJmsQueueSession()
                    .createProducer(getJmsRequestQueue());
            producer.send(jmsMessage);

            request.setAttachment(jmsMessage.getJMSMessageID().getBytes());

            _lastUsedTime = System.currentTimeMillis();
            if (_log.isDebugEnabled()) {
                _log.debug("Sent Request:" + request.getID()
                        + " on Connection:" + _connectionID + ". Message ID:"
                        + jmsMessage.getJMSMessageID());
            }
        } catch (HeaderPartException e) {
            throw new RequestException(e);
        } catch (JMSException e) {
            throw new RequestException(e);
        } finally {
            if (producer != null) {
                try {
                    producer.close();
                } catch (JMSException e) {
                    _log.error(e);
                }
            }
        }
    }

    /**
     * A response is serialized as a header message part followed by data
     * message parts. This method creates a response message for the request.
     * <p/>
     * The reply is correlated to the request by means of the JMS message ID
     * that was generated when we sent the request. That ID was attached to the
     * request object.
     * 
     * @param request the request being serviced
     * @throws RequestException if receive fails
     */
    public void recvResponse(final LegStarRequest request)
            throws RequestException {

        MessageConsumer consumer = null;
        try {
            String selector = "JMSCorrelationID='"
                    + new String(request.getAttachment()) + "'";
            if (_log.isDebugEnabled()) {
                _log.debug("Receiving response for Request:" + request.getID()
                        + " on Connection:" + _connectionID + ". Selector: "
                        + selector);
            }

            consumer = getJmsQueueSession().createConsumer(getJmsReplyQueue(),
                    selector);
            Message jmsMessage = consumer.receive(getCicsMQEndpoint()
                    .getReceiveTimeout());
            if (!(jmsMessage instanceof BytesMessage)) {
                throw new RequestException(
                        "Message received does not contain bytes");
            }
            BytesMessage message = (BytesMessage) jmsMessage;
            message.reset();

            /* Check that data length makes sense */
            long dataLength = message.getBodyLength();
            if (dataLength > Integer.MAX_VALUE) {
                throw new RequestException(
                        "Size of BytesMessage exceeds Integer.MAX_VALUE");
            }

            request.setResponseMessage(createReplyMessage(message,
                    (int) dataLength));

            _lastUsedTime = System.currentTimeMillis();
            if (_log.isDebugEnabled()) {
                _log.debug("Received response for Request:" + request.getID()
                        + " on Connection:" + _connectionID + ". Selector: "
                        + selector);
            }
        } catch (JMSException e) {
            throw new RequestException(e);
        } catch (HostReceiveException e) {
            throw new RequestException(e);
        } finally {
            if (consumer != null) {
                try {
                    consumer.close();
                } catch (JMSException e) {
                    _log.error(e);
                }
            }
        }

    }

    /**
     * Creates a JMS request message with appropriate header data. A request is
     * folded as JMS Headers and a binary payload.
     * 
     * @param request request description
     * @return the JMS message
     * @throws RequestException if formatting of JMS message fails
     */
    public abstract Message createRequestMessage(final LegStarRequest request)
            throws RequestException;

    /**
     * Creates a response message from the JMS reply. The JMS payload should
     * contain serialization of a header part followed by any number of data
     * parts.
     * 
     * @param jmsMessage the JMS response message
     * @param dataLength the data length
     * @return a response message
     * @throws HostReceiveException if response cannot be mapped to a message
     */
    public abstract LegStarMessage createReplyMessage(
            final BytesMessage jmsMessage, final int dataLength)
            throws HostReceiveException;

    /**
     * No-op for WMQ transport. {@inheritDoc}
     */
    public void commitUOW() throws RequestException {
    }

    /**
     * No-op for WMQ transport. {@inheritDoc}
     */
    public void keepUOW() throws RequestException {
    }

    /**
     * No-op for WMQ transport. {@inheritDoc}
     */
    public void rollbackUOW() throws RequestException {
    }

    /**
     * @return the identifier for this connection
     */
    public String getConnectionID() {
        return _connectionID;
    }

    /**
     * @return the CICS WMQ endpoint
     */
    public CicsMQEndpoint getCicsMQEndpoint() {
        return _cicsMQEndpoint;
    }

    /**
     * Return a hostUserID from request or, if none provided with the request,
     * from the endpoint parameter set.
     * 
     * @param request the request
     * @return a host user ID
     */
    public String getHostUserID(final LegStarRequest request) {
        String hostUserID = request.getAddress().getHostUserID();
        if (hostUserID == null) {
            return getCicsMQEndpoint().getHostUserID();
        } else {
            return hostUserID;
        }
    }

    /**
     * Return a hostCharset from request or, if none provided with the request,
     * from the endpoint parameter set.
     * 
     * @param request the request
     * @return a host user ID
     */
    public String getHostCharset(final LegStarRequest request) {
        String hostCharset = request.getAddress().getHostCharset();
        if (hostCharset == null) {
            return getCicsMQEndpoint().getHostCharset();
        } else {
            return hostCharset;
        }
    }

    /**
     * Takes a string serialized list of key value pairs and turns it into a
     * Properties object.
     * 
     * @param properties a comma separated list of key=value pairs
     * @return a Properties object
     */
    public Properties getProperties(final String properties) {
        Properties result = new Properties();
        String[] keyValuePairs = properties.split(",");
        for (String keyValuePair : keyValuePairs) {
            String[] entry = keyValuePair.split("=");
            if (entry.length == 2) {
                result.put(entry[0], entry[1]);
            }
        }
        return result;
    }

    /**
     * Return a hostPassword from request or, if none provided with the request,
     * from the endpoint parameter set.
     * 
     * @param request the request
     * @return a host user ID
     */
    public String getHostPassword(final LegStarRequest request) {
        String hostPassword = request.getAddress().getHostPassword();
        if (hostPassword == null) {
            return getCicsMQEndpoint().getHostPassword();
        } else {
            return hostPassword;
        }
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

    /**
     * @return the JNDI context
     */
    public Context getJndiContext() {
        return _jndiContext;
    }

    /**
     * @return the active JMS connection
     */
    public QueueConnection getJmsConnection() {
        return _jmsConnection;
    }

    /**
     * @return the active JMS session
     */
    public QueueSession getJmsQueueSession() {
        return _jmsQueueSession;
    }

    /**
     * @return the request queue
     */
    public Destination getJmsRequestQueue() {
        return _jmsRequestQueue;
    }

    /**
     * @return the reply queue
     */
    public Destination getJmsReplyQueue() {
        return _jmsReplyQueue;
    }

}
