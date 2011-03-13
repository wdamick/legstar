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

import com.legstar.messaging.ConnectionFactory;
import com.legstar.messaging.HostEndpoint;

/**
 * This class represents the parameters that are necessary for a client to
 * successfully connect to CICS over MQ.
 */
public class CicsMQEndpoint extends HostEndpoint {

    /* ----------------------------------------------------------------------- */
    /* Member variables */
    /* ----------------------------------------------------------------------- */
    /** The JNDI initial context factory. */
    private String _initialContextFactory;

    /** The JNDI provider URL. */
    private String _jndiProviderURL;

    /** The JNDI package prefixes for URL factory. */
    private String _jndiUrlPkgPrefixes;

    /** The JNDI additional properties. */
    private Properties _jndiProperties;

    /** The JNDI name of the JMS connection factory. */
    private String _jndiConnectionFactoryName;

    /** The JNDI name of the request queue. */
    private String _jndiRequestQueueName;

    /** The JNDI name of the reply queue. */
    private String _jndiReplyQueueName;

    /** Host MQ Bridge implementation. */
    private HostMQBridgeType _hostMQBridgeType = HostMQBridgeType.LSMSG;

    /* ----------------------------------------------------------------------- */
    /* Default values */
    /* ----------------------------------------------------------------------- */
    /** The default connection factory class. */
    private static final String DEFAULT_CONNECTION_FACTORY_CLASS = "com.legstar.mq.client.CicsMQConnectionFactory";

    /* ----------------------------------------------------------------------- */
    /* Labels */
    /* ----------------------------------------------------------------------- */
    /** Label for JNDI initial context factory. */
    private static final String INITIAL_CONTEXT_FACTORY_LABEL = "initialContextFactory";

    /** Label for JNDI JNDI provider URL. */
    private static final String JNDI_PROVIDER_URL_LABEL = "jndiProviderURL";

    /** Label for JNDI package prefixes for URL factory. */
    private static final String JNDI_URL_PKGP_REFIXES_LABEL = "jndiUrlPkgPrefixes";

    /** Label for additional JNDI properties. */
    private static final String JNDI_PROPERTIES_LABEL = "jndiProperties";

    /** Label for JNDI name of the JMS connection factory. */
    private static final String JNDI_CONNECTION_FACTORY_NAME_LABEL = "jndiConnectionFactoryName";

    /** Label for JNDI name of the request queue. */
    private static final String JNDI_REQUEST_QUEUE_NAME_LABEL = "jndiRequestQueueName";

    /** Label for JNDI name of the reply queue. */
    private static final String JNDI_REPLY_QUEUE_NAME_LABEL = "jndiReplyQueueName";

    /** Label for host MQ bridge type. */
    private static final String HOST_MQ_BRIDGE_TYPE_LABEL = "hostMQBridgeType";

    /**
     * No-arg constructor.
     */
    public CicsMQEndpoint() {
        setHostConnectionfactoryClass(DEFAULT_CONNECTION_FACTORY_CLASS);
    }

    /**
     * Constructor using an existing connection factory.
     * 
     * @param connectionFactory an instance of a connection factory
     */
    public CicsMQEndpoint(final ConnectionFactory connectionFactory) {
        super(connectionFactory);
    }

    /**
     * Copy constructor.
     * 
     * @param copyFrom the endpoint to copy from
     */
    public CicsMQEndpoint(final CicsMQEndpoint copyFrom) {
        super(copyFrom);
        setInitialContextFactory(copyFrom.getInitialContextFactory());
        setJndiProviderURL(copyFrom.getJndiProviderURL());
        setJndiUrlPkgPrefixes(copyFrom.getJndiUrlPkgPrefixes());
        setJndiProperties(copyFrom.getJndiProperties());
        setJndiConnectionFactoryName(copyFrom.getJndiConnectionFactoryName());
        setJndiRequestQueueName(copyFrom.getJndiRequestQueueName());
        setJndiReplyQueueName(copyFrom.getJndiReplyQueueName());
        setHostMQBridgeType(copyFrom.getHostMQBridgeType());
    }

    /**
     * Helper to pretty print the endpoint content.
     * 
     * @return formatted endpoint report
     */
    public String toString() {
        String report = "CICS WMQ endpoint:" + super.toString() + "["
                + INITIAL_CONTEXT_FACTORY_LABEL + "=" + _initialContextFactory
                + "," + JNDI_PROVIDER_URL_LABEL + "=" + _jndiProviderURL + ","
                + JNDI_URL_PKGP_REFIXES_LABEL + "=" + _jndiUrlPkgPrefixes + ","
                + JNDI_PROPERTIES_LABEL + "=" + _jndiProperties + ","
                + JNDI_CONNECTION_FACTORY_NAME_LABEL + "="
                + _jndiConnectionFactoryName + ","
                + JNDI_REQUEST_QUEUE_NAME_LABEL + "=" + _jndiRequestQueueName
                + "," + JNDI_REPLY_QUEUE_NAME_LABEL + "=" + _jndiReplyQueueName
                + "," + HOST_MQ_BRIDGE_TYPE_LABEL + "=" + _hostMQBridgeType
                + "]";
        return report;
    }

    /**
     * Perform a sanity check on the endpoint parameters.
     * 
     * @throws CicsMQConnectionException if check fails
     */
    public void check() throws CicsMQConnectionException {

        if (getInitialContextFactory() == null
                || getInitialContextFactory().length() == 0) {
            throw new CicsMQConnectionException(
                    "No JNDI initial context factory provided.");
        }
        if (getJndiConnectionFactoryName() == null
                || getJndiConnectionFactoryName().length() == 0) {
            throw new CicsMQConnectionException(
                    "No JNDI JMS connection factory name provided.");
        }
        if (getJndiRequestQueueName() == null
                || getJndiRequestQueueName().length() == 0) {
            throw new CicsMQConnectionException(
                    "No JNDI request queue name has been provided.");
        }
        if (getJndiReplyQueueName() == null
                || getJndiReplyQueueName().length() == 0) {
            throw new CicsMQConnectionException(
                    "No JNDI reply queue name has been provided.");
        }

    }

    /**
     * Types of mainframe MQ Bridge implementations supported.
     */
    public enum HostMQBridgeType {
        /**
         * Uses LegStar messaging (expects mainframe to process LegStar
         * messages).
         */
        LSMSG,
        /** Uses IBM CICS MQ Bridge. */
        MQCIH

    }

    /**
     * @return the Host MQ Bridge implementation
     */
    public HostMQBridgeType getHostMQBridgeType() {
        return _hostMQBridgeType;
    }

    /**
     * @param hostMQBridgeType the Host MQ Bridge implementation to set
     */
    public void setHostMQBridgeType(final HostMQBridgeType hostMQBridgeType) {
        _hostMQBridgeType = hostMQBridgeType;
    }

    /**
     * @param hostMQBridgeType the Host MQ Bridge implementation to set
     */
    public void setHostMQBridgeType(final String hostMQBridgeType) {
        _hostMQBridgeType = HostMQBridgeType.valueOf(hostMQBridgeType);
    }

    /**
     * Not supported.
     * 
     * @param maxIdleTime the maximum time to keep a pooled connection opened to
     *            set -1 means forever.
     */
    @Override
    public void setPooledMaxIdleTime(final long maxIdleTime) {
        if (maxIdleTime != DEFAULT_POOLED_MAX_IDLE_TIME) {
            throw new IllegalArgumentException(
                    "pooledMaxIdleTime not supported by this transport");
        }
        super.setPooledMaxIdleTime(maxIdleTime);
    }

    /**
     * Not supported.
     * 
     * @param pooledMaxIdleTimeCheckPeriod the time between checking idle
     *            connections to set
     */
    @Override
    public void setPooledMaxIdleTimeCheckPeriod(
            final long pooledMaxIdleTimeCheckPeriod) {
        if (pooledMaxIdleTimeCheckPeriod != DEFAULT_POOLED_MAX_IDLE_TIME_CHECK_PERIOD) {
            throw new IllegalArgumentException(
                    "pooledMaxIdleTimeCheckPeriod not supported by this transport");
        }
        super.setPooledMaxIdleTimeCheckPeriod(pooledMaxIdleTimeCheckPeriod);
    }

    /**
     * @return the JNDI initial context factory
     */
    public String getInitialContextFactory() {
        return _initialContextFactory;
    }

    /**
     * @param initialContextFactory the JNDI initial context factory to set
     */
    public void setInitialContextFactory(final String initialContextFactory) {
        _initialContextFactory = initialContextFactory;
    }

    /**
     * @return the JNDI name of the JMS connection factory
     */
    public String getJndiConnectionFactoryName() {
        return _jndiConnectionFactoryName;
    }

    /**
     * @param jndiConnectionFactoryName the JNDI name of the JMS connection
     *            factory to set
     */
    public void setJndiConnectionFactoryName(
            final String jndiConnectionFactoryName) {
        this._jndiConnectionFactoryName = jndiConnectionFactoryName;
    }

    /**
     * @return the JNDI provider URL
     */
    public String getJndiProviderURL() {
        return _jndiProviderURL;
    }

    /**
     * @param jndiProviderURL the JNDI provider URL to set
     */
    public void setJndiProviderURL(final String jndiProviderURL) {
        _jndiProviderURL = jndiProviderURL;
    }

    /**
     * @return the JNDI package prefixes for URL factory
     */
    public String getJndiUrlPkgPrefixes() {
        return _jndiUrlPkgPrefixes;
    }

    /**
     * @param jndiUrlPkgPrefixes the JNDI package prefixes for URL factory to
     *            set
     */
    public void setJndiUrlPkgPrefixes(final String jndiUrlPkgPrefixes) {
        _jndiUrlPkgPrefixes = jndiUrlPkgPrefixes;
    }

    /**
     * @return the JNDI additional properties
     */
    public Properties getJndiProperties() {
        return _jndiProperties;
    }

    /**
     * @param jndiProperties the JNDI additional properties to set
     */
    public void setJndiProperties(final Properties jndiProperties) {
        _jndiProperties = jndiProperties;
    }

    /**
     * @return the JNDI name of the request queue
     */
    public String getJndiRequestQueueName() {
        return _jndiRequestQueueName;
    }

    /**
     * @param jndiRequestQueueName the JNDI name of the request queue to set
     */
    public void setJndiRequestQueueName(final String jndiRequestQueueName) {
        _jndiRequestQueueName = jndiRequestQueueName;
    }

    /**
     * @return the JNDI name of the reply queue
     */
    public String getJndiReplyQueueName() {
        return _jndiReplyQueueName;
    }

    /**
     * @param jndiReplyQueueName the JNDI name of the reply queue to set
     */
    public void setJndiReplyQueueName(final String jndiReplyQueueName) {
        this._jndiReplyQueueName = jndiReplyQueueName;
    }
}
