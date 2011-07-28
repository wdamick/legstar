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
package com.legstar.messaging;

import com.legstar.coxb.host.HostContext;
import com.legstar.coxb.util.ClassLoadingException;
import com.legstar.coxb.util.ClassUtil;

/**
 * A host endpoint is used by adapters to forward execution requests to
 * a target host.
 * <p/>
 * Each transport refines such an endpoint to add transport specific parameters.
 * 
 */
public abstract class HostEndpoint {

    /* ----------------------------------------------------------------------- */
    /* Member variables */
    /* ----------------------------------------------------------------------- */
    /** Endpoint name and identifier. */
    private String _name;

    /** Host charset. */
    private String _hostCharset = HostContext.getDefaultHostCharsetName();

    /** Host user ID. */
    private String _hostUserID;

    /** Host password. */
    private String _hostPassword;

    /** Host trace mode. */
    private boolean _hostTraceMode;

    /** Maximum time to wait for connection. */
    private int _connectTimeout = DEFAULT_CONNECT_TIMEOUT_MSEC;

    /** Maximum time to wait for a reply to a request. */
    private int _receiveTimeout = DEFAULT_RECEIVE_TIMEOUT_MSEC;

    /** The name of a class capable of creating connections to this endpoint. */
    private String _hostConnectionfactoryClass;

    /** An instance of the connection factory. */
    private ConnectionFactory _hostConnectionfactory;

    /** The access strategy (direct or pooled). */
    private AccessStrategy _hostAccessStrategy = DEFAULT_HOST_ACCESS_STRATEGY;

    /** The connection pool maximum size (for pooled strategy). */
    private int _hostConnectionPoolSize = DEFAULT_POOL_SIZE;

    /** Maximum time to wait for an available pooled connection. */
    private int _pooledInvokeTimeout = DEFAULT_POOLED_INVOKE_TIMEOUT_MSEC;

    /** Maximum time to keep an idle pooled connection opened. */
    private long _pooledMaxIdleTime = DEFAULT_POOLED_MAX_IDLE_TIME;

    /** Time between checking idle connections for maximum idle time. */
    private long _pooledMaxIdleTimeCheckPeriod = DEFAULT_POOLED_MAX_IDLE_TIME_CHECK_PERIOD;

    /* ----------------------------------------------------------------------- */
    /* Default values */
    /* ----------------------------------------------------------------------- */
    /** Time out (in milliseconds) for initial connect. */
    public static final int DEFAULT_CONNECT_TIMEOUT_MSEC = 1000;

    /**
     * Time out (in milliseconds) for read operations
     * (waiting for host reply).
     */
    public static final int DEFAULT_RECEIVE_TIMEOUT_MSEC = 5000;

    /** Time out (in milliseconds) for invoke. */
    public static final int DEFAULT_POOLED_INVOKE_TIMEOUT_MSEC = 3000;

    /** Default maximum time to keep a pooled connection opened. */
    public static final long DEFAULT_POOLED_MAX_IDLE_TIME = -1;

    /** Default time between checking idle connections. */
    public static final long DEFAULT_POOLED_MAX_IDLE_TIME_CHECK_PERIOD = -1;

    /** If no pool size found in configuration, use this default. */
    public static final int DEFAULT_POOL_SIZE = 5;

    /** If not configured, we will use a direct strategy. */
    public static final AccessStrategy DEFAULT_HOST_ACCESS_STRATEGY =
            AccessStrategy.direct;

    /* ----------------------------------------------------------------------- */
    /* Labels */
    /* ----------------------------------------------------------------------- */
    /** Label for host endpoint name. */
    public static final String HOST_ENDPOINT_LABEL = "hostEndpoint";

    /** Label for host charset. */
    public static final String HOST_CHARSET_LABEL = "hostCharset";

    /** Label for host user ID. */
    public static final String HOST_USERID_LABEL = "hostUserID";

    /** Label for host password. */
    public static final String HOST_PASSWORD_LABEL = "hostPassword";

    /** Label for host trace mode. */
    public static final String HOST_TRACE_LABEL = "hostTraceMode";

    /** Label for connect timeout. */
    public static final String CONNECT_TIMEOUT_LABEL = "connectTimeout";

    /** Label for receive timeout. */
    public static final String RECEIVE_TIMEOUT_LABEL = "receiveTimeout";

    /** Label for host access strategy. */
    public static final String HOST_ACCESS_STRATEGY_LABEL = "hostAccessStrategy";

    /** Label for host access strategy. */
    public static final String HOST_CONNECTION_FACTORY_CLASS_LABEL = "hostConnectionfactoryClass";

    /** Label for connection pool size. */
    public static final String HOST_CONNECTION_POOL_SIZE_LABEL = "hostConnectionPoolSize";

    /** Label for pooled invoke timeout. */
    public static final String POOLED_INVOKE_TIMEOUT_LABEL = "pooledInvokeTimeout";

    /** Label for maximum time to keep an idle pooled connection opened. */
    public static final String POOLED_MAX_IDLE_TIME_LABEL = "pooledMaxIdleTime";

    /** Label for time between checking idle connections. */
    public static final String POOLED_MAX_IDLE_TIME_CHECK_PERIOD_LABEL = "pooledMaxIdleTimeCheckPeriod";

    /**
     * No-arg constructor.
     */
    public HostEndpoint() {

    }

    /**
     * Constructor using an existing connection factory.
     * 
     * @param connectionFactory an instance of a connection factory
     */
    public HostEndpoint(final ConnectionFactory connectionFactory) {
        _hostConnectionfactory = connectionFactory;
    }

    /**
     * Copy constructor.
     * 
     * @param copyFrom the endpoint to copy from
     */
    public HostEndpoint(final HostEndpoint copyFrom) {
        setConnectTimeout(copyFrom.getConnectTimeout());
        setHostAccessStrategy(copyFrom.getHostAccessStrategy());
        setHostCharset(copyFrom.getHostCharset());
        setHostConnectionfactory(copyFrom.getHostConnectionfactory());
        setHostConnectionPoolSize(copyFrom.getHostConnectionPoolSize());
        setHostConnectionfactoryClass(copyFrom.getHostConnectionfactoryClass());
        setHostPassword(copyFrom.getHostPassword());
        setHostTraceMode(copyFrom.isHostTraceMode());
        setHostUserID(copyFrom.getHostUserID());
        setName(copyFrom.getName());
        setPooledInvokeTimeout(copyFrom.getPooledInvokeTimeout());
        setPooledMaxIdleTime(copyFrom.getPooledMaxIdleTime());
        setPooledMaxIdleTimeCheckPeriod(copyFrom
                .getPooledMaxIdleTimeCheckPeriod());
        setReceiveTimeout(copyFrom.getReceiveTimeout());
    }

    /**
     * Helper to pretty print the endpoint content.
     * 
     * @return formatted endpoint report
     */
    public String toString() {
        String report = "["
                + HOST_ENDPOINT_LABEL + "=" + _name
                + "," + HOST_CHARSET_LABEL + "=" + _hostCharset
                + "," + HOST_USERID_LABEL + "=" + _hostUserID
                + "," + HOST_PASSWORD_LABEL + "=" + "********"
                + "," + HOST_TRACE_LABEL + "=" + _hostTraceMode
                + "," + CONNECT_TIMEOUT_LABEL + "=" + _connectTimeout
                + "," + RECEIVE_TIMEOUT_LABEL + "=" + _receiveTimeout
                + "," + HOST_CONNECTION_FACTORY_CLASS_LABEL + "="
                + _hostConnectionfactoryClass
                + "," + HOST_ACCESS_STRATEGY_LABEL + "=" + _hostAccessStrategy
                + "," + HOST_CONNECTION_POOL_SIZE_LABEL + "="
                + _hostConnectionPoolSize
                + "," + POOLED_INVOKE_TIMEOUT_LABEL + "="
                + _pooledInvokeTimeout
                + "," + POOLED_MAX_IDLE_TIME_LABEL + "="
                + _pooledMaxIdleTime
                + "," + POOLED_MAX_IDLE_TIME_CHECK_PERIOD_LABEL + "="
                + _pooledMaxIdleTimeCheckPeriod

                + "]";
        return report;
    }

    /**
     * Perform a sanity check on all parameters.
     * 
     * @throws ConnectionException if endpoint parameters are inconsistent
     */
    protected void checkAll() throws ConnectionException {
        if (getHostCharset() == null || getHostCharset().length() == 0) {
            throw new ConnectionException(
                    "No host character set has been provided.");
        }
        /* This allows transport endpoints to add their own checking */
        check();
    }

    /**
     * Perform a sanity check on all parameters.
     * 
     * @throws ConnectionException if endpoint parameters are inconsistent
     */
    public abstract void check() throws ConnectionException;

    /**
     * Enrich this endpoint parameters with data from address.
     * <p/>
     * This allows data such as user/password or trace mode to be dynamically
     * setup using an address.
     * 
     * @param address the origin address
     */
    public void enrich(final LegStarAddress address) {
        if (address.getHostUserID() != null
                && address.getHostUserID().length() > 0) {
            setHostUserID(address.getHostUserID());
        }
        if (address.getHostPassword() != null
                && address.getHostPassword().length() > 0) {
            setHostPassword(address.getHostPassword());
        }
        if (address.getHostCharset() != null
                && address.getHostCharset().length() > 0) {
            setHostCharset(address.getHostCharset());
        }
        if (address.isHostTraceMode()) {
            setHostTraceMode(true);
        }
    }

    /**
     * The host access strategy.
     */
    public enum AccessStrategy {
        /** Direct. */
        direct,
        /** Pooled (connection reuse). */
        pooled
    }

    /**
     * @return the endpoint name and identifier
     */
    public String getName() {
        return _name;
    }

    /**
     * @param name the endpoint name and identifier to set
     */
    public void setName(final String name) {
        this._name = name;
    }

    /**
     * @return the host character set
     */
    public String getHostCharset() {
        return _hostCharset;
    }

    /**
     * @param hostCharset the host character set to set
     */
    public void setHostCharset(final String hostCharset) {
        _hostCharset = hostCharset;
    }

    /**
     * @return the host user ID
     */
    public String getHostUserID() {
        return _hostUserID;
    }

    /**
     * @param hostUserID the host user ID to set
     */
    public void setHostUserID(final String hostUserID) {
        _hostUserID = hostUserID;
    }

    /**
     * @return the host password
     */
    public String getHostPassword() {
        return _hostPassword;
    }

    /**
     * @param hostPassword the host password to set
     */
    public void setHostPassword(final String hostPassword) {
        _hostPassword = hostPassword;
    }

    /**
     * @return the host trace mode enabled or or
     */
    public boolean isHostTraceMode() {
        return _hostTraceMode;
    }

    /**
     * @param hostTraceMode the host trace mode to set
     */
    public void setHostTraceMode(final boolean hostTraceMode) {
        _hostTraceMode = hostTraceMode;
    }

    /**
     * @return the name of a class capable of creating connections to this
     *         endpoint
     */
    public String getHostConnectionfactoryClass() {
        return _hostConnectionfactoryClass;
    }

    /**
     * @param connectionfactoryClass the name of a class capable of creating
     *            connections to this endpoint to set
     */
    public void setHostConnectionfactoryClass(
            final String connectionfactoryClass) {
        _hostConnectionfactoryClass = connectionfactoryClass;
    }

    /**
     * @return the connection pool maximum size (for pooled strategy)
     */
    public int getHostConnectionPoolSize() {
        return _hostConnectionPoolSize;
    }

    /**
     * @param connectionPoolSize the connection pool maximum size (for pooled
     *            strategy) to set
     */
    public void setHostConnectionPoolSize(final int connectionPoolSize) {
        _hostConnectionPoolSize = connectionPoolSize;
    }

    /**
     * @return the access strategy (direct or pooled)
     */
    public AccessStrategy getHostAccessStrategy() {
        return _hostAccessStrategy;
    }

    /**
     * @param accessStrategy the access strategy (direct or pooled) to set
     */
    public void setHostAccessStrategy(final AccessStrategy accessStrategy) {
        _hostAccessStrategy = accessStrategy;
    }

    /**
     * @return the maximum time to wait for an available pooled connection
     */
    public int getPooledInvokeTimeout() {
        return _pooledInvokeTimeout;
    }

    /**
     * @param invokeTimeout the maximum time to wait for an available pooled
     *            connection to set
     */
    public void setPooledInvokeTimeout(final int invokeTimeout) {
        _pooledInvokeTimeout = invokeTimeout;
    }

    /**
     * @return the maximum time to wait for connection
     */
    public int getConnectTimeout() {
        return _connectTimeout;
    }

    /**
     * @param timeout the maximum time to wait for connection to set
     */
    public void setConnectTimeout(final int timeout) {
        _connectTimeout = timeout;
    }

    /**
     * @return the maximum time to wait for a reply to a request
     */
    public int getReceiveTimeout() {
        return _receiveTimeout;
    }

    /**
     * @param timeout the maximum time to wait for a reply to a request to set
     */
    public void setReceiveTimeout(final int timeout) {
        _receiveTimeout = timeout;
    }

    /**
     * When there is no instance of the connection factory, this will try
     * to instantiate one and throws a runtime exception if that fails.
     * 
     * @return the instance of the connection factory
     */
    public ConnectionFactory getHostConnectionfactory() {
        if (_hostConnectionfactory == null) {
            if (getHostConnectionfactoryClass() != null) {
                try {
                    _hostConnectionfactory = (ConnectionFactory) ClassUtil
                            .newObject(getHostConnectionfactoryClass());
                } catch (ClassLoadingException e) {
                    throw new IllegalStateException(e);
                }
            } else {
                throw new IllegalStateException(
                        "Host endpoint has no connection factory class name");
            }
        }
        return _hostConnectionfactory;
    }

    /**
     * @param connectionfactory the instance of the connection factory to set
     */
    public void setHostConnectionfactory(
            final ConnectionFactory connectionfactory) {
        _hostConnectionfactory = connectionfactory;
    }

    /**
     * @return the maximum time to keep a pooled connection opened. -1 means
     *         forever.
     */
    public long getPooledMaxIdleTime() {
        return _pooledMaxIdleTime;
    }

    /**
     * @param maxIdleTime the maximum time to keep a pooled connection opened
     *            to set -1 means forever.
     */
    public void setPooledMaxIdleTime(final long maxIdleTime) {
        _pooledMaxIdleTime = maxIdleTime;
    }

    /**
     * Time between checking idle connections. Setting to -1 or 0 means never
     * schedule tests for idle connections. If strictly positive, then
     * a process will start every this number of milliseconds to close
     * connections that have been idle for more than
     * {@link getPooledMaxKeepAlive}.
     * 
     * @return the time between checking idle connections
     */
    public long getPooledMaxIdleTimeCheckPeriod() {
        return _pooledMaxIdleTimeCheckPeriod;
    }

    /**
     * Time between checking idle connections.
     * 
     * @param pooledMaxIdleTimeCheckPeriod the time between checking idle
     *            connections to set
     */
    public void setPooledMaxIdleTimeCheckPeriod(
            final long pooledMaxIdleTimeCheckPeriod) {
        _pooledMaxIdleTimeCheckPeriod = pooledMaxIdleTimeCheckPeriod;
    }

}
