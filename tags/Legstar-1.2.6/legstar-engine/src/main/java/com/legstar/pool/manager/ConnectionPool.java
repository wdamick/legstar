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
package com.legstar.pool.manager;

import java.rmi.server.UID;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.messaging.HostEndpoint;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.RequestException;

/**
 * Implements a pool of available host connections. The objective is to
 * throttle the number of simultaneous connections used to prevent
 * MAXTASK situations on the host. A pool also make it possible to reuse a
 * connection for multiple requests.
 * All requests serviced by a pool must share the same host endpoint
 * and the same user credentials.
 * The connection pool is transport agnostic via an abstract connection
 * factory.
 *
 */
public class ConnectionPool {

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());
    
    /** Target host endpoint associated with this connection pool. */
    private HostEndpoint _hostEndpoint;
    
    /** Target address used to connect. */
    private LegStarAddress _address;

    /** Queue of available host connections. */
    private BlockingStack < LegStarConnection > _connections;

    /** Will be true when shutdown is initiated. */
    private boolean _shuttingDown;
    
    /** How to manage pooled opened connections. */
    private SlidingWindowKeepAlivePolicy _keepAlivePolicy;

    /**
     * Construct a connection pool for an endpoint.
     * The connection pool is organized around a fixed size blocking queue.
     * Upon instantiation, all connections are created but not actually
     * connected to the host. 
     * @param address the target host address
     * @param hostEndpoint the target host endpoint
     * @throws ConnectionPoolException if pool cannot be created
     */
    public ConnectionPool(
            final LegStarAddress address,
            final HostEndpoint hostEndpoint)
    throws ConnectionPoolException {

        _address = address;
        _hostEndpoint = hostEndpoint;
        
        int poolSize = hostEndpoint.getHostConnectionPoolSize();
        
        /* Create the blocking queue */
        _connections = new BlockingStack < LegStarConnection >(poolSize);

        /* Create all connections with a unique ID each. This ID is used
         * for traceability. Because */
        try {
            for (int i = 0; i < poolSize; i++) {
                _connections.add(
                        hostEndpoint.getHostConnectionfactory().createConnection(
                                new UID().toString(), address, hostEndpoint));
            }
        } catch (ConnectionException e) {
            throw new ConnectionPoolException(e);
        }
        _keepAlivePolicy = new SlidingWindowKeepAlivePolicy(
                _connections, hostEndpoint.getPooledMaxKeepAlive());

        _shuttingDown = false;
        _log.info("Pool of size " + poolSize + ", created for endpoint: " + hostEndpoint.toString());
    }

    /**
     * Every time a client needs a connection, he will use this method to
     * checkout a connection which physically removes it from the queue.
     * This request will block if no connections are available from the pool.
     * @param timeout maximum time (in milliseconds) to wait for a connection
     * @return a pooled connection
     * @throws ConnectionPoolException if no pooled connection can be obtained
     */
    public final LegStarConnection take(
            final long timeout) throws ConnectionPoolException {
        LegStarConnection connection = null;
        if (!_shuttingDown) {
            try {
                connection = _connections.poll(timeout, TimeUnit.MILLISECONDS);
                if (connection == null) {
                    throw new ConnectionPoolException(
                    "Timed out waiting for pooled connection.");
                }
                return connection;
            } catch (InterruptedException e) {
                throw new ConnectionPoolException(e);
            }
        } else {
            throw new ConnectionPoolException("Pool is shutting down.");
        }
    }

    /**
     * This method is used to recycle connections into the pool. Normally, only
     * connections which were previously taken from the pool should be returned
     * into it.
     * @param connection the connection to recycle
     * @throws ConnectionPoolException if pooled connection cannot be recycled
     */
    public final void put(
            final LegStarConnection connection) throws ConnectionPoolException {
        if (!_shuttingDown) {
            try {
                _connections.add(connection);
                _keepAlivePolicy.closeObsoleteConnections();
            } catch (IllegalStateException e) {
                /* If we fail to return the connection to the pool that should 
                 * not prevent further processing but the pool capacity is 
                 * actually reduced by one. We should warn the administrator. */
                _log.warn("Connection could not be recycled.");
            }
        } else {
            throw new ConnectionPoolException("Pool is shutting down.");
        }
    }
    
    /**
     * On an idle system, the connection pool should be full at shutdown time.
     * If it is not, then it means some connections are being used. If it is
     * empty, there might even be requests blocking on the <code>take</code>
     * method.
     */
    public final void shutDown() {
        _log.info("Shutting down Pool " + getHostEndpoint().getName());
        _shuttingDown = true;
        if (_connections.remainingCapacity() > 0) {
            if (_connections.size() == 0) {
                _log.warn("Some requests might be waiting for connections.");
            } else {
                _log.warn("There are "
                        + (_connections.remainingCapacity() - _connections.size())
                        + " connections in use.");
            }
        }
        for (LegStarConnection connection : getConnections()) {
            try {
                connection.close();
            } catch (RequestException e) {
                _log.warn("Unable to close a connection " + e.toString());
            }
        }
    }

    /**
     * Retrieve the current set of connections.
     * @return a list of available connections in the pool
     */
    public final List < LegStarConnection > getConnections() {
        return _connections.getElementsList();
    }

    /**
     * @return the target host endpoint associated with this connection pool
     */
    public HostEndpoint getHostEndpoint() {
        return _hostEndpoint;
    }

    /**
     * @return the target address associated with this connection pool
     */
    public LegStarAddress getAddress() {
        return _address;
    }

}
