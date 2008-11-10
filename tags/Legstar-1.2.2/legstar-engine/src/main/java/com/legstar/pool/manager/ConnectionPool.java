/*******************************************************************************
 * Copyright (c) 2008 LegSem.
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
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.TimeUnit;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.ConnectionFactory;

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
	private static final Log LOG = LogFactory.getLog(ConnectionPool.class);
	
	/** The connection factory is dynamically loaded. */
	private ConnectionFactory mConnectionFactory;
	
	/** Queue of available host connections. */
	private ArrayBlockingQueue < LegStarConnection > mConnections;
	
	/** Will be true when shutdown is initiated. */
	private boolean mShuttingDown;
	
	/** The host address for this connection pool. */
	private LegStarAddress mAddress;
	
	/**
	 * Construct a connection pool for an endpoint.
	 * The connection pool is organized around a fixed size blocking queue.
	 * Upon instanciation, all connections are created but not actually
	 * connected to the host. 
	 * @param poolSize the size of the blocking queue
	 * @param address the target host address
	 * @param connectionFactory a connection factory 
	 * @throws ConnectionPoolException if pool cannot be created
	 */
	public ConnectionPool(
			final int poolSize,
			final LegStarAddress address,
			final ConnectionFactory connectionFactory)
			throws ConnectionPoolException {

		mAddress = address;
		mConnectionFactory = connectionFactory;
		
		/* Create the blocking queue */
		mConnections = new ArrayBlockingQueue < LegStarConnection >(poolSize);
		
		/* Create all connections with a unique ID each. This ID is used
		 * for traceability. Because */
		try {
			for (int i = 0; i < poolSize; i++) {
				mConnections.add(mConnectionFactory.createConnection(
						new UID().toString(), address));
			}
		} catch (ConnectionException e) {
			throw new ConnectionPoolException(e);
		}
		mShuttingDown = false;
		if (LOG.isDebugEnabled()) {
			LOG.debug("Pool created for:" + address.getReport());
		}
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
		if (!mShuttingDown) {
			try {
				connection = mConnections.poll(timeout, TimeUnit.MILLISECONDS);
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
		if (!mShuttingDown) {
			try {
				mConnections.add(connection);
			} catch (IllegalStateException e) {
				/* If we fail to return the connection to the pool that should 
				 * not prevent further processing but the pool capacity is 
				 * actually reduced by one. We should warn the administrator. */
				LOG.warn("Connection could not be recycled.");
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
		mShuttingDown = true;
		if (mConnections.remainingCapacity() == 0) {
			return;
		}
		if (mConnections.size() == 0) {
			LOG.warn("Some requests might be waiting for connections.");
		} else {
			LOG.warn("There are "
					+ (mConnections.remainingCapacity() - mConnections.size())
					+ " connections in use.");
		}
	}

	/**
	 * Retrieve the currrent set of connections.
	 * @return a list of available connections in the pool
	 */
	public final List < LegStarConnection > getConnections() {
		List < LegStarConnection > connections =
			new ArrayList < LegStarConnection >();
		mConnections.drainTo(connections);
		return connections;
	}

	/**
	 * @return the host address for this coion pool
	 */
	public final LegStarAddress getAddress() {
		return mAddress;
	}

	/**
	 * @return the connection factory used by this coion pool to create
	 * new connections
	 */
	public final ConnectionFactory getConnectionFactory() {
		return mConnectionFactory;
	}
	

}
