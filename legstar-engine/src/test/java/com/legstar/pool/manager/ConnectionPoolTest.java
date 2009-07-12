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

import java.util.List;


import junit.framework.TestCase;
import com.legstar.messaging.HostEndpoint;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.HostEndpoint.AccessStrategy;
import com.legstar.mock.client.MockEndpoint;

/**
 * Test connection pool class.
 *
 */
public class ConnectionPoolTest extends TestCase {

    /** Pool size. */
    private static final int POOL_SIZE = 1;

    /**
     * Check class constructor.
     */
    public void testInstantiation() {
        try {
            ConnectionPool connectionPool = getConnectionPool();
            assertEquals(POOL_SIZE, connectionPool.getConnections().size());
            List < LegStarConnection > connections =  connectionPool.getConnections();
            for (LegStarConnection connection : connections) {
                assertTrue(connection.getConnectionID() != null);
            }
        } catch (ConnectionPoolException e) {
            fail("testInstanciation failed " + e);
        }
    }

    /**
     * Test taking a pool item when available and when not available.
     */
    public void testTake() {
        try {
            ConnectionPool connectionPool = getConnectionPool();
            /* First take should work */
            LegStarConnection connection = connectionPool.take(1);
            assertTrue(connection.getConnectionID() != null);
            try {
                @SuppressWarnings("unused")
                LegStarConnection connection2 = connectionPool.take(1);
                fail("testTake failed");
            } catch (ConnectionPoolException e) {
                assertEquals("Timed out waiting for pooled connection.", e.getMessage());
            }
            /* Second take should timeout */
        } catch (ConnectionPoolException e) {
            fail("testTake failed " + e);
        }
    }

    /**
     * Test returning an item to the pool.
     */
    public void testPut() {
        try {
            ConnectionPool connectionPool = getConnectionPool();
            LegStarConnection connection = connectionPool.take(1);
            assertTrue(connection.getConnectionID() != null);
            connectionPool.put(connection);
            assertEquals(POOL_SIZE, connectionPool.getConnections().size());
        } catch (ConnectionPoolException e) {
            fail("testPut failed " + e);
        }
    }

    /**
     * Load a configured pool.
     * @return a connection pool
     * @throws ConnectionPoolException if pool cannot be created
     */
    private ConnectionPool getConnectionPool() throws ConnectionPoolException {
        LegStarAddress address = new LegStarAddress("TheMainframe");
        ConnectionPool connectionPool = new ConnectionPool(address, getPooledHostEndpoint());
        return connectionPool;
    }

    /**
     * @return a pooled host endpoint
     */
    public HostEndpoint getPooledHostEndpoint() {
        HostEndpoint endpoint = new MockEndpoint();
        endpoint.setName("TheMainframe");
        endpoint.setHostConnectionfactoryClass("com.legstar.mock.client.MockConnectionFactory");
        endpoint.setHostAccessStrategy(AccessStrategy.pooled);
        endpoint.setHostConnectionPoolSize(POOL_SIZE);
        endpoint.setPooledInvokeTimeout(2000);
        return endpoint;
    }
}
