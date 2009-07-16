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

import com.legstar.messaging.ConnectionException;
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
            ConnectionPool connectionPool = getConnectionPool(POOL_SIZE);
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
            ConnectionPool connectionPool = getConnectionPool(POOL_SIZE);
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
            ConnectionPool connectionPool = getConnectionPool(POOL_SIZE);
            LegStarConnection connection = connectionPool.take(1);
            assertTrue(connection.getConnectionID() != null);
            connectionPool.put(connection);
            assertEquals(POOL_SIZE, connectionPool.getConnections().size());
        } catch (ConnectionPoolException e) {
            fail("testPut failed " + e);
        }
    }
    
    /**
     * See if keep alive policy works.
     * Connections need to be used at least once to become eligible to keep alive
     * policy.
     */
    public void testKeepAlivePolicy() {
        try {
            ConnectionPool connectionPool = getConnectionPool(2);
            LegStarConnection connection1 = connectionPool.take(1);
            connection1.connectReuse(null);
            connectionPool.put(connection1);
            assertTrue(connection1.isOpen());
            /* Requesting a connection should return the last connection used
             * (Stack works as a LIFO queue)*/
            LegStarConnection connection2 = connectionPool.take(1);
            assertEquals(connection2, connection1);
            assertTrue(connection2.isOpen());
            /* Wait enough so that connection1 is eligible to close */
            Thread.sleep(2000L + 100L);
            /* put triggers the policy */
            connectionPool.put(connection2);
            LegStarConnection connection3 = connectionPool.take(1);
            /* the stack head should still be connection1 but it should be closed */
            assertEquals(connection3, connection1);
            assertFalse(connection3.isOpen());
            connectionPool.put(connection3);
            /* Now we push 2 open connections to the stack */
            connection1 = connectionPool.take(1);
            connection2 = connectionPool.take(1);
            connection1.connectReuse(null);
            connection2.connectReuse(null);
            connectionPool.put(connection1);
            connectionPool.put(connection2);
            assertTrue(connection1.isOpen());
            assertTrue(connection2.isOpen());
            /* Requesting a connection should yield the last one returned to the stack*/
            connection3 = connectionPool.take(1);
            assertEquals(connection3, connection2);
            assertTrue(connection3.isOpen());
            /* Wait enough so that connection1 is eligible to close */
            Thread.sleep(2000L + 100L);
            /* put triggers the policy */
            connectionPool.put(connection3);
            assertFalse(connection3.isOpen());
            assertFalse(connection1.isOpen());
            
        } catch (ConnectionPoolException e) {
            fail(e.getMessage());
        } catch (InterruptedException e) {
            fail(e.getMessage());
        } catch (ConnectionException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Load a configured pool.
     * @param poolSize the pool size
     * @return a connection pool
     * @throws ConnectionPoolException if pool cannot be created
     */
    private ConnectionPool getConnectionPool(
            final int poolSize) throws ConnectionPoolException {
        LegStarAddress address = new LegStarAddress("TheMainframe");
        ConnectionPool connectionPool = new ConnectionPool(address, getPooledHostEndpoint(poolSize));
        return connectionPool;
    }

    /**
     * @param poolSize the pool size
     * @return a pooled host endpoint
     */
    public HostEndpoint getPooledHostEndpoint(final int poolSize) {
        HostEndpoint endpoint = new MockEndpoint();
        endpoint.setName("TheMainframe");
        endpoint.setHostConnectionfactoryClass("com.legstar.mock.client.MockConnectionFactory");
        endpoint.setHostAccessStrategy(AccessStrategy.pooled);
        endpoint.setHostConnectionPoolSize(poolSize);
        endpoint.setPooledInvokeTimeout(2000);
        endpoint.setPooledMaxKeepAlive(2000L);
        return endpoint;
    }
}
