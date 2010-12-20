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
package com.legstar.pool.manager;

import java.util.ArrayList;
import java.util.List;

import com.legstar.messaging.HostEndpoint;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.HostEndpoint.AccessStrategy;
import com.legstar.mock.client.MockEndpoint;

import junit.framework.TestCase;

/**
 * Test the connection manager.
 *
 */
public class ConnectionManagerTest extends TestCase {

    /**
     * Request a pool from the pool manager.
     */
    public void testGetPool() {
        try {
            ConnectionPoolManager pm = new ConnectionPoolManager(getHostEndpoints());
            LegStarAddress address = new LegStarAddress("TheOtherMainframe");
            /* This should not return a pool since we are not
             * requesting creation and the pool map is initially
             * empty. */
            ConnectionPool cp = pm.getPool(address, false);
            assertEquals(null, cp);
            /* This one should create a new pool */
            cp = pm.getPool(address, true);
            assertTrue(cp.getAddress().equals(address));
            assertEquals(1, pm.getPools().size());
            assertEquals(2, cp.getConnections().size());

        } catch (ConnectionPoolException e) {
            fail("testGetPool failed " + e);
        }
    }

    /**
     * Check that 2 different addresses will get 2 different pools.
     * A pool is associated with one and only one address.
     */
    public void testMultiAdd() {
        try {
            ConnectionPoolManager pm = new ConnectionPoolManager(getHostEndpoints());
            LegStarAddress address = new LegStarAddress("TheMainframe");
            ConnectionPool cp = pm.getPool(address, true);
            LegStarAddress address2 = new LegStarAddress("TheOtherMainframe");
            ConnectionPool cp2 = pm.getPool(address2, true);
            assertTrue(cp.getAddress().equals(address));
            assertTrue(cp2.getAddress().equals(address2));
            assertEquals(2, pm.getPools().size());
            assertEquals(5, cp.getConnections().size());
            assertEquals(2, cp2.getConnections().size());
            pm.shutDown();

        } catch (ConnectionPoolException e) {
            fail("testMultiAdd failed " + e);
        }
    }

    /**
     * Check the pool manager reactions after shutdown.
     */
    public void testShutdown() {
        try {
            ConnectionPoolManager pm = new ConnectionPoolManager(getHostEndpoints());
            /* Shutdown an empty pool */
            pm.shutDown();

            LegStarAddress address = new LegStarAddress("TheMainframe");
            ConnectionPool cp = pm.getPool(address, true);
            LegStarAddress address2 = new LegStarAddress("TheOtherMainframe");
            ConnectionPool cp2 = pm.getPool(address2, true);

            /* Shutdown with pools */
            pm.shutDown();

            /* Should not be possible to take connections anymore */
            try {
                cp.take(1);
                fail("testMultiAdd failed");
            } catch (ConnectionPoolException e) {
                assertEquals("Pool is shutting down.", e.getMessage());
            }
            try {
                cp2.take(1);
                fail("testMultiAdd failed");
            } catch (ConnectionPoolException e) {
                assertEquals("Pool is shutting down.", e.getMessage());
            }

        } catch (ConnectionPoolException e) {
            fail("testMultiAdd failed " + e);
        }
    }
    
    /**
     * @return a list of endpoints
     */
    private List < HostEndpoint > getHostEndpoints() {
        List < HostEndpoint > endpoints = new ArrayList < HostEndpoint >();

        HostEndpoint endpoint1 = new MockEndpoint();
        endpoint1.setName("TheMainframe");
        endpoint1.setHostConnectionfactoryClass("com.legstar.mock.client.MockConnectionFactory");
        endpoint1.setHostAccessStrategy(AccessStrategy.direct);
        endpoints.add(endpoint1);
        
        HostEndpoint endpoint2 = new MockEndpoint();
        endpoint2.setName("TheOtherMainframe");
        endpoint2.setHostConnectionfactoryClass("com.legstar.mock.client.MockConnectionFactory");
        endpoint2.setHostAccessStrategy(AccessStrategy.pooled);
        endpoint2.setHostConnectionPoolSize(2);
        endpoint2.setPooledInvokeTimeout(2000);
        endpoints.add(endpoint2);
        
        return endpoints;
        
    }
}
