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

import org.apache.commons.configuration.ConfigurationException;

import com.legstar.host.server.Util;
import com.legstar.messaging.LegStarAddress;

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
            ConnectionPoolManager pm = new ConnectionPoolManager(
                    Util.getCombinedConfiguration());
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

        } catch (ConfigurationException e) {
            fail("testGetPool failed " + e);
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
            ConnectionPoolManager pm = new ConnectionPoolManager(
                    Util.getCombinedConfiguration());
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

        } catch (ConfigurationException e) {
            fail("testMultiAdd failed " + e);
        } catch (ConnectionPoolException e) {
            fail("testMultiAdd failed " + e);
        }
    }

    /**
     * Check the pool manager reactions after shutdown.
     */
    public void testShutdown() {
        try {
            ConnectionPoolManager pm = new ConnectionPoolManager(
                    Util.getCombinedConfiguration());
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



        } catch (ConfigurationException e) {
            fail("testMultiAdd failed " + e);
        } catch (ConnectionPoolException e) {
            fail("testMultiAdd failed " + e);
        }
    }
}
