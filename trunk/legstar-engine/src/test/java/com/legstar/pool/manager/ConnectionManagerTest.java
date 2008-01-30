/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.pool.manager;

import org.apache.commons.configuration.ConfigurationException;

import com.legstar.host.server.Util;
import com.legstar.messaging.LegStarAddress;

import junit.framework.TestCase;

public class ConnectionManagerTest extends TestCase {

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
