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

import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;

import junit.framework.TestCase;
import com.legstar.config.Config;
import com.legstar.host.server.Util;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.ConnectionFactory;

public class ConnectionPoolTest extends TestCase {

	private static final int POOL_SIZE = 1;
	
	public void testInstanciation() {
		try {
			ConnectionPool connectionPool = getConnectionPool();
			assertEquals(POOL_SIZE, connectionPool.getConnections().size());
			List < LegStarConnection > connections =  connectionPool.getConnections();
			for (LegStarConnection connection : connections) {
				assertTrue(connection.getConnectionID() != null);
			}
		} catch (ConfigurationException e) {
			fail("testInstanciation failed " + e);
		} catch (ConnectionPoolException e) {
			fail("testInstanciation failed " + e);
		}
	}
	
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
			/* Seconf take should timeout */
		} catch (ConfigurationException e) {
			fail("testTake failed " + e);
		} catch (ConnectionPoolException e) {
			fail("testTake failed " + e);
		}
	}
	
	public void testPut() {
		try {
			ConnectionPool connectionPool = getConnectionPool();
			LegStarConnection connection = connectionPool.take(1);
			assertTrue(connection.getConnectionID() != null);
			connectionPool.put(connection);
			assertEquals(POOL_SIZE, connectionPool.getConnections().size());
		} catch (ConfigurationException e) {
			fail("testPut failed " + e);
		} catch (ConnectionPoolException e) {
			fail("testPut failed " + e);
		}
	}
	
	private ConnectionPool getConnectionPool() throws ConfigurationException, ConnectionPoolException {
		LegStarAddress address = new LegStarAddress("TheMainframe");
		HierarchicalConfiguration generalConfig =
			Util.getCombinedConfiguration();
		HierarchicalConfiguration endpointConfig =
			Config.loadAddressConfiguration(generalConfig, address);
		ConnectionFactory connectionFactory = Config.loadConnectionFactory(endpointConfig);
		ConnectionPool connectionPool = new ConnectionPool(POOL_SIZE, address, connectionFactory);
		return connectionPool;
	}

}
