package com.legstar.pool.manager;

import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;

import junit.framework.TestCase;
import com.legstar.config.Config;
import com.legstar.messaging.Address;
import com.legstar.messaging.Connection;
import com.legstar.messaging.ConnectionFactory;

public class ConnectionPoolTest extends TestCase {

	private static final String CONFIG_FILE = "config.xml";
	
	private static final int POOL_SIZE = 1;
	
	public void testInstanciation() {
		try {
			ConnectionPool connectionPool = getConnectionPool();
			assertEquals(POOL_SIZE, connectionPool.getConnections().size());
			List < Connection > connections =  connectionPool.getConnections();
			for (Connection connection : connections) {
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
			Connection connection = connectionPool.take(1);
			assertTrue(connection.getConnectionID() != null);
			try {
				@SuppressWarnings("unused")
				Connection connection2 = connectionPool.take(1);
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
			Connection connection = connectionPool.take(1);
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
		Address address = new Address("TheMainframe");
		HierarchicalConfiguration generalConfig =
			Config.loadGeneralConfig(CONFIG_FILE);
		HierarchicalConfiguration endpointConfig =
			Config.loadAddressConfiguration(generalConfig, address);
		ConnectionFactory connectionFactory = Config.loadConnectionFactory(endpointConfig);
		ConnectionPool connectionPool = new ConnectionPool(POOL_SIZE, address, connectionFactory);
		return connectionPool;
	}

}
