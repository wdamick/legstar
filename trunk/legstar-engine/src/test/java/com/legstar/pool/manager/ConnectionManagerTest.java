package com.legstar.pool.manager;

import org.apache.commons.configuration.ConfigurationException;

import com.legstar.config.Config;
import com.legstar.messaging.Address;

import junit.framework.TestCase;

public class ConnectionManagerTest extends TestCase {

	private static final String CONFIG_FILE = "config.xml";
	
	public void testGetPool() {
		try {
			ConnectionPoolManager pm = new ConnectionPoolManager(
					Config.loadGeneralConfig(CONFIG_FILE));
			Address address = new Address("TheOtherMainframe");
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
					Config.loadGeneralConfig(CONFIG_FILE));
			Address address = new Address("TheMainframe");
			ConnectionPool cp = pm.getPool(address, true);
			Address address2 = new Address("TheOtherMainframe");
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
					Config.loadGeneralConfig(CONFIG_FILE));
			/* Shutdown an empty pool */
			pm.shutDown();
			
			Address address = new Address("TheMainframe");
			ConnectionPool cp = pm.getPool(address, true);
			Address address2 = new Address("TheOtherMainframe");
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
