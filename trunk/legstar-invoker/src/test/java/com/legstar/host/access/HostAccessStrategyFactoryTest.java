package com.legstar.host.access;

import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.commons.configuration.tree.xpath.XPathExpressionEngine;

import junit.framework.TestCase;

public class HostAccessStrategyFactoryTest extends TestCase {
	
	private static final String CONFIG_FILE = "config0.xml";
	/** Configuration XPath location for an endpoint. */
	private static final String HOST_ENDPOINT_CFG =
		"hostEndPoints/hostEndPoint";
	
	public void testConstructorWithDefaultStrategy() {
		try {
			HierarchicalConfiguration endpointConfig = loadEndpointConfiguration(CONFIG_FILE, "TheMainframe");
			HostAccessStrategy has = HostAccessStrategyFactory.createAccessStrategy(endpointConfig);
			assertEquals("class com.legstar.host.access.DirectHostAccessStrategy", has.getClass().toString());
		} catch (HostAccessStrategyException e) {
			fail("testConstructor failed " + e.getMessage());
		}
	}

	public void testConstructorWithPooledStrategy() {
		try {
			HierarchicalConfiguration endpointConfig = loadEndpointConfiguration("config2.xml", "TheMainframe");
			HostAccessStrategy has = HostAccessStrategyFactory.createAccessStrategy(endpointConfig);
			assertEquals("class com.legstar.host.access.PooledHostAccessStrategy", has.getClass().toString());
		} catch (HostAccessStrategyException e) {
			fail("testConstructor failed " + e.getMessage());
		}
	}

	public void testConstructorWithInvalidStrategy() {
		try {
			HierarchicalConfiguration endpointConfig = loadEndpointConfiguration("config3.xml", "TheMainframe");
			@SuppressWarnings("unused")
			HostAccessStrategy has = HostAccessStrategyFactory.createAccessStrategy(endpointConfig);
			fail("testConstructorWithInvalidStrategy failed ");
		} catch (HostAccessStrategyException e) {
			assertEquals("Unknown host access strategy.", e.getMessage());
		}
	}
	/**
	 * Get the configuration sub-hierarchy for the endpoint
	 * specified in the client request.
	 * @param generalConfigFileName the general configuration file name
	 * @param endpointName the requested endpoint
	 * @return the configuration sub hierarchy
	 * @throws HostAccessStrategyException if failed to load configuration
	 */
	private HierarchicalConfiguration loadEndpointConfiguration(
			final String generalConfigFileName,
			final String endpointName) throws HostAccessStrategyException {
		
		XMLConfiguration generalConfig;
		try {
			generalConfig = new XMLConfiguration(generalConfigFileName);
		} catch (ConfigurationException e) {
			throw new HostAccessStrategyException(e);
		}
		generalConfig.setExpressionEngine(new XPathExpressionEngine());
		String strXPath = HOST_ENDPOINT_CFG
		+ "[@name='" + endpointName + "']";
		List  endpoints = generalConfig.configurationsAt(strXPath);
		if (endpoints == null || endpoints.isEmpty()) {
			throw new HostAccessStrategyException("The requested endpoint:" 
					+ endpointName
					+ " is not defined.");
		}
		return (HierarchicalConfiguration) endpoints.get(0);
	}

}
