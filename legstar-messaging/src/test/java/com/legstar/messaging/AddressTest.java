package com.legstar.messaging;

import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.commons.configuration.tree.xpath.XPathExpressionEngine;

import com.legstar.config.Constants;

import junit.framework.TestCase;

public class AddressTest extends TestCase {
	
	private static final String CONFIG_FILE = "config.xml";
	/** Configuration XPath location for an endpoint. */
	
	public void testContructorFromConfig() {
		Address address;
		try {
			address = new Address(getEndPointConfig("TheMainframe"));
			assertEquals("TheMainframe", address.getEndPointName());
			assertEquals("IBM01140", address.getHostCharset());
			assertEquals("P390", address.getHostUserID());
			assertEquals("STREAM2", address.getHostPassword());
			assertEquals(true, address.isHostTraceMode());
		} catch (RequestException e) {
			fail("tesContructorFromConfig failed " + e);
		}
	}
	
	public void testContructorFromEmptyConfig() {
		Address address;
		try {
			address = new Address(getEndPointConfig("TheMainframeMinimal"));
			assertEquals("TheMainframeMinimal", address.getEndPointName());
			assertEquals(null, address.getHostCharset());
			assertEquals(null, address.getHostUserID());
			assertEquals(null, address.getHostPassword());
			assertEquals(false, address.isHostTraceMode());
		} catch (RequestException e) {
			fail("testContructorFromEmptyConfig failed " + e);
		}
	}
	
	public void testContructorFromPartialAddress() {
		Address partialAddress = new Address("AnotherMainframe");
		Address address;
		try {
			address = new Address(partialAddress, getEndPointConfig("TheMainframe"));
			assertEquals("AnotherMainframe", address.getEndPointName());
			assertEquals("IBM01140", address.getHostCharset());
			assertEquals("P390", address.getHostUserID());
			assertEquals("STREAM2", address.getHostPassword());
			assertEquals(true, address.isHostTraceMode());
		} catch (RequestException e) {
			fail("testContructorFromEmptyConfig failed " + e);
		}
	}
	
	public HierarchicalConfiguration getEndPointConfig(
			String endPointName) throws RequestException {
		XMLConfiguration generalConfig;
		try {
			generalConfig = new XMLConfiguration(CONFIG_FILE);
		} catch (ConfigurationException e) {
			throw new RequestException(e);
		}
		generalConfig.setExpressionEngine(new XPathExpressionEngine());
		
		String strXPath = Constants.HOST_ENDPOINT_KEY 
			+ "[@name='" + endPointName + "']";
		List  endpoints = generalConfig.configurationsAt(strXPath);
		if (endpoints == null || endpoints.isEmpty()) {
			throw new RequestException(
					"No default endpoint is not defined.");
		}
		return (HierarchicalConfiguration) endpoints.get(0);
	}

}
