package com.legstar.host.access;

import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.commons.configuration.tree.xpath.XPathExpressionEngine;

import junit.framework.TestCase;

public class DefaultConfigTest extends TestCase {
	private static final String CONFIG_FILE = "config0.xml";
	/** Configuration XPath location for an endpoint. */
	private static final String HOST_ENDPOINT_CFG =
		"hostEndPoints/hostEndPoint";
	
	public void testDefaultEndPoint() throws HostAccessStrategyException {
		XMLConfiguration generalConfig;
		try {
			generalConfig = new XMLConfiguration(CONFIG_FILE);
		} catch (ConfigurationException e) {
			throw new HostAccessStrategyException(e);
		}
		generalConfig.setExpressionEngine(new XPathExpressionEngine());
		
		String strXPath = HOST_ENDPOINT_CFG;
		List  endpoints = generalConfig.configurationsAt(strXPath);
		if (endpoints == null || endpoints.isEmpty()) {
			throw new HostAccessStrategyException(
					"No default endpoint is not defined.");
		}
		HierarchicalConfiguration sub = (HierarchicalConfiguration) endpoints.get(0);
		assertEquals("TheMainframe", sub.getString("@name"));
		
		
	}

}
