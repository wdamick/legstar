package com.legstar.host.server;

import java.util.Iterator;
import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.commons.configuration.tree.xpath.XPathExpressionEngine;

import junit.framework.TestCase;

public class ConfigReaderTest extends TestCase {
	
	public static final String CONFIG_FILE = "config.xml";
	private static final String HOST_ENDPOINT_CFG = "hostEndPoints.hostEndPoint";
	
	public void testReadConfig() throws ConfigurationException {
		XMLConfiguration config = new XMLConfiguration(CONFIG_FILE);
		List endpoints = config.configurationsAt(HOST_ENDPOINT_CFG);
		for(Iterator it = endpoints.iterator(); it.hasNext();)	{
			HierarchicalConfiguration sub = (HierarchicalConfiguration) it.next();
			System.out.println("EndPoint name=" + sub.getString("[@name]"));
			System.out.println("EndPoint ipaddress=" + sub.getString("hostIPAddress"));
			
		}
		
	}

	public void testReadConfigXPath() throws ConfigurationException {
		XMLConfiguration config = new XMLConfiguration(CONFIG_FILE);
		config.setExpressionEngine(new XPathExpressionEngine());
		
		List endpoints = config.configurationsAt("hostEndPoints/hostEndPoint[@name = 'TheOtherMainframe']");
		assertTrue(!endpoints.isEmpty());
		for(Iterator it = endpoints.iterator(); it.hasNext();)	{
			HierarchicalConfiguration sub = (HierarchicalConfiguration) it.next();
			System.out.println("EndPoint name=" + sub.getString("@name"));
			System.out.println("EndPoint ipaddress=" + sub.getString("hostIPAddress"));
			
		}
	}
	
	public void testNotFoundElement() throws ConfigurationException {
		XMLConfiguration config = new XMLConfiguration(CONFIG_FILE);
		config.setExpressionEngine(new XPathExpressionEngine());
		
		List endpoints = config.configurationsAt("hostEndPoints/hostEndPoint[@name = 'TheOtterrMainframe']");
		if (endpoints == null || endpoints.size() == 0) {
			System.out.println("The requested endpoint:" 
			+ "TheOtterrMainframe"
			+ " is not defined in the " + config.getBasePath());
		}
		assertTrue(endpoints.isEmpty());
	}

	public void testReadConfigXPathDirect() throws ConfigurationException {
		XMLConfiguration config = new XMLConfiguration(CONFIG_FILE);
		config.setExpressionEngine(new XPathExpressionEngine());
		
		List  endpoints = config.configurationsAt("hostEndPoints/hostEndPoint[@name = 'TheOtherMainframe']");
		assertTrue(!endpoints.isEmpty());
		HierarchicalConfiguration endpoint = (HierarchicalConfiguration) endpoints.get(0);
		System.out.println("EndPoint name=" + endpoint.getString("@name"));
		System.out.println("EndPoint ipaddress=" + endpoint.getString("hostIPAddress"));
	}
}
