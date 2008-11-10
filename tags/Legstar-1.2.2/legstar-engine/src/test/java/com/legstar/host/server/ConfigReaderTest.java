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
package com.legstar.host.server;

import java.util.Iterator;
import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.configuration.tree.xpath.XPathExpressionEngine;

import junit.framework.TestCase;

public class ConfigReaderTest extends TestCase {
	
	private static final String HOST_ENDPOINT_CFG = "hostEndPoints.hostEndPoint";
	
	@SuppressWarnings("unchecked")
	public void testReadConfig() throws ConfigurationException {
		HierarchicalConfiguration config = Util.getCombinedConfiguration();
		List endpoints = config.configurationsAt(HOST_ENDPOINT_CFG);
		for(Iterator it = endpoints.iterator(); it.hasNext();)	{
			HierarchicalConfiguration sub = (HierarchicalConfiguration) it.next();
			System.out.println("EndPoint name=" + sub.getString("[@name]"));
			System.out.println("EndPoint ipaddress=" + sub.getString("hostIPAddress"));
			
		}
		
	}

	@SuppressWarnings("unchecked")
	public void testReadConfigXPath() throws ConfigurationException {
		HierarchicalConfiguration config = Util.getCombinedConfiguration();
		config.setExpressionEngine(new XPathExpressionEngine());
		
		List endpoints = config.configurationsAt("hostEndPoints/hostEndPoint[@name = 'TheOtherMainframe']");
		assertTrue(!endpoints.isEmpty());
		for(Iterator it = endpoints.iterator(); it.hasNext();)	{
			HierarchicalConfiguration sub = (HierarchicalConfiguration) it.next();
			System.out.println("EndPoint name=" + sub.getString("@name"));
			System.out.println("EndPoint ipaddress=" + sub.getString("hostIPAddress"));
			
		}
	}
	
	@SuppressWarnings("unchecked")
	public void testNotFoundElement() throws ConfigurationException {
		HierarchicalConfiguration config = Util.getCombinedConfiguration();
		config.setExpressionEngine(new XPathExpressionEngine());
		
		List endpoints = config.configurationsAt("hostEndPoints/hostEndPoint[@name = 'TheOtterrMainframe']");
		if (endpoints == null || endpoints.size() == 0) {
			System.out.println("The requested endpoint:" 
			+ "TheOtterrMainframe"
			+ " is not defined in " + Util.CONFIG_FILE);
		}
		assertTrue(endpoints.isEmpty());
	}

	@SuppressWarnings("unchecked")
	public void testReadConfigXPathDirect() throws ConfigurationException {
		HierarchicalConfiguration config = Util.getCombinedConfiguration();
		config.setExpressionEngine(new XPathExpressionEngine());
		
		List  endpoints = config.configurationsAt("hostEndPoints/hostEndPoint[@name = 'TheOtherMainframe']");
		assertTrue(!endpoints.isEmpty());
		HierarchicalConfiguration endpoint = (HierarchicalConfiguration) endpoints.get(0);
		System.out.println("EndPoint name=" + endpoint.getString("@name"));
		System.out.println("EndPoint ipaddress=" + endpoint.getString("hostIPAddress"));
	}
	
}
