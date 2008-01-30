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
package com.legstar.csok.test;

import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.commons.configuration.tree.xpath.XPathExpressionEngine;

import com.legstar.csok.client.CicsSocket;
import com.legstar.csok.client.CicsSocketConnectionException;
import com.legstar.csok.client.CicsSocketConnectionFactory;
import com.legstar.messaging.LegStarAddress;

import junit.framework.TestCase;

public class CicsSocketConnectionFactoryTest extends TestCase {
	private static final String CONFIG_FILE = "config.xml";
	/** Configuration XPath location for an endpoint. */
	private static final String HOST_ENDPOINT_CFG =
		"hostEndPoints/hostEndPoint";
	
	public void testInstanciation() {
		try {
			HierarchicalConfiguration endpointConfig = loadEndpointConfiguration(CONFIG_FILE, "TheMainframe");
			CicsSocketConnectionFactory cf =
			new CicsSocketConnectionFactory(endpointConfig);
			assertTrue(cf != null);
		} catch (Exception e) {
			fail("testInstanciation failed" + e);
		}
	}
	
	public void testCreateNoUserNoPasswordNoCharset() {
		try {
			HierarchicalConfiguration endpointConfig = loadEndpointConfiguration(CONFIG_FILE, "TheMainframe");
			CicsSocketConnectionFactory cf =
			new CicsSocketConnectionFactory(endpointConfig);
			LegStarAddress address = new LegStarAddress("TheMainframe");
			CicsSocket conn = (CicsSocket) cf.createConnection("testCreateNoUserNoPasswordNoCharset", address);
			assertEquals("testCreateNoUserNoPasswordNoCharset", conn.getConnectionID());
			assertEquals("IBM01140", conn.getCicsSocketEndpoint().getHostCharset());
			assertEquals("P390", conn.getCicsSocketEndpoint().getHostUserID());
			assertEquals(false, conn.getCicsSocketEndpoint().isHostTraceMode());
		} catch (Exception e) {
			fail("testCreateWrongEndPoint failed " + e);
		}
	}

	public void testCreateUserPasswordCharset() {
		try {
			HierarchicalConfiguration endpointConfig = loadEndpointConfiguration(CONFIG_FILE, "TheMainframe");
			CicsSocketConnectionFactory cf =
			new CicsSocketConnectionFactory(endpointConfig);
			LegStarAddress address = new LegStarAddress("TheMainframe");
			address.setHostCharset("IBMTRUC0");
			address.setHostUserID("RANTANPLAN");
			address.setHostPassword("BIDULE");
			address.setHostTraceMode(true);
			CicsSocket conn = (CicsSocket) cf.createConnection("testCreateNoUserNoPasswordNoCharset", address);
			assertEquals("testCreateNoUserNoPasswordNoCharset", conn.getConnectionID());
			assertEquals("IBMTRUC0", conn.getCicsSocketEndpoint().getHostCharset());
			assertEquals("RANTANPLAN", conn.getCicsSocketEndpoint().getHostUserID());
			assertEquals(true, conn.getCicsSocketEndpoint().isHostTraceMode());
		} catch (Exception e) {
			fail("testCreateWrongEndPoint failed " + e);
		}
	}

	public void testCreateFromEmpyConfig() {
		try {
			HierarchicalConfiguration endpointConfig = loadEndpointConfiguration("config0.xml", "TheMainframe");
			CicsSocketConnectionFactory cf =
			new CicsSocketConnectionFactory(endpointConfig);
			LegStarAddress address = new LegStarAddress("TheMainframe");
			@SuppressWarnings("unused")
			CicsSocket conn = (CicsSocket) cf.createConnection("testCreateNoUserNoPasswordNoCharset", address);
			fail("testCreateFromEmpyConfig failed ");
		} catch (Exception e) {
			assertEquals("No host character set has been provided.", e.getMessage());
		}
	}

	public void testCreateFromEmpyConfig2() {
		try {
			HierarchicalConfiguration endpointConfig = loadEndpointConfiguration("config0.xml", "TheMainframe");
			CicsSocketConnectionFactory cf =
			new CicsSocketConnectionFactory(endpointConfig);
			LegStarAddress address = new LegStarAddress("TheMainframe");
			address.setHostCharset("IBMTRUC0");
			@SuppressWarnings("unused")
			CicsSocket conn = (CicsSocket) cf.createConnection("testCreateNoUserNoPasswordNoCharset", address);
			fail("testCreateFromEmpyConfig failed ");
		} catch (Exception e) {
			assertEquals("No host user ID has been provided.", e.getMessage());
		}
	}

	public void testCreateFromEmpyConfig3() {
		try {
			HierarchicalConfiguration endpointConfig = loadEndpointConfiguration("config1.xml", "TheMainframe");
			CicsSocketConnectionFactory cf =
			new CicsSocketConnectionFactory(endpointConfig);
			LegStarAddress address = new LegStarAddress("TheMainframe");
			address.setHostCharset("IBMTRUC0");
			address.setHostUserID("RANTANPLAN");
			@SuppressWarnings("unused")
			CicsSocket conn = (CicsSocket) cf.createConnection("testCreateNoUserNoPasswordNoCharset", address);
			fail("testCreateFromEmpyConfig failed ");
		} catch (Exception e) {
			assertEquals("No host IP address has been provided.", e.getMessage());
		}
	}

	public void testCreateFromEmpyConfig4() {
		try {
			HierarchicalConfiguration endpointConfig = loadEndpointConfiguration("config2.xml", "TheMainframe");
			CicsSocketConnectionFactory cf =
			new CicsSocketConnectionFactory(endpointConfig);
			LegStarAddress address = new LegStarAddress("TheMainframe");
			address.setHostCharset("IBMTRUC0");
			address.setHostUserID("RANTANPLAN");
			@SuppressWarnings("unused")
			CicsSocket conn = (CicsSocket) cf.createConnection("testCreateNoUserNoPasswordNoCharset", address);
			fail("testCreateFromEmpyConfig failed ");
		} catch (Exception e) {
			assertEquals("No host IP port has been provided.", e.getMessage());
		}
	}

	public void testCreateWithDefaultTimeouts() {
		try {
			HierarchicalConfiguration endpointConfig = loadEndpointConfiguration(CONFIG_FILE, "TheMainframe");
			CicsSocketConnectionFactory cf =
			new CicsSocketConnectionFactory(endpointConfig);
			LegStarAddress address = new LegStarAddress("TheMainframe");
			@SuppressWarnings("unused")
			CicsSocket conn = (CicsSocket) cf.createConnection("testCreateWithDefaultTimeouts", address);
			assertEquals(1000, conn.getConnectTimeout());
			assertEquals(5000, conn.getReceiveTimeout());
		} catch (Exception e) {
			fail("testCreateWithDefaultTimeouts failed" + e.getMessage());
		}
	}

	public void testCreateWithTimeoutsFromConfig() {
		try {
			HierarchicalConfiguration endpointConfig = loadEndpointConfiguration("config3.xml", "TheMainframe");
			CicsSocketConnectionFactory cf =
			new CicsSocketConnectionFactory(endpointConfig);
			LegStarAddress address = new LegStarAddress("TheMainframe");
			@SuppressWarnings("unused")
			CicsSocket conn = (CicsSocket) cf.createConnection("testCreateWithDefaultTimeouts", address);
			assertEquals(2000, conn.getConnectTimeout());
			assertEquals(7000, conn.getReceiveTimeout());
		} catch (Exception e) {
			fail("testCreateWithDefaultTimeouts failed" + e.getMessage());
		}
	}
	/**
	 * Get the configuration sub-hierarchy for the endpoint
	 * specified in the client request.
	 * @param generalConfigFileName the general configuration file name
	 * @param endpointName the requested endpoint
	 * @return the configuration sub hierarchy
	 * @throws CicsSocketConnectionException if failed to load configuration
	 */
	private HierarchicalConfiguration loadEndpointConfiguration(
			final String generalConfigFileName,
			final String endpointName) throws CicsSocketConnectionException {
		
		XMLConfiguration generalConfig;
		try {
			generalConfig = new XMLConfiguration(generalConfigFileName);
		} catch (ConfigurationException e) {
			throw new CicsSocketConnectionException(e);
		}
		generalConfig.setExpressionEngine(new XPathExpressionEngine());
		String strXPath = HOST_ENDPOINT_CFG
		+ "[@name='" + endpointName + "']";
		List < ? >  endpoints = generalConfig.configurationsAt(strXPath);
		if (endpoints == null || endpoints.isEmpty()) {
			throw new CicsSocketConnectionException("The requested endpoint:" 
					+ endpointName
					+ " is not defined.");
		}
		return (HierarchicalConfiguration) endpoints.get(0);
	}
}
