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
package com.legstar.http.client;

import org.apache.commons.configuration.HierarchicalConfiguration;

import com.legstar.http.client.CicsHttp;
import com.legstar.http.client.CicsHttpConnectionFactory;
import com.legstar.messaging.Address;
import com.legstar.config.Config;

import junit.framework.TestCase;

public class CicsHttpConnectionFactoryTest extends TestCase {

	private static final String CONFIG_FILE = "config.xml";
	/** Configuration XPath location for an endpoint. */
	
	public void testInstanciation() {
		try {
			HierarchicalConfiguration endpointConfig =
				Config.loadEndpointConfiguration(
						Config.loadGeneralConfig(CONFIG_FILE), "TheMainframe");
			CicsHttpConnectionFactory cf =
			new CicsHttpConnectionFactory(endpointConfig);
			assertTrue(cf != null);
		} catch (Exception e) {
			fail("testInstanciation failed" + e);
		}
	}
	
	public void testCreateNoUserNoPasswordNoCharset() {
		try {
			HierarchicalConfiguration endpointConfig =
				Config.loadEndpointConfiguration(
						Config.loadGeneralConfig(CONFIG_FILE), "TheMainframe");
			CicsHttpConnectionFactory cf =
			new CicsHttpConnectionFactory(endpointConfig);
			Address address = new Address("TheMainframe");
			CicsHttp conn = (CicsHttp) cf.createConnection("testCreateNoUserNoPasswordNoCharset", address);
			assertEquals("testCreateNoUserNoPasswordNoCharset", conn.getConnectionID());
			assertEquals("IBM01140", conn.getCicsHttpEndpoint().getHostCharset());
			assertEquals("P390", conn.getCicsHttpEndpoint().getHostUserID());
			assertEquals(false, conn.getCicsHttpEndpoint().isHostTraceMode());
		} catch (Exception e) {
			fail("testCreateWrongEndPoint failed " + e);
		}
	}

	public void testCreateUserPasswordCharset() {
		try {
			HierarchicalConfiguration endpointConfig =
				Config.loadEndpointConfiguration(
						Config.loadGeneralConfig(CONFIG_FILE), "TheMainframe");
			CicsHttpConnectionFactory cf =
			new CicsHttpConnectionFactory(endpointConfig);
			Address address = new Address("TheMainframe");
			address.setHostCharset("IBMTRUC0");
			address.setHostUserID("RANTANPLAN");
			address.setHostPassword("BIDULE");
			address.setHostTraceMode(true);
			CicsHttp conn = (CicsHttp) cf.createConnection("testCreateNoUserNoPasswordNoCharset", address);
			assertEquals("testCreateNoUserNoPasswordNoCharset", conn.getConnectionID());
			assertEquals("IBMTRUC0", conn.getCicsHttpEndpoint().getHostCharset());
			assertEquals("RANTANPLAN", conn.getCicsHttpEndpoint().getHostUserID());
			assertEquals(true, conn.getCicsHttpEndpoint().isHostTraceMode());
		} catch (Exception e) {
			fail("testCreateWrongEndPoint failed " + e);
		}
	}

	public void testCreateFromEmpyConfig() {
		try {
			HierarchicalConfiguration endpointConfig =
				Config.loadEndpointConfiguration(
						Config.loadGeneralConfig("config0.xml"), "TheMainframe");
			CicsHttpConnectionFactory cf =
			new CicsHttpConnectionFactory(endpointConfig);
			Address address = new Address("TheMainframe");
			@SuppressWarnings("unused")
			CicsHttp conn = (CicsHttp) cf.createConnection("testCreateNoUserNoPasswordNoCharset", address);
			fail("testCreateFromEmpyConfig failed ");
		} catch (Exception e) {
			assertEquals("No host character set has been provided.", e.getMessage());
		}
	}

	public void testCreateFromEmpyConfig3() {
		try {
			HierarchicalConfiguration endpointConfig =
				Config.loadEndpointConfiguration(
						Config.loadGeneralConfig("config1.xml"), "TheMainframe");
			CicsHttpConnectionFactory cf =
			new CicsHttpConnectionFactory(endpointConfig);
			Address address = new Address("TheMainframe");
			address.setHostCharset("IBMTRUC0");
			address.setHostUserID("RANTANPLAN");
			@SuppressWarnings("unused")
			CicsHttp conn = (CicsHttp) cf.createConnection("testCreateNoUserNoPasswordNoCharset", address);
			fail("testCreateFromEmpyConfig failed ");
		} catch (Exception e) {
			assertEquals("No host IP address has been provided.", e.getMessage());
		}
	}

	public void testCreateFromEmpyConfig4() {
		try {
			HierarchicalConfiguration endpointConfig =
				Config.loadEndpointConfiguration(
						Config.loadGeneralConfig("config2.xml"), "TheMainframe");
			CicsHttpConnectionFactory cf =
			new CicsHttpConnectionFactory(endpointConfig);
			Address address = new Address("TheMainframe");
			address.setHostCharset("IBMTRUC0");
			address.setHostUserID("RANTANPLAN");
			@SuppressWarnings("unused")
			CicsHttp conn = (CicsHttp) cf.createConnection("testCreateNoUserNoPasswordNoCharset", address);
			fail("testCreateFromEmpyConfig failed ");
		} catch (Exception e) {
			assertEquals("No host IP port has been provided.", e.getMessage());
		}
	}

	
	public void testCreateWithDefaultTimeouts() {
		try {
			HierarchicalConfiguration endpointConfig =
				Config.loadEndpointConfiguration(
						Config.loadGeneralConfig(CONFIG_FILE), "TheMainframe");
			CicsHttpConnectionFactory cf =
			new CicsHttpConnectionFactory(endpointConfig);
			Address address = new Address("TheMainframe");
			@SuppressWarnings("unused")
			CicsHttp conn = (CicsHttp) cf.createConnection("testCreateWithDefaultTimeouts", address);
			assertEquals(1000, conn.getConnectTimeout());
			assertEquals(5000, conn.getReceiveTimeout());
		} catch (Exception e) {
			fail("testCreateWithDefaultTimeouts failed" + e.getMessage());
		}
	}

	public void testCreateWithTimeoutsFromConfig() {
		try {
			HierarchicalConfiguration endpointConfig =
				Config.loadEndpointConfiguration(
						Config.loadGeneralConfig("config3.xml"), "TheMainframe");
			CicsHttpConnectionFactory cf =
			new CicsHttpConnectionFactory(endpointConfig);
			Address address = new Address("TheMainframe");
			@SuppressWarnings("unused")
			CicsHttp conn = (CicsHttp) cf.createConnection("testCreateWithDefaultTimeouts", address);
			assertEquals(2000, conn.getConnectTimeout());
			assertEquals(7000, conn.getReceiveTimeout());
		} catch (Exception e) {
			fail("testCreateWithDefaultTimeouts failed" + e.getMessage());
		}
	}
}
