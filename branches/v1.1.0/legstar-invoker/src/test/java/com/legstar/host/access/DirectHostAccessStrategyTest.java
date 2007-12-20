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
package com.legstar.host.access;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.commons.configuration.tree.xpath.XPathExpressionEngine;

import com.legstar.host.access.DirectHostAccessStrategy;
import com.legstar.host.access.HostAccessStrategyException;
import com.legstar.host.invoke.Util;
import com.legstar.messaging.Address;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.HeaderPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.Message;
import com.legstar.messaging.MessagePart;
import com.legstar.messaging.Request;
import com.legstar.config.Constants;

import junit.framework.TestCase;

public class DirectHostAccessStrategyTest extends TestCase {
	
	private static final String CONFIG_FILE = "config0.xml";
	/** Configuration XPath location for an endpoint. */
	private static final String HOST_ENDPOINT_CFG =
		"hostEndPoints/hostEndPoint";
	
	
	public void testConstructorNoFactory() {
		try {
			HierarchicalConfiguration endpointConfig = loadEndpointConfiguration("config1.xml", "TheMainframe");
			@SuppressWarnings("unused")
			DirectHostAccessStrategy dha = new DirectHostAccessStrategy(endpointConfig);
			fail("testConstructorNoFactory failed ");
		} catch (HostAccessStrategyException e) {
			assertEquals("org.apache.commons.configuration.ConfigurationException: java.lang.ClassNotFoundException: com.legstar.truc.much.CicsSocketConnectionFactory", e.getMessage());
		}
	}

	public void testConstructor() {
		try {
			HierarchicalConfiguration endpointConfig = loadEndpointConfiguration(CONFIG_FILE, "TheMainframe");
			DirectHostAccessStrategy dha = new DirectHostAccessStrategy(endpointConfig);
			assertTrue(dha != null);
		} catch (HostAccessStrategyException e) {
			fail("testConstructor failed " + e);
		}
	}
	
	public void testInvoke() {
		try {
			HierarchicalConfiguration endpointConfig = loadEndpointConfiguration(CONFIG_FILE, "TheMainframe");
			DirectHostAccessStrategy dha = new DirectHostAccessStrategy(endpointConfig);
			HashMap < String, Object > map = new HashMap < String, Object >();
			map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAE");
			map.put(Constants.CICS_LENGTH_KEY, "79");
			map.put(Constants.CICS_DATALEN_KEY, "6");
			List <MessagePart> inputParts = new ArrayList <MessagePart>();
			MessagePart inCommarea = new CommareaPart(Util.toByteArray("F0F0F0F1F0F0"));
			inputParts.add(inCommarea);
			HeaderPart dp = new HeaderPart(map, inputParts.size());
			Address address = new Address("TheMainframe");
			Message requestMessage = new Message(dp, inputParts);
			Request request = new Request("Request01", address, requestMessage);
			dha.invoke(request);
			assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c", 
					Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
		} catch (HostAccessStrategyException e) {
			fail("testInvoke failed " + e);
		} catch (HeaderPartException e) {
			fail("testInvoke failed " + e);
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
