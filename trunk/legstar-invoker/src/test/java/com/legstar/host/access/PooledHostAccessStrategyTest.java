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
package com.legstar.host.access;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.commons.configuration.tree.xpath.XPathExpressionEngine;

import com.legstar.config.Constants;
import com.legstar.host.access.PooledHostAccessStrategy;
import com.legstar.host.access.HostAccessStrategyException;
import com.legstar.host.invoke.Util;
import com.legstar.host.server.EngineHandler;
import com.legstar.host.server.EngineStartupException;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.LegStarHeaderPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarMessagePart;
import com.legstar.messaging.LegStarRequest;

import junit.framework.TestCase;

public class PooledHostAccessStrategyTest extends TestCase {
	
	private static final String CONFIG_FILE = "config0.xml";
	/** Configuration XPath location for an endpoint. */
	private static final String HOST_ENDPOINT_CFG =
		"hostEndPoints/hostEndPoint";
	
	
	public void testConstructor() {
		try {
			HierarchicalConfiguration endpointConfig = loadEndpointConfiguration(CONFIG_FILE, "TheMainframe");
			PooledHostAccessStrategy pha = new PooledHostAccessStrategy(endpointConfig);
			assertTrue(pha != null);
			assertEquals(2000, pha.getInvokeTimeout());
		} catch (HostAccessStrategyException e) {
			fail("testConstructor failed " + e);
		}
	}

	public void testConstructorWithNoConfiguredTimeout() {
		try {
			HierarchicalConfiguration endpointConfig = loadEndpointConfiguration("config1.xml", "TheMainframe");
			PooledHostAccessStrategy pha = new PooledHostAccessStrategy(endpointConfig);
			assertTrue(pha != null);
			assertEquals(3000, pha.getInvokeTimeout());
		} catch (HostAccessStrategyException e) {
			fail("testConstructor failed " + e);
		}
	}
	
	public void testInvokeWithNoEngine() {
		try {
			HierarchicalConfiguration endpointConfig = loadEndpointConfiguration(CONFIG_FILE, "TheMainframe");
			PooledHostAccessStrategy pha = new PooledHostAccessStrategy(endpointConfig);
			LegStarRequest request = createRequest();
			pha.invoke(request);
			fail("testInvokeWithNoEngine failed");
		} catch (HostAccessStrategyException e) {
			assertEquals("com.legstar.host.server.EngineNotStartedException: The host access engine is not running.",  e.getMessage());
		}
	}
	
	public void testInvokeWithEngine() {
		try {
			/* Start an engine */
			XMLConfiguration config = new XMLConfiguration(CONFIG_FILE);
			EngineHandler engHandler = new EngineHandler(config);
			engHandler.init();
			
			HierarchicalConfiguration endpointConfig = loadEndpointConfiguration(CONFIG_FILE, "TheMainframe");
			PooledHostAccessStrategy pha = new PooledHostAccessStrategy(endpointConfig);
			LegStarRequest request = createRequest();
			pha.invoke(request);
			assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c", 
					Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
			engHandler.stop();
		} catch (HostAccessStrategyException e) {
			fail("testInvokeWithEngine failed " + e);
		} catch (ConfigurationException e) {
			fail("testInvokeWithEngine failed " + e);
		} catch (EngineStartupException e) {
			fail("testInvokeWithEngine failed " + e);
		}
	}
	
	public void testInvokeWithEngineTimeout() {
		EngineHandler engHandler = null;
		try {
			/* Start an engine */
			XMLConfiguration config = new XMLConfiguration(CONFIG_FILE);
			engHandler = new EngineHandler(config);
			engHandler.init();
			
			HierarchicalConfiguration endpointConfig = loadEndpointConfiguration(CONFIG_FILE, "TheMainframe");
			PooledHostAccessStrategy pha = new PooledHostAccessStrategy(endpointConfig);
			LegStarRequest request = createLongRequest();
			pha.invoke(request);
			fail("testInvokeWithEngineTimeout failed ");
		} catch (HostAccessStrategyException e) {
			assertEquals("Timed out waiting for a response for Request:Request01", e.getMessage());
		} catch (ConfigurationException e) {
			fail("testInvokeWithEngine failed " + e);
		} catch (EngineStartupException e) {
			fail("testInvokeWithEngine failed " + e);
		} finally {
			if (engHandler != null) {
				engHandler.stop();
			}
		}
	}
	/** Create a typical request */
	private LegStarRequest createRequest() throws HostAccessStrategyException {
		HashMap < String, Object > map = new HashMap < String, Object >();
		map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAE");
		map.put(Constants.CICS_LENGTH_KEY, "79");
		map.put(Constants.CICS_DATALEN_KEY, "6");
		List <LegStarMessagePart> inputParts = new ArrayList <LegStarMessagePart>();
		LegStarMessagePart inCommarea = new CommareaPart(Util.toByteArray("F0F0F0F1F0F0"));
		inputParts.add(inCommarea);
		LegStarHeaderPart dp;
		try {
			dp = new LegStarHeaderPart(map, inputParts.size());
		} catch (HeaderPartException e) {
			throw new HostAccessStrategyException(e);
		}
		LegStarAddress address = new LegStarAddress("TheMainframe");
		LegStarMessage requestMessage = new LegStarMessage(dp, inputParts);
		LegStarRequest request = new LegStarRequest("Request01", address, requestMessage);
		return request;
	}
	
	/** Create a long request ( 4 secs) */
	private LegStarRequest createLongRequest() throws HostAccessStrategyException {
		HashMap < String, Object > map = new HashMap < String, Object >();
		map.put(Constants.CICS_PROGRAM_NAME_KEY, "T1SLEEPT");
		map.put(Constants.CICS_LENGTH_KEY, "39");
		map.put(Constants.CICS_DATALEN_KEY, "8");
		List <LegStarMessagePart> inputParts = new ArrayList <LegStarMessagePart>();
		LegStarMessagePart inCommarea = new CommareaPart(Util.toByteArray("f0f0f0f0f0f0f0f4"));
		inputParts.add(inCommarea);
		LegStarHeaderPart dp;
		try {
			dp = new LegStarHeaderPart(map, inputParts.size());
		} catch (HeaderPartException e) {
			throw new HostAccessStrategyException(e);
		}
		LegStarAddress address = new LegStarAddress("TheMainframe");
		LegStarMessage requestMessage = new LegStarMessage(dp, inputParts);
		LegStarRequest request = new LegStarRequest("Request01", address, requestMessage);
		return request;
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
		List < ? > endpoints = generalConfig.configurationsAt(strXPath);
		if (endpoints == null || endpoints.isEmpty()) {
			throw new HostAccessStrategyException("The requested endpoint:" 
					+ endpointName
					+ " is not defined.");
		}
		return (HierarchicalConfiguration) endpoints.get(0);
	}

}
