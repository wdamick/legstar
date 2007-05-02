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
