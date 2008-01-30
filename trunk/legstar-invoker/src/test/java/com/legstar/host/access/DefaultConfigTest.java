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
		List < ? >  endpoints = generalConfig.configurationsAt(strXPath);
		if (endpoints == null || endpoints.isEmpty()) {
			throw new HostAccessStrategyException(
					"No default endpoint is not defined.");
		}
		HierarchicalConfiguration sub = (HierarchicalConfiguration) endpoints.get(0);
		assertEquals("TheMainframe", sub.getString("@name"));
		
		
	}

}
