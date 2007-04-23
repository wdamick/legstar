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

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;

import com.legstar.config.Config;

import junit.framework.TestCase;

public class CicsHttpEndpointTest extends TestCase {

	private static final String CONFIG_FILE = "config.xml";
	/** Configuration XPath location for an endpoint. */

	public void testInstanciation() {
		try {
			HierarchicalConfiguration endpointConfig =
				Config.loadEndpointConfiguration(
						Config.loadGeneralConfig(CONFIG_FILE), "TheMainframe");
			CicsHttpEndpoint cicsHttpEndpoint = new CicsHttpEndpoint(endpointConfig);
			assertEquals("IBM01140", cicsHttpEndpoint.getHostCharset());
			assertEquals("192.168.0.110", cicsHttpEndpoint.getHostIPAddress());
			assertEquals(3080, cicsHttpEndpoint.getHostIPPort());
			assertEquals("STREAM2", cicsHttpEndpoint.getHostPassword());
			assertEquals("/CICS/CWBA/LSWEBBIN", cicsHttpEndpoint.getHostURLPath());
			assertEquals("P390", cicsHttpEndpoint.getHostUserID());
			System.out.println(cicsHttpEndpoint.getReport());
		} catch (ConfigurationException e) {
			fail("testInstanciation failed " + e);
		}
		
	}

	public void testInstanciation2() {
		try {
			HierarchicalConfiguration endpointConfig =
				Config.loadEndpointConfiguration(
						Config.loadGeneralConfig("config1.xml"), "TheMainframe");
			CicsHttpEndpoint cicsHttpEndpoint = new CicsHttpEndpoint(endpointConfig);
			assertEquals(null, cicsHttpEndpoint.getHostCharset());
			assertEquals(null, cicsHttpEndpoint.getHostIPAddress());
			assertEquals(0, cicsHttpEndpoint.getHostIPPort());
			assertEquals(null, cicsHttpEndpoint.getHostPassword());
			assertEquals("/CICS/CWBA/LSWEBBIN", cicsHttpEndpoint.getHostURLPath());
			assertEquals(null, cicsHttpEndpoint.getHostUserID());
			System.out.println(cicsHttpEndpoint.getReport());
		} catch (ConfigurationException e) {
			fail("testInstanciation failed " + e);
		}
		
	}

}
