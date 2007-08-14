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
package com.legstar.mq.client;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;

import com.legstar.config.Config;

import junit.framework.TestCase;

public class CicsMQEndpointTest extends TestCase {

	private static final String CONFIG_FILE = "config.xml";
	/** Configuration XPath location for an endpoint. */

	public void testInstanciation() {
		try {
			HierarchicalConfiguration endpointConfig =
				Config.loadEndpointConfiguration(
						Config.loadGeneralConfig(CONFIG_FILE), "TheMainframe");
			CicsMQEndpoint cicsMQEndpoint = new CicsMQEndpoint(endpointConfig);
			assertEquals("IBM01140", cicsMQEndpoint.getHostCharset());
			assertEquals("192.168.0.110", cicsMQEndpoint.getHostIPAddress());
			assertEquals(1414, cicsMQEndpoint.getHostIPPort());
			assertEquals("STREAM2", cicsMQEndpoint.getHostPassword());
			assertEquals("CSQ1", cicsMQEndpoint.getHostMQManager());
			assertEquals("CLIENT.TO.CSQ1", cicsMQEndpoint.getHostMQChannel());
			assertEquals("CICSA.REQUEST.QUEUE", cicsMQEndpoint.getHostMQRequestQueue());
			assertEquals("CICSA.REPLY.QUEUE", cicsMQEndpoint.getHostMQResponseQueue());
			assertEquals("P390", cicsMQEndpoint.getHostUserID());
			System.out.println(cicsMQEndpoint.getReport());
		} catch (ConfigurationException e) {
			fail("testInstanciation failed " + e);
		}
		
	}

	public void testInstanciation2() {
		try {
			HierarchicalConfiguration endpointConfig =
				Config.loadEndpointConfiguration(
						Config.loadGeneralConfig("config1.xml"), "TheMainframe");
			CicsMQEndpoint cicsMQEndpoint = new CicsMQEndpoint(endpointConfig);
			assertEquals(1414, cicsMQEndpoint.getHostIPPort());
			assertEquals("CSQ1", cicsMQEndpoint.getHostMQManager());
			System.out.println(cicsMQEndpoint.getReport());
		} catch (ConfigurationException e) {
			fail("testInstanciation failed " + e);
		}
		
	}

}
