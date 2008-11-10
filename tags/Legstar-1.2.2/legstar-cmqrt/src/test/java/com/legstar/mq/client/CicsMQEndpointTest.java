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
