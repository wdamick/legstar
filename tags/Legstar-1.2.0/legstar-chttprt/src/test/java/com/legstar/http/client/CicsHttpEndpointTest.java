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
