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
package com.legstar.config;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;

import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.ConnectionFactory;

import junit.framework.TestCase;

public class ConfigTest extends TestCase {

	private static final String CONFIG_FILE = "config.xml";
	
	public void testloadConnectionFactory() {
		try {
			LegStarAddress address = new LegStarAddress("TheMainframe");
			HierarchicalConfiguration generalConfig = Config.loadGeneralConfig(CONFIG_FILE);
			HierarchicalConfiguration endpointConfig =
				Config.loadAddressConfiguration(generalConfig, address);
			@SuppressWarnings("unused")
			ConnectionFactory connectionFactory = Config.loadConnectionFactory(endpointConfig);
			fail("testloadConnectionFactory");
		} catch (ConfigurationException e) {
			assertEquals("java.lang.ClassNotFoundException: com.legstar.csok.client.CicsSocketConnectionFactory", e.getMessage());
		}
	}

}
