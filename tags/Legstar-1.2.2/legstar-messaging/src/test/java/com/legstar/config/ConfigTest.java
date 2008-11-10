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
