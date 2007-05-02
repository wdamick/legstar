package com.legstar.config;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;

import com.legstar.messaging.Address;
import com.legstar.messaging.ConnectionFactory;

import junit.framework.TestCase;

public class ConfigTest extends TestCase {

	private static final String CONFIG_FILE = "config.xml";
	
	public void testloadConnectionFactory() {
		try {
			Address address = new Address("TheMainframe");
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
