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

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;

import com.legstar.config.Config;
import com.legstar.host.AbstractTester;

/**
 * Test the HostAccessStrategyFactory.
 *
 */
public class HostAccessStrategyFactoryTest extends AbstractTester {

    /**
     * Test the default behavior.
     */
    public void testConstructorWithDefaultStrategy() {
        try {
            HierarchicalConfiguration endpointConfig = Config.loadEndpointConfiguration(CONFIG_FILE, "TheMainframe");
            HostAccessStrategy has = HostAccessStrategyFactory.createAccessStrategy(endpointConfig);
            assertEquals("class com.legstar.host.access.DirectHostAccessStrategy", has.getClass().toString());
        } catch (HostAccessStrategyException e) {
            fail("testConstructor failed " + e.getMessage());
        } catch (ConfigurationException e) {
            fail("testConstructor failed " + e.getMessage());
        }
    }

    /**
     * Get a pooled configuration.
     */
    public void testConstructorWithPooledStrategy() {
        try {
            HierarchicalConfiguration endpointConfig = Config.loadEndpointConfiguration("config2.xml", "TheMainframe");
            HostAccessStrategy has = HostAccessStrategyFactory.createAccessStrategy(endpointConfig);
            assertEquals("class com.legstar.host.access.PooledHostAccessStrategy", has.getClass().toString());
        } catch (HostAccessStrategyException e) {
            fail("testConstructor failed " + e.getMessage());
        } catch (ConfigurationException e) {
            fail("testConstructor failed " + e.getMessage());
        }
    }

    /**
     * Check with invalid strategy.
     */
    public void testConstructorWithInvalidStrategy() {
        try {
            HierarchicalConfiguration endpointConfig = Config.loadEndpointConfiguration("config3.xml", "TheMainframe");
            HostAccessStrategyFactory.createAccessStrategy(endpointConfig);
            fail("testConstructorWithInvalidStrategy failed ");
        } catch (HostAccessStrategyException e) {
            assertEquals("Unknown host access strategy.", e.getMessage());
        } catch (ConfigurationException e) {
            fail("testConstructor failed " + e.getMessage());
        }
    }
}
