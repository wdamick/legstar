/*******************************************************************************
 * Copyright (c) 2009 LegSem.
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
import org.apache.commons.configuration.XMLConfiguration;

import com.legstar.config.Config;
import com.legstar.coxb.host.HostData;
import com.legstar.host.AbstractTester;
import com.legstar.host.server.EngineHandler;
import com.legstar.host.server.EngineStartupException;
import com.legstar.messaging.LegStarRequest;
import com.legstar.test.coxb.LsfileaeCases;

/**
 * Test the PooledHostAccessStrategy.
 *
 */
public class PooledHostAccessStrategyTest extends AbstractTester {

    /**
     * Test the default behavior.
     */
    public void testConstructor() {
        try {
            HierarchicalConfiguration endpointConfig = Config.loadEndpointConfiguration(
                    CONFIG_FILE, "TheMainframe");
            PooledHostAccessStrategy pha = new PooledHostAccessStrategy(endpointConfig);
            assertTrue(pha != null);
            assertEquals(2000, pha.getInvokeTimeout());
       } catch (ConfigurationException e) {
            fail("testConstructor failed " + e);
        }
    }

    /**
     * Test the default timeout.
     */
    public void testConstructorWithNoConfiguredTimeout() {
        try {
            HierarchicalConfiguration endpointConfig = Config.loadEndpointConfiguration(
                    "config1.xml", "TheMainframe");
            PooledHostAccessStrategy pha = new PooledHostAccessStrategy(endpointConfig);
            assertTrue(pha != null);
            assertEquals(3000, pha.getInvokeTimeout());
        } catch (ConfigurationException e) {
            fail("testConstructor failed " + e);
        }
    }

    /**
     * Test that invoke fails nicely when engine is not started.
     */
    public void testInvokeWithNoEngine() {
        try {
            HierarchicalConfiguration endpointConfig = Config.loadEndpointConfiguration(
                    CONFIG_FILE, "TheMainframe");
            PooledHostAccessStrategy pha = new PooledHostAccessStrategy(endpointConfig);
            LegStarRequest request = createLsfileaeRequest();
            pha.invoke(request);
            fail("testInvokeWithNoEngine failed");
        } catch (ConfigurationException e) {
            fail("testConstructor failed " + e);
        } catch (HostAccessStrategyException e) {
            assertEquals("com.legstar.host.server.EngineNotStartedException:"
                    + " The host access engine is not running.",  e.getMessage());
        }
    }

    /**
     * Test that invoke works when engine started.
     */
    public void testInvokeWithEngine() {
        try {
            /* Start an engine */
            XMLConfiguration config = new XMLConfiguration(CONFIG_FILE);
            EngineHandler engHandler = new EngineHandler(config);
            engHandler.init();

            HierarchicalConfiguration endpointConfig = Config.loadEndpointConfiguration(CONFIG_FILE, "TheMainframe");
            PooledHostAccessStrategy pha = new PooledHostAccessStrategy(endpointConfig);
            LegStarRequest request = createLsfileaeRequest();
            pha.invoke(request);
            assertEquals(LsfileaeCases.getHostBytesHexReply100(), 
                    HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
            engHandler.stop();
        } catch (HostAccessStrategyException e) {
            fail("testInvokeWithEngine failed " + e);
        } catch (ConfigurationException e) {
            fail("testInvokeWithEngine failed " + e);
        } catch (EngineStartupException e) {
            fail("testInvokeWithEngine failed " + e);
        }
    }

    /**
     * Test invoke which will timeout.
     */
    public void testInvokeWithEngineTimeout() {
        EngineHandler engHandler = null;
        try {
            /* Start an engine */
            XMLConfiguration config = new XMLConfiguration(CONFIG_FILE);
            engHandler = new EngineHandler(config);
            engHandler.init();

            HierarchicalConfiguration endpointConfig = Config.loadEndpointConfiguration(
                    CONFIG_FILE, "TheMainframe");
            PooledHostAccessStrategy pha = new PooledHostAccessStrategy(endpointConfig);
            LegStarRequest request = createT1sleeptRequest();
            pha.invoke(request);
            fail("testInvokeWithEngineTimeout failed ");
        } catch (HostAccessStrategyException e) {
            assertEquals("Timed out waiting for a response for Request:Request01", e.getMessage());
        } catch (ConfigurationException e) {
            fail("testInvokeWithEngine failed " + e);
        } catch (EngineStartupException e) {
            fail("testInvokeWithEngine failed " + e);
        } finally {
            if (engHandler != null) {
                engHandler.stop();
            }
        }
    }
}
