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
package com.legstar.messaging;

import org.apache.commons.configuration.ConfigurationException;

import com.legstar.config.Config;

import junit.framework.TestCase;

/**
 * Test the LegStarAddress class.
 * Primarily we are interested in how this class gets generated using configuration files.
 *
 */
public class LegStarAddressTest extends TestCase {

    /** The configuration file. */
    private static final String CONFIG_FILE = "config.xml";

    /**
     * Test construction from a configuration file.
     */
    public void testContructorFromConfig() {
        LegStarAddress address;
        try {
            address = new LegStarAddress(Config.loadEndpointConfiguration(CONFIG_FILE, "TheMainframe"));
            assertEquals("TheMainframe", address.getEndPointName());
            assertEquals("IBM01140", address.getHostCharset());
            assertEquals("P390", address.getHostUserID());
            assertEquals("STREAM2", address.getHostPassword());
            assertEquals(true, address.isHostTraceMode());
        } catch (ConfigurationException e) {
            fail("tesContructorFromConfig failed " + e);
        }
    }

    /**
     * Test construction from a configuration file with minimal endpoint parameters.
     */
    public void testContructorFromEmptyConfig() {
        LegStarAddress address;
        try {
            address = new LegStarAddress(Config.loadEndpointConfiguration(CONFIG_FILE, "TheMainframeMinimal"));
            assertEquals("TheMainframeMinimal", address.getEndPointName());
            assertEquals(null, address.getHostCharset());
            assertEquals(null, address.getHostUserID());
            assertEquals(null, address.getHostPassword());
            assertEquals(false, address.isHostTraceMode());
        } catch (ConfigurationException e) {
            fail("testContructorFromEmptyConfig failed " + e);
        }
    }

    /**
     * Test the ability to complement a partial address using a configured endpoint.
     */
    public void testContructorFromPartialAddress() {
        LegStarAddress partialAddress = new LegStarAddress("AnotherMainframe");
        LegStarAddress address;
        try {
            address = new LegStarAddress(partialAddress, Config.loadEndpointConfiguration(CONFIG_FILE, "TheMainframe"));
            assertEquals("AnotherMainframe", address.getEndPointName());
            assertEquals("IBM01140", address.getHostCharset());
            assertEquals("P390", address.getHostUserID());
            assertEquals("STREAM2", address.getHostPassword());
            assertEquals(true, address.isHostTraceMode());
        } catch (ConfigurationException e) {
            fail("testContructorFromEmptyConfig failed " + e);
        }
    }

}
