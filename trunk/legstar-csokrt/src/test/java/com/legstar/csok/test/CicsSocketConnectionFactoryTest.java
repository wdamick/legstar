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
package com.legstar.csok.test;

import org.apache.commons.configuration.HierarchicalConfiguration;

import com.legstar.config.Config;
import com.legstar.csok.client.CicsSocket;
import com.legstar.csok.client.CicsSocketConnectionFactory;
import com.legstar.messaging.LegStarAddress;

import junit.framework.TestCase;

/**
 * Test the Socket connection factory.
 *
 */
public class CicsSocketConnectionFactoryTest extends TestCase {
    
    
    /** Configuration file.*/
    private static final String CONFIG_FILE = "config.xml";
    
    /** An endpoint defined in the configuration file.*/
    private static final String ENDPOINT_NAME = "CICSTS23";

    /**
     * Test instantiation.
     */
    public void testInstantiation() {
        try {
            HierarchicalConfiguration endpointConfig = Config.loadEndpointConfiguration(CONFIG_FILE, ENDPOINT_NAME);
            CicsSocketConnectionFactory cf =
                new CicsSocketConnectionFactory(endpointConfig);
            assertTrue(cf != null);
        } catch (Exception e) {
            fail("testInstanciation failed" + e);
        }
    }

    /**
     * Get user/password and charset from the configuration file.
     */
    public void testCreateNoUserNoPasswordNoCharset() {
        try {
            HierarchicalConfiguration endpointConfig = Config.loadEndpointConfiguration(CONFIG_FILE, ENDPOINT_NAME);
            CicsSocketConnectionFactory cf =
                new CicsSocketConnectionFactory(endpointConfig);
            LegStarAddress address = new LegStarAddress(ENDPOINT_NAME);
            CicsSocket conn = (CicsSocket) cf.createConnection("testCreateNoUserNoPasswordNoCharset", address);
            assertEquals("testCreateNoUserNoPasswordNoCharset", conn.getConnectionID());
            assertEquals("IBM01140", conn.getCicsSocketEndpoint().getHostCharset());
            assertEquals("P390", conn.getCicsSocketEndpoint().getHostUserID());
            assertEquals(false, conn.getCicsSocketEndpoint().isHostTraceMode());
        } catch (Exception e) {
            fail("testCreateWrongEndPoint failed " + e);
        }
    }

    /**
     * Get user/password and charset from the address.
     */
    public void testCreateUserPasswordCharset() {
        try {
            HierarchicalConfiguration endpointConfig = Config.loadEndpointConfiguration(CONFIG_FILE, ENDPOINT_NAME);
            CicsSocketConnectionFactory cf =
                new CicsSocketConnectionFactory(endpointConfig);
            LegStarAddress address = new LegStarAddress(ENDPOINT_NAME);
            address.setHostCharset("IBMTRUC0");
            address.setHostUserID("RANTANPLAN");
            address.setHostPassword("BIDULE");
            address.setHostTraceMode(true);
            CicsSocket conn = (CicsSocket) cf.createConnection("testCreateNoUserNoPasswordNoCharset", address);
            assertEquals("testCreateNoUserNoPasswordNoCharset", conn.getConnectionID());
            assertEquals("IBMTRUC0", conn.getCicsSocketEndpoint().getHostCharset());
            assertEquals("RANTANPLAN", conn.getCicsSocketEndpoint().getHostUserID());
            assertEquals(true, conn.getCicsSocketEndpoint().isHostTraceMode());
        } catch (Exception e) {
            fail("testCreateWrongEndPoint failed " + e);
        }
    }

    /**
     * Check what happens if no user/password charset is provided at all.
     */
    public void testCreateFromEmpyConfig() {
        try {
            HierarchicalConfiguration endpointConfig = Config.loadEndpointConfiguration("config0.xml", ENDPOINT_NAME);
            CicsSocketConnectionFactory cf =
                new CicsSocketConnectionFactory(endpointConfig);
            LegStarAddress address = new LegStarAddress(ENDPOINT_NAME);
            cf.createConnection("testCreateNoUserNoPasswordNoCharset", address);
            fail("testCreateFromEmpyConfig failed ");
        } catch (Exception e) {
            assertEquals("No host character set has been provided.", e.getMessage());
        }
    }

    /**
     * Check what happens if charset is provided but no user/password.
     */
    public void testCreateFromEmpyConfig2() {
        try {
            HierarchicalConfiguration endpointConfig = Config.loadEndpointConfiguration("config0.xml", ENDPOINT_NAME);
            CicsSocketConnectionFactory cf =
                new CicsSocketConnectionFactory(endpointConfig);
            LegStarAddress address = new LegStarAddress(ENDPOINT_NAME);
            address.setHostCharset("IBMTRUC0");
            cf.createConnection("testCreateNoUserNoPasswordNoCharset", address);
            fail("testCreateFromEmpyConfig failed ");
        } catch (Exception e) {
            assertEquals("No host user ID has been provided.", e.getMessage());
        }
    }

    /**
     * Test with a very empty configuration.
     */
    public void testCreateFromEmpyConfig3() {
        try {
            HierarchicalConfiguration endpointConfig = Config.loadEndpointConfiguration("config1.xml", ENDPOINT_NAME);
            CicsSocketConnectionFactory cf =
                new CicsSocketConnectionFactory(endpointConfig);
            LegStarAddress address = new LegStarAddress(ENDPOINT_NAME);
            address.setHostCharset("IBMTRUC0");
            address.setHostUserID("RANTANPLAN");
            cf.createConnection("testCreateNoUserNoPasswordNoCharset", address);
            fail("testCreateFromEmpyConfig failed ");
        } catch (Exception e) {
            assertEquals("No host IP address has been provided.", e.getMessage());
        }
    }

    /**
     * Now only IP address is in the configuration file.
     */
    public void testCreateFromEmpyConfig4() {
        try {
            HierarchicalConfiguration endpointConfig = Config.loadEndpointConfiguration("config2.xml", ENDPOINT_NAME);
            CicsSocketConnectionFactory cf =
                new CicsSocketConnectionFactory(endpointConfig);
            LegStarAddress address = new LegStarAddress(ENDPOINT_NAME);
            address.setHostCharset("IBMTRUC0");
            address.setHostUserID("RANTANPLAN");
            cf.createConnection("testCreateNoUserNoPasswordNoCharset", address);
            fail("testCreateFromEmpyConfig failed ");
        } catch (Exception e) {
            assertEquals("No host IP port has been provided.", e.getMessage());
        }
    }

    /**
     * Check the default timeout values.
     */
    public void testCreateWithDefaultTimeouts() {
        try {
            HierarchicalConfiguration endpointConfig = Config.loadEndpointConfiguration(CONFIG_FILE, ENDPOINT_NAME);
            CicsSocketConnectionFactory cf =
                new CicsSocketConnectionFactory(endpointConfig);
            LegStarAddress address = new LegStarAddress(ENDPOINT_NAME);
            CicsSocket conn = (CicsSocket) cf.createConnection("testCreateWithDefaultTimeouts", address);
            assertEquals(1000, conn.getConnectTimeout());
            assertEquals(5000, conn.getReceiveTimeout());
        } catch (Exception e) {
            fail("testCreateWithDefaultTimeouts failed" + e.getMessage());
        }
    }

    /**
     * Test with a config file that provides timeout values.
     */
    public void testCreateWithTimeoutsFromConfig() {
        try {
            HierarchicalConfiguration endpointConfig = Config.loadEndpointConfiguration("config3.xml", ENDPOINT_NAME);
            CicsSocketConnectionFactory cf =
                new CicsSocketConnectionFactory(endpointConfig);
            LegStarAddress address = new LegStarAddress(ENDPOINT_NAME);
            CicsSocket conn = (CicsSocket) cf.createConnection("testCreateWithDefaultTimeouts", address);
            assertEquals(2000, conn.getConnectTimeout());
            assertEquals(7000, conn.getReceiveTimeout());
        } catch (Exception e) {
            fail("testCreateWithDefaultTimeouts failed" + e.getMessage());
        }
    }
}
