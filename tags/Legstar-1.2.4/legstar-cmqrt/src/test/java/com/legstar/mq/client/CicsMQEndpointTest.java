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
package com.legstar.mq.client;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;

import com.legstar.config.Config;

import junit.framework.TestCase;

/**
 * Test the WMQ endpoint.
 *
 */
public class CicsMQEndpointTest extends TestCase {

    /** Configuration file.*/
    private static final String CONFIG_FILE = "config.xml";

    /** An endpoint defined in the configuration file.*/
    private static final String LSMSG_ENDPOINT_NAME = "CICSTS23-LSMSG";

    /** An endpoint defined in the configuration file.*/
    private static final String MQCIH_ENDPOINT_NAME = "CICSTS23-MQCIH";
    /**
     * Instantiate from full configuration.
     */
    public void testInstantiation() {
        try {
            HierarchicalConfiguration endpointConfig =
                Config.loadEndpointConfiguration(CONFIG_FILE, LSMSG_ENDPOINT_NAME);
            CicsMQEndpoint cicsMQEndpoint = new CicsMQEndpoint(endpointConfig);
            assertEquals("IBM01140", cicsMQEndpoint.getHostCharset());
            assertEquals("mainframe", cicsMQEndpoint.getHostIPAddress());
            assertEquals(1414, cicsMQEndpoint.getHostIPPort());
            assertEquals("STREAM2", cicsMQEndpoint.getHostPassword());
            assertEquals("CSQ1", cicsMQEndpoint.getHostMQManager());
            assertEquals("CLIENT.TO.CSQ1", cicsMQEndpoint.getHostMQChannel());
            assertEquals("CICSA.REQUEST.QUEUE", cicsMQEndpoint.getHostMQRequestQueue());
            assertEquals("CICSA.REPLY.QUEUE", cicsMQEndpoint.getHostMQResponseQueue());
            assertEquals("P390", cicsMQEndpoint.getHostUserID());
            assertEquals("LSMSG", cicsMQEndpoint.getHostMQBridgeType().toString());
            System.out.println(cicsMQEndpoint.getReport());
        } catch (ConfigurationException e) {
            fail(e.getMessage());
        }

    }

    /**
     * Instantiate from empty configuration.
     */
    public void testInstantiation2() {
        try {
            HierarchicalConfiguration endpointConfig =
                Config.loadEndpointConfiguration("config1.xml", LSMSG_ENDPOINT_NAME);
            CicsMQEndpoint cicsMQEndpoint = new CicsMQEndpoint(endpointConfig);
            assertEquals(1414, cicsMQEndpoint.getHostIPPort());
            assertEquals("CSQ1", cicsMQEndpoint.getHostMQManager());
            assertEquals("LSMSG", cicsMQEndpoint.getHostMQBridgeType().toString());
            System.out.println(cicsMQEndpoint.getReport());
        } catch (ConfigurationException e) {
            fail(e.getMessage());
        }

    }

    /**
     * Instantiate with IBM MQCIH.
     */
    public void testInstantiationMqcih() {
        try {
            HierarchicalConfiguration endpointConfig = Config.loadEndpointConfiguration(
                    CONFIG_FILE, MQCIH_ENDPOINT_NAME);
            CicsMQEndpoint cicsMQEndpoint = new CicsMQEndpoint(endpointConfig);
            assertEquals("IBM01140", cicsMQEndpoint.getHostCharset());
            assertEquals("mainframe", cicsMQEndpoint.getHostIPAddress());
            assertEquals(1414, cicsMQEndpoint.getHostIPPort());
            assertEquals("STREAM2", cicsMQEndpoint.getHostPassword());
            assertEquals("CSQ1", cicsMQEndpoint.getHostMQManager());
            assertEquals("CLIENT.TO.CSQ1", cicsMQEndpoint.getHostMQChannel());
            assertEquals("CICS01.BRIDGE.REQUEST.QUEUE", cicsMQEndpoint.getHostMQRequestQueue());
            assertEquals("CICS01.BRIDGE.REPLY.QUEUE", cicsMQEndpoint.getHostMQResponseQueue());
            assertEquals("P390", cicsMQEndpoint.getHostUserID());
            assertEquals("MQCIH", cicsMQEndpoint.getHostMQBridgeType().toString());
            System.out.println(cicsMQEndpoint.getReport());
        } catch (ConfigurationException e) {
            fail(e.getMessage());
        }

    }
}
