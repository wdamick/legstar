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
package com.legstar.http.client;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;

import com.legstar.config.Config;

import junit.framework.TestCase;

/**
 * Test the Http endpoint.
 *
 */
public class CicsHttpEndpointTest extends TestCase {

    /** Configuration file.*/
    private static final String CONFIG_FILE = "config.xml";

    /** An endpoint defined in the configuration file.*/
    private static final String ENDPOINT_NAME = "CICSTS23";

    /**
     * Instantiate from full configuration.
     */
    public void testInstantiation() {
        try {
            HierarchicalConfiguration endpointConfig = Config.loadEndpointConfiguration(CONFIG_FILE, ENDPOINT_NAME);
            CicsHttpEndpoint cicsHttpEndpoint = new CicsHttpEndpoint(endpointConfig);
            assertEquals("IBM01140", cicsHttpEndpoint.getHostCharset());
            assertEquals("mainframe", cicsHttpEndpoint.getHostIPAddress());
            assertEquals(3080, cicsHttpEndpoint.getHostIPPort());
            assertEquals("STREAM2", cicsHttpEndpoint.getHostPassword());
            assertEquals("/CICS/CWBA/LSWEBBIN", cicsHttpEndpoint.getHostURLPath());
            assertEquals("P390", cicsHttpEndpoint.getHostUserID());
            System.out.println(cicsHttpEndpoint.getReport());
        } catch (ConfigurationException e) {
            fail("testInstanciation failed " + e);
        }

    }

    /**
     * Instantiate from empty configuration.
     */
    public void testInstantiation2() {
        try {
            HierarchicalConfiguration endpointConfig = Config.loadEndpointConfiguration("config1.xml", ENDPOINT_NAME);
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
