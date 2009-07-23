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


import junit.framework.TestCase;

/**
 * Test the LegStarAddress class.
 * Primarily we are interested in how this class gets generated using configuration files.
 *
 */
public class LegStarAddressTest extends TestCase {

    /**
     * Test construction from a configuration file.
     */
    public void testContructorFromConfig() {
        LegStarAddress address;
        address = new LegStarAddress(getHostEndpoint());
        assertEquals("TheMainframe", address.getEndPointName());
        assertEquals("IBM01140", address.getHostCharset());
        assertEquals("P390", address.getHostUserID());
        assertEquals("STREAM2", address.getHostPassword());
        assertEquals(true, address.isHostTraceMode());
    }

    /**
     * Test construction from a configuration file with minimal endpoint parameters.
     */
    public void testContructorFromEmptyConfig() {
        LegStarAddress address;
        address = new LegStarAddress(getHostEndpointMinimal());
        assertEquals("TheMainframeMinimal", address.getEndPointName());
        assertEquals("IBM01140", address.getHostCharset());
        assertEquals(null, address.getHostUserID());
        assertEquals(null, address.getHostPassword());
        assertEquals(false, address.isHostTraceMode());
    }

    /**
     * Test the ability to complement a partial address using a configured endpoint.
     */
    public void testContructorFromPartialAddress() {
        LegStarAddress partialAddress = new LegStarAddress("AnotherMainframe");
        LegStarAddress address;
        address = new LegStarAddress(partialAddress, getHostEndpoint());
        assertEquals("AnotherMainframe", address.getEndPointName());
        assertEquals("IBM01140", address.getHostCharset());
        assertEquals("P390", address.getHostUserID());
        assertEquals("STREAM2", address.getHostPassword());
        assertEquals(true, address.isHostTraceMode());
    }
    
    
    /**
     * @return a sample host endpoint
     */
    private HostEndpoint getHostEndpoint() {
        HostEndpoint endpoint = (new HostEndpointTest()).new HostEndpointImpl();
        endpoint.setHostConnectionfactoryClass("com.legstar.csok.client.CicsSocketConnectionFactory");
        endpoint.setName("TheMainframe");
        endpoint.setHostCharset("IBM01140");
        endpoint.setHostUserID("P390");
        endpoint.setHostPassword("STREAM2");
        endpoint.setHostTraceMode(true);
        endpoint.setPooledInvokeTimeout(2000);
        return endpoint;
    }

    /**
     * @return a minimal sample host endpoint
     */
    private HostEndpoint getHostEndpointMinimal() {
        HostEndpoint endpoint = (new HostEndpointTest()).new HostEndpointImpl();
        endpoint.setName("TheMainframeMinimal");
        return endpoint;
    }
}
