/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.http.client;

import junit.framework.TestCase;

/**
 * Test the Http endpoint.
 *
 */
public class CicsHttpEndpointTest extends TestCase {

    /**
     * Instantiate from full configuration.
     */
    public void testInstantiation() {
        CicsHttpEndpoint endpoint = new CicsHttpEndpoint(
                AbstractHttpConnectionTester.getCicsTs23Endpoint());
        assertEquals("IBM01140", endpoint.getHostCharset());
        assertEquals("mainframe", endpoint.getHostIPAddress());
        assertEquals(3080, endpoint.getHostIPPort());
        assertEquals("STREAM2", endpoint.getHostPassword());
        assertEquals("/CICS/CWBA/LSWEBBIN", endpoint.getHostURLPath());
        assertEquals("P390", endpoint.getHostUserID());
        assertEquals("CICS Http endpoint:[hostEndpoint=CICSTS23,"
                + "hostCharset=IBM01140,"
                + "hostUserID=P390,"
                + "hostPassword=********,"
                + "hostTraceMode=false,"
                + "connectTimeout=1000,"
                + "receiveTimeout=5000,"
                + "hostConnectionfactoryClass=com.legstar.http.client.CicsHttpConnectionFactory,"
                + "hostAccessStrategy=direct,"
                + "hostConnectionPoolSize=5,"
                + "pooledInvokeTimeout=3000]"
                + "[hostURLProtocol=http,"
                + "hostIPAddress=mainframe,"
                + "hostIPPort=3080,"
                + "hostURLPath=/CICS/CWBA/LSWEBBIN]",
                endpoint.toString());
    }

    /**
     * Instantiate from empty configuration.
     */
    public void testInstantiation2() {
        CicsHttpEndpoint endpoint = new CicsHttpEndpoint();
        assertEquals("IBM01140", endpoint.getHostCharset());
        assertEquals(null, endpoint.getHostIPAddress());
        assertEquals(0, endpoint.getHostIPPort());
        assertEquals(null, endpoint.getHostPassword());
        assertEquals("/CICS/CWBA/LSWEBBIN", endpoint.getHostURLPath());
        assertEquals(null, endpoint.getHostUserID());
        assertEquals("CICS Http endpoint:[hostEndpoint=null,"
                + "hostCharset=IBM01140,"
                + "hostUserID=null,"
                + "hostPassword=********,"
                + "hostTraceMode=false,"
                + "connectTimeout=1000,"
                + "receiveTimeout=5000,"
                + "hostConnectionfactoryClass=com.legstar.http.client.CicsHttpConnectionFactory,"
                + "hostAccessStrategy=direct,"
                + "hostConnectionPoolSize=5,"
                + "pooledInvokeTimeout=3000]"
                + "[hostURLProtocol=http,"
                + "hostIPAddress=null,"
                + "hostIPPort=0,"
                + "hostURLPath=/CICS/CWBA/LSWEBBIN]",
                endpoint.toString());
    }

}
