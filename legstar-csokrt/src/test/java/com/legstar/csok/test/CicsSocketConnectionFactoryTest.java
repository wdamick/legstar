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
package com.legstar.csok.test;

import com.legstar.csok.client.CicsSocket;
import com.legstar.csok.client.CicsSocketConnectionFactory;
import com.legstar.csok.client.CicsSocketEndpoint;
import com.legstar.messaging.LegStarAddress;

import junit.framework.TestCase;

/**
 * Test the Socket connection factory.
 *
 */
public class CicsSocketConnectionFactoryTest extends TestCase {
    
    
    /**
     * Test instantiation.
     */
    public void testInstantiation() {
        try {
            CicsSocketConnectionFactory cf = new CicsSocketConnectionFactory();
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
            CicsSocketConnectionFactory cf = new CicsSocketConnectionFactory();
            CicsSocketEndpoint endpoint = AbstractSocketConnectionTester.getCicsTs23Endpoint();
            LegStarAddress address = new LegStarAddress(endpoint.getName());
            CicsSocket conn = (CicsSocket) cf.createConnection(getName(), address, endpoint);
            assertEquals(getName(), conn.getConnectionID());
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
            CicsSocketConnectionFactory cf = new CicsSocketConnectionFactory();
            CicsSocketEndpoint endpoint = AbstractSocketConnectionTester.getCicsTs23Endpoint();
            LegStarAddress address = new LegStarAddress(endpoint.getName());
            address.setHostCharset("IBMTRUC0");
            address.setHostUserID("RANTANPLAN");
            address.setHostPassword("BIDULE");
            address.setHostTraceMode(true);
            CicsSocket conn = (CicsSocket) cf.createConnection(getName(), address, endpoint);
            assertEquals(getName(), conn.getConnectionID());
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
            CicsSocketConnectionFactory cf = new CicsSocketConnectionFactory();
            CicsSocketEndpoint endpoint = AbstractSocketConnectionTester.getCicsTs23Endpoint();
            endpoint.setHostUserID(null);
            endpoint.setHostPassword(null);
            endpoint.setHostCharset(null);
            LegStarAddress address = new LegStarAddress(endpoint.getName());
            cf.createConnection(getName(), address, endpoint);
            fail("testCreateFromEmpyConfig failed ");
        } catch (Exception e) {
            assertEquals("No host character set has been provided.", e.getMessage());
        }
    }

    /**
     * Test with a very empty configuration.
     */
    public void testCreateFromEmpyConfig3() {
        try {
            CicsSocketConnectionFactory cf = new CicsSocketConnectionFactory();
            CicsSocketEndpoint endpoint = new CicsSocketEndpoint();
            LegStarAddress address = new LegStarAddress(endpoint.getName());
            address.setHostCharset("IBMTRUC0");
            address.setHostUserID("RANTANPLAN");
            cf.createConnection(getName(), address, endpoint);
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
            CicsSocketConnectionFactory cf = new CicsSocketConnectionFactory();
            CicsSocketEndpoint endpoint = new CicsSocketEndpoint();
            endpoint.setHostIPAddress("192.168.0.111");
            LegStarAddress address = new LegStarAddress(endpoint.getName());
            address.setHostCharset("IBMTRUC0");
            address.setHostUserID("RANTANPLAN");
            cf.createConnection(getName(), address, endpoint);
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
            CicsSocketConnectionFactory cf = new CicsSocketConnectionFactory();
            CicsSocketEndpoint endpoint = AbstractSocketConnectionTester.getCicsTs23Endpoint();
            LegStarAddress address = new LegStarAddress(endpoint.getName());
            CicsSocket conn = (CicsSocket) cf.createConnection(getName(), address, endpoint);
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
            CicsSocketConnectionFactory cf = new CicsSocketConnectionFactory();
            CicsSocketEndpoint endpoint = AbstractSocketConnectionTester.getCicsTs23Endpoint();
            endpoint.setConnectTimeout(2000);
            endpoint.setReceiveTimeout(7000);
            LegStarAddress address = new LegStarAddress(endpoint.getName());
            CicsSocket conn = (CicsSocket) cf.createConnection(getName(), address, endpoint);
            assertEquals(2000, conn.getConnectTimeout());
            assertEquals(7000, conn.getReceiveTimeout());
        } catch (Exception e) {
            fail("testCreateWithDefaultTimeouts failed" + e.getMessage());
        }
    }
}
