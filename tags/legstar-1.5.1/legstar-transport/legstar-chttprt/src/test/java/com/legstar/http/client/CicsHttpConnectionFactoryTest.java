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

import com.legstar.messaging.LegStarAddress;

import junit.framework.TestCase;

/**
 * Test the Http connection factory.
 *
 */
public class CicsHttpConnectionFactoryTest extends TestCase {

    /** An endpoint defined in the configuration file.*/
    private static final String ENDPOINT_NAME = "CICSTS23";


    /**
     * Test instantiation.
     */
    public void testInstantiation() {
        try {
            CicsHttpConnectionFactory cf = new CicsHttpConnectionFactory();
            assertTrue(cf != null);
        } catch (Exception e) {
            fail("testInstantiation failed" + e);
        }
    }

    /**
     * Get user/password and charset from the configuration file.
     */
    public void testCreateNoUserNoPasswordNoCharset() {
        try {
            CicsHttpConnectionFactory cf = new CicsHttpConnectionFactory();
            LegStarAddress address = new LegStarAddress(ENDPOINT_NAME);
            CicsHttp conn = (CicsHttp) cf.createConnection("testCreateNoUserNoPasswordNoCharset",
                    address, AbstractHttpConnectionTester.getCicsTs23Endpoint());
            assertEquals("testCreateNoUserNoPasswordNoCharset", conn.getConnectionID());
            assertEquals("IBM01140", conn.getCicsHttpEndpoint().getHostCharset());
            assertEquals("P390", conn.getCicsHttpEndpoint().getHostUserID());
            assertEquals(false, conn.getCicsHttpEndpoint().isHostTraceMode());
        } catch (Exception e) {
            fail("testCreateWrongEndPoint failed " + e);
        }
    }

    /**
     * Get user/password and charset from the address.
     */
    public void testCreateUserPasswordCharset() {
        try {
            CicsHttpConnectionFactory cf = new CicsHttpConnectionFactory();
            LegStarAddress address = new LegStarAddress(ENDPOINT_NAME);
            address.setHostCharset("IBMTRUC0");
            address.setHostUserID("RANTANPLAN");
            address.setHostPassword("BIDULE");
            address.setHostTraceMode(true);
            CicsHttp conn = (CicsHttp) cf.createConnection("testCreateNoUserNoPasswordNoCharset",
                    address, AbstractHttpConnectionTester.getCicsTs23Endpoint());
            assertEquals("testCreateNoUserNoPasswordNoCharset", conn.getConnectionID());
            assertEquals("IBMTRUC0", conn.getCicsHttpEndpoint().getHostCharset());
            assertEquals("RANTANPLAN", conn.getCicsHttpEndpoint().getHostUserID());
            assertEquals(true, conn.getCicsHttpEndpoint().isHostTraceMode());
        } catch (Exception e) {
            fail("testCreateWrongEndPoint failed " + e);
        }
    }

    /**
     * Check what happens if no user/password charset is provided at all.
     */
    public void testCreateFromEmptyConfig() {
        try {
            CicsHttpEndpoint endpoint = AbstractHttpConnectionTester.getCicsTs23Endpoint();
            endpoint.setHostUserID(null);
            endpoint.setHostPassword(null);
            endpoint.setHostCharset(null);
            CicsHttpConnectionFactory cf = new CicsHttpConnectionFactory();
            LegStarAddress address = new LegStarAddress(ENDPOINT_NAME);
            cf.createConnection("testCreateNoUserNoPasswordNoCharset", address, endpoint);
            fail("testCreateFromEmptyConfig failed ");
        } catch (Exception e) {
            assertEquals("No host character set has been provided.", e.getMessage());
        }
    }

    /**
     * Test with a config file with no ip host/port.
     */
    public void testCreateFromVeryEmptyConfig() {
        try {
            CicsHttpEndpoint endpoint = AbstractHttpConnectionTester.getCicsTs23Endpoint();
            endpoint.setHostIPAddress(null);
            CicsHttpConnectionFactory cf = new CicsHttpConnectionFactory();
            LegStarAddress address = new LegStarAddress(ENDPOINT_NAME);
            address.setHostCharset("IBMTRUC0");
            address.setHostUserID("RANTANPLAN");
            cf.createConnection("testCreateNoUserNoPasswordNoCharset", address, endpoint);
            fail("testCreateFromEmpyConfig failed ");
        } catch (Exception e) {
            assertEquals("No host IP address has been provided.", e.getMessage());
        }
    }

    /**
     * Check the default timeout values.
     */
    public void testCreateWithDefaultTimeouts() {
        try {
            CicsHttpEndpoint endpoint = AbstractHttpConnectionTester.getCicsTs23Endpoint();
            CicsHttpConnectionFactory cf = new CicsHttpConnectionFactory();
            LegStarAddress address = new LegStarAddress(ENDPOINT_NAME);
            CicsHttp conn = (CicsHttp) cf.createConnection(
                    "testCreateWithDefaultTimeouts", address, endpoint);
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
            CicsHttpEndpoint endpoint = AbstractHttpConnectionTester.getCicsTs23Endpoint();
            endpoint.setConnectTimeout(2000);
            endpoint.setReceiveTimeout(7000);
            CicsHttpConnectionFactory cf = new CicsHttpConnectionFactory();
            LegStarAddress address = new LegStarAddress(ENDPOINT_NAME);
            CicsHttp conn = (CicsHttp) cf.createConnection(
                    "testCreateWithDefaultTimeouts", address, endpoint);
            assertEquals(2000, conn.getConnectTimeout());
            assertEquals(7000, conn.getReceiveTimeout());
        } catch (Exception e) {
            fail("testCreateWithDefaultTimeouts failed" + e.getMessage());
        }
    }
}
