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

import java.io.UnsupportedEncodingException;
import java.util.HashMap;

import com.legstar.csok.client.CicsSocket;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;
import com.legstar.test.coxb.LsfileaeCases;
import com.legstar.config.Constants;
import com.legstar.coxb.host.HostData;

/**
 * Test the main CicsSocket class.
 * This is also used as the test bench for the mainframe Socket programs.
 *
 */
public class CicsTs23SocketTest extends AbstractSocketConnectionTester {
    
    /** {@inheritDoc} */
    public void setUp() throws Exception {
        super.setUp("CICSTS23");
    }

    /**
     * Test formatting of Client Initiation Message (CSKL).
     * @throws UnsupportedEncodingException if conversion fails
     */
    public void testCIMFormat() throws UnsupportedEncodingException {
        byte[] cim = CicsSocket.formatCIM("cicsUserID", "cicsPassword", "connectionID", true, HOST_CHARSET);
        assertEquals(
                /*  c i c s U s e r                (USERID)*/
                  "838983a2e4a28599"
                /*  c i c s P a s s                (PASSWORD)*/
                + "838983a2d781a2a2"
                /*  c o n n e c t i o n I D        (CONNECTION ID)*/
                + "839695958583a3899695c9c440404040"
                /*  1                              (TRACE ON = 1, OFF = 0)*/
                + "f1"
                /*  E C                            (EYE CATCHER must be EC)*/
                + "e2d2", HostData.toHexString(cim));
    }

    /**
     * Test the message formatting method.
     * @throws UnsupportedEncodingException if conversion fails
     */
    public void testFormatMessage() throws UnsupportedEncodingException {
        byte[] mph = CicsSocket.formatMessageType("LSOKDATA", HOST_CHARSET);
        /*            L S O K D A T A   */
        assertEquals("d3e2d6d2c4c1e3c140", HostData.toHexString(mph));
    }

    /** Attempt to connect with the wrong USER ID. */
    public void testSecurityViolation() {
        try {
            getEndpoint().setHostUserID("TARBOUCH");
            CicsSocket cs = new CicsSocket(getName(), getEndpoint());
            cs.connect(HOST_USERID);
            fail("testSecurityViolation failed");
        } catch (ConnectionException e) {
            assertEquals("com.legstar.messaging.RequestException:"
                    + " SEE06908 The USERID is not known to the external security manager.",
                    e.getMessage().trim());
        }
    }

    /** Close when no connection exist. */
    public void testClosePremature() {
        try {
            CicsSocket cs = new CicsSocket(getName(), getEndpoint());
            cs.close();
        } catch (RequestException e) {
            fail("testClosePremature failed=" + e);
        }
    }

    /** Connect and immediately close. */
    public void testConnectAndClose() {
        try {
            CicsSocket cs = new CicsSocket(getName(), getEndpoint());
            cs.connect(HOST_PASSWORD);
            cs.close();
        } catch (ConnectionException e) {
            fail("testConnectAndClose failed=" + e);
        } catch (RequestException e) {
            fail("testConnectAndClose failed=" + e);
        }
    }

    /**
     * Connect and send a message part.
     */
    public void testConnectSendPart() {
        try {
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_LENGTH_KEY, "79");
            map.put(Constants.CICS_DATALEN_KEY, "6");

            LegStarRequest request = getRequest(map, getAddress());
            request.getRequestMessage().addDataPart(new CommareaPart(null));
            getConnection().sendRequest(request);
            
        } catch (RequestException e) {
            fail("testConnectSendPart failed=" + e);
        }
    }

    /** 
     * Connect and send 2 message parts.
     */
    public void testConnectSend2Parts() {
        try {
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_LENGTH_KEY, "79");
            map.put(Constants.CICS_DATALEN_KEY, "6");

            LegStarRequest request = getRequest(map, getAddress());
            request.getRequestMessage().addDataPart(new CommareaPart(HostData.toByteArray("F3F4F5")));
            request.getRequestMessage().addDataPart(new CommareaPart(HostData.toByteArray("F0F0F0F1F0F0")));
            getConnection().sendRequest(request);

        } catch (RequestException e) {
            fail("testConnectSendPart failed=" + e);
        }
    }

    /** Send too much input parts.
     *  */
    public void testSendTooManyParts() {
        try {
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_LENGTH_KEY, "79");
            map.put(Constants.CICS_DATALEN_KEY, "6");

            LegStarRequest request = getRequest(map, getAddress());
            for (int i = 0; i < 11; i++) {
                request.getRequestMessage().addDataPart(new CommareaPart(null));
            }
            getConnection().sendRequest(request);
            getConnection().recvResponse(request);
            fail("testTooManyParts failed=");
            
        } catch (RequestException e) {
            assertEquals("Too many input message parts.", e.getMessage());
        }
    }

    /**
     * Test that missing program name is checked correctly.
     */
    public void testMissingProgramName() {
        try {
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_LENGTH_KEY, "79");
            map.put(Constants.CICS_DATALEN_KEY, "6");
            map.put(Constants.CICS_SYSID_KEY, "CICZ");
            map.put(Constants.CICS_SYNCONRET_KEY, "1");
            map.put(Constants.CICS_TRANSID_KEY, "MIRO");

            LegStarRequest request = getRequest(map, getAddress());
            request.getRequestMessage().addDataPart(new CommareaPart(null));
            getConnection().sendRequest(request);
            getConnection().recvResponse(request);
            fail("testMissingProgramName failed=");
        } catch (RequestException e) {
            assertEquals("No CICS program name was provided.", e.getMessage());
        }
    }

    /**
     * Test that a commarea message part is required.
     */
    public void testMissingCommareapart() {
        try {
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAE");
            map.put(Constants.CICS_LENGTH_KEY, "79");
            map.put(Constants.CICS_DATALEN_KEY, "6");
            map.put(Constants.CICS_SYSID_KEY, "CICZ");
            map.put(Constants.CICS_SYNCONRET_KEY, "1");
            map.put(Constants.CICS_TRANSID_KEY, "MIRO");
            
            LegStarRequest request = getRequest(map, getAddress());
            getConnection().sendRequest(request);
            getConnection().recvResponse(request);
            fail("testMissingProgramName failed=");
        } catch (RequestException e) {
            assertEquals("No input message part for commarea.", e.getMessage());
        }
    }

    /** 
     * Test that a maximum of one message part is accepted for commarea.
     */
    public void testTooManyCommareaparts() {
        try {
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAE");
            map.put(Constants.CICS_LENGTH_KEY, "79");
            map.put(Constants.CICS_DATALEN_KEY, "6");
            map.put(Constants.CICS_SYSID_KEY, "CICZ");
            map.put(Constants.CICS_SYNCONRET_KEY, "1");
            map.put(Constants.CICS_TRANSID_KEY, "MIRO");

            LegStarRequest request = getRequest(map, getAddress());
            request.getRequestMessage().addDataPart(new CommareaPart(null));
            request.getRequestMessage().addDataPart(new CommareaPart(null));
            getConnection().sendRequest(request);
            getConnection().recvResponse(request);
            fail("testTooManyCommareaparts failed=");
        } catch (RequestException e) {
            assertEquals("Too many message parts for commarea.", e.getMessage());
        }
    }

    /** 
     * Test that datalength is correctly checked against length.
     */
    public void testDataLengthGtLength() {
        try {
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAE");
            map.put(Constants.CICS_LENGTH_KEY, "6");
            map.put(Constants.CICS_DATALEN_KEY, "79");
            map.put(Constants.CICS_SYSID_KEY, "CICZ");
            map.put(Constants.CICS_SYNCONRET_KEY, "1");
            map.put(Constants.CICS_TRANSID_KEY, "MIRO");

            LegStarRequest request = getRequest(map, getAddress());
            request.getRequestMessage().addDataPart(new CommareaPart(null));
            getConnection().sendRequest(request);
            getConnection().recvResponse(request);
            fail("testDataLengthGtLength failed");
        } catch (RequestException e) {
            assertEquals("Data length cannot exceed commarea length.", e.getMessage());
        }
    }

    /** 
     * Send all possible header key/value pairs. 
     */
    public void testSendHeaderCommarea() {
        try {
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAE");
            map.put(Constants.CICS_LENGTH_KEY, "79");
            map.put(Constants.CICS_DATALEN_KEY, "6");
            map.put(Constants.CICS_SYSID_KEY, "CICS");
            map.put(Constants.CICS_SYNCONRET_KEY, "1");
            map.put(Constants.CICS_TRANSID_KEY, "CSMI");

            LegStarRequest request = getRequest(map, getAddress());
            request.getRequestMessage().addDataPart(new CommareaPart(
                    HostData.toByteArray(LsfileaeCases.getHostBytesHexRequest100())));
            getConnection().sendRequest(request);
            getConnection().recvResponse(request);
            assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                    HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
            getConnection().commitUOW();
        } catch (RequestException e) {
            fail("testSendHeaderCommarea failed=" + e);
        }
    }

    /** 
     * Case where the commarea should not be reallocated (data sent is large enough to
     * hold the reply).
     * Check the CICS _log.
     */
    public void testNoReallocateContent() {
        try {
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAE");
            map.put(Constants.CICS_LENGTH_KEY, "79");
            map.put(Constants.CICS_DATALEN_KEY, "3");

            LegStarRequest request = getRequest(map, getAddress());
            request.getRequestMessage().addDataPart(new CommareaPart(
                    HostData.toByteArray(LsfileaeCases.getHostBytesHexReply100())));
            getConnection().sendRequest(request);
            getConnection().recvResponse(request);
            assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                    HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
            getConnection().keepUOW();
        } catch (RequestException e) {
            fail("testNoReallocateContent failed=" + e);
        }
    }

    /**
     * Test that a program name shorter than 8 chars works.
     */
    public void testShortProgramName() {
        try {
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFAE");
            map.put(Constants.CICS_LENGTH_KEY, "79");
            map.put(Constants.CICS_DATALEN_KEY, "6");

            LegStarRequest request = getRequest(map, getAddress());
            request.getRequestMessage().addDataPart(new CommareaPart(
                    HostData.toByteArray(LsfileaeCases.getHostBytesHexRequest100())));
            getConnection().sendRequest(request);
            getConnection().recvResponse(request);
            assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                    HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
            getConnection().commitUOW();
        } catch (RequestException e) {
            fail("testShortProgram failed=" + e);
        }
    }

    /** 
     * Case where the remote CICS program gets an ASRA abend.
     * This test does not work. When the ASRA happens, our code does not recover
     * control and therefore is unable to sent notification back over sockets.
     * The result is a timeout.
     */
    public void doesNotWorktestAsraAbend() {
        try {
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_PROGRAM_NAME_KEY, "T1ABEND");
            map.put(Constants.CICS_LENGTH_KEY, "4");
            map.put(Constants.CICS_DATALEN_KEY, "4");

            LegStarRequest request = getRequest(map, getAddress());
            request.getRequestMessage().addDataPart(new CommareaPart(HostData.toByteArray("C1E2D9C1")));
            getConnection().sendRequest(request);
            getConnection().recvResponse(request);
        } catch (RequestException e) {
            fail("testAsraAbend failed=" + e);
        }
    }
}
