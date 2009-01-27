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

import com.legstar.coxb.host.HostData;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;
import com.legstar.test.coxb.LsfileaeCases;
import com.legstar.test.coxb.T1volumeCases;

/**
 * Test the main CicsMQ class.
 * This is also used as the test bench for the mainframe WMQ programs.
 *
 */
public class CicsMQTest extends AbstractTester {

    /** {@inheritDoc} */
    public void setUp() throws Exception {
        super.setUp("CICSTS23");
    }

    /**
     * Test simple instantiation.
     */
    public void testInstantiation() {
        try {
            CicsMQ cicsMQ = new CicsMQ("testInstantiation", getEndpoint(),
                    DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
            assertFalse(cicsMQ == null);
        } catch (CicsMQConnectionException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test connect/close.
     */
    public void testConnectClose() {
        try {
            CicsMQ cicsMQ = new CicsMQ("testInstantiation", getEndpoint(),
                    DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
            cicsMQ.connect("tiramisu");
            cicsMQ.close();
        } catch (ConnectionException e) {
            fail(e.getMessage());
        } catch (RequestException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Try to connect to a host not running WMQ.
     */
    public void testConnectWrongHost() {
        try {
            getEndpoint().setHostIPAddress(" ");
            CicsMQ cicsMQ = new CicsMQ("testInstantiation", getEndpoint(),
                    DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
            cicsMQ.connect("tiramisu");
            fail("testConnectFailure");
        } catch (ConnectionException e) {
            assertTrue(e.getMessage().contains("MQJE010"));
        }
    }

    /**
     * Try to connect to a host using the wrong port.
     */
    public void testConnectWrongPort() {
        try {
            getEndpoint().setHostIPPort(1517);
            CicsMQ cicsMQ = new CicsMQ("testInstantiation", getEndpoint(),
                    DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
            cicsMQ.connect("tiramisu");
            fail("testConnectFailure");
        } catch (ConnectionException e) {
            assertTrue(e.getMessage().contains("MQJE011"));
        }
    }

    /**
     * Try to connect twice.
     */
    public void testConnectReuse() {
        try {
            CicsMQ cicsMQ = new CicsMQ("testInstantiation", getEndpoint(),
                    DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
            cicsMQ.connectReuse("tiramisu");
            /* We should have a valid MQ Manager */
            assertTrue(cicsMQ.getRequestQueue().isOpen());
            /* This should be accepted */
            cicsMQ.connectReuse("tiramisu");
            assertTrue(cicsMQ.getRequestQueue().isOpen());
            cicsMQ.close();
            assertFalse(cicsMQ.getRequestQueue().isOpen());
        } catch (ConnectionException e) {
            fail(e.getMessage());
        } catch (RequestException e) {
            fail(e.getMessage());
        }
    }

    /**
     * A simple send/receive.
     */
    public void testSendRequest() {
        try {
            LegStarRequest request = getLsfileaeRequest100();
            request.getAddress().setHostTraceMode(true);
            getConnection().sendRequest(request);
            getConnection().recvResponse(request);
            assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
            assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                    HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
        } catch (RequestException e) {
            fail("testSendRequest failed " + e);
        }
    }

    /**
     * Test a failing request.
     */
    public void testSendRequestWithInvalidProg() {
        try {
            LegStarRequest request = createInvalidRequest();
            getConnection().sendRequest(request);
            getConnection().recvResponse(request);
            fail("testSendRequest failed ");
        } catch (RequestException e) {
            assertTrue(e.getMessage().contains("CICS command=LINK COMMAREA failed, resp=PGMIDERR"));
        }
    }

    /**
     * Test a request that takes a long time.
     */
    public void testLongRequest() {
        try {
            LegStarRequest request = createLongRequest();
            getConnection().sendRequest(request);
            getConnection().recvResponse(request);
            assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
            assertTrue(HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()).
                    startsWith("f0f0f0f0f0f0f0f3"));
        } catch (RequestException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test with large payload.
     */
    public void testLargeRequest() {
        try {
            LegStarRequest request = createLargeRequest();
            getConnection().sendRequest(request);
            getConnection().recvResponse(request);
            T1volumeCases.checkByteArray(request.getResponseMessage().getDataParts().get(0).getContent());
        } catch (RequestException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Sending 2 requests, the second one being shorter than the first one and then waiting
     * for the first one to come back. This way, when the second one finally replies, there
     * are actually 2 messages in the reply queue. This ensures that we pick-up the right one
     * for each request.
     */
    public void testLongRequestSequence() {
        try {
            LegStarRequest request1 = createLongRequest();
            LegStarRequest request2 = createLongRequest("f0f0f0f0f0f0f0f2");

            getConnection().sendRequest(request1);
            getConnection().sendRequest(request2);

            getConnection().recvResponse(request1);
            assertEquals(1, request1.getResponseMessage().getHeaderPart().getDataPartsNumber());
            assertEquals(39, request1.getResponseMessage().getDataParts().get(0).getContent().length);
            assertTrue(HostData.toHexString(request1.getResponseMessage().getDataParts().get(0).getContent()).
                    startsWith("f0f0f0f0f0f0f0f3"));

            getConnection().recvResponse(request2);
            assertEquals(1, request2.getResponseMessage().getHeaderPart().getDataPartsNumber());
            assertEquals(39, request2.getResponseMessage().getDataParts().get(0).getContent().length);
            assertTrue(HostData.toHexString(request2.getResponseMessage().getDataParts().get(0).getContent()).
                    startsWith("f0f0f0f0f0f0f0f2"));
        } catch (RequestException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Handler transaction LEGQ belongs to transaction class LEGQTCLS which has a MAXACTIVE of 2.
     * By sending more than 2 long request, we are sure to reach a high load situation.
     */
    public void testHighLoad() {
        try {

            LegStarRequest request1 = createLongRequest("f0f0f0f0f0f0f0f4");
            getConnection().sendRequest(request1);

            LegStarRequest request2 = createLongRequest("f0f0f0f0f0f0f0f3");
            getConnection().sendRequest(request2);

            LegStarRequest request3 = createLongRequest("f0f0f0f0f0f0f0f2");
            getConnection().sendRequest(request3);

            LegStarRequest request4 = createLongRequest("f0f0f0f0f0f0f0f1");
            getConnection().sendRequest(request4);

            getConnection().recvResponse(request1);
            assertEquals(1, request1.getResponseMessage().getHeaderPart().getDataPartsNumber());
            assertEquals(39, request1.getResponseMessage().getDataParts().get(0).getContent().length);
            assertTrue(HostData.toHexString(request1.getResponseMessage().getDataParts().get(0).getContent()).
                    startsWith("f0f0f0f0f0f0f0f4"));

            getConnection().recvResponse(request2);
            assertEquals(1, request2.getResponseMessage().getHeaderPart().getDataPartsNumber());
            assertEquals(39, request2.getResponseMessage().getDataParts().get(0).getContent().length);
            assertTrue(HostData.toHexString(request2.getResponseMessage().getDataParts().get(0).getContent()).
                    startsWith("f0f0f0f0f0f0f0f3"));

            getConnection().recvResponse(request3);
            assertEquals(1, request3.getResponseMessage().getHeaderPart().getDataPartsNumber());
            assertEquals(39, request3.getResponseMessage().getDataParts().get(0).getContent().length);
            assertTrue(HostData.toHexString(request3.getResponseMessage().getDataParts().get(0).getContent()).
                    startsWith("f0f0f0f0f0f0f0f2"));

            getConnection().recvResponse(request4);
            assertEquals(1, request4.getResponseMessage().getHeaderPart().getDataPartsNumber());
            assertEquals(39, request4.getResponseMessage().getDataParts().get(0).getContent().length);
            assertTrue(HostData.toHexString(request4.getResponseMessage().getDataParts().get(0).getContent()).
                    startsWith("f0f0f0f0f0f0f0f1"));
        } catch (RequestException e) {
            fail(e.getMessage());
        }
    }

}
