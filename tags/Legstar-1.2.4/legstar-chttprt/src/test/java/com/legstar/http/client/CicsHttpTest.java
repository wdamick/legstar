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
import org.apache.commons.httpclient.auth.AuthScope;
import org.apache.commons.httpclient.methods.PostMethod;

import com.legstar.config.Config;
import com.legstar.coxb.host.HostData;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;
import com.legstar.test.coxb.LsfileaeCases;

/**
 * Test the main CicsHttp class.
 * This is also used as the test bench for the mainframe Http programs.
 *
 */
public class CicsHttpTest extends AbstractTester {

    /** {@inheritDoc} */
    public void setUp() throws Exception {
        super.setUp("CICSTS23");
    }

    /**
     * Test simple instantiation.
     */
    public void testInstantiation() {
        CicsHttp cicsHttp = new CicsHttp("testInstantiation", getEndpoint(),
                DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
        assertEquals("mainframe", cicsHttp.getHostConfig().getHost());
        assertEquals(3080, cicsHttp.getHostConfig().getPort());
        assertEquals(cicsHttp.getHostConfig().getHost(), cicsHttp.getCicsHttpEndpoint().getHostIPAddress());
        assertEquals(cicsHttp.getHostConfig().getPort(), cicsHttp.getCicsHttpEndpoint().getHostIPPort());
        assertEquals(true, cicsHttp.getHttpClient().getParams().isAuthenticationPreemptive());
    }

    /**
     * Try a connection.
     */
    public void testConnect() {
        try {
            CicsHttp cicsHttp = new CicsHttp("testConnect", getEndpoint(),
                    DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
            cicsHttp.connect("TRUC");
            AuthScope as = new AuthScope("mainframe",
                    3080,
                    AuthScope.ANY_HOST);
            assertEquals("P390:TRUC", cicsHttp.getHttpState().getCredentials(as).toString());
        } catch (ConnectionException e) {
            fail("testConnect failed " + e);
        }
    }

    /**
     * Test the post method method.
     */
    public void testPostMethodCreation() {
        try {
            LegStarRequest request = getLsfileaeRequest100();
            PostMethod postMethod = getConnection().createPostMethod(request);
            assertEquals("POST", postMethod.getName());
            assertEquals("CICSTraceMode: false",
                    postMethod.getRequestHeader(CicsHttp.REQUEST_TRACE_MODE_HHDR).toString().trim());
            assertEquals("CICSRequestID: testPostMethodCreation",
                    postMethod.getRequestHeader(CicsHttp.REQUEST_ID_HHDR).toString().trim());
            assertEquals("/CICS/CWBA/LSWEBBIN",
                    postMethod.getPath());

        } catch (RequestException e) {
            fail("testPostMethodCreation failed " + e);
        }
    }

    /**
     * Send a request before connecting.
     */
    public void testSendRequestOutOfSync() {
        try {
            CicsHttp cicsHttp = new CicsHttp("testSendRequestOutOfSync", getEndpoint(),
                    DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
            cicsHttp.sendRequest(getLsfileaeRequest100());
            fail("testPostMethodCreation failed ");
        } catch (RequestException e) {
            assertEquals("No prior connect. Missing host credentials.", e.getMessage());
        }
    }

    /**
     * Send a request to the wrong address.
     */
    public void testSendRequestToWrongIPAddress() {
        try {
            CicsHttp cicsHttp = new CicsHttp("testSendRequestToWrongIPAddress", getEndpoint(),
                    DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
            CicsHttpEndpoint endPoint = cicsHttp.getCicsHttpEndpoint();
            endPoint.setHostIPAddress("192.168.0.117");
            cicsHttp.setCicsHttpEndpoint(endPoint);
            cicsHttp.setConnectTimeout(1500);
            cicsHttp.connect("zaratoustra");
            cicsHttp.sendRequest(getLsfileaeRequest100());
            fail("testPostMethodCreation failed ");

        } catch (ConnectionException e) {
            fail("testSendRequestOutOfSync failed " + e);
        } catch (RequestException e) {
            assertEquals("org.apache.commons.httpclient.ConnectTimeoutException:"
                    + " The host did not accept the connection within timeout of 1500 ms", e.getMessage());
        }
    }

    /**
     * Send a request to the wrong IP port.
     */
    public void testSendRequestToWrongIPPort() {
        try {
            CicsHttp cicsHttp = new CicsHttp("testSendRequestToWrongIPPort", getEndpoint(),
                    DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
            CicsHttpEndpoint endPoint = cicsHttp.getCicsHttpEndpoint();
            endPoint.setHostIPPort(12768);
            cicsHttp.setCicsHttpEndpoint(endPoint);
            cicsHttp.setConnectTimeout(1500);
            cicsHttp.connect("zaratoustra");
            cicsHttp.sendRequest(getLsfileaeRequest100());
            fail("testPostMethodCreation failed ");

        } catch (ConnectionException e) {
            fail("testSendRequestOutOfSync failed " + e);
        } catch (RequestException e) {
            assertEquals("java.net.ConnectException: Connection refused: connect", e.getMessage());
        }
    }

    /**
     * Send a request with the wrong user ID.
     */
    public void testSendRequestWithWrongUserID() {
        try {
            CicsHttp cicsHttp = new CicsHttp("testSendRequestWithWrongUserID", getEndpoint(),
                    DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
            CicsHttpEndpoint endPoint = cicsHttp.getCicsHttpEndpoint();
            endPoint.setHostUserID("tartempion");
            cicsHttp.setCicsHttpEndpoint(endPoint);
            cicsHttp.setConnectTimeout(1500);
            cicsHttp.connect("zaratoustra");
            LegStarRequest request = getLsfileaeRequest100();
            cicsHttp.sendRequest(request);
            cicsHttp.recvResponse(request);
            fail("testPostMethodCreation failed ");

        } catch (ConnectionException e) {
            fail("testSendRequestOutOfSync failed " + e);
        } catch (RequestException e) {
            /* Due to a bug in CICS TS 2.3, the content has the wrong content-length
             * which results in a truncation of the message returned. */
            assertEquals("Basic Authentication Error <!doctype html public \"-//IETF//DTD HTM", e.getMessage());
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
     * Send/receive without credentials.
     */
    public void testSendRequestWithNoCredentials() {
        try {
            /* This config has no default credentials, but the listener has basic
             * auth turned off so this should be ok */
            HierarchicalConfiguration endpointConfig =
                        Config.loadEndpointConfiguration("config5.xml", "CICSTS23");
            CicsHttp cicsHttp = new CicsHttp("testSendRequestWithNoCredentials", new CicsHttpEndpoint(
                    endpointConfig), DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);

            cicsHttp.connect(null);
            LegStarRequest request = getLsfileaeRequest100();
            cicsHttp.sendRequest(request);
            cicsHttp.recvResponse(request);
            assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
            assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                    HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
        } catch (ConnectionException e) {
            fail("testSendRequestWithNoCredentials failed " + e);
        } catch (RequestException e) {
            fail("testSendRequestWithNoCredentials failed " + e);
        } catch (ConfigurationException e) {
            fail("testSendRequestWithNoCredentials failed " + e);
        }
    }

    /**
     * Test a situation where the request time will exceed the timeout.
     */
    public void testReceiveTimeout() {
        try {
            getConnection().setReceiveTimeout(2000);
            LegStarRequest request = createLongRequest(); // Will not respond within 2 secs
            getConnection().sendRequest(request);
            getConnection().recvResponse(request);
            fail("testReceiveTimeout failed ");
        } catch (RequestException e) {
            assertEquals("java.net.SocketTimeoutException: Read timed out", e.getMessage());
        }
    }

    /**
     * This tests that even after a receive timeout, we can still use the HttpClient
     * (after all some data leftover by the failing call might get in the way).
     */
    public void testReceiveTimeoutReuseHttpClient() {
        try {
            getConnection().setReceiveTimeout(2000);
            LegStarRequest request = createLongRequest(); // Will not respond within 2 secs
            try {
                getConnection().sendRequest(request);
                getConnection().recvResponse(request);
            } catch (RequestException e) {
                LegStarRequest request2 = getLsfileaeRequest100();
                getConnection().sendRequest(request2);
                getConnection().recvResponse(request2);
                assertEquals(1, request2.getResponseMessage().getHeaderPart().getDataPartsNumber());
                assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                        HostData.toHexString(request2.getResponseMessage().getDataParts().get(0).getContent()));
            }
        } catch (RequestException e) {
            fail("testReceiveTimeoutReuseHttpClient failed " + e);
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

}
