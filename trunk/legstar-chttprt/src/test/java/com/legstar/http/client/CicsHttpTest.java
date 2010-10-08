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

import org.apache.commons.httpclient.HostConfiguration;
import org.apache.commons.httpclient.HttpState;
import org.apache.commons.httpclient.auth.AuthScope;
import org.apache.commons.httpclient.methods.PostMethod;

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
public class CicsHttpTest extends AbstractHttpConnectionTester {

    /** {@inheritDoc} */
    public void setUp() throws Exception {
        super.setUp("CICSTS23");
    }

    /**
     * Test simple instantiation.
     */
    public void testInstantiation() {
        CicsHttp cicsHttp = new CicsHttp(getName(), getEndpoint());
        assertEquals("mainframe", cicsHttp.getHttpClient().getHostConfiguration().getHost());
        assertEquals(3080, cicsHttp.getHttpClient().getHostConfiguration().getPort());
        assertEquals(cicsHttp.getHttpClient().getHostConfiguration().getHost(),
                cicsHttp.getCicsHttpEndpoint().getHostIPAddress());
        assertEquals(cicsHttp.getHttpClient().getHostConfiguration().getPort(),
                cicsHttp.getCicsHttpEndpoint().getHostIPPort());
        assertEquals(true, cicsHttp.getHttpClient().getParams().isAuthenticationPreemptive());
    }

    /**
     * Try to create a host configuration.
     */
    public void testCreateHostConfiguration() {
        CicsHttp cicsHttp = new CicsHttp(getName(), getEndpoint());
        HostConfiguration hostConfiguration = cicsHttp.createHostConfiguration(getEndpoint());
        assertEquals("mainframe", hostConfiguration.getHost());
        assertEquals("http://mainframe:3080", hostConfiguration.getHostURL());
        assertEquals(3080, hostConfiguration.getPort());
        assertEquals("http:80", hostConfiguration.getProtocol().toString());
    }

    /**
     * Try to create a state.
     */
    public void testCreateHttpState() {
        CicsHttp cicsHttp = new CicsHttp(getName(), getEndpoint());
        HttpState state = cicsHttp.createHttpState("mainframe", 3080, "P390", "TRUC", null);
        AuthScope as = new AuthScope("mainframe", 3080, null, AuthScope.ANY_SCHEME);
        assertEquals("P390:TRUC", state.getCredentials(as).toString());
    }

    /**
     * Test the post method method.
     */
    public void testCreatePostMethod() {
        try {
            LegStarRequest request = getLsfileaeRequest100(getAddress());
            PostMethod postMethod = getConnection().createPostMethod(request, "/CICS/CWBA/LSWEBBIN");
            assertEquals("POST", postMethod.getName());
            assertEquals("CICSTraceMode: false",
                    postMethod.getRequestHeader(CicsHttp.REQUEST_TRACE_MODE_HHDR).toString().trim());
            assertTrue(postMethod.getRequestHeader(
                    CicsHttp.REQUEST_ID_HHDR).toString().contains("CICSRequestID: "));
            assertEquals("/CICS/CWBA/LSWEBBIN",
                    postMethod.getPath());

        } catch (RequestException e) {
            fail("testPostMethodCreation failed " + e);
        }
    }

    /**
     * Send a request to the wrong address.
     */
    public void testSendRequestToWrongIPAddress() {
        try {
            CicsHttpEndpoint endpoint = getEndpoint();
            endpoint.setHostIPAddress("192.168.0.117");
            endpoint.setConnectTimeout(1500);
            CicsHttp cicsHttp = new CicsHttp(getName(), endpoint);
            cicsHttp.connect("zaratoustra");
            cicsHttp.sendRequest(getLsfileaeRequest100(getAddress()));
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
            CicsHttpEndpoint endpoint = getEndpoint();
            endpoint.setHostIPPort(12768);
            endpoint.setConnectTimeout(1500);
            CicsHttp cicsHttp = new CicsHttp(getName(), endpoint);
            cicsHttp.connect("zaratoustra");
            cicsHttp.sendRequest(getLsfileaeRequest100(getAddress()));
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
            CicsHttpEndpoint endpoint = getEndpoint();
            endpoint.setHostUserID("tartempion");
            endpoint.setConnectTimeout(1500);
            CicsHttp cicsHttp = new CicsHttp(getName(), endpoint);
            cicsHttp.connect("zaratoustra");
            LegStarRequest request = getLsfileaeRequest100(getAddress());
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
            LegStarRequest request = getLsfileaeRequest100(getAddress());
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
            CicsHttpEndpoint endpoint = getEndpoint();
            endpoint.setHostIPPort(3081);
            endpoint.setHostUserID(null);
            endpoint.setHostPassword(null);
            CicsHttp cicsHttp = new CicsHttp(getName(), endpoint);

            cicsHttp.connect(null);
            LegStarRequest request = getLsfileaeRequest100(getAddress());
            cicsHttp.sendRequest(request);
            cicsHttp.recvResponse(request);
            assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
            assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                    HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
        } catch (ConnectionException e) {
            fail("testSendRequestWithNoCredentials failed " + e);
        } catch (RequestException e) {
            fail("testSendRequestWithNoCredentials failed " + e);
        }
    }

    /**
     * Test a situation where the request time will exceed the timeout.
     */
    public void testReceiveTimeout() {
        try {
            getEndpoint().setReceiveTimeout(2000);
            LegStarRequest request = createLongRequest(getAddress()); // Will not respond within 2 secs
            CicsHttp cicsHttp = new CicsHttp(getName(), getEndpoint());
            cicsHttp.connect(HOST_PASSWORD);
            cicsHttp.sendRequest(request);
            cicsHttp.recvResponse(request);
            cicsHttp.close();
            fail("testReceiveTimeout failed ");
        } catch (RequestException e) {
            assertEquals("java.net.SocketTimeoutException: Read timed out", e.getMessage());
        } catch (ConnectionException e) {
            fail(e.getMessage());
        }
    }

    /**
     * This tests that even after a receive timeout, we can still use the HttpClient
     * (after all some data leftover by the failing call might get in the way).
     */
    public void testReceiveTimeoutReuseHttpClient() {
        try {
            getEndpoint().setReceiveTimeout(2000);
            LegStarRequest request = createLongRequest(getAddress()); // Will not respond within 2 secs
            try {
                getConnection().sendRequest(request);
                getConnection().recvResponse(request);
            } catch (RequestException e) {
                LegStarRequest request2 = getLsfileaeRequest100(getAddress());
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
            LegStarRequest request = createInvalidRequest(getAddress());
            getConnection().sendRequest(request);
            getConnection().recvResponse(request);
            fail("testSendRequest failed ");
        } catch (RequestException e) {
            assertTrue(e.getMessage().contains("CICS command=LINK COMMAREA failed, resp=PGMIDERR"));
        }
    }

}
