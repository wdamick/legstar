/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.http.client;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.httpclient.auth.AuthScope;
import org.apache.commons.httpclient.methods.PostMethod;

import com.legstar.config.Config;
import com.legstar.config.Constants;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.LegStarHeaderPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarMessagePart;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;

import junit.framework.TestCase;

public class CicsHttpTest extends TestCase {

	private static final String CONFIG_FILE = "config.xml";
	
	private CicsHttpEndpoint mHttpEndpoint;
	/** Time out (in milliseconds) for initial connect. */
	private static final int DEFAULT_CONNECT_TIMEOUT_MSEC = 1000;
	
	/** Time out (in milliseconds) for read operations
	 *  (waiting for host reply). */
	private static final int DEFAULT_READ_TIMEOUT_MSEC = 5000;
	
	protected void setUp() throws Exception {
		super.setUp();
		HierarchicalConfiguration endpointConfig =
			Config.loadEndpointConfiguration(
					Config.loadGeneralConfig(CONFIG_FILE), "TheMainframe");
		mHttpEndpoint = new CicsHttpEndpoint(endpointConfig);
	}
	
	public void testInstantiation() {
		CicsHttp cicsHttp = new CicsHttp("testInstantiation", mHttpEndpoint, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
		assertEquals("192.168.0.110", cicsHttp.getHostConfig().getHost());
		assertEquals(3080, cicsHttp.getHostConfig().getPort());
		assertEquals(cicsHttp.getHostConfig().getHost(), cicsHttp.getCicsHttpEndpoint().getHostIPAddress());
		assertEquals(cicsHttp.getHostConfig().getPort(), cicsHttp.getCicsHttpEndpoint().getHostIPPort());
		assertEquals(true, cicsHttp.getHttpClient().getParams().isAuthenticationPreemptive());
	}
	
	public void testConnect() {
		try {
			CicsHttp cicsHttp = new CicsHttp("testConnect", mHttpEndpoint, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
			cicsHttp.connect("TRUC");
			AuthScope as = new AuthScope("192.168.0.110",
					3080,
					AuthScope.ANY_HOST);
			assertEquals("P390:TRUC", cicsHttp.getHttpState().getCredentials(as).toString());
		} catch (ConnectionException e) {
			fail("testConnect failed " + e);
		}
	}
	
	public void testPostMethodCreation() {
		try {
			CicsHttp cicsHttp = new CicsHttp("testPostMethodCreation", mHttpEndpoint, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
			HashMap < String, Object> map = new HashMap < String, Object>();
			map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAE");
			map.put(Constants.CICS_LENGTH_KEY, "79");
			map.put(Constants.CICS_DATALEN_KEY, "6");
			map.put(Constants.CICS_SYSID_KEY, "CICZ");
			map.put(Constants.CICS_SYNCONRET_KEY, "true");
			map.put(Constants.CICS_TRANSID_KEY, "MIRO");
			List <LegStarMessagePart> inputParts = new ArrayList <LegStarMessagePart>();
			LegStarMessagePart inCommarea = new CommareaPart(Util.toByteArray("F0F0F0F1F0F0"));
			inputParts.add(inCommarea);
			LegStarHeaderPart dp = new LegStarHeaderPart(map, inputParts.size());
			LegStarMessage requestMessage = new LegStarMessage(dp, inputParts);
			LegStarAddress address = new LegStarAddress("TheMainframe");
			LegStarRequest request = new LegStarRequest("Request01", address, requestMessage);
			
			PostMethod postMethod = cicsHttp.createPostMethod(request);
			assertEquals("POST", postMethod.getName());
			assertEquals("CICSTraceMode: false", postMethod.getRequestHeader(CicsHttp.REQUEST_TRACE_MODE_HHDR).toString().trim());
			assertEquals("CICSRequestID: Request01", postMethod.getRequestHeader(CicsHttp.REQUEST_ID_HHDR).toString().trim());
			assertEquals("/CICS/CWBA/LSWEBBIN", postMethod.getPath());
			
		} catch (HeaderPartException e) {
			fail("testPostMethodCreation failed " + e);
		} catch (RequestException e) {
			fail("testPostMethodCreation failed " + e);
		}
	}
	
	public void testSendRequestOutOfSync() {
		try {
			CicsHttp cicsHttp = new CicsHttp("testSendRequestOutOfSync", mHttpEndpoint, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
			cicsHttp.sendRequest(createStdRequest());
			fail("testPostMethodCreation failed ");
			
		} catch (HeaderPartException e) {
			fail("testSendRequestOutOfSync failed " + e);
		} catch (RequestException e) {
			assertEquals("No prior connect. Missing host credentials.", e.getMessage());
		}
	}

	public void testSendRequestToWrongIPAddress() {
		try {
			CicsHttp cicsHttp = new CicsHttp("testSendRequestToWrongIPAddress", mHttpEndpoint, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
			CicsHttpEndpoint endPoint = cicsHttp.getCicsHttpEndpoint();
			endPoint.setHostIPAddress("192.168.0.117");
			cicsHttp.setCicsHttpEndpoint(endPoint);
			cicsHttp.setConnectTimeout(1500);
			cicsHttp.connect("zaratoustra");
			cicsHttp.sendRequest(createStdRequest());
			fail("testPostMethodCreation failed ");
			
		} catch (HeaderPartException e) {
			fail("testSendRequestOutOfSync failed " + e);
		} catch (ConnectionException e) {
			fail("testSendRequestOutOfSync failed " + e);
		} catch (RequestException e) {
			assertEquals("org.apache.commons.httpclient.ConnectTimeoutException: The host did not accept the connection within timeout of 1500 ms", e.getMessage());
		}
	}

	public void testSendRequestToWrongIPPort() {
		try {
			CicsHttp cicsHttp = new CicsHttp("testSendRequestToWrongIPPort", mHttpEndpoint, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
			CicsHttpEndpoint endPoint = cicsHttp.getCicsHttpEndpoint();
			endPoint.setHostIPPort(12768);
			cicsHttp.setCicsHttpEndpoint(endPoint);
			cicsHttp.setConnectTimeout(1500);
			cicsHttp.connect("zaratoustra");
			cicsHttp.sendRequest(createStdRequest());
			fail("testPostMethodCreation failed ");
			
		} catch (HeaderPartException e) {
			fail("testSendRequestOutOfSync failed " + e);
		} catch (ConnectionException e) {
			fail("testSendRequestOutOfSync failed " + e);
		} catch (RequestException e) {
			assertEquals("java.net.ConnectException: Connection refused: connect", e.getMessage());
		}
	}

	public void testSendRequestWithWrongUserID() {
		try {
			CicsHttp cicsHttp = new CicsHttp("testSendRequestWithWrongUserID", mHttpEndpoint, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
			CicsHttpEndpoint endPoint = cicsHttp.getCicsHttpEndpoint();
			endPoint.setHostUserID("tartempion");
			cicsHttp.setCicsHttpEndpoint(endPoint);
			cicsHttp.setConnectTimeout(1500);
			cicsHttp.connect("zaratoustra");
			LegStarRequest request = createStdRequest();
			cicsHttp.sendRequest(request);
			cicsHttp.recvResponse(request);
			fail("testPostMethodCreation failed ");
			
		} catch (HeaderPartException e) {
			fail("testSendRequestOutOfSync failed " + e);
		} catch (ConnectionException e) {
			fail("testSendRequestOutOfSync failed " + e);
		} catch (RequestException e) {
			/* Due to a bug in CICS TS 2.3, the content has the wrong content-length
			 * which results in a truncation of the message returned. */
			assertEquals("Basic Authentication Error <!doctype html public \"-//IETF//DTD HTM", e.getMessage());
		}
	}

	public void testSendRequest() {
		try {
			CicsHttp cicsHttp = new CicsHttp("testSendRequest", mHttpEndpoint, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
			cicsHttp.connect("STREAM2");
			LegStarRequest request = createStdRequest();
			request.getAddress().setHostTraceMode(true);
			cicsHttp.sendRequest(request);
			cicsHttp.recvResponse(request);
			assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
			assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
					  Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
		} catch (HeaderPartException e) {
			fail("testSendRequest failed " + e);
		} catch (ConnectionException e) {
			fail("testSendRequest failed " + e);
		} catch (RequestException e) {
			fail("testSendRequest failed " + e);
		}
	}
	
	public void testSendRequestWithNoCredentials() {
		try {
			/* This config has no default credentials, also the listener has basic
			 * auth turned off so this should be ok */
			HierarchicalConfiguration endpointConfig =
				Config.loadEndpointConfiguration(
						Config.loadGeneralConfig("config5.xml"), "TheMainframe");
			CicsHttp cicsHttp = new CicsHttp("testSendRequestWithNoCredentials", new CicsHttpEndpoint(endpointConfig), DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);

			cicsHttp.connect(null);
			LegStarRequest request = createStdRequest();
			cicsHttp.sendRequest(request);
			cicsHttp.recvResponse(request);
			assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
			assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
					  Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
		} catch (HeaderPartException e) {
			fail("testSendRequestWithNoCredentials failed " + e);
		} catch (ConnectionException e) {
			fail("testSendRequestWithNoCredentials failed " + e);
		} catch (RequestException e) {
			fail("testSendRequestWithNoCredentials failed " + e);
		} catch (ConfigurationException e) {
			fail("testSendRequestWithNoCredentials failed " + e);
		}
	}
	
	
	/* with CICS TS 2.3, there is no support for HTTP 1.1. HTTPClient will not keep
	 * the session alive. */
	public void testSend2Requests() {
		try {
			CicsHttp cicsHttp = new CicsHttp("testSend2Requests", mHttpEndpoint, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
			cicsHttp.connect(null); // let config pick the password
			LegStarRequest request = createStdRequest();
			cicsHttp.sendRequest(request);
			cicsHttp.recvResponse(request);
			assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
			assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
					  Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
			LegStarRequest request2 = createLongRequest();
			cicsHttp.sendRequest(request2);
			cicsHttp.recvResponse(request2);
			assertEquals(1, request2.getResponseMessage().getHeaderPart().getDataPartsNumber());
			assertEquals("f0f0f0f0f0f0f0f3d7f3f9f040404040c3e6c2c1",
					  Util.toHexString(request2.getResponseMessage().getDataParts().get(0).
							  getContent()).substring(0, 40));
		} catch (HeaderPartException e) {
			fail("testSend2Requests failed " + e);
		} catch (ConnectionException e) {
			fail("testSend2Requests failed " + e);
		} catch (RequestException e) {
			fail("testSend2Requests failed " + e);
		}
	}
	
	/* with CICS TS 2.3, there is no support for HTTP 1.1. HTTPClient will not keep
	 * the session alive. */
	public void testSendRequestMultiple() {
		try {
			CicsHttp cicsHttp = new CicsHttp("testSendRequestMultiple", mHttpEndpoint, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
			for(int i = 0; i < 2; i++) {
				cicsHttp.connectReuse(null); // let config pick the password
				LegStarRequest request = createStdRequest();
				cicsHttp.sendRequest(request);
				cicsHttp.recvResponse(request);
				assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
				assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
						  Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
			}
		} catch (HeaderPartException e) {
			fail("testSend2Requests failed " + e);
		} catch (ConnectionException e) {
			fail("testSend2Requests failed " + e);
		} catch (RequestException e) {
			fail("testSend2Requests failed " + e);
		}
	}
	
	public void testReceiveTimeout() {
		try {
			CicsHttp cicsHttp = new CicsHttp("testReceiveTimeout", mHttpEndpoint, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
			cicsHttp.connect("STREAM2");
			cicsHttp.setReceiveTimeout(2000);
			LegStarRequest request = createLongRequest(); // Will not respond within 2 secs
			cicsHttp.sendRequest(request);
			cicsHttp.recvResponse(request);
			fail("testReceiveTimeout failed ");
		} catch (HeaderPartException e) {
			fail("testReceiveTimeout failed " + e);
		} catch (ConnectionException e) {
			fail("testReceiveTimeout failed " + e);
		} catch (RequestException e) {
			assertEquals("java.net.SocketTimeoutException: Read timed out", e.getMessage());
		}
	}

	/** This tests that even after a receive timeout, we can still use the HttpClient
	 * (after all some data leftover by the failing call might get in the way)*/
	public void testReceiveTimeoutReuseHttpClient() {
		try {
			CicsHttp cicsHttp = new CicsHttp("testReceiveTimeoutReuseHttpClient", mHttpEndpoint, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
			cicsHttp.connect("STREAM2");
			cicsHttp.setReceiveTimeout(2000);
			LegStarRequest request = createLongRequest(); // Will not respond within 2 secs
			try {
				cicsHttp.sendRequest(request);
				cicsHttp.recvResponse(request);
			} catch (RequestException e) {
				LegStarRequest request2 = createStdRequest();
				cicsHttp.sendRequest(request2);
				cicsHttp.recvResponse(request2);
				assertEquals(1, request2.getResponseMessage().getHeaderPart().getDataPartsNumber());
				assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
						  Util.toHexString(request2.getResponseMessage().getDataParts().get(0).getContent()));
			}
		} catch (HeaderPartException e) {
			fail("testReceiveTimeoutReuseHttpClient failed " + e);
		} catch (ConnectionException e) {
			fail("testReceiveTimeoutReuseHttpClient failed " + e);
		} catch (RequestException e) {
			fail("testReceiveTimeoutReuseHttpClient failed " + e);
		}
	}
	
	public void testSendRequestWithInvalidProg() {
		try {
			CicsHttp cicsHttp = new CicsHttp("testSendRequestWithInvalidProg", mHttpEndpoint, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
			cicsHttp.connect("STREAM2");
			LegStarRequest request = createInvalidRequest();
			cicsHttp.sendRequest(request);
			cicsHttp.recvResponse(request);
			fail("testSendRequest failed ");
		} catch (HeaderPartException e) {
			fail("testSendRequest failed " + e);
		} catch (ConnectionException e) {
			fail("testSendRequest failed " + e);
		} catch (RequestException e) {
			assertTrue(e.getMessage().contains("CICS command=LINK COMMAREA failed, resp=PGMIDERR"));
		}
	}
	
	private LegStarRequest createStdRequest() throws HeaderPartException {
		LegStarAddress address = new LegStarAddress("TheMainframe");
		HashMap < String, Object> map = new HashMap < String, Object>();
		map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAE");
		map.put(Constants.CICS_LENGTH_KEY, "79");
		map.put(Constants.CICS_DATALEN_KEY, "6");
		List <LegStarMessagePart> inputParts = new ArrayList <LegStarMessagePart>();
		LegStarMessagePart inCommarea = new CommareaPart(Util.toByteArray("F0F0F0F1F0F0"));
		inputParts.add(inCommarea);
		LegStarHeaderPart dp = new LegStarHeaderPart(map, inputParts.size());
		LegStarMessage requestMessage = new LegStarMessage(dp, inputParts);
		LegStarRequest request = new LegStarRequest("Request01", address, requestMessage);
		return request;
		
	}

	private LegStarRequest createLongRequest() throws HeaderPartException {
		LegStarAddress address = new LegStarAddress("TheMainframe");
		HashMap < String, Object> map = new HashMap < String, Object>();
		map.put(Constants.CICS_PROGRAM_NAME_KEY, "T1SLEEPT");
		map.put(Constants.CICS_LENGTH_KEY, "39");
		map.put(Constants.CICS_DATALEN_KEY, "8");
		List <LegStarMessagePart> inputParts = new ArrayList <LegStarMessagePart>();
		/* will sleep for 3 secs */
		LegStarMessagePart inCommarea = new CommareaPart(Util.toByteArray("f0f0f0f0f0f0f0f3"));
		inputParts.add(inCommarea);
		LegStarHeaderPart dp = new LegStarHeaderPart(map, inputParts.size());
		LegStarMessage requestMessage = new LegStarMessage(dp, inputParts);
		LegStarRequest request = new LegStarRequest("Request01", address, requestMessage);
		return request;
	}

	private LegStarRequest createInvalidRequest() throws HeaderPartException {
		LegStarAddress address = new LegStarAddress("TheMainframe");
		HashMap < String, Object> map = new HashMap < String, Object>();
		map.put(Constants.CICS_PROGRAM_NAME_KEY, "TARATOZ");
		map.put(Constants.CICS_LENGTH_KEY, "79");
		map.put(Constants.CICS_DATALEN_KEY, "6");
		List <LegStarMessagePart> inputParts = new ArrayList <LegStarMessagePart>();
		LegStarMessagePart inCommarea = new CommareaPart(Util.toByteArray("F0F0F0F1F0F0"));
		inputParts.add(inCommarea);
		LegStarHeaderPart dp = new LegStarHeaderPart(map, inputParts.size());
		LegStarMessage requestMessage = new LegStarMessage(dp, inputParts);
		LegStarRequest request = new LegStarRequest("Request01", address, requestMessage);
		return request;
		
	}

}
