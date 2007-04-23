/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.http.client;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.httpclient.auth.AuthScope;
import org.apache.commons.httpclient.methods.PostMethod;

import com.legstar.config.Config;
import com.legstar.config.Constants;
import com.legstar.messaging.Address;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.HeaderPart;
import com.legstar.messaging.Message;
import com.legstar.messaging.MessagePart;
import com.legstar.messaging.Request;
import com.legstar.messaging.RequestException;

import junit.framework.TestCase;

public class CicsHttpTest extends TestCase {

	private static final String CONFIG_FILE = "config.xml";
	
	private CicsHttpConnectionFactory mfactory;
	
	protected void setUp() throws Exception {
		super.setUp();
		HierarchicalConfiguration endpointConfig =
			Config.loadEndpointConfiguration(
					Config.loadGeneralConfig(CONFIG_FILE), "TheMainframe");
		mfactory =
			new CicsHttpConnectionFactory(endpointConfig);
	}
	
	public void testInstantiation() {
		try {
			Address address = new Address("TheMainframe");
			CicsHttp cicsHttp = (CicsHttp) mfactory.createConnection("testPostMethodCreation", address);
			assertEquals("192.168.0.110", cicsHttp.getHostConfig().getHost());
			assertEquals(3080, cicsHttp.getHostConfig().getPort());
			assertEquals(cicsHttp.getHostConfig().getHost(), cicsHttp.getCicsHttpEndpoint().getHostIPAddress());
			assertEquals(cicsHttp.getHostConfig().getPort(), cicsHttp.getCicsHttpEndpoint().getHostIPPort());
			assertEquals(true, cicsHttp.getHttpClient().getParams().isAuthenticationPreemptive());
		} catch (ConnectionException e) {
			fail("testInstantiation failed " + e);
		}
	}
	
	public void testConnect() {
		try {
			Address address = new Address("TheMainframe");
			CicsHttp cicsHttp = (CicsHttp) mfactory.createConnection("testPostMethodCreation", address);
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
			Address address = new Address("TheMainframe");
			CicsHttp cicsHttp = (CicsHttp) mfactory.createConnection("testPostMethodCreation", address);

			HashMap < String, String > map = new HashMap < String, String >();
			map.put(Constants.CICS_PROGRAM_KEY, "LSFILEAE");
			map.put(Constants.CICS_LENGTH_KEY, "79");
			map.put(Constants.CICS_DATALEN_KEY, "6");
			map.put(Constants.CICS_SYSID_KEY, "CICZ");
			map.put(Constants.CICS_SYNCONRET_KEY, "true");
			map.put(Constants.CICS_TRANSID_KEY, "MIRO");
			List <MessagePart> inputParts = new ArrayList <MessagePart>();
			MessagePart inCommarea = new CommareaPart(Util.toByteArray("F0F0F0F1F0F0"));
			inputParts.add(inCommarea);
			HeaderPart dp = new HeaderPart(map, inputParts.size(), "IBM01140");
			Message requestMessage = new Message(dp, inputParts);
			Request request = new Request("Request01", address, requestMessage);
			
			PostMethod postMethod = cicsHttp.createPostMethod(request);
			assertEquals("POST", postMethod.getName());
			assertEquals("CICSProgram: LSFILEAE", postMethod.getRequestHeader(Constants.CICS_PROGRAM_KEY).toString().trim());
			assertEquals("CICSLength: 79", postMethod.getRequestHeader(Constants.CICS_LENGTH_KEY).toString().trim());
			assertEquals("CICSDataLength: 6", postMethod.getRequestHeader(Constants.CICS_DATALEN_KEY).toString().trim());
			assertEquals("CICSSysID: CICZ", postMethod.getRequestHeader(Constants.CICS_SYSID_KEY).toString().trim());
			assertEquals("CICSSyncOnReturn: true", postMethod.getRequestHeader(Constants.CICS_SYNCONRET_KEY).toString().trim());
			assertEquals("CICSTransID: MIRO", postMethod.getRequestHeader(Constants.CICS_TRANSID_KEY).toString().trim());
			assertEquals("/CICS/CWBA/LSWEBBIN", postMethod.getPath());
			
		} catch (UnsupportedEncodingException e) {
			fail("testPostMethodCreation failed " + e);
		} catch (ConnectionException e) {
			fail("testPostMethodCreation failed " + e);
		} catch (RequestException e) {
			fail("testPostMethodCreation failed " + e);
		}
	}
	
	public void testSendRequestOutOfSync() {
		try {
			Address address = new Address("TheMainframe");
			CicsHttp cicsHttp = (CicsHttp) mfactory.createConnection("testPostMethodCreation", address);
			cicsHttp.sendRequest(createStdRequest());
			fail("testPostMethodCreation failed ");
			
		} catch (UnsupportedEncodingException e) {
			fail("testSendRequestOutOfSync failed " + e);
		} catch (ConnectionException e) {
			fail("testSendRequestOutOfSync failed " + e);
		} catch (RequestException e) {
			assertEquals("No prior connect. Missing host credentials.", e.getMessage());
		}
	}

	public void testSendRequestToWrongIPAddress() {
		try {
			Address address = new Address("TheMainframe");
			CicsHttp cicsHttp = (CicsHttp) mfactory.createConnection("testPostMethodCreation", address);
			CicsHttpEndpoint endPoint = cicsHttp.getCicsHttpEndpoint();
			endPoint.setHostIPAddress("192.168.0.117");
			cicsHttp.setCicsHttpEndpoint(endPoint);
			cicsHttp.setConnectTimeout(1500);
			cicsHttp.connect("zaratoustra");
			cicsHttp.sendRequest(createStdRequest());
			fail("testPostMethodCreation failed ");
			
		} catch (UnsupportedEncodingException e) {
			fail("testSendRequestOutOfSync failed " + e);
		} catch (ConnectionException e) {
			fail("testSendRequestOutOfSync failed " + e);
		} catch (RequestException e) {
			assertEquals("org.apache.commons.httpclient.ConnectTimeoutException: The host did not accept the connection within timeout of 1500 ms", e.getMessage());
		}
	}

	public void testSendRequestToWrongIPPort() {
		try {
			Address address = new Address("TheMainframe");
			CicsHttp cicsHttp = (CicsHttp) mfactory.createConnection("testPostMethodCreation", address);
			CicsHttpEndpoint endPoint = cicsHttp.getCicsHttpEndpoint();
			endPoint.setHostIPPort(12768);
			cicsHttp.setCicsHttpEndpoint(endPoint);
			cicsHttp.setConnectTimeout(1500);
			cicsHttp.connect("zaratoustra");
			cicsHttp.sendRequest(createStdRequest());
			fail("testPostMethodCreation failed ");
			
		} catch (UnsupportedEncodingException e) {
			fail("testSendRequestOutOfSync failed " + e);
		} catch (ConnectionException e) {
			fail("testSendRequestOutOfSync failed " + e);
		} catch (RequestException e) {
			assertEquals("java.net.ConnectException: Connection refused: connect", e.getMessage());
		}
	}

	public void testSendRequestWithWrongUserID() {
		try {
			Address address = new Address("TheMainframe");
			CicsHttp cicsHttp = (CicsHttp) mfactory.createConnection("testPostMethodCreation", address);
			CicsHttpEndpoint endPoint = cicsHttp.getCicsHttpEndpoint();
			endPoint.setHostUserID("tartempion");
			cicsHttp.setCicsHttpEndpoint(endPoint);
			cicsHttp.setConnectTimeout(1500);
			cicsHttp.connect("zaratoustra");
			Request request = createStdRequest();
			cicsHttp.sendRequest(request);
			cicsHttp.recvResponse(request);
			fail("testPostMethodCreation failed ");
			
		} catch (UnsupportedEncodingException e) {
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
			Address address = new Address("TheMainframe");
			CicsHttp cicsHttp = (CicsHttp) mfactory.createConnection("testPostMethodCreation", address);

			cicsHttp.connect("STREAM2");
			Request request = createStdRequest();
			cicsHttp.sendRequest(request);
			cicsHttp.recvResponse(request);
			assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
			assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
					  Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
		} catch (UnsupportedEncodingException e) {
			fail("testSendRequest failed " + e);
		} catch (ConnectionException e) {
			fail("testSendRequest failed " + e);
		} catch (RequestException e) {
			fail("testSendRequest failed " + e);
		}
	}
	
	public void testSendRequestWithHostTracesAndNoDefaultCredentials() {
		try {
			/* This config has no default credentials, also the listener has basic
			 * auth turned on */
			HierarchicalConfiguration endpointConfig =
				Config.loadEndpointConfiguration(
						Config.loadGeneralConfig("config4.xml"), "TheMainframe");
			mfactory =
				new CicsHttpConnectionFactory(endpointConfig);
			Address address = new Address("TheMainframe");
			address.setHostUserID("IBMUSER");
			address.setHostPassword("STREAM2");
			address.setHostTraceMode(true);
			CicsHttp cicsHttp = (CicsHttp) mfactory.createConnection("testPostMethodCreation", address);

			cicsHttp.connect("STREAM2");
			Request request = createStdRequest();
			cicsHttp.sendRequest(request);
			cicsHttp.recvResponse(request);
			assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
			assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
					  Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
		} catch (UnsupportedEncodingException e) {
			fail("testSendRequestWithHostTracesAndDefaultCredentials failed " + e);
		} catch (ConnectionException e) {
			fail("testSendRequestWithHostTracesAndDefaultCredentials failed " + e);
		} catch (RequestException e) {
			fail("testSendRequestWithHostTracesAndDefaultCredentials failed " + e);
		} catch (ConfigurationException e) {
			fail("testSendRequestWithHostTracesAndDefaultCredentials failed " + e);
		}
	}
	
	public void testSendRequestWithNoCredentials() {
		try {
			/* This config has no default credentials, also the listener has basic
			 * auth turned on */
			HierarchicalConfiguration endpointConfig =
				Config.loadEndpointConfiguration(
						Config.loadGeneralConfig("config5.xml"), "TheMainframe");
			mfactory =
				new CicsHttpConnectionFactory(endpointConfig);
			Address address = new Address("TheMainframe");
			CicsHttp cicsHttp = (CicsHttp) mfactory.createConnection("testPostMethodCreation", address);

			cicsHttp.connect(null);
			Request request = createStdRequest();
			cicsHttp.sendRequest(request);
			cicsHttp.recvResponse(request);
			assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
			assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
					  Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
		} catch (UnsupportedEncodingException e) {
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
			Address address = new Address("TheMainframe");
			CicsHttp cicsHttp = (CicsHttp) mfactory.createConnection("testPostMethodCreation", address);

			cicsHttp.connect(null); // let config pick the password
			Request request = createStdRequest();
			cicsHttp.sendRequest(request);
			cicsHttp.recvResponse(request);
			assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
			assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
					  Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
			Request request2 = createLongRequest();
			cicsHttp.sendRequest(request2);
			cicsHttp.recvResponse(request2);
			assertEquals(1, request2.getResponseMessage().getHeaderPart().getDataPartsNumber());
			assertEquals("f0f0f0f0f0f0f0f3d7f3f9f040404040c3e6c2c1",
					  Util.toHexString(request2.getResponseMessage().getDataParts().get(0).
							  getContent()).substring(0, 40));
		} catch (UnsupportedEncodingException e) {
			fail("testSend2Requests failed " + e);
		} catch (ConnectionException e) {
			fail("testSend2Requests failed " + e);
		} catch (RequestException e) {
			fail("testSend2Requests failed " + e);
		}
	}
	
	public void testReceiveTimeout() {
		try {
			Address address = new Address("TheMainframe");
			CicsHttp cicsHttp = (CicsHttp) mfactory.createConnection("testPostMethodCreation", address);

			cicsHttp.connect("STREAM2");
			cicsHttp.setReceiveTimeout(2000);
			Request request = createLongRequest(); // Will not respond within 2 secs
			cicsHttp.sendRequest(request);
			cicsHttp.recvResponse(request);
			fail("testReceiveTimeout failed ");
		} catch (UnsupportedEncodingException e) {
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
			Address address = new Address("TheMainframe");
			CicsHttp cicsHttp = (CicsHttp) mfactory.createConnection("testPostMethodCreation", address);

			cicsHttp.connect("STREAM2");
			cicsHttp.setReceiveTimeout(2000);
			Request request = createLongRequest(); // Will not respond within 2 secs
			try {
				cicsHttp.sendRequest(request);
				cicsHttp.recvResponse(request);
			} catch (RequestException e) {
				Request request2 = createStdRequest();
				cicsHttp.sendRequest(request2);
				cicsHttp.recvResponse(request2);
				assertEquals(1, request2.getResponseMessage().getHeaderPart().getDataPartsNumber());
				assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
						  Util.toHexString(request2.getResponseMessage().getDataParts().get(0).getContent()));
			}
		} catch (UnsupportedEncodingException e) {
			fail("testReceiveTimeoutReuseHttpClient failed " + e);
		} catch (ConnectionException e) {
			fail("testReceiveTimeoutReuseHttpClient failed " + e);
		} catch (RequestException e) {
			fail("testReceiveTimeoutReuseHttpClient failed " + e);
		}
	}
	
	public void testSendRequestWithInvalidProg() {
		try {
			Address address = new Address("TheMainframe");
			CicsHttp cicsHttp = (CicsHttp) mfactory.createConnection("testPostMethodCreation", address);

			cicsHttp.connect("STREAM2");
			Request request = createInvalidRequest();
			cicsHttp.sendRequest(request);
			cicsHttp.recvResponse(request);
			fail("testSendRequest failed ");
		} catch (UnsupportedEncodingException e) {
			fail("testSendRequest failed " + e);
		} catch (ConnectionException e) {
			fail("testSendRequest failed " + e);
		} catch (RequestException e) {
			assertTrue(e.getMessage().contains("CICS command=LINK failed, resp=PGMIDERR, resp2=24 "));
		}
	}
	
	private Request createStdRequest() throws UnsupportedEncodingException {
		Address address = new Address("TheMainframe");
		HashMap < String, String > map = new HashMap < String, String >();
		map.put(Constants.CICS_PROGRAM_KEY, "LSFILEAE");
		map.put(Constants.CICS_LENGTH_KEY, "79");
		map.put(Constants.CICS_DATALEN_KEY, "6");
		List <MessagePart> inputParts = new ArrayList <MessagePart>();
		MessagePart inCommarea = new CommareaPart(Util.toByteArray("F0F0F0F1F0F0"));
		inputParts.add(inCommarea);
		HeaderPart dp = new HeaderPart(map, inputParts.size(), "IBM01140");
		Message requestMessage = new Message(dp, inputParts);
		Request request = new Request("Request01", address, requestMessage);
		return request;
		
	}

	private Request createLongRequest() throws UnsupportedEncodingException {
		Address address = new Address("TheMainframe");
		HashMap < String, String > map = new HashMap < String, String >();
		map.put(Constants.CICS_PROGRAM_KEY, "T1SLEEPT");
		map.put(Constants.CICS_LENGTH_KEY, "39");
		map.put(Constants.CICS_DATALEN_KEY, "8");
		List <MessagePart> inputParts = new ArrayList <MessagePart>();
		/* will sleep for 3 secs */
		MessagePart inCommarea = new CommareaPart(Util.toByteArray("f0f0f0f0f0f0f0f3"));
		inputParts.add(inCommarea);
		HeaderPart dp = new HeaderPart(map, inputParts.size(), "IBM01140");
		Message requestMessage = new Message(dp, inputParts);
		Request request = new Request("Request01", address, requestMessage);
		return request;
	}

	private Request createInvalidRequest() throws UnsupportedEncodingException {
		Address address = new Address("TheMainframe");
		HashMap < String, String > map = new HashMap < String, String >();
		map.put(Constants.CICS_PROGRAM_KEY, "TARATOZ");
		map.put(Constants.CICS_LENGTH_KEY, "79");
		map.put(Constants.CICS_DATALEN_KEY, "6");
		List <MessagePart> inputParts = new ArrayList <MessagePart>();
		MessagePart inCommarea = new CommareaPart(Util.toByteArray("F0F0F0F1F0F0"));
		inputParts.add(inCommarea);
		HeaderPart dp = new HeaderPart(map, inputParts.size(), "IBM01140");
		Message requestMessage = new Message(dp, inputParts);
		Request request = new Request("Request01", address, requestMessage);
		return request;
		
	}

}
