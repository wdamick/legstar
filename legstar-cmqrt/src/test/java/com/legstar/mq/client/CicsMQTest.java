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
package com.legstar.mq.client;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import java.rmi.server.UID;


import org.apache.commons.configuration.HierarchicalConfiguration;

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

public class CicsMQTest extends TestCase {
	
	private static final String CONFIG_FILE = "config.xml";
	
	private static final boolean HOST_TRACE_MODE = true;

	private CicsMQEndpoint mMQEndpoint;
	
	/** Time out (in milliseconds) for initial connect. */
	private static final int DEFAULT_CONNECT_TIMEOUT_MSEC = 1000;
	
	/** Time out (in milliseconds) for read operations
	 *  (waiting for host reply). */
	private static final int DEFAULT_READ_TIMEOUT_MSEC = 6000;
	
	protected void setUp() throws Exception {
		super.setUp();
		HierarchicalConfiguration endpointConfig =
			Config.loadEndpointConfiguration(
					Config.loadGeneralConfig(CONFIG_FILE), "TheMainframe");
		mMQEndpoint = new CicsMQEndpoint(endpointConfig);
	}
	
	public void testInstanciate() throws Exception {
		CicsMQ cicsMQ = new CicsMQ("testInstantiation", mMQEndpoint, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
		assertFalse(cicsMQ == null);
	}
	
	public void testConnectClose() {
		try {
			CicsMQ cicsMQ = new CicsMQ("testInstantiation", mMQEndpoint, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
			cicsMQ.connect("tiramisu");
			cicsMQ.close();
		} catch (ConnectionException e) {
			fail(e.getMessage());
		} catch (RequestException e) {
			fail(e.getMessage());
		}
	}
	
	public void testConnectWrongHost() {
		try {
			mMQEndpoint.setHostIPAddress(" ");
			CicsMQ cicsMQ = new CicsMQ("testInstantiation", mMQEndpoint, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
			cicsMQ.connect("tiramisu");
			fail("testConnectFailure");
		} catch (ConnectionException e) {
			assertTrue(e.getMessage().contains("MQJE010"));
		}
	}

	public void testConnectWrongPort() {
		try {
			mMQEndpoint.setHostIPPort(1517);
			CicsMQ cicsMQ = new CicsMQ("testInstantiation", mMQEndpoint, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
			cicsMQ.connect("tiramisu");
			fail("testConnectFailure");
		} catch (ConnectionException e) {
			assertTrue(e.getMessage().contains("MQJE011"));
		}
	}
	
	public void testConnectReuse() {
		try {
			CicsMQ cicsMQ = new CicsMQ("testInstantiation", mMQEndpoint, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
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
	
	public void testStandardRequest() {
		try {
			CicsMQ cicsMQ = new CicsMQ("testStandardRequest", mMQEndpoint, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
			cicsMQ.connect("tiramisu");
			LegStarRequest request = createStdRequest();
			request.getAddress().setHostTraceMode(HOST_TRACE_MODE);
			cicsMQ.sendRequest(request);
			cicsMQ.recvResponse(request);
			assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
			assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
					Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
			cicsMQ.close();
		} catch (ConnectionException e) {
			fail(e.getMessage());
		} catch (RequestException e) {
			fail(e.getMessage());
		} catch (HeaderPartException e) {
			fail(e.getMessage());
		}
	}

	public void testInvalidRequest() {
		CicsMQ cicsMQ = null;
		try {
			cicsMQ = new CicsMQ("testInvalidRequest", mMQEndpoint, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
			cicsMQ.connect("tiramisu");
			LegStarRequest request = createInvalidRequest();
			request.getAddress().setHostTraceMode(HOST_TRACE_MODE);
			cicsMQ.sendRequest(request);
			cicsMQ.recvResponse(request);
			fail();
		} catch (ConnectionException e) {
			fail(e.getMessage());
		} catch (RequestException e) {
			assertEquals("com.legstar.messaging.HostReceiveException: CICS command=LINK COMMAREA failed, resp=PGMIDERR, resp2=3", e.getMessage());
		} catch (HeaderPartException e) {
			fail(e.getMessage());
		} finally {
			if (cicsMQ != null) {
				try {
					cicsMQ.close();
				} catch (RequestException e) {
					e.printStackTrace();
				}
			}
		}
	}

	public void testLongRequest() {
		try {
			CicsMQ cicsMQ = new CicsMQ("testLongRequest", mMQEndpoint, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
			cicsMQ.connect("tiramisu");
			LegStarRequest request = createLongRequest("f3");
			request.getAddress().setHostTraceMode(HOST_TRACE_MODE);
			cicsMQ.sendRequest(request);
			cicsMQ.recvResponse(request);
			assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
			assertTrue(Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()).
					startsWith("f0f0f0f0f0f0f0f3"));
			cicsMQ.close();
		} catch (ConnectionException e) {
			fail(e.getMessage());
		} catch (RequestException e) {
			fail(e.getMessage());
		} catch (HeaderPartException e) {
			fail(e.getMessage());
		}
	}

	public void testLargeRequest() {
		try {
			byte[] startEC = Util.toByteArray("d7c7d47ec9c7e8c3d9c3e3d36bd9c5c7");
			byte[] endEC = Util.toByteArray("d7c1d9d47e4d7dd5d6c4e8d5c1d46bd3");
			CicsMQ cicsMQ = new CicsMQ("testLargeRequest", mMQEndpoint, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
			cicsMQ.connect("tiramisu");
			LegStarRequest request = createLargeRequest();
			request.getAddress().setHostTraceMode(HOST_TRACE_MODE);
			cicsMQ.sendRequest(request);
			cicsMQ.recvResponse(request);
			System.arraycopy(request.getResponseMessage().getDataParts().get(0).getContent(), 0, startEC, 0, 16);
			System.arraycopy(request.getResponseMessage().getDataParts().get(0).getContent(), 32751, endEC, 0, 16);
			assertEquals("d7c1d9d47e4d7dd5d6c4e8d5c1d46bd3", Util.toHexString(startEC));
			assertEquals("d7c7d47ec9c7e8c3d9c3e3d36bd9c5c7", Util.toHexString(endEC));
			cicsMQ.close();
		} catch (ConnectionException e) {
			fail(e.getMessage());
		} catch (RequestException e) {
			fail(e.getMessage());
		} catch (HeaderPartException e) {
			fail(e.getMessage());
		}
	}

	/**
	 * Sending 2 requests, the second one being shorter than the first one and then waiting
	 * for the first one to come back. This way, when the second one finally replies, there
	 * are actually 2 messages in the reply queue. This ensures that we pick-up the right one
	 * for each request.
	 * @throws InterruptedException 
	 */
	public void testLongRequestSequence() throws InterruptedException {
		try {
			CicsMQ cicsMQ = new CicsMQ("testLongRequest", mMQEndpoint, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
			cicsMQ.connect("tiramisu");
			LegStarRequest request1 = createLongRequest("f3");
			request1.getAddress().setHostTraceMode(HOST_TRACE_MODE);
			cicsMQ.sendRequest(request1);
			
			LegStarRequest request2 = createLongRequest("f2");
			request2.getAddress().setHostTraceMode(HOST_TRACE_MODE);
			cicsMQ.sendRequest(request2);
			
			cicsMQ.recvResponse(request1);
			assertEquals(1, request1.getResponseMessage().getHeaderPart().getDataPartsNumber());
			assertEquals(39, request1.getResponseMessage().getDataParts().get(0).getContent().length);
			assertTrue(Util.toHexString(request1.getResponseMessage().getDataParts().get(0).getContent()).
					startsWith("f0f0f0f0f0f0f0f3"));
			
			cicsMQ.recvResponse(request2);
			assertEquals(1, request2.getResponseMessage().getHeaderPart().getDataPartsNumber());
			assertEquals(39, request2.getResponseMessage().getDataParts().get(0).getContent().length);
			assertTrue(Util.toHexString(request2.getResponseMessage().getDataParts().get(0).getContent()).
					startsWith("f0f0f0f0f0f0f0f2"));
			cicsMQ.close();
		} catch (ConnectionException e) {
			fail(e.getMessage());
		} catch (RequestException e) {
			fail(e.getMessage());
		} catch (HeaderPartException e) {
			fail(e.getMessage());
		}
	}

	/**
	 * Handler transaction LEGQ belongs to transaction class LEGQTCLS which has a MAXACTIVE of 2.
	 * By sending more than 2 long request, we are sure to reach a high load situation.
	 * @throws InterruptedException 
	 */
	public void testHighLoad() throws InterruptedException {
		try {
			CicsMQ cicsMQ = new CicsMQ("testLongRequest", mMQEndpoint, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
			cicsMQ.connect("tiramisu");

			LegStarRequest request1 = createLongRequest("f4");
			request1.getAddress().setHostTraceMode(HOST_TRACE_MODE);
			cicsMQ.sendRequest(request1);
			
			LegStarRequest request2 = createLongRequest("f3");
			request2.getAddress().setHostTraceMode(HOST_TRACE_MODE);
			cicsMQ.sendRequest(request2);
			
			LegStarRequest request3 = createLongRequest("f2");
			request3.getAddress().setHostTraceMode(HOST_TRACE_MODE);
			cicsMQ.sendRequest(request3);
			
			LegStarRequest request4 = createLongRequest("f1");
			request4.getAddress().setHostTraceMode(HOST_TRACE_MODE);
			cicsMQ.sendRequest(request4);

			cicsMQ.recvResponse(request1);
			assertEquals(1, request1.getResponseMessage().getHeaderPart().getDataPartsNumber());
			assertEquals(39, request1.getResponseMessage().getDataParts().get(0).getContent().length);
			assertTrue(Util.toHexString(request1.getResponseMessage().getDataParts().get(0).getContent()).
					startsWith("f0f0f0f0f0f0f0f4"));

			cicsMQ.recvResponse(request2);
			assertEquals(1, request2.getResponseMessage().getHeaderPart().getDataPartsNumber());
			assertEquals(39, request2.getResponseMessage().getDataParts().get(0).getContent().length);
			assertTrue(Util.toHexString(request2.getResponseMessage().getDataParts().get(0).getContent()).
					startsWith("f0f0f0f0f0f0f0f3"));

			cicsMQ.recvResponse(request3);
			assertEquals(1, request3.getResponseMessage().getHeaderPart().getDataPartsNumber());
			assertEquals(39, request3.getResponseMessage().getDataParts().get(0).getContent().length);
			assertTrue(Util.toHexString(request3.getResponseMessage().getDataParts().get(0).getContent()).
					startsWith("f0f0f0f0f0f0f0f2"));

			cicsMQ.recvResponse(request4);
			assertEquals(1, request4.getResponseMessage().getHeaderPart().getDataPartsNumber());
			assertEquals(39, request4.getResponseMessage().getDataParts().get(0).getContent().length);
			assertTrue(Util.toHexString(request4.getResponseMessage().getDataParts().get(0).getContent()).
					startsWith("f0f0f0f0f0f0f0f1"));
			cicsMQ.close();
		} catch (ConnectionException e) {
			fail(e.getMessage());
		} catch (RequestException e) {
			fail(e.getMessage());
		} catch (HeaderPartException e) {
			fail(e.getMessage());
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
		String uid = new UID().toString();
		String[] comps = uid.split(":");
		String requestID = comps[1] + comps[2];
		LegStarRequest request = new LegStarRequest(requestID, address, requestMessage);
		return request;
	}
	
	private LegStarRequest createLongRequest(String hexSleepTime) throws HeaderPartException {
		LegStarAddress address = new LegStarAddress("TheMainframe");
		HashMap < String, Object> map = new HashMap < String, Object>();
		map.put(Constants.CICS_PROGRAM_NAME_KEY, "T1SLEEPT");
		map.put(Constants.CICS_LENGTH_KEY, "39");
		map.put(Constants.CICS_DATALEN_KEY, "8");
		List <LegStarMessagePart> inputParts = new ArrayList <LegStarMessagePart>();
		/* will sleep for hexSleepTime secs */
		LegStarMessagePart inCommarea = new CommareaPart(Util.toByteArray("f0f0f0f0f0f0f0" + hexSleepTime));
		inputParts.add(inCommarea);
		LegStarHeaderPart dp = new LegStarHeaderPart(map, inputParts.size());
		LegStarMessage requestMessage = new LegStarMessage(dp, inputParts);
		String uid = new UID().toString();
		String[] comps = uid.split(":");
		String requestID = comps[1] + comps[2];
		LegStarRequest request = new LegStarRequest(requestID, address, requestMessage);
		return request;
	}

	private LegStarRequest createLargeRequest() throws HeaderPartException {
		LegStarAddress address = new LegStarAddress("TheMainframe");
		HashMap < String, Object> map = new HashMap < String, Object>();
		map.put(Constants.CICS_PROGRAM_NAME_KEY, "T1VOLUME");
		map.put(Constants.CICS_LENGTH_KEY, "32767");
		map.put(Constants.CICS_DATALEN_KEY, "32767");
		List <LegStarMessagePart> inputParts = new ArrayList <LegStarMessagePart>();
		byte[] content = new byte[32767];
		byte[] startEC = Util.toByteArray("d7c7d47ec9c7e8c3d9c3e3d36bd9c5c7");
		byte[] endEC = Util.toByteArray("d7c1d9d47e4d7dd5d6c4e8d5c1d46bd3");
		System.arraycopy(startEC, 0, content, 0, 16);
		System.arraycopy(endEC, 0, content, 32751, 16);
		LegStarMessagePart inCommarea1 = new CommareaPart(content);
		inputParts.add(inCommarea1);
		LegStarHeaderPart dp = new LegStarHeaderPart(map, inputParts.size());
		LegStarMessage requestMessage = new LegStarMessage(dp, inputParts);
		String uid = new UID().toString();
		String[] comps = uid.split(":");
		String requestID = comps[1] + comps[2];
		LegStarRequest request = new LegStarRequest(requestID, address, requestMessage);
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
		String uid = new UID().toString();
		String[] comps = uid.split(":");
		String requestID = comps[1] + comps[2];
		LegStarRequest request = new LegStarRequest(requestID, address, requestMessage);
		return request;
		
	}
}
