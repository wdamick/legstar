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

public class CicsHttpPerformanceTest extends TestCase {
	private static final String CONFIG_FILE = "config.xml";
	
	private CicsHttpEndpoint mHttpEndpointTS23;
	private CicsHttpEndpoint mHttpEndpointTS31;
	/** Time out (in milliseconds) for initial connect. */
	private static final int DEFAULT_CONNECT_TIMEOUT_MSEC = 1000;
	/** Time out (in milliseconds) for read operations
	 *  (waiting for host reply). */
	private static final int DEFAULT_READ_TIMEOUT_MSEC = 5000;
	
	protected void setUp() throws Exception {
		super.setUp();
		HierarchicalConfiguration endpointTS23 =
			Config.loadEndpointConfiguration(
					Config.loadGeneralConfig(CONFIG_FILE), "TheMainframe");
		HierarchicalConfiguration endpointTS31 =
			Config.loadEndpointConfiguration(
					Config.loadGeneralConfig(CONFIG_FILE), "TheMainframeTS31");
		mHttpEndpointTS23 = new CicsHttpEndpoint(endpointTS23);
		mHttpEndpointTS31 = new CicsHttpEndpoint(endpointTS31);
	}
	/* with CICS TS 2.3, there is no support for HTTP 1.1. HTTPClient will not keep
	 * the session alive. The log should show 2 physical TCPIP opens */
	public void testSend2Requests() {
		try {
			CicsHttp cicsHttp = new CicsHttp("testSend2Requests", mHttpEndpointTS23, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
			cicsHttp.setConnectTimeout(2000);
			cicsHttp.connect(null); // let config pick the password
			LegStarRequest request = createStdRequest();
			cicsHttp.sendRequest(request);
			cicsHttp.recvResponse(request);
			assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
			assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
					  Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
			cicsHttp.sendRequest(request);
			cicsHttp.recvResponse(request);
			assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
			assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
					  Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
		} catch (HeaderPartException e) {
			fail("testSend2Requests failed " + e);
		} catch (ConnectionException e) {
			fail("testSend2Requests failed " + e);
		} catch (RequestException e) {
			fail("testSend2Requests failed " + e);
		}
	}
	
	/* with CICS TS 3.1, there is support for HTTP 1.1. HTTPClient will keep
	 * the session alive. The log should show 1 physical TCPIP open only */
	public void testSend2RequestsTs31() {
		try {
			CicsHttp cicsHttp = new CicsHttp("testSend2Requests", mHttpEndpointTS31, DEFAULT_CONNECT_TIMEOUT_MSEC, DEFAULT_READ_TIMEOUT_MSEC);
			cicsHttp.setConnectTimeout(2000);
			cicsHttp.connect(null); // let config pick the password
			LegStarRequest request = createStdRequest();
			cicsHttp.sendRequest(request);
			cicsHttp.recvResponse(request);
			assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
			assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
					  Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
			cicsHttp.sendRequest(request);
			cicsHttp.recvResponse(request);
			assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
			assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
					  Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
		} catch (HeaderPartException e) {
			fail("testSend2Requests failed " + e);
		} catch (ConnectionException e) {
			fail("testSend2Requests failed " + e);
		} catch (RequestException e) {
			fail("testSend2Requests failed " + e);
		}
	}

	private LegStarRequest createStdRequest() throws HeaderPartException {
		LegStarAddress address = new LegStarAddress("TheMainframe");
		HashMap < String, Object > map = new HashMap < String, Object >();
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


}
