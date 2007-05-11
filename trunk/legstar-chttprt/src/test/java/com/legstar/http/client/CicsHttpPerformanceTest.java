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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.configuration.HierarchicalConfiguration;

import com.legstar.config.Config;
import com.legstar.config.Constants;
import com.legstar.messaging.Address;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.HeaderPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.Message;
import com.legstar.messaging.MessagePart;
import com.legstar.messaging.Request;
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
			Request request = createStdRequest();
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
			Request request = createStdRequest();
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

	private Request createStdRequest() throws HeaderPartException {
		Address address = new Address("TheMainframe");
		HashMap < String, Object > map = new HashMap < String, Object >();
		map.put(Constants.CICS_PROGRAM_KEY, "LSFILEAE");
		map.put(Constants.CICS_LENGTH_KEY, "79");
		map.put(Constants.CICS_DATALEN_KEY, "6");
		List <MessagePart> inputParts = new ArrayList <MessagePart>();
		MessagePart inCommarea = new CommareaPart(Util.toByteArray("F0F0F0F1F0F0"));
		inputParts.add(inCommarea);
		HeaderPart dp = new HeaderPart(map, inputParts.size());
		Message requestMessage = new Message(dp, inputParts);
		Request request = new Request("Request01", address, requestMessage);
		return request;
		
	}


}
