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
package com.legstar.csok.test;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.configuration.ConfigurationException;

import com.legstar.config.Constants;
import com.legstar.csok.client.CicsSocket;
import com.legstar.csok.client.CicsSocketEndpoint;
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

public class VolumeTest extends TestCase {

	private static final int MAX_ITERATIONS = 5;
	
	private static final String HOST_CHARSET = "IBM01140";
	
	/** Single thread iterating through simple requests with wait time
	 * @throws UnsupportedEncodingException */
	public void testSingleIterateSimpleWithWait() throws UnsupportedEncodingException {
		long startTime = System.currentTimeMillis();
		try {
			CicsSocketEndpoint endpoint = Util.getEndpoint("TheMainframe");
			endpoint.setHostTraceMode(false); // dont flood the host
			CicsSocket cs = new CicsSocket("testSingleIterateSimpleWithWait", endpoint, 1000, 5000);
			cs.connect("STREAM2");
			HashMap < String, Object > map = new HashMap < String, Object >();
			map.put(Constants.CICS_PROGRAM_NAME_KEY, "T1SLEEPT");
			map.put(Constants.CICS_LENGTH_KEY, "39");
			map.put(Constants.CICS_DATALEN_KEY, "8");
			List <LegStarMessagePart> inputParts = new ArrayList <LegStarMessagePart>();
			LegStarMessagePart inCommarea1 = new CommareaPart(Util.toByteArray("f0f0f0f0f0f0f0f3"));
			inputParts.add(inCommarea1);
			LegStarHeaderPart dp = new LegStarHeaderPart(map, inputParts.size());
			LegStarAddress address = new LegStarAddress("TheMainframe");
			LegStarMessage requestMessage = new LegStarMessage(dp, inputParts);
			LegStarRequest request = new LegStarRequest("testSingleIterateSimpleWithWait", address, requestMessage);
			for (int i = 0; i < MAX_ITERATIONS; i++) {
				cs.sendRequest(request);
				cs.recvResponse(request);
				cs.keepUOW();
				String reply = new String(request.getResponseMessage().getDataParts().get(0).getContent(), HOST_CHARSET);
				assertEquals("00000003P390    LEG1", reply.substring(0, 20));
			}
			cs.close();
			long endTime = System.currentTimeMillis();
			System.out.println("Duration millisecs=" + (endTime - startTime));
		} catch (HeaderPartException e) {
			fail("testConnectSendPart failed=" + e);
		} catch (ConnectionException e) {
			fail("testSingleIterateSimpleWithWait failed=" + e);
		} catch (RequestException e) {
			fail("testSingleIterateSimpleWithWait failed=" + e);
		} catch (ConfigurationException e) {
			fail("testSingleIterateSimpleWithWait failed=" + e);
		}
	}

	/** Single thread iterating through simple requests
	 * @throws UnsupportedEncodingException */
	public void testSingleIterateSimpleConnectionReused() throws UnsupportedEncodingException {
		long startTime = System.currentTimeMillis();
		try {
			CicsSocketEndpoint endpoint = Util.getEndpoint("TheMainframe");
			endpoint.setHostTraceMode(false); // dont flood the host
			CicsSocket cs = new CicsSocket("testSingleIterateSimpleConnectionReused", endpoint, 1000, 5000);
			cs.connect("STREAM2");
			HashMap < String, Object > map = new HashMap < String, Object >();
			map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAE");
			map.put(Constants.CICS_LENGTH_KEY, "79");
			map.put(Constants.CICS_DATALEN_KEY, "6");
			List <LegStarMessagePart> inputParts = new ArrayList <LegStarMessagePart>();
			LegStarMessagePart inCommarea = new CommareaPart(Util.toByteArray("F0F0F0F1F0F0"));
			inputParts.add(inCommarea);
			LegStarHeaderPart dp = new LegStarHeaderPart(map, inputParts.size());
			LegStarAddress address = new LegStarAddress("TheMainframe");
			LegStarMessage requestMessage = new LegStarMessage(dp, inputParts);
			LegStarRequest request = new LegStarRequest("testSingleIterateSimpleConnectionReused", address, requestMessage);
			for (int i = 0; i < MAX_ITERATIONS; i++) {
				cs.sendRequest(request);
				cs.recvResponse(request);
				cs.commitUOW();
				assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
						Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
			}
			cs.close();
			long endTime = System.currentTimeMillis();
			System.out.println("Duration millisecs=" + (endTime - startTime));
		} catch (HeaderPartException e) {
			fail("testConnectSendPart failed=" + e);
		} catch (ConnectionException e) {
			fail("testSingleIterateSimpleConnectionReused failed=" + e);
		} catch (RequestException e) {
			fail("testSingleIterateSimpleConnectionReused failed=" + e);
		} catch (ConfigurationException e) {
			fail("testSingleIterateSimpleConnectionReused failed=" + e);
		}
	}

	/** Single thread iterating through simple requests
	 * @throws UnsupportedEncodingException */
	public void testSingleIterateSimple() throws UnsupportedEncodingException {
		long startTime = System.currentTimeMillis();
		try {
			CicsSocketEndpoint endpoint = Util.getEndpoint("TheMainframe");
			endpoint.setHostTraceMode(false); // dont flood the host
			CicsSocket cs = new CicsSocket("testSingleIterateSimple", endpoint, 1000, 5000);
			HashMap < String, Object > map = new HashMap < String, Object >();
			map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAE");
			map.put(Constants.CICS_LENGTH_KEY, "79");
			map.put(Constants.CICS_DATALEN_KEY, "6");
			List <LegStarMessagePart> inputParts = new ArrayList <LegStarMessagePart>();
			LegStarMessagePart inCommarea = new CommareaPart(Util.toByteArray("F0F0F0F1F0F0"));
			inputParts.add(inCommarea);
			LegStarHeaderPart dp = new LegStarHeaderPart(map, inputParts.size());
			LegStarAddress address = new LegStarAddress("TheMainframe");
			LegStarMessage requestMessage = new LegStarMessage(dp, inputParts);
			LegStarRequest request = new LegStarRequest("testSingleIterateSimple", address, requestMessage);
			for (int i = 0; i < MAX_ITERATIONS; i++) {
				cs.connect("STREAM2");
				cs.sendRequest(request);
				cs.recvResponse(request);
				cs.commitUOW();
				cs.close();
				assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
						Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
			}
			long endTime = System.currentTimeMillis();
			System.out.println("Duration millisecs=" + (endTime - startTime));
		} catch (HeaderPartException e) {
			fail("testConnectSendPart failed=" + e);
		} catch (ConnectionException e) {
			fail("testSingleIterateSimple failed=" + e);
		} catch (RequestException e) {
			fail("testSingleIterateSimple failed=" + e);
		} catch (ConfigurationException e) {
			fail("testSingleIterateSimple failed=" + e);
		}
	}

	/** Single thread iterates thru large data requests
	 * @throws UnsupportedEncodingException */
	public void testSingleIterateVolume() throws UnsupportedEncodingException {
		try {
			CicsSocketEndpoint endpoint = Util.getEndpoint("TheMainframe");
			endpoint.setHostTraceMode(true); // dont flood the host
			CicsSocket cs = new CicsSocket("testSingleIterateVolume", endpoint, 1000, 5000);
			cs.connect("STREAM2");
			HashMap < String, Object > map = new HashMap < String, Object >();
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
			LegStarAddress address = new LegStarAddress("TheMainframe");
			LegStarMessage requestMessage = new LegStarMessage(dp, inputParts);
			LegStarRequest request = new LegStarRequest("testSingleIterateVolume", address, requestMessage);
			for (int i = 0; i < MAX_ITERATIONS; i++) {
				cs.sendRequest(request);
				cs.recvResponse(request);
				cs.keepUOW();
				System.arraycopy(request.getResponseMessage().getDataParts().get(0).getContent(), 0, startEC, 0, 16);
				System.arraycopy(request.getResponseMessage().getDataParts().get(0).getContent(), 32751, endEC, 0, 16);
				assertEquals("d7c1d9d47e4d7dd5d6c4e8d5c1d46bd3", Util.toHexString(startEC));
				assertEquals("d7c7d47ec9c7e8c3d9c3e3d36bd9c5c7", Util.toHexString(endEC));
			}
			cs.close();
		} catch (HeaderPartException e) {
			fail("testConnectSendPart failed=" + e);
		} catch (ConnectionException e) {
			fail("testSingleIterateVolume failed=" + e);
		} catch (RequestException e) {
			e.printStackTrace();
			fail("testSingleIterateVolume failed=" + e);
		} catch (ConfigurationException e) {
			fail("testSingleIterateVolume failed=" + e);
		}
	}
	
}
