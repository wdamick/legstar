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

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.configuration.HierarchicalConfiguration;

import com.legstar.config.Config;
import com.legstar.config.Constants;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.ContainerPart;
import com.legstar.messaging.LegStarHeaderPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarMessagePart;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;

import junit.framework.TestCase;

public class CicsTs31HttpTest extends TestCase {
	
	private static final boolean TRACE_MODE = true;
	private static final int MAX_ITERATIONS = 100;
	
	private static final String CONFIG_FILE = "config.xml";
	
	private CicsHttpEndpoint mHttpEndpoint;
	
	protected void setUp() throws Exception {
		super.setUp();
		HierarchicalConfiguration endpointConfig =
			Config.loadEndpointConfiguration(
					Config.loadGeneralConfig(CONFIG_FILE), "TheMainframeTS31");
		mHttpEndpoint = new CicsHttpEndpoint(endpointConfig);
	}
	
	public void testCommareaProgram() throws UnsupportedEncodingException {
		try {
			mHttpEndpoint.setHostTraceMode(TRACE_MODE);
			CicsHttp cs = new CicsHttp("testHostTraces", mHttpEndpoint, 1000, 5000);
			cs.connect(null);
			HashMap < String, Object > map = new HashMap < String, Object >();
			map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAE");
			map.put(Constants.CICS_LENGTH_KEY, "79");
			map.put(Constants.CICS_DATALEN_KEY, "6");
			List <LegStarMessagePart> inputParts = new ArrayList <LegStarMessagePart>();
			LegStarMessagePart inCommarea = new CommareaPart(Util.toByteArray("F0F0F0F1F0F0"));
			inputParts.add(inCommarea);
			LegStarHeaderPart dp = new LegStarHeaderPart(map, inputParts.size());
			LegStarAddress address = new LegStarAddress("CICSTS31");
			LegStarMessage requestMessage = new LegStarMessage(dp, inputParts);
			LegStarRequest request = new LegStarRequest("Request01", address, requestMessage);
			cs.sendRequest(request);
			cs.recvResponse(request);
			assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
					  Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
			cs.commitUOW();
			cs.close();
		} catch (ConnectionException e) {
			fail("testHostTraces failed=" + e);
		} catch (RequestException e) {
			fail("testHostTraces failed=" + e);
		} catch (HeaderPartException e) {
			fail("testHostTraces failed=" + e);
		}
	}

	public void testShortProgram() throws UnsupportedEncodingException {
		try {
			CicsHttp cs = new CicsHttp("testShortProgram", mHttpEndpoint, 1000, 5000);
			mHttpEndpoint.setHostTraceMode(TRACE_MODE);
			cs.connect(null);
			HashMap < String, Object > map = new HashMap < String, Object >();
			map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFAC");
			map.put(Constants.CICS_CHANNEL_KEY, "LSFAC-CHANNEL");
			String[] outContainers = {"RESPONSE-CTN"};
			map.put(Constants.CICS_OUT_CONTAINERS_KEY, outContainers);
			List <LegStarMessagePart> inputParts = new ArrayList <LegStarMessagePart>();
			LegStarMessagePart inContainer = new ContainerPart("REQUEST-CTN", Util.toByteArray("F0F0F0F1F0F0"));
			inputParts.add(inContainer);
			LegStarHeaderPart dp = new LegStarHeaderPart(map, inputParts.size());
			LegStarAddress address = new LegStarAddress("CICSTS31");
			LegStarMessage requestMessage = new LegStarMessage(dp, inputParts);
			LegStarRequest request = new LegStarRequest("Request01", address, requestMessage);
			cs.sendRequest(request);
			cs.recvResponse(request);
			assertEquals("RESPONSE-CTN",
					  request.getResponseMessage().getDataParts().get(0).getID());
			assertEquals("f0f0f0f1f0f0e24b40c44b40c2d6d9d4c1d54040404040404040e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040f3f2f1f5f6f7f7f8f2f640f1f140f8f15bf0f1f0f04bf1f15c5c5c5c5c5c5c5c5c",
					  Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
			cs.commitUOW();
			cs.close();
		} catch (ConnectionException e) {
			fail("testShortProgram failed=" + e);
		} catch (RequestException e) {
			fail("testShortProgram failed=" + e);
		} catch (HeaderPartException e) {
			fail("testShortProgram failed=" + e);
		}
	}

	public void test2ContainersIn2Out() throws UnsupportedEncodingException {
		try {
			CicsHttp cs = new CicsHttp("test2ContainersIn2Out", mHttpEndpoint, 1000, 5000);
			mHttpEndpoint.setHostTraceMode(TRACE_MODE);
			cs.connect(null);
			HashMap < String, Object > map = new HashMap < String, Object >();
			map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAC");
			map.put(Constants.CICS_CHANNEL_KEY, "LSFILEAC-CHANNEL");
			String[] outContainers = {"ReplyData","ReplyStatus"};
			map.put(Constants.CICS_OUT_CONTAINERS_KEY, outContainers);
			List <LegStarMessagePart> inputParts = new ArrayList <LegStarMessagePart>();
			LegStarMessagePart inQueryData = new ContainerPart("QueryData",
					Util.toByteArray("D45C4040404040404040404040404040404040405C404040404040404040404040404040404040405C40404040404040"));
			inputParts.add(inQueryData);
			LegStarMessagePart inQueryLimit = new ContainerPart("QueryLimit", Util.toByteArray("000000010F000005000F"));
			inputParts.add(inQueryLimit);
			LegStarHeaderPart dp = new LegStarHeaderPart(map, inputParts.size());
			LegStarAddress address = new LegStarAddress("CICSTS31");
			LegStarMessage requestMessage = new LegStarMessage(dp, inputParts);
			LegStarRequest request = new LegStarRequest("Request01", address, requestMessage);
			cs.sendRequest(request);
			cs.recvResponse(request);
			/* Get the status container first */
			assertEquals("ReplyStatus",
					  request.getResponseMessage().getDataParts().get(1).getID());
			assertEquals("0000f0f07af0f07af0f0000000011f00000000000000004040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040",
					  Util.toHexString(request.getResponseMessage().getDataParts().get(1).getContent()));
			/* Then get the data container */
			assertEquals("ReplyData",
					  request.getResponseMessage().getDataParts().get(0).getID());
			assertEquals("000000001ff0f0f0f1f0f4d44b40c24b40c4d6d4c2c5e84040404040404040d3d6d5c4d6d56bc5d5c7d3c1d5c4404040404040f1f2f8f4f6f2f9f3f2f640f1f140f8f15bf0f9f9f94bf9f95c5c5c5c5c5c5c5c5c",
					  Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
			cs.commitUOW();
			cs.close();
		} catch (ConnectionException e) {
			fail("test2ContainersIn2Out failed=" + e);
		} catch (RequestException e) {
			fail("test2ContainersIn2Out failed=" + e);
		} catch (HeaderPartException e) {
			fail("test2ContainersIn2Out failed=" + e);
		}
	}
	
	/* Output containers are always optional. If we request the wrong one, there is no errors
	 * but the content of the wrong container should be null. */
	public void test1ContainersInWithWrongOutContainerName() throws UnsupportedEncodingException {
		try {
			CicsHttp cs = new CicsHttp("test1ContainersInWithWrongOutContainerName", mHttpEndpoint, 1000, 5000);
			mHttpEndpoint.setHostTraceMode(TRACE_MODE);
			cs.connect(null);
			HashMap < String, Object > map = new HashMap < String, Object >();
			map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAC");
			map.put(Constants.CICS_CHANNEL_KEY, "LSFILEAC-CHANNEL");
			String[] outContainers = {"ReplyBibi","ReplyStatus"};
			map.put(Constants.CICS_OUT_CONTAINERS_KEY, outContainers);
			List <LegStarMessagePart> inputParts = new ArrayList <LegStarMessagePart>();
			LegStarMessagePart inQueryData = new ContainerPart("QueryData",
					Util.toByteArray("D45C4040404040404040404040404040404040405C404040404040404040404040404040404040405C40404040404040"));
			inputParts.add(inQueryData);
			LegStarHeaderPart dp = new LegStarHeaderPart(map, inputParts.size());
			LegStarAddress address = new LegStarAddress("CICSTS31");
			LegStarMessage requestMessage = new LegStarMessage(dp, inputParts);
			LegStarRequest request = new LegStarRequest("Request01", address, requestMessage);
			cs.sendRequest(request);
			cs.recvResponse(request);
			/* Get the status container first */
			assertEquals("ReplyStatus",
					  request.getResponseMessage().getDataParts().get(1).getID());
			assertEquals("0000f0f07af0f07af0f0000000044f00000000000000004040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040",
					  Util.toHexString(request.getResponseMessage().getDataParts().get(1).getContent()));
			/* Then get the data container */
			assertEquals("ReplyBibi",
					  request.getResponseMessage().getDataParts().get(0).getID());
			assertTrue(null == request.getResponseMessage().getDataParts().get(0).getContent());
			cs.commitUOW();
			cs.close();
		} catch (ConnectionException e) {
			fail("test1ContainersInWithWrongOutContainerName failed=" + e);
		} catch (RequestException e) {
			fail("test1ContainersInWithWrongOutContainerName failed=" + e);
		} catch (HeaderPartException e) {
			fail("test1ContainersInWithWrongOutContainerName failed=" + e);
		}
	}

	public void test1ContainersIn2Out() throws UnsupportedEncodingException {
		try {
			CicsHttp cs = new CicsHttp("test1ContainersIn2Out", mHttpEndpoint, 1000, 5000);
			mHttpEndpoint.setHostTraceMode(TRACE_MODE);
			cs.connect(null);
			HashMap < String, Object > map = new HashMap < String, Object >();
			map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAC");
			map.put(Constants.CICS_CHANNEL_KEY, "LSFILEAC-CHANNEL");
			String[] outContainers = {"ReplyData","ReplyStatus"};
			map.put(Constants.CICS_OUT_CONTAINERS_KEY, outContainers);
			List <LegStarMessagePart> inputParts = new ArrayList <LegStarMessagePart>();
			LegStarMessagePart inQueryData = new ContainerPart("QueryData",
					Util.toByteArray("D45C4040404040404040404040404040404040405C404040404040404040404040404040404040405C40404040404040"));
			inputParts.add(inQueryData);
			LegStarHeaderPart dp = new LegStarHeaderPart(map, inputParts.size());
			LegStarAddress address = new LegStarAddress("CICSTS31");
			LegStarMessage requestMessage = new LegStarMessage(dp, inputParts);
			LegStarRequest request = new LegStarRequest("Request01", address, requestMessage);
			cs.sendRequest(request);
			cs.recvResponse(request);
			/* Get the status container first */
			assertEquals("ReplyStatus",
					  request.getResponseMessage().getDataParts().get(1).getID());
			assertEquals("0000f0f07af0f07af0f0000000044f00000000000000004040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040",
					  Util.toHexString(request.getResponseMessage().getDataParts().get(1).getContent()));
			/* Then get the data container */
			assertEquals("ReplyData",
					  request.getResponseMessage().getDataParts().get(0).getID());
			assertEquals("000000006ff0f0f0f1f0f4d44b40c24b40c4d6d4c2c5e84040404040404040d3d6d5c4d6d56bc5d5c7d3c1d5c4404040404040f1f2f8f4f6f2f9f3f2f640f1f140f8f15bf0f9f9f94bf9f95c5c5c5c5c5c5c5c5cf0f0f7f2f4f8d44b40d14b40c1e8d9c5e2404040404040404040d9c5c4e6d6d6c440c3c9e3e86b40c3c1d3c64b40f3f3f3f1f2f1f2f1f1f140f1f040f7f55bf0f0f0f94bf8f85c5c5c5c5c5c5c5c5cf0f0f7f7f7f9d4d9e24b40c14b40e2e3c5e6c1d9e34040404040e2c1d540d1d6e2c56b40c3c1d3c9c64b40404040f4f1f5f1f2f1f2f0f0f340f0f140f7f55bf0f0f0f94bf8f85c5c5c5c5c5c5c5c5cf1f0f0f0f0f0d44b40c1c4c1d4e2404040404040404040404040e3d6d9d6d5e3d66b40d6d5e3c1d9c9d640404040f0f3f4f1f5f1f2f1f2f640f1f140f8f15bf0f0f1f04bf0f05c5c5c5c5c5c5c5c5cf6f0f0f0f0f0d44bc64b40d4c1e2d6d540404040404040404040c4e4c2d3c9d56b40c9d9c5d3c1d5c44040404040f1f2f3f9f8f7f8f0f2f640f1f140f8f15bf0f0f1f04bf0f05c5c5c5c5c5c5c5c5cf7f0f0f0f0f0d44b40c2d9c1d5c4d6d540404040404040404040c4c1d3d3c1e26b40e3c5e7c1e240404040404040f5f7f9f8f4f3f2f0f2f640f1f140f8f15bf0f0f0f24bf0f05c5c5c5c5c5c5c5c5c",
					  Util.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
			cs.commitUOW();
			cs.close();
		} catch (ConnectionException e) {
			fail("test1ContainersIn2Out failed=" + e);
		} catch (RequestException e) {
			fail("test1ContainersIn2Out failed=" + e);
		} catch (HeaderPartException e) {
			fail("test1ContainersIn2Out failed=" + e);
		}
	}

	public void testLargeContainer() throws UnsupportedEncodingException {
		try {
			CicsHttp cs = new CicsHttp("testLargeContainer", mHttpEndpoint, 1000, 5000);
			mHttpEndpoint.setHostTraceMode(TRACE_MODE);
			cs.connect(null);
			HashMap < String, Object > map = new HashMap < String, Object >();
			map.put(Constants.CICS_PROGRAM_NAME_KEY, "T1VOLUMC");
			map.put(Constants.CICS_CHANNEL_KEY, "T1VOLUMC-CHANNEL");
			String[] outContainers = {"RESPONSE-CTN"};
			map.put(Constants.CICS_OUT_CONTAINERS_KEY, outContainers);
			List <LegStarMessagePart> inputParts = new ArrayList <LegStarMessagePart>();
			byte[] content = new byte[65536];
			byte[] startEC = Util.toByteArray("d7c7d47ec9c7e8c3d9c3e3d36bd9c5c7");
			byte[] endEC = Util.toByteArray("d7c1d9d47e4d7dd5d6c4e8d5c1d46bd3");
			System.arraycopy(startEC, 0, content, 0, 16);
			System.arraycopy(endEC, 0, content, 65520, 16);
			LegStarMessagePart inContainer = new ContainerPart("REQUEST-CTN", content);
			inputParts.add(inContainer);
			LegStarHeaderPart dp = new LegStarHeaderPart(map, inputParts.size());
			LegStarAddress address = new LegStarAddress("CICSTS31");
			LegStarMessage requestMessage = new LegStarMessage(dp, inputParts);
			LegStarRequest request = new LegStarRequest("testLargeContainer", address, requestMessage);
			cs.sendRequest(request);
			cs.recvResponse(request);
			assertEquals("RESPONSE-CTN",
					  request.getResponseMessage().getDataParts().get(0).getID());
			System.arraycopy(request.getResponseMessage().getDataParts().get(0).getContent(), 0, startEC, 0, 16);
			System.arraycopy(request.getResponseMessage().getDataParts().get(0).getContent(), 65520, endEC, 0, 16);
			assertEquals("d7c1d9d47e4d7dd5d6c4e8d5c1d46bd3", Util.toHexString(startEC));
			assertEquals("d7c7d47ec9c7e8c3d9c3e3d36bd9c5c7", Util.toHexString(endEC));
			cs.commitUOW();
			cs.close();
		} catch (ConnectionException e) {
			fail("testLargeContainer failed=" + e);
		} catch (RequestException e) {
			fail("testLargeContainer failed=" + e);
		} catch (HeaderPartException e) {
			fail("testLargeContainer failed=" + e);
		}
	}
	
	/* By iterating thru the execution of the large container test case, we make sure
	 * memory if effectively reclaimed between calls and CICS does not SOS */
	public void testForCICSSOS() throws UnsupportedEncodingException {
		try {
			CicsHttp cs = new CicsHttp("testLargeContainer", mHttpEndpoint, 1000, 5000);
			mHttpEndpoint.setHostTraceMode(false);
			cs.connect(null);
			HashMap < String, Object > map = new HashMap < String, Object >();
			map.put(Constants.CICS_PROGRAM_NAME_KEY, "T1VOLUMC");
			map.put(Constants.CICS_CHANNEL_KEY, "T1VOLUMC-CHANNEL");
			String[] outContainers = {"RESPONSE-CTN"};
			map.put(Constants.CICS_OUT_CONTAINERS_KEY, outContainers);
			List <LegStarMessagePart> inputParts = new ArrayList <LegStarMessagePart>();
			byte[] content = new byte[65536];
			byte[] startEC = Util.toByteArray("d7c7d47ec9c7e8c3d9c3e3d36bd9c5c7");
			byte[] endEC = Util.toByteArray("d7c1d9d47e4d7dd5d6c4e8d5c1d46bd3");
			System.arraycopy(startEC, 0, content, 0, 16);
			System.arraycopy(endEC, 0, content, 65520, 16);
			LegStarMessagePart inContainer = new ContainerPart("REQUEST-CTN", content);
			inputParts.add(inContainer);
			LegStarHeaderPart dp = new LegStarHeaderPart(map, inputParts.size());
			LegStarAddress address = new LegStarAddress("CICSTS31");
			LegStarMessage requestMessage = new LegStarMessage(dp, inputParts);
			for (int i = 0; i < MAX_ITERATIONS; i++) {
				LegStarRequest request = new LegStarRequest("Request01", address, requestMessage);
				cs.sendRequest(request);
				cs.recvResponse(request);
				assertEquals("RESPONSE-CTN",
						  request.getResponseMessage().getDataParts().get(0).getID());
				System.arraycopy(request.getResponseMessage().getDataParts().get(0).getContent(), 0, startEC, 0, 16);
				System.arraycopy(request.getResponseMessage().getDataParts().get(0).getContent(), 65520, endEC, 0, 16);
				assertEquals("d7c1d9d47e4d7dd5d6c4e8d5c1d46bd3", Util.toHexString(startEC));
				assertEquals("d7c7d47ec9c7e8c3d9c3e3d36bd9c5c7", Util.toHexString(endEC));
				cs.commitUOW();
			}
			cs.close();
		} catch (ConnectionException e) {
			fail("testLargeContainer failed=" + e);
		} catch (RequestException e) {
			fail("testLargeContainer failed=" + e);
		} catch (HeaderPartException e) {
			fail("testLargeContainer failed=" + e);
		}
	}
}
