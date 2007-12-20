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
package com.legstar.messaging;


import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import com.legstar.config.Constants;
import junit.framework.TestCase;

/** Tests for Header parts. */
public class HeaderTest extends TestCase {

	/** Tests JSON string builder. */
	public final void testStringize() {

		/* Use a linked hash map so that the order is guaranteed which
		 * simplifies the assert clause. With a normal HashMap we would have
		 * to test all combinations since the order is not guaranteed. */
		HashMap < String, Object > map = new LinkedHashMap < String, Object >();
		String json;
		/* Test with empty map */
		json = HeaderPart.getJsonFromMap(map);
		assertEquals("{}", json);

		/* Test with 1 entry map */
		map.put("CICSProgramName", "LSFILEAE");
		json = HeaderPart.getJsonFromMap(map);
		assertEquals("{\"CICSProgramName\":\"LSFILEAE\"}", json);

		/* Test with n entries map */
		map.put("CICSLength", "79");
		map.put("CICSDataLength", "6");
		json = HeaderPart.getJsonFromMap(map);
		assertEquals("{\"CICSProgramName\":\"LSFILEAE\",\"CICSLength\":\"79\","
				+ "\"CICSDataLength\":\"6\"}", json);
	}

	/** Tests JSON string builder with arrays. */
	public final void testStringizeArray() {

		HashMap < String, Object > map = new LinkedHashMap < String, Object >();
		String json;

		/* Test with an empty array*/
		map.put(Constants.CICS_CHANNEL_KEY, "MyCICSChannel");
		map.put(Constants.CICS_OUT_CONTAINERS_KEY, null);
		json = HeaderPart.getJsonFromMap(map);
		assertEquals("{\"CICSChannel\":\"MyCICSChannel\"}", json);

		/* Test with a one element array*/
		String[] array = {"ContainerA"};
		map.put(Constants.CICS_CHANNEL_KEY, "MyCICSChannel");
		map.put(Constants.CICS_OUT_CONTAINERS_KEY, array);
		json = HeaderPart.getJsonFromMap(map);
		assertEquals("{\"CICSChannel\":\"MyCICSChannel\",\"CICSOutContainers\":[\"ContainerA\"]}", json);

		/* Test with more than one element array*/
		String[] array2 = {"ContainerA","ContainerB"};
		map.put(Constants.CICS_CHANNEL_KEY, "MyCICSChannel");
		map.put(Constants.CICS_OUT_CONTAINERS_KEY, array2);
		json = HeaderPart.getJsonFromMap(map);
		assertEquals("{\"CICSChannel\":\"MyCICSChannel\",\"CICSOutContainers\":[\"ContainerA\",\"ContainerB\"]}", json);
	}

	/** Test construction of a header part.
	 * @throws HeaderPartException if charset is wrong
	 *  */
	public final void testHeaderConstructor()
	throws HeaderPartException {

		HashMap < String, Object > map = new LinkedHashMap < String, Object >();
		map.put("CICSLength", "79");
		map.put("CICSDataLength", "6");
		List < MessagePart > inputParts = new ArrayList < MessagePart >();
		MessagePart inCommarea = new CommareaPart(null);
		inputParts.add(inCommarea);
		HeaderPart dp = new HeaderPart(map, inputParts.size());
		/*            INPARTS  */
		assertEquals("0000000100000028c07fc3c9c3e2d3859587a3887f7a7ff7f97f6b"
				+ "7fc3c9c3e2c481a381d3859587a3887f7a7ff67fd0",
				Util.toHexString(dp.getContent()));
		assertEquals(1, dp.getDataPartsNumber());
		assertEquals(68, dp.getHostSize());
		assertEquals(40, dp.getJsonStringLen());
		assertEquals("{\"CICSLength\":\"79\",\"CICSDataLength\":\"6\"}", dp.getJsonString());
	}

	public final void testDefaultHeaderConstructor() throws HeaderPartException {
		try {
			HeaderPart hp = new HeaderPart();
			assertEquals("0000000000000000", Util.toHexString(hp.getContent()));
			assertEquals(0, hp.getDataPartsNumber());
			assertEquals(28, hp.getHostSize());
			assertEquals(0, hp.getJsonStringLen());
			assertTrue(null == hp.getJsonString());
		} catch (HeaderPartException e) {
			fail("testDefaultHeaderConstructor failed");
		}
	}
	public final void testHeaderConstructorFromJsonString() {
		try {
			HeaderPart headerPart = new HeaderPart(5, "{\"CICSProgramName\":\"LSFILEAE\"}");
			assertEquals("LSOKHEAD", headerPart.getID());
			assertEquals(5, headerPart.getDataPartsNumber());
			assertEquals(58, headerPart.getHostSize());
			assertEquals(30, headerPart.getJsonStringLen());
			assertEquals("{\"CICSProgramName\":\"LSFILEAE\"}", headerPart.getJsonString());
		} catch (HeaderPartException e) {
			fail("testHeaderConstructorFromAnotherHeader failed " + e);
		}
	}
	/* Test creation of a header with container parts */
	public final void testHeaderWithContainers() throws HeaderPartException {

		HashMap < String, Object > map = new LinkedHashMap < String, Object >();
		map.put("CICSLength", "79");
		map.put("CICSDataLength", "6");
		List < MessagePart > inputParts = new ArrayList < MessagePart >();
		MessagePart inContainer1 = new ContainerPart("CONTAINERA");
		inputParts.add(inContainer1);
		MessagePart inContainer2 = new ContainerPart("CONTAINERB");
		inputParts.add(inContainer2);
		HeaderPart dp = new HeaderPart(map, inputParts.size());
		/*            INPARTS  */
		assertEquals("0000000200000028c07fc3c9c3e2d3859587a3887f7a7ff7f97f6b"
				+ "7fc3c9c3e2c481a381d3859587a3887f7a7ff67fd0",
				Util.toHexString(dp.getContent()));
	}
	
	public final void testModifications() throws HeaderPartException {
		HeaderPart headerPart = new HeaderPart(5, "{\"CICSProgram\":\"LSFILEAE\"}");
		headerPart.setDataPartsNumber(3);
		assertEquals(3, headerPart.getDataPartsNumber());
		
		headerPart.setJsonString("{\"CICSLength\":\"79\",\"CICSDataLength\":\"6\"}");
		assertEquals("{\"CICSLength\":\"79\",\"CICSDataLength\":\"6\"}", headerPart.getJsonString());
		
		HashMap < String, Object > map = new LinkedHashMap < String, Object >();
		map.put("CICSChannel", "MyChannel");
		headerPart.setKeyValues(map);
		assertEquals("{\"CICSChannel\":\"MyChannel\"}", headerPart.getJsonString());
		
	}
}

