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


import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;

import junit.framework.TestCase;

/** Tests for Header parts. */
public class HeaderTest extends TestCase {
	
	/** Tests JSON string builder. */
	public final void testStringize() {
		
		/* Use a linked hash map so that the order is guaranteed which
		 * simplifies the asser clause. With a normal HashMap we would have
		 * to test all combinations since the order is not guaranteed. */
		HashMap < String, String > map = new LinkedHashMap < String, String >();
		String json;
		/* Test with empty map */
		json = HeaderPart.stringizeKeyValues(map);
		assertEquals("{}", json);
		
		/* Test with 1 entry map */
		map.put("CICSProgram", "LSFILEAE");
		json = HeaderPart.stringizeKeyValues(map);
		assertEquals("{\"CICSProgram\":\"LSFILEAE\"}", json);

		/* Test with n entries map */
		map.put("CICSLength", "79");
		map.put("CICSDataLength", "6");
		json = HeaderPart.stringizeKeyValues(map);
		assertEquals("{\"CICSProgram\":\"LSFILEAE\",\"CICSLength\":\"79\","
				+ "\"CICSDataLength\":\"6\"}", json);
	}

	/** Test construction of a header part.
	 * @throws UnsupportedEncodingException if charset is wrong
	 *  */
	public final void testHeaderConstructor()
			throws UnsupportedEncodingException {
		
		HashMap < String, String > map = new LinkedHashMap < String, String >();
		map.put("CICSLength", "79");
		map.put("CICSDataLength", "6");
		List < MessagePart > inputParts = new ArrayList < MessagePart >();
		MessagePart inCommarea = new CommareaPart(null);
		inputParts.add(inCommarea);
		HeaderPart dp = new HeaderPart(map, inputParts.size(), "IBM01140");
        /*            INPARTS  */
		assertEquals("0000000100000028c07fc3c9c3e2d3859587a3887f7a7ff7f97f6b"
				 + "7fc3c9c3e2c481a381d3859587a3887f7a7ff67fd0",
				 Util.toHexString(dp.getContent()));
	}
	
	public final void testHeaderConstructorFromWrongMessageType() {
		CommareaPart messagePart = new CommareaPart(Util.toByteArray("f0f1f1"));
		try {
			@SuppressWarnings("unused")
			HeaderPart hp = new HeaderPart(messagePart);
			fail("testHeaderConstructorFromWrongMessageType failed");
		} catch (HeaderPartException e) {
			assertEquals("Message part is not a header part.", e.getMessage());
		}
	}
	public final void testHeaderConstructorFromAnotherHeader() {
		HeaderPart messagePart;
		try {
			messagePart = new HeaderPart(
					new HashMap < String, String >(), 5, "IBM01140");
			HeaderPart headerPart = new HeaderPart(messagePart);
			assertEquals("LSOKHEAD", headerPart.getID());
			assertEquals(5, headerPart.getDataPartsNumber());
		} catch (UnsupportedEncodingException e) {
			fail("testHeaderConstructorFromAnotherHeader failed " + e);
		} catch (HeaderPartException e) {
			fail("testHeaderConstructorFromAnotherHeader failed " + e);
		}
	}
}
