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
package com.legstar.messaging;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;

import com.legstar.config.Constants;
import com.legstar.util.Util;

import junit.framework.TestCase;

public class LegStarMessagePartTest extends TestCase {
	
	public final void testHostStreamHeaderPart() throws IOException {
		try {
			HashMap < String, Object > map = new HashMap < String, Object >();
			map.put(Constants.CICS_LENGTH_KEY, "79");
			map.put(Constants.CICS_DATALEN_KEY, "6");
			LegStarHeaderPart headerPart = new LegStarHeaderPart(map, 5);
			assertEquals("LSOKHEAD", headerPart.getID());
			assertEquals(5, headerPart.getDataPartsNumber());
			InputStream hostStream = headerPart.sendToHost();
			byte[] headerBytes = new byte[68];
			int rc;
			int pos = 0;
			while ((rc = hostStream.read(headerBytes, pos, headerBytes.length - pos)) > 0) {
				pos += rc;
			}
			/*            L S O K H E A D                       48       5      40{                                                                             } */
			assertEquals("d3e2d6d2c8c5c1c44040404040404040000000300000000500000028c07fc3c9c3e2c481a381d3859587a3887f7a7ff67f6b7fc3c9c3e2d3859587a3887f7a7ff7f97fd0", Util.toHexString(headerBytes));
		} catch (HeaderPartException e) {
			fail("testHostSerializeHeaderPart failed " + e);
		}
	}
	
	public final void testrecvFromHost() throws HostReceiveException {
		byte[] hostBytes = Util.toByteArray("d3e2d6d2c8c5c1c44040404040404040000000300000000500000028c07fc3c9c3e2c481a381d3859587a3887f7a7ff67f6b7fc3c9c3e2d3859587a3887f7a7ff7f97fd0");
		ByteArrayInputStream hostStream = new ByteArrayInputStream(hostBytes);
		LegStarMessagePart part = new LegStarMessagePart();
		part.recvFromHost(hostStream);
		assertEquals("LSOKHEAD", part.getID());
		assertEquals(48, part.getContent().length);
		assertEquals("0000000500000028c07fc3c9c3e2c481a381d3859587a3887f7a7ff67f6b7fc3c9c3e2d3859587a3887f7a7ff7f97fd0", Util.toHexString(part.getContent()));
	}

	public final void testrecvFromHostTooSmall() {
		byte[] hostBytes = Util.toByteArray("d3e2d6d2c8c5c1c44040404040");
		ByteArrayInputStream hostStream = new ByteArrayInputStream(hostBytes);
		LegStarMessagePart part = new LegStarMessagePart();
		try {
			part.recvFromHost(hostStream);
			fail("failed testrecvFromHostTooSmall");
		} catch (HostReceiveException e) {
			assertEquals("Invalid message part. No ID", e.getMessage());
		}
	}

	public final void testrecvFromHostNegativeContentLen() {
		byte[] hostBytes = Util.toByteArray("d3e2d6d2c8c5c1c44040404040404040F00000300000000500000028c07fc3c9c3e2c481a381d3859587a3887f7a7ff67f6b7fc3c9c3e2d3859587a3887f7a7ff7f97fd0");
		ByteArrayInputStream hostStream = new ByteArrayInputStream(hostBytes);
		LegStarMessagePart part = new LegStarMessagePart();
		try {
			part.recvFromHost(hostStream);
			fail("failed testrecvFromHostTooSmall");
		} catch (HostReceiveException e) {
			assertEquals("Invalid message part content length", e.getMessage());
		}
	}

	public final void testrecvFromHostNoContent() throws HostReceiveException {
		byte[] hostBytes = Util.toByteArray("d3e2d6d2c8c5c1c4404040404040404000000000");
		ByteArrayInputStream hostStream = new ByteArrayInputStream(hostBytes);
		LegStarMessagePart part = new LegStarMessagePart();
		part.recvFromHost(hostStream);
		assertEquals("LSOKHEAD", part.getID());
		assertTrue(null == part.getContent());
	}

	public final void testrecvErrorMessageFromHost() {
		byte[] hostBytes = Util.toByteArray("d3e2d6d2c5d9d9f040c3c9c3e240839694948195847ed3c9d5d240c3d6d4d4c1d9c5c1408681899385846b409985a2977ed7c7d4c9c4c5d9d96b409985a297f27ef3");
		ByteArrayInputStream hostStream = new ByteArrayInputStream(hostBytes);
		LegStarMessagePart part = new LegStarMessagePart();
		try {
			part.recvFromHost(hostStream);
			fail();
		} catch (HostReceiveException e) {
			assertEquals("CICS command=LINK COMMAREA failed, resp=PGMIDERR, resp2=3", e.getMessage());
		}
	}

	public final void testrecvCorruptedHeader() {
		byte[] hostBytes = Util.toByteArray("d4e2d6d2c8c5c1c4404040404040404000000000");
		ByteArrayInputStream hostStream = new ByteArrayInputStream(hostBytes);
		try {
			LegStarHeaderPart part = new LegStarHeaderPart();
			part.recvFromHost(hostStream);
			fail();
		} catch (HostReceiveException e) {
			assertEquals("Invalid message header. Expected LSOKHEAD, received MSOKHEAD", e.getMessage());
		} catch (HeaderPartException e) {
			fail(e.getMessage());
		}
	}
}
