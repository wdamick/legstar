package com.legstar.messaging;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;

import com.legstar.config.Constants;

import junit.framework.TestCase;

public class MessagePartTest extends TestCase {
	
	public final void testHostStreamHeaderPart() throws IOException {
		try {
			HashMap < String, Object > map = new HashMap < String, Object >();
			map.put(Constants.CICS_LENGTH_KEY, "79");
			map.put(Constants.CICS_DATALEN_KEY, "6");
			HeaderPart headerPart = new HeaderPart(map, 5);
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
		MessagePart part = new MessagePart();
		part.recvFromHost(hostStream);
		assertEquals("LSOKHEAD", part.getID());
		assertEquals(48, part.getContent().length);
		assertEquals("0000000500000028c07fc3c9c3e2c481a381d3859587a3887f7a7ff67f6b7fc3c9c3e2d3859587a3887f7a7ff7f97fd0", Util.toHexString(part.getContent()));
	}

	public final void testrecvFromHostTooSmall() {
		byte[] hostBytes = Util.toByteArray("d3e2d6d2c8c5c1c44040404040");
		ByteArrayInputStream hostStream = new ByteArrayInputStream(hostBytes);
		MessagePart part = new MessagePart();
		try {
			part.recvFromHost(hostStream);
			fail("failed testrecvFromHostTooSmall");
		} catch (HostReceiveException e) {
			assertEquals("Cannot read message part header", e.getMessage());
		}
	}

	public final void testrecvFromHostNegativeContentLen() {
		byte[] hostBytes = Util.toByteArray("d3e2d6d2c8c5c1c44040404040404040F00000300000000500000028c07fc3c9c3e2c481a381d3859587a3887f7a7ff67f6b7fc3c9c3e2d3859587a3887f7a7ff7f97fd0");
		ByteArrayInputStream hostStream = new ByteArrayInputStream(hostBytes);
		MessagePart part = new MessagePart();
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
		MessagePart part = new MessagePart();
		part.recvFromHost(hostStream);
		assertEquals("LSOKHEAD", part.getID());
		assertTrue(null == part.getContent());
	}
}
