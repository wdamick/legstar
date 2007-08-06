package com.legstar.messaging;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import com.legstar.config.Constants;

import junit.framework.TestCase;

public class MessageTest extends TestCase {
	
	public final void testHostStreamMessage() throws IOException {
		try {
			List <MessagePart> inputParts = new ArrayList <MessagePart>();
			MessagePart inQueryData = new ContainerPart("QueryData",Util.toByteArray("F1F2F3F4"));
			inputParts.add(inQueryData);
			MessagePart inQueryLimit = new ContainerPart("QueryLimit", Util.toByteArray("F5F6"));
			inputParts.add(inQueryLimit);
			HashMap < String, Object > map = new HashMap < String, Object >();
			map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAC");
			map.put(Constants.CICS_CHANNEL_KEY, "LSFILEAC-CHANNEL");
			String[] outContainers = {"ReplyData","ReplyStatus"};
			map.put(Constants.CICS_OUT_CONTAINERS_KEY, outContainers);
			HeaderPart dp = new HeaderPart(map, inputParts.size());
			Message requestMessage = new Message(dp, inputParts);
			InputStream hostStream = requestMessage.sendToHost();
			byte[] headerBytes = new byte[185];
			int rc;
			int pos = 0;
			while ((rc = hostStream.read(headerBytes, pos, headerBytes.length - pos)) > 0) {
				pos += rc;
			}
			/*            L S O K H E A D                      119       2     111{                                                                                                                                                                                                                            }Q u e r y D a t a                      4 1 2 3 4Q u e r y L i m i t                    2 5 6*/
			assertEquals("d3e2d6d2c8c5c1c4404040404040404000000077000000020000006fc07fc3c9c3e2d6a4a3c39695a38189958599a27f7aad7fd9859793a8c481a3817f6b7fd9859793a8e2a381a3a4a27fbd6b7fc3c9c3e2c38881959585937f7a7fd3e2c6c9d3c5c1c360c3c8c1d5d5c5d37f6b7fc3c9c3e2d7999687998194d58194857f7a7fd3e2c6c9d3c5c1c37fd0d8a48599a8c481a3814040404040404000000004f1f2f3f4d8a48599a8d3899489a340404040404000000002f5f6", Util.toHexString(headerBytes));
		} catch (HeaderPartException e) {
			fail("testHostSerializeHeaderPart failed " + e);
		}
	}
	
	public final void testGetSize() throws HeaderPartException {
		
		/* Test on an empty message */
		HeaderPart headerPart = new HeaderPart();
		List < MessagePart > dataParts = new ArrayList < MessagePart >();
		Message message = new Message(headerPart, dataParts);
		assertEquals(28, message.getHostSize());
		
		/* Test on an message with one part */
		HashMap < String, Object > map = new HashMap < String, Object >();
		map.put(Constants.CICS_LENGTH_KEY, "79");
		map.put(Constants.CICS_DATALEN_KEY, "6");
		headerPart = new HeaderPart(map, 5);
		dataParts = new ArrayList < MessagePart >();
		MessagePart part = new MessagePart("part1", null);
		dataParts.add(part);
		message = new Message(headerPart, dataParts);
		assertEquals(88, message.getHostSize());
		assertEquals("{\"CICSDataLength\":\"6\",\"CICSLength\":\"79\"}", headerPart.getJsonString());
	}
	
	public final void testRecvFromHost() throws HeaderPartException, HostReceiveException {
//		byte[] hostBytes = Util.toByteArray("d3e2d6d2c8c5c1c4404040404040404000000073000000020000006bc07fc3c9c3e2d6a4a3c39695a38189958599a27f7aad7fd9859793a8c481a3817f6b7fd9859793a8e2a381a3a4a27fbd6b7fc3c9c3e2d79996879981947f7a7fd3e2c6c9d3c5c1c37f6b7fc3c9c3e2c38881959585937f7a7fd3e2c6c9d3c5c1c360c3c8c1d5d5c5d37fd0d8a48599a8c481a3814040404040404000000004f1f2f3f4d8a48599a8d3899489a340404040404000000002f5f6");
		byte[] hostBytes = Util.toByteArray("d3e2d6d2c8c5c1c4404040404040404000000077000000020000006fc07fc3c9c3e2d6a4a3c39695a38189958599a27f7aad7fd9859793a8c481a3817f6b7fd9859793a8e2a381a3a4a27fbd6b7fc3c9c3e2c38881959585937f7a7fd3e2c6c9d3c5c1c360c3c8c1d5d5c5d37f6b7fc3c9c3e2d7999687998194d58194857f7a7fd3e2c6c9d3c5c1c37fd0d8a48599a8c481a3814040404040404000000004f1f2f3f4d8a48599a8d3899489a340404040404000000002f5f6");
		ByteArrayInputStream hostStream = new ByteArrayInputStream(hostBytes);
		Message message = new Message();
		message.recvFromHost(hostStream);
		assertEquals(111, message.getHeaderPart().getJsonStringLen());
		assertEquals("{\"CICSOutContainers\":[\"ReplyData\",\"ReplyStatus\"],\"CICSChannel\":\"LSFILEAC-CHANNEL\",\"CICSProgramName\":\"LSFILEAC\"}", message.getHeaderPart().getJsonString());
		assertEquals(2, message.getHeaderPart().getDataPartsNumber());
		assertEquals("QueryData", message.getDataParts().get(0).getID());
		assertEquals("QueryLimit", message.getDataParts().get(1).getID());
		assertEquals("F1F2F3F4", Util.toHexString(message.getDataParts().get(0).getContent()).toUpperCase());
		assertEquals("F5F6", Util.toHexString(message.getDataParts().get(1).getContent()).toUpperCase());
		
	}
	
	public final void testRecvEmptyMessageFromHost() throws HeaderPartException, HostReceiveException {
		byte[] hostBytes = Util.toByteArray("d3e2d6d2c8c5c1c440404040404040400000000000000000");
		ByteArrayInputStream hostStream = new ByteArrayInputStream(hostBytes);
		Message message = new Message();
		message.recvFromHost(hostStream);
		assertEquals(0, message.getHeaderPart().getJsonStringLen());
		assertEquals(null, message.getHeaderPart().getJsonString());
		assertEquals(0, message.getHeaderPart().getDataPartsNumber());
		
	}
}
