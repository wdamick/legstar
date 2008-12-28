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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import com.legstar.config.Constants;
import com.legstar.coxb.host.HostData;

import junit.framework.TestCase;

/**
 * Test the LegStarMessage structure.
 *
 */
public class LegStarMessageTest extends TestCase {
    
    /** A sample LegStarMessage content.*/
    public static final String MULTIPART_MESSAGE =
        /*L S O K H E A D                 (Message part ID)*/
        "d3e2d6d2c8c5c1c44040404040404040"
        /*      119                       (Message part content length)*/
        + "00000077"
        /*        2                       (Header part number of parts)*/
        + "00000002"
        /*      111                       (Header part JSON string length)*/
        + "0000006f"
        /* { " C I C S O u t C o n t a i n e r s " : [ " R e p l y D a t a " , " R e p l y S t a t u s " ] , */
        + "c07fc3c9c3e2d6a4a3c39695a38189958599a27f7aad7fd9859793a8c481a3817f6b7fd9859793a8e2a381a3a4a27fbd6b"
        /* " C I C S C h a n n e l " : " L S F I L E A C - C H A N N E L " , */
        + "7fc3c9c3e2c38881959585937f7a7fd3e2c6c9d3c5c1c360c3c8c1d5d5c5d37f6b"
        /* " C I C S P r o g r a m N a m e " : " L S F I L E A C " } */
        + "7fc3c9c3e2d7999687998194d58194857f7a7fd3e2c6c9d3c5c1c37fd0"
        /* R e p l y D a t a              (Message part ID)*/
        + "d9859793a8c481a38140404040404040"
        /*        4                       (Message part content length)*/
        + "00000004"
        /*  1 2 3 4*/
        + "f1f2f3f4"
        /* R e p l y S t a t u s           (Message part ID)*/
        + "d9859793a8e2a381a3a4a2404040404"
        /*         2                       (Message part content length)*/
        + "000000002"
        /*  5 6*/
        + "f5f6";
    
    /** A sample JSON content for a multipart type message. */
    public static final String MULTIPART_MESSAGE_JSON =
        "{\"CICSOutContainers\":[\"ReplyData\",\"ReplyStatus\"],"
        + "\"CICSChannel\":\"LSFILEAC-CHANNEL\","
        + "\"CICSProgramName\":\"LSFILEAC\"}";


    /**
     * Create a multipart message and test that it serializes correctly.
     * @throws IOException if test fails
     */
    public final void testHostStreamMessage() throws IOException {
        try {
            List < LegStarMessagePart > inputParts = new ArrayList < LegStarMessagePart >();
            LegStarMessagePart inQueryData = new ContainerPart(
                    "ReplyData", HostData.toByteArray("F1F2F3F4"));
            inputParts.add(inQueryData);
            LegStarMessagePart inQueryLimit = new ContainerPart(
                    "ReplyStatus", HostData.toByteArray("F5F6"));
            inputParts.add(inQueryLimit);
            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAC");
            map.put(Constants.CICS_CHANNEL_KEY, "LSFILEAC-CHANNEL");
            String[] outContainers = {"ReplyData", "ReplyStatus"};
            map.put(Constants.CICS_OUT_CONTAINERS_KEY, outContainers);
            LegStarHeaderPart headerPart = new LegStarHeaderPart(map, inputParts.size());
            LegStarMessage requestMessage = new LegStarMessage(headerPart, inputParts);
            InputStream hostStream = requestMessage.sendToHost();
            byte[] headerBytes = new byte[185];
            int rc;
            int pos = 0;
            while ((rc = hostStream.read(headerBytes, pos, headerBytes.length - pos)) > 0) {
                pos += rc;
            }
            assertEquals(MULTIPART_MESSAGE, HostData.toHexString(headerBytes));
        } catch (HeaderPartException e) {
            fail("testHostSerializeHeaderPart failed " + e);
        }
    }

    /**
     * Check that message size is calculated correctly.
     * @throws HeaderPartException if tes fails
     */
    public final void testGetSize() throws HeaderPartException {

        /* Test on an empty message */
        LegStarHeaderPart headerPart = new LegStarHeaderPart();
        List < LegStarMessagePart > dataParts = new ArrayList < LegStarMessagePart >();
        LegStarMessage message = new LegStarMessage(headerPart, dataParts);
        assertEquals(28, message.getHostSize());

        /* Test on an message with one part */
        HashMap < String, Object > map = new HashMap < String, Object >();
        map.put(Constants.CICS_LENGTH_KEY, "79");
        map.put(Constants.CICS_DATALEN_KEY, "6");
        headerPart = new LegStarHeaderPart(map, 5);
        dataParts = new ArrayList < LegStarMessagePart >();
        LegStarMessagePart part = new LegStarMessagePart("part1", null);
        dataParts.add(part);
        message = new LegStarMessage(headerPart, dataParts);
        assertEquals(88, message.getHostSize());
        String jsonString = headerPart.getJsonString();
        assertTrue(jsonString.contains("\"CICSDataLength\":\"6\""));
        assertTrue(jsonString.contains("\"CICSLength\":\"79\""));
    }

    /**
     * Try to receive a multipart message.
     * @throws HeaderPartException if header is invalid
     * @throws HostReceiveException if receive fails
     */
    public final void testRecvFromHost() throws HeaderPartException, HostReceiveException {
        byte[] hostBytes = HostData.toByteArray(MULTIPART_MESSAGE);
        ByteArrayInputStream hostStream = new ByteArrayInputStream(hostBytes);
        LegStarMessage message = new LegStarMessage();
        message.recvFromHost(hostStream);
        assertEquals(111, message.getHeaderPart().getJsonStringLen());
        assertEquals(MULTIPART_MESSAGE_JSON, message.getHeaderPart().getJsonString());
        assertEquals(2, message.getHeaderPart().getDataPartsNumber());
        assertEquals("ReplyData", message.getDataParts().get(0).getID());
        assertEquals("ReplyStatus", message.getDataParts().get(1).getID());
        assertEquals("F1F2F3F4", HostData.toHexString(message.getDataParts().get(0).getContent()).toUpperCase());
        assertEquals("F5F6", HostData.toHexString(message.getDataParts().get(1).getContent()).toUpperCase());
    }

    /**
     * Empty messages (no parts) are acceptable.
     * @throws HeaderPartException if header is invalid
     * @throws HostReceiveException if receive fails
     */
    public final void testRecvEmptyMessageFromHost() throws HeaderPartException, HostReceiveException {
        byte[] hostBytes = HostData.toByteArray(
                "d3e2d6d2c8c5c1c44040404040404040"
                + "00000000"
                + "00000000");
        ByteArrayInputStream hostStream = new ByteArrayInputStream(hostBytes);
        LegStarMessage message = new LegStarMessage();
        message.recvFromHost(hostStream);
        assertEquals(0, message.getHeaderPart().getJsonStringLen());
        assertEquals(null, message.getHeaderPart().getJsonString());
        assertEquals(0, message.getHeaderPart().getDataPartsNumber());
    }

    /**
     * Check that message serializes correctly into a string.
     * @throws Exception if test fails
     */
    public final void testToString() throws Exception {
        byte[] hostBytes = HostData.toByteArray(MULTIPART_MESSAGE);
        ByteArrayInputStream hostStream = new ByteArrayInputStream(hostBytes);
        LegStarMessage message = new LegStarMessage();
        message.recvFromHost(hostStream);
        assertTrue(message.toString().contains("dataPartsNumber=2"));
        assertTrue(message.toString().contains("\"CICSOutContainers\":[\"ReplyData\",\"ReplyStatus\"]"));
        assertTrue(message.toString().contains("\"CICSChannel\":\"LSFILEAC-CHANNEL\""));
        assertTrue(message.toString().contains("\"CICSProgramName\":\"LSFILEAC\""));
        assertTrue(message.toString().contains("id=ReplyData, content=[f1f2f3f4]"));
        assertTrue(message.toString().contains("id=ReplyStatus, content=[f5f6]"));

    }

    /**
     * Check that equality is tested correctly between messages.
     * @throws Exception if test fails
     */
    public final void testEquals() throws Exception {
        byte[] hostBytes = HostData.toByteArray(MULTIPART_MESSAGE);
        byte[] hostBytes2 = HostData.toByteArray(MULTIPART_MESSAGE.replace("f5f6", "f7f8"));
        LegStarMessage message = new LegStarMessage();
        message.recvFromHost(new ByteArrayInputStream(hostBytes));
        LegStarMessage message2 = new LegStarMessage();
        message2.recvFromHost(new ByteArrayInputStream(hostBytes));
        assertTrue(message2.equals(message));
        LegStarMessage message3 = new LegStarMessage();
        message3.recvFromHost(new ByteArrayInputStream(hostBytes2));
        assertFalse(message3.equals(message));

    }
}
