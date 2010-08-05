/*******************************************************************************
 * Copyright (c) 2009 LegSem.
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


    /** A messsage with one part.*/
    public static final String SINGLEPART_MESSAGE =
        /*L S O K H E A D                 (Message part ID)*/
        "d3e2d6d2c8c5c1c44040404040404040"
        /*       48                       (Message part content length)*/
        + "00000030"
        /*        1                       (Header part number of parts)*/
        + "00000001"
        /*       40                       (Header part JSON string length)*/
        + "00000028"
        /* { " C I C S D a t a L e n g t h " : " 6 " , " C I C S L e n g t h " : " 7 9 " }  */
        + "c07fc3c9c3e2c481a381d3859587a3887f7a7ff67f6b7fc3c9c3e2d3859587a3887f7a7ff7f97fd0"
        /*  C O N T A I N E R             (Message part ID)*/
        + "c3d6d5e3c1c9d5c5d940404040404040"
        /*        4                       (Message part content length)*/
        + "00000004"
        /*  1 2 3 4 */
        + "01020304";
    
    /** A single pazrt message with default (empty) header. */
    public static final String SINGLEPART_MINIMAL_MESSAGE =
        /*L S O K H E A D                 (Message part ID)*/
        "d3e2d6d2c8c5c1c44040404040404040"
        /*        8                       (Message part content length)*/
        + "00000008"
        /*        1                       (Header part number of parts)*/
        + "00000001"
        /*        0                       (Header part JSON string length)*/
        + "00000000"
        /*  C O N T A I N E R             (Message part ID)*/
        + "d3e2d6d2c3d6d4d4c1d9c5c140404040"
        /*        4                       (Message part content length)*/
        + "00000004"
        /*  1 2 3 4 */
        + "01020304";

    /**
     * Create a multipart message and test that it serializes correctly.
     * @throws IOException if test fails
     */
    public void testHostStreamMessage() throws IOException {
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
        } catch (HostMessageFormatException e) {
            fail("testHostSerializeHeaderPart failed " + e);
        }
    }

    /**
     * Check that message size is calculated correctly.
     * @throws HeaderPartException if tes fails
     */
    public void testGetSize() throws HeaderPartException {

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
     * @throws HostMessageFormatException if receive fails
     */
    public void testRecvFromHost() throws HeaderPartException, HostMessageFormatException {
        byte[] hostBytes = HostData.toByteArray(MULTIPART_MESSAGE);
        ByteArrayInputStream hostStream = new ByteArrayInputStream(hostBytes);
        LegStarMessage message = new LegStarMessage();
        message.recvFromHost(hostStream);
        assertEquals(111, message.getHeaderPart().getJsonStringLen());
        assertEquals(MULTIPART_MESSAGE_JSON, message.getHeaderPart().getJsonString());
        assertEquals(2, message.getHeaderPart().getDataPartsNumber());
        assertEquals("ReplyData", message.getDataParts().get(0).getPartID());
        assertEquals("ReplyStatus", message.getDataParts().get(1).getPartID());
        assertEquals("F1F2F3F4", HostData.toHexString(message.getDataParts().get(0).getContent()).toUpperCase());
        assertEquals("F5F6", HostData.toHexString(message.getDataParts().get(1).getContent()).toUpperCase());
    }

    /**
     * Empty messages (no parts) are acceptable.
     * @throws HeaderPartException if header is invalid
     * @throws HostMessageFormatException if receive fails
     */
    public void testRecvEmptyMessageFromHost() throws HeaderPartException, HostMessageFormatException {
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
    public void testToString() throws Exception {
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
    public void testEquals() throws Exception {
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

    /**
     * Create a multipart message and test that it serializes correctly.
     * @throws IOException if test fails
     */
    public void testToByteArray() throws IOException {
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
            assertEquals(MULTIPART_MESSAGE, HostData.toHexString(requestMessage.toByteArray()));
        } catch (HeaderPartException e) {
            fail("testHostSerializeHeaderPart failed " + e);
        } catch (HostMessageFormatException e) {
            fail("testHostSerializeHeaderPart failed " + e);
        }
    }
    
    /**
     * Check convenience methods to quickly get content from single part messages.
     */
    public void testGetContentFromHostBytes() {
        try {
            LegStarMessage.getContentFromHostBytes(HostData.toByteArray(MULTIPART_MESSAGE));
        } catch (HostMessageFormatException e) {
            assertEquals("Multi-part messages not supported", e.getMessage());
        }
        try {
            byte[] content = LegStarMessage.getContentFromHostBytes(
                    HostData.toByteArray(SINGLEPART_MESSAGE));
            assertEquals("01020304", HostData.toHexString(content));
        } catch (HostMessageFormatException e) {
            fail("testgetContentFromHostBytes failed " + e);
        }
    }

    /**
     * Check convenience methods to quickly single part messages from content.
     */
    public void testGetHostBytesFromContent() {
        try {
            byte[] payload = LegStarMessage.getHostBytesFromContent(
                    HostData.toByteArray("01020304"));
            assertEquals(SINGLEPART_MINIMAL_MESSAGE, HostData.toHexString(payload));
        } catch (HostMessageFormatException e) {
            fail("testGetHostBytesFromContent failed " + e);
        }
    }
    
    /**
     * Test FromByteArray.
     */
    public void testFromByteArray() {
        try {
            LegStarMessage legStarMessage = new LegStarMessage();
            legStarMessage.fromByteArray(HostData.toByteArray(SINGLEPART_MINIMAL_MESSAGE), 0);
            assertEquals(1, legStarMessage.getHeaderPart().getDataPartsNumber());
            assertEquals(1, legStarMessage.getDataParts().size());
            assertEquals("LSOKCOMMAREA", legStarMessage.getDataParts().get(0).getPartID());
            assertEquals("01020304", HostData.toHexString(legStarMessage.getDataParts().get(0).getContent()));
        } catch (HeaderPartException e) {
            fail("testFromByteArray failed " + e);
        } catch (HostMessageFormatException e) {
            fail("testFromByteArray failed " + e);
        }
        
        try {
            LegStarMessage legStarMessage = new LegStarMessage();
            legStarMessage.fromByteArray(HostData.toByteArray(MULTIPART_MESSAGE), 0);
            assertEquals(2, legStarMessage.getHeaderPart().getDataPartsNumber());
            assertEquals(2, legStarMessage.getDataParts().size());
            assertEquals("ReplyData", legStarMessage.getDataParts().get(0).getPartID());
            assertEquals("f1f2f3f4", HostData.toHexString(legStarMessage.getDataParts().get(0).getContent()));
            assertEquals("ReplyStatus", legStarMessage.getDataParts().get(1).getPartID());
            assertEquals("f5f6", HostData.toHexString(legStarMessage.getDataParts().get(1).getContent()));
        } catch (HeaderPartException e) {
            fail("testFromByteArray failed " + e);
        } catch (HostMessageFormatException e) {
            fail("testFromByteArray failed " + e);
        }
        
    }
}
