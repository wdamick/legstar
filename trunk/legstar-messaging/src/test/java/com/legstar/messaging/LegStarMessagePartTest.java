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

import java.io.UnsupportedEncodingException;
import com.legstar.coxb.host.HostData;

import junit.framework.TestCase;

/**
 * Test the LegStarMessagePart class.
 *
 */
public class LegStarMessagePartTest extends TestCase {
    
    /** A simplistic header part (as a sample of a message part).*/
    public static final String LSOKHEAD_SAMPLE =
        /*L S O K H E A D                 (Message part ID)*/
        "d3e2d6d2c8c5c1c44040404040404040"
        /*       48                       (Message part content length)*/
        + "00000030"
        /*        5                       (Header part number of parts)*/
        + "00000005"
        /*       40                       (Header part JSON string length)*/
        + "00000028"
        /* { " C I C S D a t a L e n g t h " : " 6 " , " C I C S L e n g t h " : " 7 9 " }  */
        + "c07fc3c9c3e2c481a381d3859587a3887f7a7ff67f6b7fc3c9c3e2d3859587a3887f7a7ff7f97fd0";

    /** An error report message part.*/
    public static final String LSOKERR0_SAMPLE =
        /*L S O K E R R 0                 (Eye catcher)*/
        "d3e2d6d2c5d9d9f0"
        /*                                (Error essage separator)*/
        + "40"
        /* C I C S   c o m m a n d = L I N K   C O M M A R E A   f a i l e d , */
        + "c3c9c3e240839694948195847ed3c9d5d240c3d6d4d4c1d9c5c1408681899385846b"
        /*     r e s p = P G M I D E R R ,   r e s p 2 = 3*/
          + "409985a2977ed7c7d4c9c4c5d9d96b409985a297f27ef3";

    /** An simple message part.*/
    public static final String PART_SAMPLE =
        /*C O N T A I N E R               (Message part ID)*/
        "c3d6d5e3c1c9d5c5d940404040404040"
        /*        4                       (Message part content length)*/
        + "00000004"
        /*  1 2 3 4 */
        + "01020304";

    /**
     * Create a message part with a content size that is larger then the actual payload size.
     * This situation happens with variable size arrays where the content size is large enough
     * to hold the maximum size array while we want to send only the available items.
     * Fixes issue 27
     */
    public final void testHostPayloadLtContentSize() {
        try {
            byte[] content = {0x01, 0x02, 0x03, 0x04, 0x05, 0x06 };
            LegStarMessagePart part = new LegStarMessagePart("CONTAINER", content);
            assertEquals(6, part.getPayloadSize());
            part.setPayloadSize(4);
            byte[] serializedContent = part.toByteArray();
            assertEquals(PART_SAMPLE, HostData.toHexString(serializedContent));
        } catch (HostMessageFormatException e) {
            fail("testHostStreamPayloadLtContentSize failed " + e);
        }
    }

    /**
     * If host sends data that is too small to hold even a message part ID,
     * we should fail gracefully.
     */
    public final void testrecvFromHostTooSmall() {
        byte[] hostBytes = HostData.toByteArray("d3e2d6d2c8c5c1c44040404040");
        LegStarMessagePart part = new LegStarMessagePart();
        try {
            part.fromByteArray(hostBytes, 0);
            fail("failed testrecvFromHostTooSmall");
        } catch (HostMessageFormatException e) {
            assertEquals("Invalid message part. No ID", e.getMessage());
        }
    }

    /**
     * If a message part sent by host seems enormous, we should
     * fail gracefully.
     */
    public final void testrecvFromHostNegativeContentLen() {
        byte[] hostBytes = HostData.toByteArray(
                "d3e2d6d2c8c5c1c44040404040404040"
                /* This is 4026531888 bytes content*/
                + "F0000030"
                + "00000005"
                + "00000000");
        LegStarMessagePart part = new LegStarMessagePart();
        try {
            part.fromByteArray(hostBytes, 0);
            fail("failed testrecvFromHostTooSmall");
        } catch (HostMessageFormatException e) {
            assertEquals("Invalid message part content length -268435408",
                    e.getMessage());
        }
    }

    /**
     * It is acceptable that there is no content for a message part.
     */
    public final void testrecvFromHostNoContent() {
        try {
            byte[] hostBytes = HostData.toByteArray(
                    "d3e2d6d2c8c5c1c44040404040404040"
                    /* This is 0 bytes content*/
                    + "00000000");
            LegStarMessagePart part = new LegStarMessagePart();
            part.fromByteArray(hostBytes, 0);
            assertEquals("LSOKHEAD", part.getPartID());
            assertTrue(null == part.getContent());
        } catch (HostMessageFormatException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Application errors are returned by the host with a special eye catcher to
     * distinguish them from actual message parts.
     */
    public final void testrecvErrorMessageFromHost() {
        byte[] hostBytes = HostData.toByteArray(LSOKERR0_SAMPLE);
        LegStarMessagePart part = new LegStarMessagePart();
        try {
            part.fromByteArray(hostBytes, 0);
            fail();
        } catch (HostMessageFormatException e) {
            assertEquals("CICS command=LINK COMMAREA failed, resp=PGMIDERR, resp2=3", e.getMessage());
        }
    }

    /**
     * If host data does not start with something recognizable we should fail gracefully.
     */
    public final void testrecvCorruptedHeader() {
        byte[] hostBytes = HostData.toByteArray(
                /*M S O K H E A D       (invalid eye catcher)          */
                "d4e2d6d2c8c5c1c4404040404040404000000000");
        try {
            LegStarHeaderPart part = new LegStarHeaderPart();
            part.fromByteArray(hostBytes, 0);
            fail();
        } catch (HostMessageFormatException e) {
            assertEquals("Invalid message part ID. Expected LSOKHEAD, received MSOKHEAD",
                    e.getMessage());
        } catch (HeaderPartException e) {
            fail(e.getMessage());
        }
    }
    
    /**
     * Test if arbitrary payload can be identified as LegStarMessage.
     */
    public final void testHeaderRecognition() {
        try {
            assertFalse(LegStarHeaderPart.isLegStarHeader(
                    null));
            assertFalse(LegStarHeaderPart.isLegStarHeader(
                    HostData.toByteArray(LSOKHEAD_SAMPLE.substring(0, 30))));
            assertFalse(LegStarHeaderPart.isLegStarHeader(
                    HostData.toByteArray(PART_SAMPLE)));
            assertTrue(LegStarHeaderPart.isLegStarHeader(
                    HostData.toByteArray(LSOKHEAD_SAMPLE)));
        } catch (UnsupportedEncodingException e) {
            fail(e.getMessage());
        }
        
    }
    /**
     * Test serializing part in a byte array.
     */
    public final void testToByteArray() {
        try {
            byte[] content = {0x01, 0x02, 0x03, 0x04, 0x05, 0x06 };
            LegStarMessagePart part = new LegStarMessagePart("CONTAINER", content);
            part.setPayloadSize(4);
                         /*C O N T A I N E R                     4 1 2 3 4*/
            assertEquals("c3d6d5e3c1c9d5c5d9404040404040400000000401020304",
                    HostData.toHexString(part.toByteArray()));
            part.setPayloadSize(0);
                         /*C O N T A I N E R                     0*/
            assertEquals("c3d6d5e3c1c9d5c5d94040404040404000000000",
                    HostData.toHexString(part.toByteArray()));
        } catch (HostMessageFormatException e) {
            fail(e.getMessage());
        }
    }
    
    
    /**
     * Test GetContent.
     */
    public final void testSetContent() {
        try {
            byte[] content = {0x01, 0x02, 0x03, 0x04, 0x05, 0x06 };
            LegStarMessagePart part = new LegStarMessagePart();
            int pos = part.setContent(content, 0, 0);
            assertTrue(part.getContent() == null);
            assertEquals(0, pos);
            pos = part.setContent(content, 1, 3);
            assertEquals("020304", HostData.toHexString(part.getContent()));
            assertEquals(4, pos);
        } catch (HostMessageFormatException e) {
            fail(e.getMessage());
        }
    }
    
    /**
     * Test GetContentLength.
     */
    public final void testGetContentLength() {
        try {
            LegStarMessagePart part = new LegStarMessagePart();
            assertEquals(0, part.getContentLength(HostData.toByteArray("00000000"), 0));
            assertEquals(16777216, part.getContentLength(HostData.toByteArray("01000000"), 0));
        } catch (HostMessageFormatException e) {
            fail(e.getMessage());
        }
        try {
            LegStarMessagePart part = new LegStarMessagePart();
            part.getContentLength(HostData.toByteArray("01000001"), 0);
        } catch (HostMessageFormatException e) {
            assertEquals("Invalid message part content length 16777217", e.getMessage());
        }
    }
    
    /**
     * Test SetPartID.
     */
    public void testSetPartID() {
        try {
            LegStarMessagePart part = new LegStarMessagePart();
            part.setPartID("A VERY VERY VERY LONG PART ID");
        } catch (HostMessageFormatException e) {
            assertEquals("Invalid message part ID A VERY VERY VERY LONG PART ID",
                    e.getMessage());
        }
        try {
            LegStarMessagePart part = new LegStarMessagePart();
            part.setPartID("NORMAL PART ID");
            assertEquals("NORMAL PART ID", part.getPartID());
        } catch (HostMessageFormatException e) {
            fail(e.getMessage());
        }
        try {
            LegStarMessagePart part = new LegStarMessagePart();
            part.setPartID("LSOKHEAD");
            part.setPartID("SOMETHINGELSE");
        } catch (HostMessageFormatException e) {
            assertEquals("Invalid message part ID. Expected LSOKHEAD, received SOMETHINGELSE",
                    e.getMessage());
        }
        
    }
    
    /**
     * Test GetHostException.
     */
    public void testGetHostException() {
        LegStarMessagePart part = new LegStarMessagePart();
        HostMessageFormatException e = part.getHostException(
                HostData.toByteArray(LSOKERR0_SAMPLE), 0);
        assertEquals("CICS command=LINK COMMAREA failed, resp=PGMIDERR, resp2=3",
                e.getMessage());
    }
    
    /**
     * Test FromByteArray.
     */
    public void testFromByteArray() {
        try {
            LegStarMessagePart part = new LegStarMessagePart();
            part.fromByteArray(new byte[0], 0);
        } catch (HostMessageFormatException e) {
            assertEquals("Invalid message part",
                    e.getMessage());
        }
        try {
            LegStarMessagePart part = new LegStarMessagePart();
            part.fromByteArray(HostData.toByteArray(LSOKERR0_SAMPLE), 0);
        } catch (HostMessageFormatException e) {
            assertEquals("CICS command=LINK COMMAREA failed, resp=PGMIDERR, resp2=3",
                    e.getMessage());
        }
        try {
            LegStarMessagePart part = new LegStarMessagePart();
            part.fromByteArray(new byte[10], 0);
        } catch (HostMessageFormatException e) {
            assertEquals("Invalid message part. No ID",
                    e.getMessage());
        }
        try {
            LegStarMessagePart part = new LegStarMessagePart();
            part.fromByteArray(HostData.toByteArray(PART_SAMPLE), 0);
            assertEquals("CONTAINER", part.getPartID());
            assertEquals(4, part.getContent().length);
            assertEquals(4, part.getPayloadSize());
            assertEquals("01020304", HostData.toHexString(part.getContent()));
        } catch (HostMessageFormatException e) {
            fail(e.getMessage());
        }
        try {
            LegStarMessagePart part = new LegStarMessagePart();
            part.fromByteArray(HostData.toByteArray(LSOKHEAD_SAMPLE), 0);
            assertEquals("LSOKHEAD", part.getPartID());
            assertEquals(48, part.getContent().length);
            assertEquals(48, part.getPayloadSize());
            assertEquals(
                    "00000005"
                    + "00000028"
                    + "c07fc3c9c3e2c481a381d3859587a3887f7a7ff67f6b7fc3c9c3e2d3859587a3887f7a7ff7f97fd0",
                    HostData.toHexString(part.getContent()));
        } catch (HostMessageFormatException e) {
            fail(e.getMessage());
        }
    }
}
