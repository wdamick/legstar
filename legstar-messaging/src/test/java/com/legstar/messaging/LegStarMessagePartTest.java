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

    /**
     * Create a header message part and test how it serializes into host format.
     * @throws IOException if test fails
     */
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
            assertEquals(LSOKHEAD_SAMPLE, HostData.toHexString(headerBytes));
        } catch (HeaderPartException e) {
            fail("testHostSerializeHeaderPart failed " + e);
        }
    }

    /**
     * Test receiving a message part from the host.
     * @throws HostReceiveException if test fails
     */
    public final void testrecvFromHost() throws HostReceiveException {
        byte[] hostBytes = HostData.toByteArray(LSOKHEAD_SAMPLE);
        ByteArrayInputStream hostStream = new ByteArrayInputStream(hostBytes);
        LegStarMessagePart part = new LegStarMessagePart();
        part.recvFromHost(hostStream);
        assertEquals("LSOKHEAD", part.getID());
        assertEquals(48, part.getContent().length);
        assertEquals(LSOKHEAD_SAMPLE.substring(40), HostData.toHexString(part.getContent()));
    }

    /**
     * If host sends data that is too small to hold even a message part ID,
     * we should fail gracefully.
     */
    public final void testrecvFromHostTooSmall() {
        byte[] hostBytes = HostData.toByteArray("d3e2d6d2c8c5c1c44040404040");
        ByteArrayInputStream hostStream = new ByteArrayInputStream(hostBytes);
        LegStarMessagePart part = new LegStarMessagePart();
        try {
            part.recvFromHost(hostStream);
            fail("failed testrecvFromHostTooSmall");
        } catch (HostReceiveException e) {
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
        ByteArrayInputStream hostStream = new ByteArrayInputStream(hostBytes);
        LegStarMessagePart part = new LegStarMessagePart();
        try {
            part.recvFromHost(hostStream);
            fail("failed testrecvFromHostTooSmall");
        } catch (HostReceiveException e) {
            assertEquals("Invalid message part content length", e.getMessage());
        }
    }

    /**
     * It is acceptable that there is no content for a message part.
     * @throws HostReceiveException if test fails
     */
    public final void testrecvFromHostNoContent() throws HostReceiveException {
        byte[] hostBytes = HostData.toByteArray(
                "d3e2d6d2c8c5c1c44040404040404040"
                /* This is 0 bytes content*/
                + "00000000");
        ByteArrayInputStream hostStream = new ByteArrayInputStream(hostBytes);
        LegStarMessagePart part = new LegStarMessagePart();
        part.recvFromHost(hostStream);
        assertEquals("LSOKHEAD", part.getID());
        assertTrue(null == part.getContent());
    }

    /**
     * Application errors are returned by the host with a special eye catcher to
     * distinguish them from actual message parts.
     */
    public final void testrecvErrorMessageFromHost() {
        byte[] hostBytes = HostData.toByteArray(LSOKERR0_SAMPLE);
        ByteArrayInputStream hostStream = new ByteArrayInputStream(hostBytes);
        LegStarMessagePart part = new LegStarMessagePart();
        try {
            part.recvFromHost(hostStream);
            fail();
        } catch (HostReceiveException e) {
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
