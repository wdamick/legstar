/*******************************************************************************
 * Copyright (c) 2011 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.host;

import junit.framework.TestCase;

/**
 * Test the HostData class.
 *
 */
public class HostDataTest extends TestCase {
    
    /**
     * Test the hexadecimal string representation of the host data.
     */
    public void testToHexString() {
        assertTrue(null == HostData.toHexString(null));
        try {
            assertTrue(null == HostData.toHexString(new byte[0], 5, 10));
            fail("Length error not detected");
        } catch (IllegalArgumentException e) {
            assertEquals("Invalid start or length parameter", e.getMessage());
        }
        assertEquals("00", HostData.toHexString(new byte[] {0x00}));
        assertEquals("0001", HostData.toHexString(new byte[] {0x00, 0x01}));
        assertEquals("0001ff", HostData.toHexString(new byte[] {0x00, 0x01, -0x01}));
        assertEquals("", HostData.toHexString(new byte[] {0x00, 0x01}, 0, 0));
        assertEquals("00", HostData.toHexString(new byte[] {0x00, 0x01}, 0, 1));
        assertEquals("01", HostData.toHexString(new byte[] {0x00, 0x01}, 1, 1));
    }

    /**
     * Test the hexadecimal string representation of the host data with maximum.
     */
    public void testToHexStringWithMax() {
        assertTrue(null == HostData.toHexString(null, 100));
        try {
            assertTrue(null == HostData.toHexString(new byte[0], 5, 10, 100));
            fail("Length error not detected");
        } catch (IllegalArgumentException e) {
            assertEquals("Invalid start or length parameter", e.getMessage());
        }
        try {
            assertEquals("0001", HostData.toHexString(new byte[] {0x00, 0x01}, 1));
            fail("Length error not detected");
        } catch (IllegalArgumentException e) {
            assertEquals("maxBytes cannot be smaller than 2", e.getMessage());
        }
        assertEquals("0001", HostData.toHexString(new byte[] {0x00, 0x01}, 2));
        assertEquals("0001", HostData.toHexString(new byte[] {0x00, 0x01}, 3));
        assertEquals("00....02", HostData.toHexString(new byte[] {0x00, 0x01, 0x02}, 2));
        assertEquals("00....03", HostData.toHexString(new byte[] {0x00, 0x01, 0x02, 0x03}, 2));
        assertEquals("00....03", HostData.toHexString(new byte[] {0x00, 0x01, 0x02, 0x03}, 3));
        assertEquals("00010203", HostData.toHexString(new byte[] {0x00, 0x01, 0x02, 0x03}, 4));
        assertEquals("00....04", HostData.toHexString(new byte[] {0x00, 0x01, 0x02, 0x03, 0x04}, 0, 5, 2));
        assertEquals("00....03", HostData.toHexString(new byte[] {0x00, 0x01, 0x02, 0x03, 0x04}, 0, 4, 2));
        assertEquals("01....04", HostData.toHexString(new byte[] {0x00, 0x01, 0x02, 0x03, 0x04}, 1, 4, 2));
        assertEquals("01020304", HostData.toHexString(new byte[] {0x00, 0x01, 0x02, 0x03, 0x04}, 1, 4, 4));
    }
}
