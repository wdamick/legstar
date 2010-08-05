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
package com.legstar.coxb.convert.simple;

import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;

/**
 * Test the COBOL PIC X TYPE without conversion.
 *
 */
public class OctetStreamTest extends TestCase {

    /**
     * PIC X(4).
     */
    public void testToHostSimple() {
        try {
            byte[] hostBytes = new byte[4];
            byte[] javaBytes = HostData.toByteArray("0c15e900");
            assertEquals(4, CobolOctetStreamSimpleConverter.toHostSingle(
                    javaBytes, 4, hostBytes, 0));
            assertEquals("0c15e900", HostData.toHexString(hostBytes));
        } catch (HostException e) {
            fail(e.getMessage());
        }
    }

    /**
     * PIC X(4).
     */
    public void testFromHostSimple() {
        try {
            byte[] hostSource = HostData.toByteArray("c1c2c3c4");
            byte[] javaBytes = CobolOctetStreamSimpleConverter.fromHostSingle(4, hostSource, 0);
            assertEquals("c1c2c3c4", HostData.toHexString(javaBytes));
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Case where the host truncated that data so there are not enough bytes. Code
     * should pad with null bytes.
     */
    public void testFromHostPartialData() {
        try {
            byte[] hostSource = HostData.toByteArray("c1c2c3c4");
            byte[] javaBytes = CobolOctetStreamSimpleConverter.fromHostSingle(8, hostSource, 0);
            assertEquals(8, javaBytes.length);
            assertEquals("c1c2c3c400000000", HostData.toHexString(javaBytes));
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Same as above but we are already beyond the offset.
     */
    public void testFromHostPartialDataPastOffset() {
        try {
            byte[] hostSource = HostData.toByteArray("c1c2c3c4");
            byte[] javaBytes = CobolOctetStreamSimpleConverter.fromHostSingle(4, hostSource, 4);
            assertEquals(4, javaBytes.length);
            assertEquals("00000000", HostData.toHexString(javaBytes));
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }
}
