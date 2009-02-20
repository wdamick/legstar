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
package com.legstar.coxb.convert.simple.test;

import com.legstar.coxb.convert.simple.CobolOctetStreamSimpleConverter;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;

/**
 * Test the COBOL PIC X TYPE without conversion.
 *
 */
public class OctetStreamTest extends TestCase {

    public void testToHostSimple () throws HostException{
        // Create a host buffer
        byte[] hostBytes = new byte[4];

        byte[] javaBytes = {0x0c, 0x15, -0x17, 0x00};
        assertEquals(4, CobolOctetStreamSimpleConverter.toHostSingle(javaBytes,4, hostBytes, 0));
        assertEquals("0c15e900", HostData.toHexString(hostBytes));
    }

    public void testFromHostSimple () throws HostException{
        // Create a host buffer
        byte[] hostSource = {-0x3F, -0x3E, -0x3D, -0x3C};

        byte[] javaBytes = CobolOctetStreamSimpleConverter.fromHostSingle(4, hostSource, 0);
        assertEquals(hostSource[0], javaBytes[0]);
        assertEquals(hostSource[1], javaBytes[1]);
        assertEquals(hostSource[2], javaBytes[2]);
        assertEquals(hostSource[3], javaBytes[3]);
    }

    /**
     * Case where the host truncated that data so there are not enough bytes. Code
     * should pad with null bytes.
     * @throws HostException if test fails
     */
    public void testFromHostPartialData () throws HostException{
        // Create a host buffer
        byte[] hostSource = {-0x3F, -0x3E, -0x3D, -0x3C};

        byte[] javaBytes = CobolOctetStreamSimpleConverter.fromHostSingle(8, hostSource, 0);
        assertEquals(8, javaBytes.length);
        assertEquals(hostSource[0], javaBytes[0]);
        assertEquals(hostSource[1], javaBytes[1]);
        assertEquals(hostSource[2], javaBytes[2]);
        assertEquals(hostSource[3], javaBytes[3]);
        assertEquals(0x00, javaBytes[4]);
        assertEquals(0x00, javaBytes[5]);
        assertEquals(0x00, javaBytes[6]);
        assertEquals(0x00, javaBytes[7]);
    }

    /**
     * Same as above but we are already beyond the offset.
     * @throws HostException if test fails
     */
    public void testFromHostPartialDataPastOffset () throws HostException{
        // Create a host buffer
        byte[] hostSource = {-0x3F, -0x3E, -0x3D, -0x3C};

        byte[] javaBytes = CobolOctetStreamSimpleConverter.fromHostSingle(4, hostSource, 4);
        assertEquals(4, javaBytes.length);
        assertEquals(0x00, javaBytes[0]);
        assertEquals(0x00, javaBytes[1]);
        assertEquals(0x00, javaBytes[2]);
        assertEquals(0x00, javaBytes[3]);
    }
}
