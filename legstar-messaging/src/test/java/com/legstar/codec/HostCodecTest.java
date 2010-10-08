/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.codec;

import java.io.UnsupportedEncodingException;

import com.legstar.coxb.host.HostData;

import junit.framework.TestCase;

/**
 * Test the HostCodec class.
 *
 */
public class HostCodecTest extends TestCase {
 
    /** Mainframe character set. */
    private static final String HOST_CHARSET = "IBM01140";

    /**
     * Test the HostCodec methods.
     * @throws UnsupportedEncodingException if conversion fails
     */
    public void testToHostBytes() throws UnsupportedEncodingException {
        /* Truncation case */
        byte[] hostBytes1 = new byte[8];
        HostCodec.toHostBytes("cicsUserID", hostBytes1, 0, 8, HOST_CHARSET);
        assertEquals("838983a2e4a28599", HostData.toHexString(hostBytes1));
        /* Fill case */
        byte[] hostBytes2 = new byte[8];
        HostCodec.toHostBytes("cics", hostBytes2, 0, 8, HOST_CHARSET);
        assertEquals("838983a240404040", HostData.toHexString(hostBytes2));
        /* Fill in the middle case */
        byte[] hostBytes3 = new byte[12];
        HostCodec.toHostBytes("cicsuser", hostBytes3, 2, 8, HOST_CHARSET);
        assertEquals("0000838983a2a4a285990000", HostData.toHexString(hostBytes3));
    }


}
