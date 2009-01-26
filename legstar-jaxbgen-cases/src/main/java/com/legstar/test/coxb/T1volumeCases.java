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
package com.legstar.test.coxb;

import com.legstar.coxb.host.HostData;

import junit.framework.TestCase;


/**
 * Used to simulate latge volumes of data.
 *
 */
public class T1volumeCases extends TestCase {

    /**
     * We put something recognizable at the beginning and end of the data chunk,
     * the rest is unimportant.
     * @param size the total data size
     * @return a byte array of the requested size with some recognizable content.
     */
    public static byte[] getHostBytes(final int size) { 
        byte[] content = new byte[size];
        byte[] startEC = HostData.toByteArray("d7c7d47ec9c7e8c3d9c3e3d36bd9c5c7");
        byte[] endEC = HostData.toByteArray("d7c1d9d47e4d7dd5d6c4e8d5c1d46bd3");
        System.arraycopy(startEC, 0, content, 0, 16);
        System.arraycopy(endEC, 0, content, size - 16, 16);
        return content;
    }

    /**
     * Check that the byte array contains the expected values.
     * @param byteArray the byte array to check
     */
    public static void checkByteArray(final byte[] byteArray) {
        int size = byteArray.length;
        byte[] startEC = new byte[16];
        byte[] endEC = new byte[16];
        System.arraycopy(byteArray, 0, startEC, 0, 16);
        System.arraycopy(byteArray, size - 16, endEC, 0, 16);
        assertEquals("d7c1d9d47e4d7dd5d6c4e8d5c1d46bd3", HostData.toHexString(startEC));
        assertEquals("d7c7d47ec9c7e8c3d9c3e3d36bd9c5c7", HostData.toHexString(endEC));
    }
}
