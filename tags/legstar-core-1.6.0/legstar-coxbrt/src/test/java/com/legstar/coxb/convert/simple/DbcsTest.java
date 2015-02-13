/*******************************************************************************
 * Copyright (c) 2015 LegSem.
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

import junit.framework.TestCase;

/**
 * Test capability to support various character sets.
 *
 */
public class DbcsTest extends TestCase {

    /** Happy NIU year. */
    private static final String CHINESE_CHARS = "牛年快乐";

    /** Simplified Chinese EBCDIC (CCSID 935). */
    private static final String CHINESE_EBCDIC_DBCS_CHARSET = "IBM935";

    /**
     * Test conversion to a chinese EBCDIC code page IBM935 using pure DBCS.
     */
    public void testDbcsToHost() {
        try {
            byte[] hostBytes = new byte[CHINESE_CHARS.length() * 2];
            CobolDbcsSimpleConverter.toHostSingle(
                    CHINESE_CHARS, CHINESE_EBCDIC_DBCS_CHARSET, hostBytes.length, false, hostBytes, 0);
            assertEquals("534352e9508d50d5", HostData.toHexString(hostBytes));
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
 
        /* Convert in a COBOL field that is larger than expected */
        try {
            byte[] hostBytes = new byte[4 + CHINESE_CHARS.length() * 2];
            CobolDbcsSimpleConverter.toHostSingle(
                    CHINESE_CHARS, CHINESE_EBCDIC_DBCS_CHARSET, hostBytes.length, false, hostBytes, 0);
            assertEquals("534352e9508d50d540404040", HostData.toHexString(hostBytes));
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }

    }

    /**
     * Test conversion from a chinese EBCDIC code page IBM935 using pure DBCS.
     */
    public void testHostToDbcs() {
        try {
            byte[] hostBytes = HostData.toByteArray("534352e9508d50d5");
            String javaString = CobolDbcsSimpleConverter.fromHostSingle(
                    CHINESE_EBCDIC_DBCS_CHARSET, hostBytes.length, hostBytes, 0);
            assertEquals(CHINESE_CHARS, javaString);
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
 
    }
}
