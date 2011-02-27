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
package com.legstar.coxb.convert.simple;

import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.coxb.host.HostData;

import junit.framework.TestCase;

/**
 * Test capability to support various character sets.
 *
 */
public class CharsetsTest extends TestCase {

    /** Happy NIU year. */
    private static final String CHINESE_CHARS = "牛年快乐";

    /** Simplified Chinese EBCDIC (CCSID 935). */
    private static final String CHINESE_EBCDIC_DBCS_CHARSET = "IBM935";

    /**
     * Test code points with UTF-16.
     * @throws Exception for encoding exception
     */
    public void testUTF16() throws Exception {

        /* Java is naturally UTF-16 */
        assertEquals("725b", Integer.toHexString(Character.codePointAt(CHINESE_CHARS, 0)));
        assertEquals("5e74", Integer.toHexString(Character.codePointAt(CHINESE_CHARS, 1)));
        assertEquals("5feb", Integer.toHexString(Character.codePointAt(CHINESE_CHARS, 2)));
        assertEquals("4e50", Integer.toHexString(Character.codePointAt(CHINESE_CHARS, 3)));

        /* A UTF-16 byte representation gets a 2 bytes BOM xfeff */
        byte[] bytes = CHINESE_CHARS.getBytes("UTF-16");
        assertEquals("feff725b5e745feb4e50", HostData.toHexString(bytes));

        /* This is how the new IBM COBOL NATIONAL types expect UTF-16 (Big Endian) */
        bytes = CHINESE_CHARS.getBytes("UTF-16BE");
        assertEquals("725b5e745feb4e50", HostData.toHexString(bytes));


        bytes = CHINESE_CHARS.getBytes("UTF-16LE");
        assertEquals("5b72745eeb5f504e", HostData.toHexString(bytes));
    }

    /**
     * Test the COBOL NATIONAL support.
     */
    public void testNationalToHost() {
        /* Convert in a COBOL field of the exact size */
        try {
            byte[] hostBytes = new byte[CHINESE_CHARS.length() * 2];
            CobolNationalSimpleConverter.toHostSingle(CHINESE_CHARS,
                    hostBytes.length, false, hostBytes, 0);
            assertEquals("725b5e745feb4e50", HostData.toHexString(hostBytes));
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }

        /* Convert in a COBOL field that is larger than expected */
        try {
            byte[] hostBytes = new byte[2 + CHINESE_CHARS.length() * 2];
            CobolNationalSimpleConverter.toHostSingle(CHINESE_CHARS,
                    hostBytes.length, false, hostBytes, 0);
            assertEquals("725b5e745feb4e500020", HostData.toHexString(hostBytes));
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test conversion to a chinese EBCDIC code page IBM935.
     * Observe the shift-in 0x0e and shift-out characters 0x0f. These are expected in
     * a regular PIC X but not in a PIC G.
     */
    public void testStringToHost() {
        try {
            byte[] hostBytes = new byte[2 + CHINESE_CHARS.length() * 2];
            CobolStringSimpleConverter.toHostSingle(
                    CHINESE_CHARS, CHINESE_EBCDIC_DBCS_CHARSET, hostBytes.length, false, hostBytes, 0);
            assertEquals("0e534352e9508d50d50f", HostData.toHexString(hostBytes));
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
 
        /* Convert in a COBOL field that is larger than expected */
        try {
            byte[] hostBytes = new byte[4 + CHINESE_CHARS.length() * 2];
            CobolStringSimpleConverter.toHostSingle(
                    CHINESE_CHARS, CHINESE_EBCDIC_DBCS_CHARSET, hostBytes.length, false, hostBytes, 0);
            assertEquals("0e534352e9508d50d50f4040", HostData.toHexString(hostBytes));
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }

        /* See what happens when western characters are interspersed.
         * The final number of characters depends on how many separated sequences
         * of DBCS characters there are. Each sequenced is bound with 0x0e and ox0f */
        try {
            byte[] hostBytes = new byte[6 + CHINESE_CHARS.length() * 2];
            CobolStringSimpleConverter.toHostSingle(
                    CHINESE_CHARS.substring(0, 2) + "ab" + CHINESE_CHARS.substring(2),
                    CHINESE_EBCDIC_DBCS_CHARSET, hostBytes.length, false, hostBytes, 0);
            assertEquals("0e534352e90f81820e508d50d50f", HostData.toHexString(hostBytes));
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }

}
