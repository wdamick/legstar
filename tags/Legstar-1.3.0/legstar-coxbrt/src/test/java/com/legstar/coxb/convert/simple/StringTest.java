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
 * Test the COBOL PIC X TYPE with conversion.
 *
 */
public class StringTest extends TestCase {

    /** Mainframe EBCDIC US/CAN character set.*/
    private static final String US_HOST_CHARSET = "IBM01140";
    
    /** Mainframe EBCDIC French character set.*/
    private static final String FRENCH_HOST_CHARSET = "IBM01147";
    
    /** Mainframe Latin-1 character set.*/
    private static final String LATIN1_HOST_CHARSET = "ISO-8859-1";
    
    /**
     * Generic conversion from java String to COBOL.
     * @param hostCharset the mainframe character set
     * @param byteLength the COBOL receiving field size 
     * @param isJustifiedRight if justified right
     * @param inputValue the string value
     * @param expectedValue the expected value as a hex string
     */
    public static void toHost(
            final String hostCharset,
            final int byteLength,
            final boolean isJustifiedRight,
            final String inputValue,
            final String expectedValue) {
        byte[] hostBytes = new byte[byteLength];
        try {
            String javaString = inputValue;
            assertEquals(byteLength, CobolStringSimpleConverter.toHostSingle(
                    javaString, hostCharset, byteLength, isJustifiedRight, hostBytes, 0));
            assertEquals(expectedValue, HostData.toHexString(hostBytes));
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }
    
    /**
     * Generic conversion from COBOL to java String.
     * @param hostCharset the mainframe character set
     * @param byteLength the COBOL receiving field size 
     * @param inputValue the input value as a hex string
     * @param expectedValue the expected value as a string
     */
    public static void fromHost(
            final String hostCharset,
            final int byteLength,
            final String inputValue,
            final String expectedValue) {
        try {
            byte[] hostSource = HostData.toByteArray(inputValue);
            String javaString = CobolStringSimpleConverter.fromHostSingle(
                    hostCharset, byteLength, hostSource, 0);
            assertEquals(expectedValue, javaString.toString());
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }
    
    /**
     * Test with wrong character set.
     */
    public void testToHostUnsupportedCharset() {
        try {
            String unknownCharSet = "IBM0114Q"; // An unavailable charset
            byte[] hostBytes = new byte[4];
            String javaString = "ABCD";
            CobolStringSimpleConverter.toHostSingle(
                    javaString, unknownCharSet, 4, false, hostBytes, 0);
            fail("charset not tested correctly");
        } catch (HostException he) {
            assertEquals("UnsupportedEncodingException:IBM0114Q", he.getMessage());
        }
    }

    /**
     * Simple string.
     */
    public void testToHostABCD() {
        toHost(US_HOST_CHARSET, 4, false, "ABCD", "c1c2c3c4");
    }

    /**
     * Simple string.
     */
    public void testFromHostABCD() {
        fromHost(US_HOST_CHARSET, 4, "c1c2c3c4", "ABCD");
    }

    /**
     * Test justification right and left.
     */
    public void testToHostJustified() {
        toHost(US_HOST_CHARSET, 6, true, "ABCD", "4040c1c2c3c4");
        toHost(US_HOST_CHARSET, 6, false, "ABCD", "c1c2c3c44040");
        toHost(US_HOST_CHARSET, 6, true, "ABCDEF", "c1c2c3c4c5c6");
        toHost(US_HOST_CHARSET, 6, false, "ABCDEF", "c1c2c3c4c5c6");
    }

    /**
     * Test with an ASCII charset.
     */
    public void testLatinCharset() {
        toHost(LATIN1_HOST_CHARSET, 6, true, "ABCD", "202041424344");
        fromHost(LATIN1_HOST_CHARSET, 6, "202041424344", "ABCD");
    }
    
    /**
     * Test passing binary content to the host. Binary content is only supported
     * when sending data to host.
     */
    public void testBinaryContent() {
        toHost(US_HOST_CHARSET, 6, true, "0x0000", "000000000000");
    }
    
    /**
     * Test truncation.
     */
    public void testToHostTruncate() {
        toHost(US_HOST_CHARSET, 4, true, "ABCDEFG", "c1c2c3c4");
    }

    /**
     * Test overflow detection.
     */
    public void testToHostWriteOverflow() {
        try {
            byte[] hostBytes = new byte[4];
            String javaString = "ABCDEFG";
            CobolStringSimpleConverter.toHostSingle(
                    javaString, US_HOST_CHARSET, 6, false, hostBytes, 0);
            fail("overflow not detected");
        } catch (HostException he) {
            assertEquals("Attempt to write past end of host source buffer."
                    + " Host data at offset 0=0x00000000",
                    he.getMessage());
        }
    }

    /**
     * Test character set.
     */
    public void testToHostCharset() {
        toHost(FRENCH_HOST_CHARSET, 43, false,
                "ça c'est un problème élémentaire à résoudre",
                "e08140837d85a2a340a495409799968293d0948540c093c0948595a381899985407c4099c0a296a4849985");

        toHost(US_HOST_CHARSET, 43, false,
                "ça c'est un problème élémentaire à résoudre",
                "488140837d85a2a340a49540979996829354948540519351948595a3818999854044409951a296a4849985");
    }

    /**
     * Test character set.
     */
    public void testFromHostCharset() {
        fromHost(FRENCH_HOST_CHARSET, 43,
                "e08140837d85a2a340a495409799968293d0948540c093c0948595a381899985407c4099c0a296a4849985",
                "ça c'est un problème élémentaire à résoudre");

        fromHost(US_HOST_CHARSET, 43,
                "e08140837d85a2a340a495409799968293d0948540c093c0948595a381899985407c4099c0a296a4849985",
                "\\a c'est un probl}me {l{mentaire @ r{soudre");
    }

    /**
     * Test that host low-values are replaced by spaces.
     */
    public void testFromHostLowValueReplacement() {
        fromHost(US_HOST_CHARSET, 4, "c100c3c4", "A CD");
    }

    /**
     * Case where the host truncated that data so there are not enough bytes. Code
     * should pad with null bytes.
     */
    public void testFromHostPartialData() {
        fromHost(US_HOST_CHARSET, 8, "c1c2c3c4", "ABCD");
    }

    /**
     * Case where the mainframe data has already been exhausted (we are past the
     * last offset). 
     */
    public void testFromHostPartialDataPastOffset() {
        try {
            byte[] hostSource = HostData.toByteArray("c1c2c3c4");
            String javaString = CobolStringSimpleConverter.fromHostSingle(
                    US_HOST_CHARSET, 4, hostSource, 4);
            assertTrue(null == javaString);
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }
}
