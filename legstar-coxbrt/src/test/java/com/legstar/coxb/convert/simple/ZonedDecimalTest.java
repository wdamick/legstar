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

import java.math.BigDecimal;

import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;

/**
 * Test the COBOL ZONED DECIMAL TYPE.
 * 
 */
public class ZonedDecimalTest extends TestCase {

    /** Mainframe EBCDIC US/CAN character set. */
    private static final String US_HOST_CHARSET = "IBM01140";

    /** Mainframe EBCDIC French character set. */
    private static final String FRENCH_HOST_CHARSET = "IBM01147";

    /** Mainframe Latin-1 character set. */
    private static final String LATIN1_HOST_CHARSET = "ISO-8859-1";

    /**
     * Generic conversion from java BigDecimal to COBOL ZONED DECIMAL.
     * 
     * @param byteLength the COBOL receiving field size
     * @param totalDigits total number of digits
     * @param fractionDigits number of fraction digits
     * @param signed true if signed
     * @param isSignSeparate true if sign is separate
     * @param isSignLeading true if sign is leading
     * @param hostCharset the mainframe character set
     * @param inputValue the input value as a string
     * @param expectedValue the expected value as a hex string
     */
    public static void toHost(
            final int byteLength,
            final int totalDigits,
            final int fractionDigits,
            final boolean signed,
            final boolean isSignSeparate,
            final boolean isSignLeading,
            final String hostCharset,
            final String inputValue,
            final String expectedValue) {
        try {
            byte[] hostBytes = new byte[byteLength];
            BigDecimal javaDecimal;
            if (inputValue == null) {
                javaDecimal = null;
            } else {
                javaDecimal = new BigDecimal(inputValue);
            }
            assertEquals(byteLength, CobolZonedDecimalSimpleConverter
                    .toHostSingle(
                            javaDecimal, byteLength, totalDigits,
                            fractionDigits,
                            signed, isSignSeparate, isSignLeading, hostBytes,
                            0, hostCharset));
            assertEquals(expectedValue, HostData.toHexString(hostBytes));
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Generic conversion from COBOL ZONED DECIMAL to java BigDecimal.
     * 
     * @param byteLength the COBOL receiving field size
     * @param totalDigits total number of digits
     * @param fractionDigits number of fraction digits
     * @param signed true if signed
     * @param isSignSeparate true if sign is separate
     * @param isSignLeading true if sign is leading
     * @param hostCharset the mainframe character set
     * @param inputValue the input value as a hex string
     * @param expectedValue the expected value as a string
     */
    public static void fromHost(
            final int byteLength,
            final int totalDigits,
            final int fractionDigits,
            final boolean signed,
            final boolean isSignSeparate,
            final boolean isSignLeading,
            final String hostCharset,
            final String inputValue,
            final String expectedValue) {
        try {
            byte[] hostBytes = HostData.toByteArray(inputValue);
            BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter
                    .fromHostSingle(
                            byteLength, totalDigits, fractionDigits,
                            signed, isSignSeparate, isSignLeading, hostBytes,
                            0, hostCharset);
            assertEquals(expectedValue, javaDecimal.toString());
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test scaling.
     */
    public void testScaling() {
        toHost(18, 18, 5, false, false, false, US_HOST_CHARSET,
                "256.85", "f0f0f0f0f0f0f0f0f0f0f2f5f6f8f5f0f0f0");
        toHost(18, 18, 5, false, false, false, US_HOST_CHARSET,
                "256.852568", "f0f0f0f0f0f0f0f0f0f0f2f5f6f8f5f2f5f6");
        toHost(18, 18, 5, false, false, false, US_HOST_CHARSET,
                "256.85257", "f0f0f0f0f0f0f0f0f0f0f2f5f6f8f5f2f5f7");
        toHost(18, 18, 0, false, false, false, US_HOST_CHARSET,
                "256.85257", "f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f2f5f6");
    }

    /**
     * Test the virtualization of the decimal point.
     */
    public void testVirtualizing() {
        toHost(18, 18, 5, false, false, false, US_HOST_CHARSET,
                ".256", "f0f0f0f0f0f0f0f0f0f0f0f0f0f2f5f6f0f0");
    }

    /**
     * Test that separate sign gets included.
     */
    public void testSignSeparate() {
        toHost(10, 9, 0, true, true, true, US_HOST_CHARSET,
                 "456", "4ef0f0f0f0f0f0f4f5f6");
        toHost(10, 9, 0, true, true, false, US_HOST_CHARSET,
                 "456", "f0f0f0f0f0f0f4f5f64e");
        toHost(10, 9, 0, true, true, true, US_HOST_CHARSET,
                 "-456", "60f0f0f0f0f0f0f4f5f6");
        toHost(10, 9, 0, true, true, false, US_HOST_CHARSET,
                 "-456", "f0f0f0f0f0f0f4f5f660");
    }

    /**
     * Check an ascii character set.
     */
    public void testHostCharset() {
        toHost(18, 17, 2, true, true, true, LATIN1_HOST_CHARSET,
                "-123456789012345.12", "2d3132333435363738393031323334353132");
    }

    /**
     * Test overflow situations.
     */
    public void testOverflow() {
        byte[] hostBytes = new byte[9];
        BigDecimal decimal = new BigDecimal("25689745623");
        try {
            CobolZonedDecimalSimpleConverter.toHostSingle(
                    decimal, 9, 9, 0, false, false, false, hostBytes, 0,
                    US_HOST_CHARSET);
        } catch (CobolConversionException e) {
            assertEquals("BigDecimal value too large for target Cobol field."
                    + " Host data at offset 0=0x000000000000000000",
                    e.getMessage());
        }
        decimal = new BigDecimal("256897456");
        try {
            long offset = CobolZonedDecimalSimpleConverter.toHostSingle(
                    decimal, 9, 9, 0, false, false, false, hostBytes, 0,
                    US_HOST_CHARSET);
            assertEquals(9, offset);
            assertEquals("f2f5f6f8f9f7f4f5f6", HostData.toHexString(hostBytes));
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }

    /**
     * 9(6)V9(5).
     */
    public void testToHost456790p00675() {
        toHost(11, 11, 5, false, false, true, FRENCH_HOST_CHARSET,
                "+456790.00675", "f4f5f6f7f9f0f0f0f6f7f5");
    }

    /**
     * 9(15)V9(2).
     */
    public void testToHostM123456789012345p12() {
        toHost(17, 17, 2, true, false, false, FRENCH_HOST_CHARSET,
                "-123456789012345.12", "f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5f1d2");
    }

    /**
     * Zero java value.
     */
    public void testToHostZero() {
        toHost(17, 17, 0, false, false, false, FRENCH_HOST_CHARSET,
                "0", "f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0");
    }

    /**
     * Null java value.
     */
    public void testToHostNull() {
        toHost(17, 17, 0, false, false, false, FRENCH_HOST_CHARSET,
                null, "f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0");
    }

    /**
     * Minus zero java value.
     */
    public void testToHostMZero() {
        toHost(9, 9, 0, false, false, true, FRENCH_HOST_CHARSET,
                "-0", "f0f0f0f0f0f0f0f0f0");
    }

    /**
     * S9(5)V9(5) SIGN IS LEADING SEPARATE.
     */
    public void testToHostM67000p56007() {
        toHost(11, 10, 5, true, true, true, FRENCH_HOST_CHARSET,
                "-67000.56007", "60f6f7f0f0f0f5f6f0f0f7");
    }

    /**
     * S9(5)V9(5).
     */
    public void testToHostM67000p56007Trunc() {
        toHost(10, 10, 2, true, false, false, FRENCH_HOST_CHARSET,
                "-67000.56007", "f0f0f0f6f7f0f0f0f5d6");
    }

    /**
     * Case 1 digit.
     */
    public void testToHostSingleDigit() {
        toHost(1, 1, 0, false, false, false, FRENCH_HOST_CHARSET,
                "1", "f1");
    }

    /**
     * 9(1)V9(5).
     */
    public void testToHostp12345() {
        toHost(6, 6, 5, false, false, false, FRENCH_HOST_CHARSET,
                ".12345", "f0f1f2f3f4f5");
    }

    /**
     * Test scaling.
     */
    public void testToHostScaling() {
        toHost(10, 10, 3, false, false, false, FRENCH_HOST_CHARSET,
                "1234567.89", "f1f2f3f4f5f6f7f8f9f0");
    }

    /**
     * Test padding.
     */
    public void testToHostPadding() {
        toHost(5, 5, 0, false, false, false, FRENCH_HOST_CHARSET,
                "12", "f0f0f0f1f2");
    }

    /**
     * Test a large value.
     */
    public void testToHostLarge() {
        toHost(
                31,
                31,
                0,
                false,
                false,
                false,
                FRENCH_HOST_CHARSET,
                "1234567890123456789012345678901",
                "f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5f6f7f8f9f0f1");
    }

    /**
     * S9(2)V9(6) SIGN IS LEADING SEPARATE.
     */
    public void testToHostSignSepLead() {
        toHost(9, 8, 6, true, true, true, FRENCH_HOST_CHARSET,
                "-45.98754", "60f4f5f9f8f7f5f4f0");
    }

    /**
     * S9(2)V9(6) SIGN IS LEADING.
     */
    public void testToHostSignLead() {
        toHost(8, 8, 6, true, false, true, FRENCH_HOST_CHARSET,
                "-45.98754", "d4f5f9f8f7f5f4f0");
    }

    /**
     * S9(2)V9(6) SIGN IS TRAILING SEPARATE.
     */
    public void testToHostSignSepTrail() {
        toHost(9, 8, 6, true, true, false, FRENCH_HOST_CHARSET,
                "-45.98754", "f4f5f9f8f7f5f4f060");
    }

    /**
     * S9(2)V9(6) SIGN IS TRAILING.
     */
    public void testToHostSignTrail() {
        toHost(8, 8, 6, true, false, false, FRENCH_HOST_CHARSET,
                "-45.98754", "f4f5f9f8f7f5f4d0");
    }

    /**
     * Java decimal too large.
     */
    public void testToHostInvalid() {
        try {
            byte[] hostBytes = new byte[1];
            BigDecimal javaDecimal = new BigDecimal("235.87");
            CobolZonedDecimalSimpleConverter.toHostSingle(
                    javaDecimal, 1, 1, 0, false, false, false, hostBytes, 0,
                    FRENCH_HOST_CHARSET);
            fail("BigDecimal too large not detected");
        } catch (HostException he) {
            assertEquals("BigDecimal value too large for target Cobol field."
                    + " Host data at offset 0=0x00", he.getMessage());
        }
    }

    /**
     * 9(6)V9(5).
     */
    public void testFromHost456790p00675() {
        fromHost(11, 11, 5, false, false, true, FRENCH_HOST_CHARSET,
                "f4f5f6f7f9f0f0f0f6f7f5", "456790.00675");
    }

    /**
     * 9(15)V9(2).
     */
    public void testFromHostM123456789012345p12() {
        fromHost(17, 17, 2, true, false, false, FRENCH_HOST_CHARSET,
                "f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5f1d2", "-123456789012345.12");
    }

    /**
     * Host zero value.
     */
    public void testFromHostZero() {
        fromHost(17, 17, 0, false, false, false, FRENCH_HOST_CHARSET,
                "f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0", "0");
    }

    /**
     * S9(5)V9(5) SIGN LEADING SEPARATE.
     */
    public void testFromHostM67000p56007() {
        fromHost(11, 10, 5, true, true, true, FRENCH_HOST_CHARSET,
                "60f6f7f0f0f0f5f6f0f0f7", "-67000.56007");
    }

    /**
     * Case of truncation.
     */
    public void testFromHostM67000p56007Trunc() {
        fromHost(10, 10, 2, true, false, false, FRENCH_HOST_CHARSET,
                "f0f0f0f6f7f0f0f0f5d6", "-67000.56");
    }

    /**
     * Case single digit.
     */
    public void testFromHostSingleDigit() {
        fromHost(1, 1, 0, false, false, true, FRENCH_HOST_CHARSET,
                "f1", "1");
    }

    /**
     * 9(1)V9(5).
     */
    public void testFromHostp12345() {
        fromHost(6, 6, 5, false, false, true, FRENCH_HOST_CHARSET,
                "f0f1f2f3f4f5", "0.12345");
    }

    /**
     * Test scaling.
     */
    public void testFromHostScaling() {
        fromHost(10, 10, 3, false, false, true, FRENCH_HOST_CHARSET,
                "f1f2f3f4f5f6f7f8f9f0", "1234567.890");
    }

    /**
     * Test padding.
     */
    public void testFromHostPadding() {
        fromHost(5, 5, 0, false, false, true, FRENCH_HOST_CHARSET,
                "f0f0f0f1f2", "12");
    }

    /**
     * Test large value.
     */
    public void testFromHostLarge() {
        fromHost(
                31,
                31,
                0,
                false,
                false,
                true,
                FRENCH_HOST_CHARSET,
                "f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5f6f7f8f9f0f1",
                "1234567890123456789012345678901");
    }

    /**
     * S9(2)V9(6) SIGN LEADING SEPARATE.
     */
    public void testFromSignSepLead() {
        fromHost(9, 8, 6, true, true, true, FRENCH_HOST_CHARSET,
                "60f4f5f9f8f7f5f4f0", "-45.987540");
    }

    /**
     * S9(2)V9(6) SIGN LEADING.
     */
    public void testFromSignLead() {
        fromHost(8, 8, 6, true, false, true, FRENCH_HOST_CHARSET,
                "d4f5f9f8f7f5f4f0", "-45.987540");
    }

    /**
     * S9(2)V9(6) SIGN TRAILING SEPARATE.
     */
    public void testFromSignSepTrail() {
        fromHost(9, 8, 6, true, true, false, FRENCH_HOST_CHARSET,
                "f4f5f9f8f7f5f4f060", "-45.987540");
    }

    /**
     * S9(2)V9(6) SIGN TRAILING.
     */
    public void testFromSignTrail() {
        fromHost(8, 8, 6, true, false, false, FRENCH_HOST_CHARSET,
                "f4f5f9f8f7f5f4d0", "-45.987540");
    }

    /**
     * Host is sending invalid data.
     */
    public void testFromHostInvalid() {
        try {
            byte[] hostBytes = HostData.toByteArray("1A");
            CobolZonedDecimalSimpleConverter.fromHostSingle(
                    1, 1, 0, false, false, true, hostBytes, 0,
                    FRENCH_HOST_CHARSET);
            fail("Invalid zoned decimal not detected");
        } catch (HostException he) {
            assertEquals(
                    "Host data contains a byte that is not a valid zoned decimal byte."
                            + " Host data at offset 0=0x1a", he.getMessage());
        }
    }

    /**
     * Host is sending invalid data (second case).
     */
    public void testFromHostInvalid2() {
        try {
            byte[] hostBytes = HostData.toByteArray("0A1f");
            CobolZonedDecimalSimpleConverter.fromHostSingle(
                    2, 2, 0, false, false, true, hostBytes, 0,
                    FRENCH_HOST_CHARSET);
            fail("Invalid zoned decimal not detected");
        } catch (HostException he) {
            assertEquals(
                    "Host data contains a byte that is not a valid zoned decimal byte."
                            + " Host data at offset 0=0x0a1f", he.getMessage());
        }
    }

    /**
     * Case where there is not enough data left in the host buffer. The code
     * should consider
     * that trailing nulls were omitted by the host.
     */
    public void testToHostPartialData() {
        fromHost(8, 8, 2, false, false, true, FRENCH_HOST_CHARSET,
                "f4f5f9f8f760", "0.00");
    }

    /**
     * Same as above but this time we are already past the offset.
     */
    public void testToHostPartialDataPastOffset() {
        fromHost(8, 8, 2, false, false, true, FRENCH_HOST_CHARSET,
                "45679000675f", "0.00");
    }

    /**
     * Host is sending data with space characters instead of 0.
     */
    public void testFromHostWithSpaceChar() {
        fromHost(2, 2, 0, false, false, true, US_HOST_CHARSET,
                "40F0", "0");
    }

    /**
     * Host is sending data with low values instead of 0.
     */
    public void testFromHostWithLowValues() {
        fromHost(2, 2, 0, false, false, true, US_HOST_CHARSET,
                "0000", "0");
    }

    /**
     * Host is sending data already in ASCII.
     */
    public void testFromHostWithAsciiChars() {
        fromHost(18, 17, 2, true, true, true, LATIN1_HOST_CHARSET,
                "2d3132333435363738393031323334353132",
                "-123456789012345.12");
    }

}
