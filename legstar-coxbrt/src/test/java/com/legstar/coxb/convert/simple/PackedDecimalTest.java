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
package com.legstar.coxb.convert.simple;

import java.math.BigDecimal;

import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

import junit.framework.TestCase;

/**
 * Test the COBOL PACKED DECIMAL TYPE.
 * 
 */
public class PackedDecimalTest extends TestCase {

    /**
     * Generic conversion from java BigDecimal to COBOL PACKED DECIMAL.
     * 
     * @param byteLength the COBOL receiving field size
     * @param totalDigits total number of digits
     * @param fractionDigits number of fraction digits
     * @param signed true if signed
     * @param inputValue the input value as a string
     * @param expectedValue the expected value as a hex string
     */
    public static void toHost(
            final int byteLength,
            final int totalDigits,
            final int fractionDigits,
            final boolean signed,
            final String inputValue,
            final String expectedValue) {
        try {
            byte[] hostBytes = new byte[byteLength];
            BigDecimal javaDecimal = new BigDecimal(inputValue);
            assertEquals(byteLength, CobolPackedDecimalSimpleConverter
                    .toHostSingle(
                            javaDecimal, byteLength, totalDigits,
                            fractionDigits, signed, hostBytes, 0));
            assertEquals(expectedValue, HostData.toHexString(hostBytes));
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Generic conversion from COBOL PACKED DECIMAL to java BigDecimal.
     * 
     * @param byteLength the COBOL receiving field size
     * @param totalDigits total number of digits
     * @param fractionDigits number of fraction digits
     * @param inputValue the input value as a hex string
     * @param expectedValue the expected value as a string
     */
    public static void fromHost(
            final int byteLength,
            final int totalDigits,
            final int fractionDigits,
            final String inputValue,
            final String expectedValue) {
        try {
            byte[] hostBytes = HostData.toByteArray(inputValue);
            BigDecimal javaDecimal = CobolPackedDecimalSimpleConverter
                    .fromHostSingle(
                            byteLength, totalDigits, fractionDigits, hostBytes,
                            0);
            assertEquals(expectedValue, javaDecimal.toString());
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }

    /**
     * PIC 9(6)V9(5).
     */
    public void testToHost456790p00675() {
        toHost(6, 11, 5, false, "+456790.00675", "45679000675f");
    }

    /**
     * PIC 9(6)V9(5).
     */
    public void testFromHost456790p00675() {
        fromHost(6, 11, 5, "45679000675f", "456790.00675");
    }

    /**
     * PIC 9(15)V9(2).
     */
    public void testToHostM123456789012345p12() {
        toHost(9, 17, 2, true, "-123456789012345.12", "12345678901234512d");
    }

    /**
     * PIC 9(15)V9(2).
     */
    public void testFromHostM123456789012345p12() {
        fromHost(9, 17, 2, "12345678901234512d", "-123456789012345.12");
    }

    /**
     * PIC 9(15)V9(2).
     */
    public void testToHostZero() {
        toHost(9, 17, 2, true, "0", "00000000000000000c");
    }

    /**
     * Case where input is null.
     */
    public void testToHostNull() {
        try {
            byte[] hostBytes = new byte[9];
            BigDecimal javaDecimal = null;
            assertEquals(9, CobolPackedDecimalSimpleConverter.toHostSingle(
                    javaDecimal, 9, 17, 2, true, hostBytes, 0));
            assertEquals("00000000000000000c", HostData.toHexString(hostBytes));
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Case where input is zero.
     */
    public void testFromHostZero() {
        fromHost(9, 17, 2, "00000000000000000c", "0.00");
    }

    /**
     * Case where input is minus zero.
     */
    public void testToHostMZero() {
        toHost(9, 17, 2, true, "-0", "00000000000000000c");
    }

    /**
     * PIC 9(6)V9(5).
     */
    public void testToHostM67000p56007() {
        toHost(6, 10, 5, true, "-67000.56007", "06700056007d");
    }

    /**
     * PIC 9(6)V9(5).
     */
    public void testFromHostM67000p56007() {
        fromHost(6, 10, 5, "06700056007d", "-67000.56007");
    }

    /**
     * PIC 9(8)V9(2).
     */
    public void testToHostM67000p56007Trunc() {
        toHost(6, 10, 2, true, "-67000.56007", "00006700056d");
    }

    /**
     * PIC 9(8)V9(2).
     */
    public void testFromHostM67000p56007Trunc() {
        fromHost(6, 10, 2, "06700056007d", "-67000560.07");
    }

    /**
     * PIC 9(1).
     */
    public void testToHostSingleDigit() {
        toHost(1, 1, 0, false, "1", "1f");
    }

    /**
     * PIC 9(1).
     */
    public void testFromHostSingleDigit() {
        fromHost(1, 1, 0, "1f", "1");
    }

    /**
     * PIC 9(1)V9(5).
     */
    public void testToHostp12345() {
        toHost(4, 6, 5, false, ".12345", "0012345f");
    }

    /**
     * PIC 9(1)V9(5).
     */
    public void testFromHostp12345() {
        fromHost(4, 6, 5, "0012345f", "0.12345");
    }

    /**
     * Changing scale.
     */
    public void testToHostScaling() {
        toHost(6, 10, 3, false, "1234567.89", "01234567890f");
    }

    /**
     * Changing scale.
     */
    public void testFromHostScaling() {
        fromHost(6, 10, 3, "01234567890f", "1234567.890");
    }

    /**
     * Padding.
     */
    public void testToHostPadding() {
        toHost(3, 5, 0, false, "12", "00012f");
    }

    /**
     * Padding.
     */
    public void testFromHostPadding() {
        fromHost(3, 5, 0, "00012f", "12");
    }

    /**
     * Large value.
     */
    public void testToHostLarge() {
        toHost(16, 31, 0, false, "1234567890123456789012345678901",
                "1234567890123456789012345678901f");
    }

    /**
     * Large value.
     */
    public void testFromHostLarge() {
        fromHost(16, 31, 0, "1234567890123456789012345678901f",
                "1234567890123456789012345678901");
    }

    /**
     * Invalid host data.
     */
    public void testFromHostInvalid() {
        try {
            byte[] hostBytes = HostData.toByteArray("1A");
            CobolPackedDecimalSimpleConverter.fromHostSingle(1, 1, 0,
                    hostBytes, 0);
            fail("Invalid packed decimal not detected");
        } catch (HostException he) {
            assertEquals(
                    "Host data last byte is not a valid packed decimal byte. Host data at offset 0=0x1a",
                    he.getMessage());
        }
    }

    /**
     * Invalid host data.
     */
    public void testFromHostInvalid2() {
        try {
            byte[] hostBytes = HostData.toByteArray("0A1f");
            CobolPackedDecimalSimpleConverter.fromHostSingle(2, 2, 0,
                    hostBytes, 0);
            fail("Invalid packed decimal not detected");
        } catch (HostException he) {
            assertEquals(
                    "Host data contains a byte that is not a valid packed decimal byte."
                            + " Host data at offset 0=0x0a1f",
                    he.getMessage());
        }
    }

    /**
     * Invalid java data.
     */
    public void testToHostInvalid() {
        try {
            byte[] hostBytes = new byte[1];
            BigDecimal javaDecimal = new BigDecimal("235.87");
            CobolPackedDecimalSimpleConverter.toHostSingle(javaDecimal, 1, 1,
                    0, false, hostBytes, 0);
            fail("BigDecimal too large not detected");
        } catch (HostException he) {
            assertEquals(
                    "BigDecimal value too large for target Cobol field. Host data at offset 0=0x00",
                    he.getMessage());
        }
    }

    /**
     * Case where there is not enough data left in the host buffer. The code
     * should consider
     * that trailing nulls were omitted by the host.
     */
    public void testToHostPartialData() {
        fromHost(8, 8, 2, "45679000675f", "0.00");
    }

    /**
     * Same as above but this time we are already past the offset.
     */
    public void testToHostPartialDataPastOffset() {
        fromHost(8, 8, 2, "45679000675f", "0.00");
    }

}
