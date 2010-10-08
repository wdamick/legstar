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
import com.legstar.coxb.host.HostException;

import java.math.BigDecimal;
import junit.framework.TestCase;

/**
 * Test the COBOL BINARY TYPE.
 *
 */
public class BinaryTest extends TestCase {

    /**
     * Generic conversion from java BigDecimal to COBOL.
     * @param byteLength the COBOL receiving field size 
     * @param signed true if signed
     * @param inputValue the input value as a string
     * @param expectedValue the expected value as a hex string
     */
    public static void toHost(
            final int byteLength,
            final boolean signed,
            final String inputValue,
            final String expectedValue) {
        try {
            byte[] hostBytes = new byte[byteLength];
            BigDecimal javaDecimal = new BigDecimal(inputValue);
            assertEquals(byteLength, CobolBinarySimpleConverter.toHostSingle(
                    javaDecimal, byteLength, signed, hostBytes, 0));
            assertEquals(expectedValue, HostData.toHexString(hostBytes));
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Generic conversion from COBOL to java BigDecimal.
     * @param byteLength the COBOL receiving field size 
     * @param signed true if signed
     * @param totalDigits total number of digits
     * @param fractionDigits number of fraction digits
     * @param inputValue the input value as a hex string
     * @param expectedValue the expected value as a string
     */
    public static void fromHost(
            final int byteLength,
            final boolean signed,
            final int totalDigits,
            final int fractionDigits,
            final String inputValue,
            final String expectedValue) {
        try {
            byte[] hostBytes = HostData.toByteArray(inputValue);
            BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(
                    byteLength, signed, totalDigits, fractionDigits, hostBytes, 0);
            assertEquals(expectedValue, javaDecimal.toString());
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }

    /**
     * 9(6)V99 COMP.
     */
    public void testToHost1234p00() {
        toHost(4, false, "1234.00", "0001e208");
    }

    /**
     * 9(6)V99 COMP.
     */
    public void testFromHost1234p00() {
        fromHost(4, false, 8, 2, "0001e208", "1234.00");
    }

    /**
     * S9(6)V99 COMP.
     */
    public void testToHostM1234p00() {
        toHost(4, true, "-1234.00", "fffe1df8");
    }

    /**
     * S9(6)V99 COMP.
     */
    public void testFromHostM1234p00() {
        fromHost(4, true, 8, 2, "fffe1df8", "-1234.00");
    }

    /**
     * Case where the receiving buffer is too small.
     */
    public void testToHostWriteOverflow() {
        try {
            byte[] hostBytes = new byte[2];
            BigDecimal javaDecimal = new BigDecimal("123456789012");
            CobolBinarySimpleConverter.toHostSingle(javaDecimal, 4, false, hostBytes, 0);
            fail("overflow not detected");
        } catch (HostException he) {
            assertEquals("Attempt to write past end of host source buffer. Host data at offset 0=0x0000",
                    he.getMessage());
        }
    }

    /**
     * Case where there is not enough data left in the host buffer. The code should consider
     * that trailing nulls were omitted by the host.
     */
    public void testToHostPartialData() {
        fromHost(8, true, 8, 2, "fffe1df8", "-5299989643264.00");
    }

    /**
     * Same as above but this time we are already past the offset.
     */
    public void testToHostPartialDataPastOffset() {
        try {
            // Create a host buffer with 4 bytes when 8 are necessary
            byte[] hostBytes = HostData.toByteArray("fffe1df8");

            BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(
                    8, true, 8, 2, hostBytes, 5);
            assertEquals("0.00",  javaDecimal.toString());
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Case where the java bigdecimal holds a value that is too big for the COBOL field.
     */
    public void testToHostJavaTooLarge() {
        try {
            byte[] hostBytes = new byte[4];

            BigDecimal javaDecimal = new BigDecimal("4294967296");
            CobolBinarySimpleConverter.toHostSingle(javaDecimal, 4, false, hostBytes, 0);
            fail("overflow not detected");
        } catch (HostException he) {
            assertEquals("Java binary too large for Cobol element. Host data at offset 0=0x00000000",
                    he.getMessage());
        }
    }

    /**
     * Largest COBOL value.
     */
    public void testToHostJavaMax() {
        toHost(4, false, "4294967295", "ffffffff");
    }

    /**
     * Largest COBOL value minus 1.
     */
    public void testToHostJavaMaxM1() {
        toHost(4, false, "4294967294", "fffffffe");
    }

    /**
     * Test padding with 0.
     */
    public void testToHostJavaFill() {
        toHost(4, false, "7", "00000007");
    }

    /**
     * Zero value.
     */
    public void testToHostZero() {
        toHost(2, false, "0", "0000");
    }

    /**
     * Zero value.
     */
    public void testFromZero() {
        fromHost(2, false, 4, 0, "0000", "0");
    }

    /**
     * 9(4) COMP small value.
     */
    public void testToHost74() {
        toHost(2, false, "74", "004a");
    }

    /**
     * 9(4) COMP small value.
     */
    public void testFrom74() {
        fromHost(2, false, 4, 0, "004a", "74");
    }

    /**
     * 9(4) COMP medium value.
     */
    public void testToHost127() {
        toHost(2, false, "127", "007f");
    }

    /**
     * 9(4) COMP medium value.
     */
    public void testFrom127() {
        fromHost(2, false, 4, 0, "007f", "127");
    }

    /**
     * 9(4) COMP large value.
     */
    public void testToHost32769() {
        toHost(2, false, "32769", "8001");
    }

    /**
     * 9(4) COMP large value.
     */
    public void testFrom32769() {
        fromHost(2, false, 4, 0, "8001", "32769");
    }

    /**
     * 9(4) COMP-5 large value.
     */
    public void testToHost65535() {
        toHost(2, false, "65535", "ffff");
    }

    /**
     * 9(4) COMP-5 large value.
     */
    public void testFrom65535() {
        fromHost(2, false, 4, 0, "ffff", "65535");
    }

    /**
     * S9(4) COMP large value.
     */
    public void testToHostM32768() {
        toHost(2, true, "-32768", "8000"); 
    }

    /**
     * S9(4) COMP large value.
     */
    public void testFromM32768() {
        fromHost(2, true, 4, 0, "8000", "-32768");
    }

    /**
     * S9(4) COMP medium value.
     */
    public void testToHostM128() {
        toHost(2, true, "-128", "ff80");
    }

    /**
     * S9(4) COMP medium value.
     */
    public void testFromM128() {
        fromHost(2, true, 4, 0, "ff80", "-128");
    }

    /**
     * S9(4) COMP small value.
     */
    public void testToHostM75() {
        toHost(2, true, "-75", "ffb5");
    }

    /**
     * S9(4) COMP small value.
     */
    public void testFromM75() {
        fromHost(2, true, 4, 0, "ffb5", "-75");
    }

    /**
     * 9(4) COMP medium value.
     */
    public void testToHost1045() {
        toHost(2, false, "1045", "0415");
    }

    /**
     * 9(4) COMP medium value.
     */
    public void testFrom1045() {
        fromHost(2, false, 4, 0, "0415", "1045");
    }

    /**
     * 9(4) COMP largest value minus 1.
     */
    public void testToHost32767() {
        toHost(2, true, "32767", "7fff");
    }

    /**
     * 9(4) COMP largest value minus 1.
     */
    public void testFrom32767() {
        fromHost(2, true, 4, 0, "7fff", "32767");
    }

    /**
     * 9(8) COMP zero value.
     */
    public void testToHost4BytesZero() {
        toHost(4, false, "0", "00000000");
    }

    /**
     * 9(8) COMP zero value.
     */
    public void testFrom4BytesZero() {
        fromHost(4, false, 8, 0, "00000000", "0");
    }

    /**
     * 9(8) COMP small value.
     */
    public void testToHost4Bytes74() {
        toHost(4, false, "74", "0000004a");
    }

    /**
     * 9(8) COMP small value.
     */
    public void testFrom4Bytes74() {
        fromHost(4, false, 8, 0, "0000004a", "74");
    }

    /**
     * 9(8) COMP medium value.
     */
    public void testToHost4Bytes65534() {
        toHost(4, false, "65534", "0000fffe");
    }

    /**
     * 9(8) COMP medium value.
     */
    public void testFrom4Bytes65534() {
        fromHost(4, false, 8, 0, "0000fffe", "65534");
    }

    /**
     * 9(8) COMP large value.
     */
    public void testToHost4Bytes2147483649() {
        toHost(4, false, "2147483649", "80000001");
    }

    /**
     * 9(8) COMP large value.
     */
    public void testFrom4Bytes2147483649() {
        fromHost(4, false, 8, 0, "80000001", "2147483649");
    }

    /**
     * 9(8) COMP largest value.
     */
    public void testToHost4Bytes4294967295() {
        toHost(4, false, "4294967295", "ffffffff");
    }

    /**
     * 9(8) COMP largest value.
     */
    public void testFrom4Bytes4294967295() {
        fromHost(4, false, 8, 0, "ffffffff", "4294967295");
    }

    /**
     * S9(8) COMP largest negative value.
     */
    public void testToHost4BytesM2147483648() {
        toHost(4, true, "-2147483648", "80000000");
    }

    /**
     * S9(8) COMP largest negative value.
     */
    public void testFrom4BytesM2147483648() {
        fromHost(4, true, 8, 0, "80000000", "-2147483648");
    }

    /**
     * S9(8) COMP small negative value.
     */
    public void testToHost4BytesM75() {
        toHost(4, true, "-75", "ffffffb5");
    }

    /**
     * S9(8) COMP small negative value.
     */
    public void testFrom4BytesM75() {
        fromHost(4, true, 8, 0, "ffffffb5", "-75");
    }

    /**
     * S9(8) COMP small negative value.
     */
    public void testToHost4BytesM128() {
        toHost(4, true, "-128", "ffffff80");
    }

    /**
     * S9(8) COMP small negative value.
     */
    public void testFrom4BytesM128() {
        fromHost(4, true, 8, 0, "ffffff80", "-128");
    }

    /**
     * S9(8) COMP average positive value.
     */
    public void testToHost4Bytes123456789() {
        toHost(4, true, "123456789", "075bcd15");
    }

    /**
     * S9(8) COMP average positive value.
     */
    public void testFrom4Bytes123456789() {
        fromHost(4, true, 8, 0, "075bcd15", "123456789");
    }

    /**
     * S9(8) COMP large positive value.
     */
    public void testToHost4Bytes2147483647() {
        toHost(4, true, "2147483647", "7fffffff");
    }

    /**
     * S9(8) COMP large positive value.
     */
    public void testFrom4Bytes2147483647() {
        fromHost(4, true, 8, 0, "7fffffff", "2147483647");
    }

    /**
     * S9(8) COMP zero value.
     */
    public void testToHost8BytesZero() {
        toHost(8, false, "0", "0000000000000000");
    }

    /**
     * S9(8) COMP zero value.
     */
    public void testFrom8BytesZero() {
        fromHost(8, false, 18, 0, "0000000000000000", "0");
    }

    /**
     * 9(18) COMP small value.
     */
    public void testToHost8Bytes74() {
        toHost(8, false, "74", "000000000000004a");
    }

    /**
     * 9(18) COMP small value.
     */
    public void testFrom8Bytes74() {
        fromHost(8, false, 18, 0, "000000000000004a", "74");
    }

    /**
     * 9(18) COMP average value.
     */
    public void testToHost8Bytes4294967294() {
        toHost(8, false, "4294967294", "00000000fffffffe");
    }

    /**
     * 9(18) COMP average value.
     */
    public void testFrom8Bytes4294967294() {
        fromHost(8, false, 18, 0, "00000000fffffffe", "4294967294");
    }

    /**
     * 9(18) COMP large value.
     */
    public void testToHost8Bytes18446744069414584318() {
        toHost(8, false, "18446744069414584318", "fffffffefffffffe");
    }

    /**
     * 9(18) COMP large value.
     */
    public void testFrom8Bytes18446744069414584318() {
        fromHost(8, false, 18, 0, "fffffffefffffffe", "18446744069414584318");
    }

    /**
     * 9(18) COMP largest value.
     */
    public void testToHost8Bytes18446744073709551615() {
        toHost(8, false, "18446744073709551615", "ffffffffffffffff");
    }

    /**
     * 9(18) COMP largest value.
     */
    public void testFrom8Bytes18446744073709551615() {
        fromHost(8, false, 18, 0, "ffffffffffffffff", "18446744073709551615");
    }

    /**
     * S9(18) COMP largest negative value.
     */
    public void testToHost8BytesM9223372036854775808() {
        toHost(8, true, "-9223372036854775808", "8000000000000000");
    }

    /**
     * S9(18) COMP largest negative value.
     */
    public void testFrom8BytesM9223372036854775808() {
        fromHost(8, true, 18, 0, "8000000000000000", "-9223372036854775808");
    }

    /**
     * S9(18) COMP small negative value.
     */
    public void testToHost8BytesM75() {
        toHost(8, true, "-75", "ffffffffffffffb5");
    }

    /**
     * S9(18) COMP small negative value.
     */
    public void testFrom8BytesM75() {
        fromHost(8, true, 18, 0, "ffffffffffffffb5", "-75");
    }

    /**
     * S9(18) COMP average negative value.
     */
    public void testToHost8BytesM4294967294() {
        toHost(8, true, "-4294967294", "ffffffff00000002");
    }

    /**
     * S9(18) COMP average negative value.
     */
    public void testFrom8BytesM4294967294() {
        fromHost(8, true, 18, 0, "ffffffff00000002", "-4294967294");
    }

    /**
     * S9(18) COMP average positive value.
     */
    public void testToHost8Bytes17179869183() {
        toHost(8, true, "17179869183", "00000003ffffffff");
    }

    /**
     * S9(18) COMP average positive value.
     */
    public void testFrom8Bytes17179869183() {
        fromHost(8, true, 18, 0, "00000003ffffffff", "17179869183");
    }

    /**
     * S9(18) COMP largest positive value.
     */
    public void testToHost8Bytes9223372036854775807() {
        toHost(8, true, "9223372036854775807", "7fffffffffffffff");
    }

    /**
     * S9(18) COMP largest positive value.
     */
    public void testFrom8Bytes9223372036854775807() {
        fromHost(8, true, 18, 0, "7fffffffffffffff", "9223372036854775807");
    }

}
