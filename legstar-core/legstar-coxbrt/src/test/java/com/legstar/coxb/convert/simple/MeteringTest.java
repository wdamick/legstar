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

import java.math.BigDecimal;

import junit.framework.TestCase;

import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.coxb.host.HostContext;
import com.legstar.coxb.host.HostData;

/**
 * This class is designed to work with Jakarta JMeter 2.1.2 and above.
 * 
 */
public class MeteringTest extends TestCase {

    /** A character set to use for string conversions. */
    private static final String STRING_FRENCH_CHARSET = "IBM01147";

    /** The EBCDIC encoding if a character string. */
    private static final byte[] STRING_HOST_BYTES = HostData
            .toByteArray("e08140837d85a2a340a495409799968293d0948540c093c0948595a381899985407c4099c0a296a4849985");

    /** The java string. */
    private static final String STRING_JAVA = "ça c'est un problème élémentaire à résoudre";

    /** The packed decimal encoding of a decimal. */
    private static final byte[] PACKED_DECIMAL_HOST_BYTES = HostData
            .toByteArray("12345678901234512d");

    /** The zoned decimal encoding of a decimal. */
    private static final byte[] ZONED_DECIMAL_HOST_BYTES = HostData
            .toByteArray("f1f2f3f4f5f6f7f8f9f0f1f2f3f4f5f1d2");

    /** The BigDecimal equivalent to the COBOL decimal. */
    private static final BigDecimal DECIMAL_JAVA = new BigDecimal(
            "-123456789012345.12");

    /** The BINARY encoding of an integer. */
    private static final byte[] BINARY_HOST_BYTES = HostData
            .toByteArray("80000000");

    /** The BigDecimal equivalent to the COBOL integer. */
    private static final BigDecimal INTEGER_JAVA = new BigDecimal("-2147483648");

    /**
     * Convert from COBOL PIC X to Java String.
     */
    public void testAlphanumericToString() {
        try {
            String javaString = CobolStringSimpleConverter.fromHostSingle(
                    STRING_FRENCH_CHARSET, STRING_HOST_BYTES.length,
                    STRING_HOST_BYTES, 0);
            assertEquals(STRING_JAVA, javaString);
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Convert from COBOL Packed decimal to BigDecimal.
     */
    public void testPackedDecimalToBigDecimal() {
        try {
            BigDecimal javaDecimal = CobolPackedDecimalSimpleConverter
                    .fromHostSingle(9, 17, 2, PACKED_DECIMAL_HOST_BYTES, 0);
            assertEquals(0, javaDecimal.compareTo(DECIMAL_JAVA));
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Convert from COBOL Zoned decimal to BigDecimal.
     */
    public void testZonedDecimalToBigDecimal() {
        try {
            BigDecimal javaDecimal = CobolZonedDecimalSimpleConverter
                    .fromHostSingle(17, 17, 2, true, false, false,
                            ZONED_DECIMAL_HOST_BYTES, 0, HostContext
                                    .getHostIntegerSigns(STRING_FRENCH_CHARSET));
            assertEquals(0, javaDecimal.compareTo(DECIMAL_JAVA));
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Convert from COBOL BINARY to BigDecimal.
     */
    public void testBinaryToBigDecimal() {
        try {
            BigDecimal javaDecimal = CobolBinarySimpleConverter.fromHostSingle(
                    4, true, 8, 0, BINARY_HOST_BYTES, 0);
            assertEquals(0, javaDecimal.compareTo(INTEGER_JAVA));
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Convert from Java String to COBOL PIC X.
     */
    public void testStringToAlphanumeric() {
        try {
            byte[] hostBytes = new byte[STRING_HOST_BYTES.length];
            CobolStringSimpleConverter.toHostSingle(STRING_JAVA,
                    STRING_FRENCH_CHARSET, null, false,
                    STRING_HOST_BYTES.length, false, hostBytes, 0);
            assertEquals(STRING_HOST_BYTES[0], hostBytes[0]);
            assertEquals(STRING_HOST_BYTES[STRING_HOST_BYTES.length - 1],
                    hostBytes[STRING_HOST_BYTES.length - 1]);
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Convert from BigDecimal to COBOL Packed decimal.
     */
    public void testBigDecimalToPackedDecimal() {
        try {
            byte[] hostBytes = new byte[PACKED_DECIMAL_HOST_BYTES.length];
            CobolPackedDecimalSimpleConverter
                    .toHostSingle(DECIMAL_JAVA,
                            PACKED_DECIMAL_HOST_BYTES.length, 17, 2, true,
                            hostBytes, 0);
            assertEquals(PACKED_DECIMAL_HOST_BYTES[0], hostBytes[0]);
            assertEquals(
                    PACKED_DECIMAL_HOST_BYTES[PACKED_DECIMAL_HOST_BYTES.length - 1],
                    hostBytes[PACKED_DECIMAL_HOST_BYTES.length - 1]);
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Convert from BigDecimal to COBOL Zoned Decimal.
     */
    public void testBigDecimalToZonedDecimal() {
        try {
            byte[] hostBytes = new byte[ZONED_DECIMAL_HOST_BYTES.length];
            CobolZonedDecimalSimpleConverter.toHostSingle(DECIMAL_JAVA,
                    ZONED_DECIMAL_HOST_BYTES.length, 17, 2, true, false, false,
                    hostBytes, 0,
                    HostContext.getHostIntegerSigns(STRING_FRENCH_CHARSET));
            assertEquals(ZONED_DECIMAL_HOST_BYTES[0], hostBytes[0]);
            assertEquals(
                    ZONED_DECIMAL_HOST_BYTES[ZONED_DECIMAL_HOST_BYTES.length - 1],
                    hostBytes[ZONED_DECIMAL_HOST_BYTES.length - 1]);
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Convert from BigDecimal to COBOL Binary.
     */
    public void testBigDecimalToBinary() {
        try {
            byte[] hostBytes = new byte[BINARY_HOST_BYTES.length];
            CobolBinarySimpleConverter.toHostSingle(INTEGER_JAVA,
                    BINARY_HOST_BYTES.length, true, hostBytes, 0);
            assertEquals(BINARY_HOST_BYTES[0], hostBytes[0]);
            assertEquals(BINARY_HOST_BYTES[BINARY_HOST_BYTES.length - 1],
                    hostBytes[BINARY_HOST_BYTES.length - 1]);
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }
}
