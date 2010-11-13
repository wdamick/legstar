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

import java.io.UnsupportedEncodingException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolArrayZonedDecimalBinding;
import com.legstar.coxb.ICobolZonedDecimalBinding;
import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.coxb.convert.ICobolZonedDecimalConverter;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

/**
 * This is a concrete implementation of marshal/unmarshal operations of java
 * numerics to cobol zoned decimals.
 * <p/>
 * Zoned decimals, also known as external decimals, are defined with PIC 9(n) or
 * PIC S9(n) with the implicit or explicit DISPLAY usage. They contain character
 * representation of digits and signs apart from a special case where the
 * numeric is defined as PIC S9(n) without the SIGN IS SEPARATE clause. In this
 * case, the sign info shares a byte with one of the digits (either the first
 * one if SIGN IS LEADING or the last one if SIGN is TRAILING).
 * <p/>
 * This version does not use code pages anymore when converting from host to
 * java. Using the Charset conversion is very CPU intensive. Rather we use a
 * limited set of digits, either EBCDIC or ASCII.
 * 
 */
public class CobolZonedDecimalSimpleConverter extends CobolSimpleConverter
        implements ICobolZonedDecimalConverter {

    /**
     * Digits are all present with a 0 default value.
     * Max number of digits is 31.
     */
    private static final String DIGIT_PATTERN =
            "0000000000000000000000000000000";

    /** Ebcdic code point for plus sign. */
    private static final byte PLUS_EBCDIC = 0x4E;

    /** Ebcdic code point for minus sign. */
    private static final byte MINUS_EBCDIC = 0x60;

    /**
     * Ordered list of characters that might appear in a zoned decimal (ignoring
     * overpunch).
     */
    private static final byte[] ZONED_DECIMAL_CHARS = new byte[] {
            (byte) 0xF0, (byte) 0xF1, (byte) 0xF2, (byte) 0xF3, (byte) 0xF4,
            (byte) 0xF5, (byte) 0xF6, (byte) 0xF7, (byte) 0xF8, (byte) 0xF9,
            0x40, 0x4E, 0x60 };

    /** Java characters corresponding to the previous array. */
    private static final char[] JAVA_CHARS = new char[] { '0',
            '1', '2', '3', '4', '5', '6', '7', '8', '9',
            '0', '+', '-' };

    /**
     * @param cobolContext the Cobol compiler parameters in effect
     */
    public CobolZonedDecimalSimpleConverter(final CobolContext cobolContext) {
        super(cobolContext);
    }

    /** {@inheritDoc} */
    public int toHost(
            final ICobolZonedDecimalBinding ce,
            final byte[] hostTarget,
            final int offset) throws HostException {
        int newOffset = 0;
        try {
            newOffset = toHostSingle(ce.getBigDecimalValue(),
                    ce.getByteLength(),
                    ce.getTotalDigits(),
                    ce.getFractionDigits(),
                    ce.isSigned(),
                    ce.isSignSeparate(),
                    ce.isSignLeading(),
                    hostTarget,
                    offset,
                    getCobolContext().getHostCharsetName());
        } catch (CobolConversionException e) {
            throwHostException(ce, e);
        }
        return newOffset;
    }

    /** {@inheritDoc} */
    public int toHost(
            final ICobolArrayZonedDecimalBinding ce,
            final byte[] hostTarget,
            final int offset,
            final int currentOccurs)
            throws HostException {
        int newOffset = offset;
        try {
            for (BigDecimal javaSource : ce.getBigDecimalList()) {
                newOffset = toHostSingle(javaSource,
                        ce.getItemByteLength(),
                        ce.getTotalDigits(),
                        ce.getFractionDigits(),
                        ce.isSigned(),
                        ce.isSignSeparate(),
                        ce.isSignLeading(),
                        hostTarget,
                        newOffset,
                        getCobolContext().getHostCharsetName());
            }
            /* If necessary, fill in the array with missing items */
            for (int i = ce.getBigDecimalList().size(); i < currentOccurs; i++) {
                newOffset = toHostSingle(BigDecimal.ZERO,
                        ce.getItemByteLength(),
                        ce.getTotalDigits(),
                        ce.getFractionDigits(),
                        ce.isSignSeparate(),
                        ce.isSignLeading(),
                        ce.isSigned(),
                        hostTarget,
                        newOffset,
                        getCobolContext().getHostCharsetName());
            }
        } catch (CobolConversionException e) {
            throwHostException(ce, e);
        }
        return newOffset;
    }

    /** {@inheritDoc} */
    public int fromHost(
            final ICobolZonedDecimalBinding ce,
            final byte[] hostSource,
            final int offset)
            throws HostException {
        int newOffset = offset;
        try {
            BigDecimal javaDecimal = fromHostSingle(ce.getByteLength(),
                    ce.getTotalDigits(),
                    ce.getFractionDigits(),
                    ce.isSigned(),
                    ce.isSignSeparate(),
                    ce.isSignLeading(),
                    hostSource,
                    newOffset,
                    getCobolContext().getHostCharsetName());
            ce.setBigDecimalValue(javaDecimal);
            newOffset += ce.getByteLength();
        } catch (CobolConversionException e) {
            throwHostException(ce, e);
        }
        return newOffset;
    }

    /** {@inheritDoc} */
    public int fromHost(
            final ICobolArrayZonedDecimalBinding ce,
            final byte[] hostSource,
            final int offset,
            final int currentOccurs)
            throws HostException {
        List < BigDecimal > lArray = new ArrayList < BigDecimal >();
        int newOffset = offset;
        try {
            for (int i = 0; i < currentOccurs; i++) {
                BigDecimal javaDecimal = fromHostSingle(ce.getItemByteLength(),
                        ce.getTotalDigits(),
                        ce.getFractionDigits(),
                        ce.isSigned(),
                        ce.isSignSeparate(),
                        ce.isSignLeading(),
                        hostSource,
                        newOffset,
                        getCobolContext().getHostCharsetName());
                lArray.add(javaDecimal);
                newOffset += ce.getItemByteLength();
            }
            ce.setBigDecimalList(lArray);
        } catch (CobolConversionException e) {
            throwHostException(ce, e);
        }
        return newOffset;
    }

    /**
     * Converts a Java BigDecimal to a host zoned decimal.
     * 
     * @param javaDecimal java decimal to convert
     * @param cobolByteLength host byte length
     * @param totalDigits Cobol element total number of digits
     * @param fractionDigits Cobol element number of fractional digits
     * @param isSigned Cobol element is signed or unsigned
     * @param isSignSeparate Cobol element sign occupies own byte
     * @param isSignLeading Cobol element sign in first byte
     * @param hostTarget target host buffer
     * @param offset offset in target host buffer
     * @return offset after host buffer is updated
     * @param hostCharsetName host character set
     * @throws CobolConversionException if conversion fails
     */
    public static final int toHostSingle(
            final BigDecimal javaDecimal,
            final int cobolByteLength,
            final int totalDigits,
            final int fractionDigits,
            final boolean isSigned,
            final boolean isSignSeparate,
            final boolean isSignLeading,
            final byte[] hostTarget,
            final int offset,
            final String hostCharsetName)
            throws CobolConversionException {

        /* Check that we are still within the host target range */
        int lastOffset = offset + cobolByteLength;
        if (lastOffset > hostTarget.length) {
            throw (new CobolConversionException(
                    "Attempt to write past end of host source buffer",
                    new HostData(hostTarget), offset, cobolByteLength));
        }
        int iTarget = offset; /* points to current byte in host data */

        /* Get a string representation of target numeric */
        String sDecimal = formatString(javaDecimal, totalDigits,
                fractionDigits,
                isSigned, isSignSeparate, isSignLeading);

        /* Get the bytes representation in the target host character set */
        byte[] bDecimal;
        try {
            bDecimal = sDecimal.getBytes(hostCharsetName);
        } catch (UnsupportedEncodingException e) {
            throw new CobolConversionException("UnsupportedEncodingException "
                    + e.getMessage(),
                    new HostData(hostTarget), offset, cobolByteLength);
        }

        /* Check for overflows and formatting errors */
        int targetSize = totalDigits;
        if (isSigned && isSignSeparate) {
            targetSize++;
        }
        if (bDecimal.length > targetSize) {
            throw (new CobolConversionException(
                    "BigDecimal value too large for target Cobol field",
                    new HostData(hostTarget), offset, cobolByteLength));
        }
        if (bDecimal.length < targetSize) {
            throw (new CobolConversionException(
                    "Formatted BigDecimal too small for target Cobol field",
                    new HostData(hostTarget), offset, cobolByteLength));
        }

        /* Populate the target host buffer */
        for (int i = 0; i < bDecimal.length; i++) {
            hostTarget[iTarget] = bDecimal[i];
            iTarget++;
        }

        /*
         * Imbed the sign when it is not separate. Observe that this
         * might create invalid digits for host systems that do not
         * understand zoned decimals.
         */
        if (isSigned) {
            if (!isSignSeparate) {
                if (isSignLeading) {
                    if (javaDecimal.signum() == -1) {
                        hostTarget[offset] = (byte) (hostTarget[offset] - 0x20);
                    } else {
                        hostTarget[offset] = (byte) (hostTarget[offset] - 0x30);
                    }
                } else {
                    if (javaDecimal.signum() == -1) {
                        hostTarget[iTarget - 1] =
                                (byte) (hostTarget[iTarget - 1] - 0x20);
                    } else {
                        hostTarget[iTarget - 1] =
                                (byte) (hostTarget[iTarget - 1] - 0x30);
                    }
                }
            }
        }

        return iTarget;
    }

    /**
     * Applies a pattern derived from the COBOL field attributes to format
     * a String comprising all digits and signs when they are separate.
     * <p/>
     * If the numeric is signed but sign is not separate (ie is imbedded),
     * removes all signs from the resulting string.
     * <p/>
     * If the java decimal has more precision than the target COBOL field,
     * additional digits are lost (no rounding is done).
     * 
     * @param javaDecimal java decimal to convert
     * @param totalDigits Cobol element total number of digits
     * @param fractionDigits Cobol element number of fractional digits
     * @param isSigned Cobol element is signed or unsigned
     * @param isSignSeparate Cobol element sign occupies own byte
     * @param isSignLeading Cobol element sign in first byte
     * @return a formatted string unchecked for overflow conditions
     */
    public static String formatString(
            final BigDecimal javaDecimal,
            final int totalDigits,
            final int fractionDigits,
            final boolean isSigned,
            final boolean isSignSeparate,
            final boolean isSignLeading) {

        /*
         * Get the unscaled value of the bigdecimal to convert. The decimal
         * point
         * is virtual in zoned decimals.
         */
        BigInteger workInteger;
        if (javaDecimal == null) {
            workInteger = BigInteger.ZERO;
        } else {
            workInteger = javaDecimal.movePointRight(fractionDigits)
                    .toBigInteger();
        }

        String absString = workInteger.abs().toString();

        /* Pad with leading zeros */
        if (absString.length() < totalDigits) {
            absString = DIGIT_PATTERN.substring(0, totalDigits
                    - absString.length())
                    + absString;
        }

        /* Add sign character when explicitly separate */
        if (isSigned) {
            if (isSignSeparate) {
                if (isSignLeading) {
                    if (workInteger.signum() == -1) {
                        return '-' + absString;
                    } else {
                        return '+' + absString;
                    }
                } else {
                    if (workInteger.signum() == -1) {
                        return absString + '-';
                    } else {
                        return absString + '+';
                    }
                }
            } else {
                /* Leave it up to the caller to imbed the sign */
                return absString;
            }
        } else {
            return absString;
        }

    }

    /**
     * Converts a host packed decimal to a Java BigDecimal.
     * 
     * @param cobolByteLength host byte length
     * @param totalDigits Cobol element total number of digits
     * @param fractionDigits Cobol element number of fractional digits
     * @param isSigned Cobol element is signed or unsigned
     * @param isSignSeparate Cobol element sign occupies own byte
     * @param isSignLeading Cobol element sign in first byte
     * @param hostSource source host buffer
     * @param offset offset in source host buffer
     * @param hostCharsetName host character set (not used)
     * @return offset after host buffer is read
     * @throws CobolConversionException if conversion fails
     */
    public static final BigDecimal fromHostSingle(
            final int cobolByteLength,
            final int totalDigits,
            final int fractionDigits,
            final boolean isSigned,
            final boolean isSignSeparate,
            final boolean isSignLeading,
            final byte[] hostSource,
            final int offset,
            final String hostCharsetName)
            throws CobolConversionException {

        int lastOffset = offset + cobolByteLength;

        /*
         * Check that we are still within the host source range.
         * If not, consider the host optimized its payload by truncating
         * trailing nulls in which case, we just need to initialize and return.
         */
        if (lastOffset > hostSource.length) {
            return new BigDecimal(0).setScale(fractionDigits);
        }
        if (lastOffset < 1) {
            throw (new CobolConversionException(
                    "Invalid host byte length",
                    new HostData(hostSource), offset, cobolByteLength));
        }

        int sourceSize = totalDigits;
        if (isSigned) {
            sourceSize++;
        }
        char[] workDecimal = new char[sourceSize];

        /*
         * Transfer source bytes to work byte array. The objective is that
         * the work byte array contains only valid digits ready for code
         * page translation.
         * The first and last source bytes might be sign bytes. These happen
         * for signed decimals with imbedded sign (not separate). Such a byte
         * encodes both the sign (high order 4 bits) and a digit (low order
         * 4 bits) so it results in 2 bytes in the work byte array.
         */
        int i = 0;
        byte hostByte;
        for (int iSource = offset; iSource < lastOffset; iSource++) {
            hostByte = hostSource[iSource];
            if (isSigned) {
                if (i == 0) {
                    if (isSignLeading) {
                        if (isSignSeparate) {
                            workDecimal[i] = toJavaChar(hostByte);
                        } else {
                            /*
                             * Sign code gives the first target byte and the
                             * embedded digit gives the next byte.
                             */
                            int signCode = hostByte & 0xF0;
                            if (signCode == 0xd0) {
                                workDecimal[i] = toJavaChar(MINUS_EBCDIC);
                            } else {
                                workDecimal[i] = toJavaChar(PLUS_EBCDIC);
                            }
                            i++;
                            workDecimal[i] = toJavaChar(
                                    (byte) ((hostByte & 0x0F) + 0xF0));
                        }
                    } else {
                        /*
                         * reserve first byte for sign which will only be
                         * determined when we reach the last source byte
                         */
                        i++;
                        workDecimal[i] = toJavaChar(hostByte);
                    }
                } else if (i == (sourceSize - 1)) {
                    if (!isSignLeading && !isSignSeparate) {
                        /*
                         * Sign code gives the first target byte and the
                         * embedded digit gives the last byte.
                         */
                        int signCode = hostByte & 0xF0;
                        if (signCode == 0xd0) {
                            workDecimal[0] = toJavaChar(MINUS_EBCDIC);
                        } else {
                            workDecimal[0] = toJavaChar(PLUS_EBCDIC);
                        }
                        workDecimal[i] = toJavaChar((byte) ((hostByte & 0x0F) + 0xF0));
                    } else {
                        workDecimal[i] = toJavaChar(hostByte);
                    }
                } else if (i == sourceSize) {
                    /*
                     * last character from source is the sign.
                     * Java expects it at the beginning.
                     */
                    if (!isSignLeading && isSignSeparate) {
                        workDecimal[0] = toJavaChar(hostByte);
                    } else {
                        workDecimal[i] = toJavaChar(hostByte);
                    }
                } else {
                    workDecimal[i] = toJavaChar(hostByte);
                }
            } else {
                workDecimal[i] = toJavaChar(hostByte);
            }
            i++;
        }

        /* Turn the char array into a decimal with the required scale */
        BigDecimal result;
        try {
            result = new BigDecimal(workDecimal);
        } catch (NumberFormatException e) {
            throw (new CobolConversionException(
                    "Host data contains a byte that is not a valid zoned"
                            + " decimal byte",
                    new HostData(hostSource), offset, cobolByteLength));
        }
        if (fractionDigits == 0) {
            return result;
        } else {
            return result.movePointLeft(fractionDigits);
        }
    }

    /**
     * Lookup an assumed EBCDIC digit or sign character. If found, this will
     * return the equivalent java digit or sign.
     * If not found, there is a slight chance that the payload is already
     * in java encoding so we give it a chance.
     * 
     * @param hostByte the mainframe digit or sign
     * @return the corresponding java digit or '\0' if not found
     */
    public static char toJavaChar(final byte hostByte) {
        int i = Arrays.binarySearch(ZONED_DECIMAL_CHARS, hostByte);
        if (i < 0) {
            // Can't use binary search because content is not ordered
            for (i = 0; i < JAVA_CHARS.length; i++) {
                if (JAVA_CHARS[i] == (char) hostByte) {
                    return JAVA_CHARS[i];
                }
            }
        } else {
            return JAVA_CHARS[i];
        }
        return '\0';
    }

}
