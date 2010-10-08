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

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolArrayPackedDecimalBinding;
import com.legstar.coxb.ICobolPackedDecimalBinding;
import com.legstar.coxb.convert.ICobolPackedDecimalConverter;
import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * This is a concrete implementation of marshal/unmarshal operations of java 
 * numerics to cobol packed decimals.
 *
 * @author Fady Moussallam
 * 
 */
public class CobolPackedDecimalSimpleConverter extends CobolSimpleConverter
implements ICobolPackedDecimalConverter {

    /**
     * @param cobolContext the Cobol compiler parameters in effect
     */
    public CobolPackedDecimalSimpleConverter(final CobolContext cobolContext) {
        super(cobolContext);
    }

    /** {@inheritDoc} */
    public int toHost(
            final ICobolPackedDecimalBinding ce,
            final byte[] hostTarget,
            final int offset)
    throws HostException {
        int newOffset = 0;
        try {
            newOffset = toHostSingle(ce.getBigDecimalValue(),
                    ce.getByteLength(),
                    ce.getTotalDigits(),
                    ce.getFractionDigits(),
                    ce.isSigned(),
                    hostTarget,
                    offset);
        } catch (CobolConversionException e) {
            throwHostException(ce, e);
        }
        return newOffset;
    }

    /** {@inheritDoc} */
    public int toHost(
            final ICobolArrayPackedDecimalBinding ce,
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
                        hostTarget,
                        newOffset);
            }
            /* If necessary, fill in the array with missing items */
            for (int i = ce.getBigDecimalList().size();
            i < currentOccurs; i++) {
                newOffset = toHostSingle(BigDecimal.ZERO,
                        ce.getItemByteLength(),
                        ce.getTotalDigits(),
                        ce.getFractionDigits(),
                        ce.isSigned(),
                        hostTarget,
                        newOffset);
            }
        } catch (CobolConversionException e) {
            throwHostException(ce, e);
        }
        return newOffset;
    }

    /** {@inheritDoc} */
    public int fromHost(
            final ICobolPackedDecimalBinding ce,
            final byte[] hostSource,
            final int offset)
    throws HostException {
        int newOffset = offset;
        try {
            BigDecimal javaDecimal = fromHostSingle(ce.getByteLength(),
                    ce.getTotalDigits(),
                    ce.getFractionDigits(),
                    hostSource,
                    newOffset);
            ce.setBigDecimalValue(javaDecimal);
            newOffset += ce.getByteLength();
        } catch (CobolConversionException e) {
            throwHostException(ce, e);
        }
        return newOffset;
    }

    /** {@inheritDoc} */
    public int fromHost(
            final ICobolArrayPackedDecimalBinding ce,
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
                        hostSource,
                        newOffset);
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
     *  Converts a Java BigDecimal to a host packed decimal.
     * 
     * @param javaDecimal java decimal to convert
     * @param cobolByteLength host byte length
     * @param totalDigits Cobol element total number of digits
     * @param fractionDigits Cobol element number of fractional digits
     * @param isSigned Cobol element is signed or unsigned
     * @param hostTarget target host buffer
     * @param offset offset in target host buffer
     * @return offset after host buffer is updated
     * @throws CobolConversionException if conversion fails
     */
    public static final int toHostSingle(
            final BigDecimal javaDecimal,
            final int cobolByteLength,
            final int totalDigits,
            final int fractionDigits,
            final boolean isSigned,
            final byte[] hostTarget,
            final int offset)
    throws CobolConversionException {

        /* Check that we are still within the host target range */
        int lastOffset = offset + cobolByteLength;
        if (lastOffset > hostTarget.length) {
            throw (new CobolConversionException(
                    "Attempt to write past end of host source buffer",
                    new HostData(hostTarget), offset, cobolByteLength));
        }

        /* Provide a default if input is null */
        BigDecimal localDecimal = javaDecimal;
        if (localDecimal == null) {
            localDecimal = BigDecimal.ZERO;
        }

        /* Get a string representation of the decimal value */
        String sDecimal = localDecimal.toString();

        /* if the Java decimal has a different scale than target cobol field,
         * adjust scale */
        if (localDecimal.scale() != fractionDigits) {
            sDecimal = localDecimal.setScale(
                    fractionDigits, BigDecimal.ROUND_DOWN).toString();
        }

        /* Determine the number of digits that the java decimal holds */
        int javaDigits = 0;
        for (int i = 0; i < sDecimal.length(); i++) {
            if (Character.isDigit(sDecimal.charAt(i))) {
                javaDigits++;
            }
        }

        if (javaDigits > totalDigits) {
            throw (new CobolConversionException(
                    "BigDecimal value too large for target Cobol field",
                    new HostData(hostTarget), offset, cobolByteLength));
        }

        /* Number of digits that are needed to pad the java value if it has
         * less digits than the target cobol field */
        int pad = totalDigits - javaDigits;

        /**
         * As we encounter digits in the string representation of the
         * BigDecimal, we group them by 2 and create a byte value where the
         * first digit occupies the left half-byte and the second digit goes
         * into the right half-byte.
         */
        int iTarget = offset;   /* points to current byte in host data */
        boolean flip = false;   /* indicates when it is time to add a byte
         * to host data */
        int bByte = 0;          /* represents the byte value to be appended
         * to host data */

        /* If the number of digits is even, we need to add a leading 0 value
         * half-byte. */
        if (totalDigits % 2 == 0) {
            flip = true;
        }

        /* Start by padding to the left with zeroes as necessary */
        for (int i = 0; i < pad; i++) {
            if (flip) {
                hostTarget[iTarget] = (byte) 0x00;
                iTarget++;
                flip = false;
            } else {
                flip = true;
            }
        }

        /* Translate digit characters into there numeric value
         * and populate right and left half bytes */
        for (int i = 0; i < sDecimal.length(); i++) {
            char sC = sDecimal.charAt(i);
            if (Character.isDigit(sC)) {
                int digit = Character.digit(sC, 10);
                if (flip) {
                    bByte += digit;
                    hostTarget[iTarget] = (byte) bByte;
                    iTarget++;
                    flip = false;
                } else {
                    bByte = digit * 16;
                    flip = true;
                }
            }
        }

        /* The last half-byte is the sign. The rule is 0xF for unsigned
         * decimals otherwise 0xD is negative and 0xC is positive */
        if (!isSigned) {
            bByte += 0x0F;
        } else {
            if (localDecimal.signum() == -1) {
                bByte += 0x0D;
            } else {
                bByte += 0x0C;
            }
        }
        hostTarget[iTarget] = (byte) bByte;
        iTarget++;

        return iTarget;
    }

    /** Converts a host packed decimal to a Java BigDecimal.
     * 
     * @param cobolByteLength host byte length
     * @param totalDigits Cobol element total number of digits
     * @param fractionDigits Cobol element number of fractional digits
     * @param hostSource source host buffer
     * @param offset offset in source host buffer
     * @return offset after host buffer is read
     * @throws CobolConversionException if conversion fails
     */
    public static final BigDecimal fromHostSingle(
            final int cobolByteLength,
            final int totalDigits,
            final int fractionDigits,
            final byte[] hostSource,
            final int offset)
    throws CobolConversionException {

        /* To initialize the BigDecimal, we construct a String that represents
         * the decimal value held in the Cobol packed decimal */
        StringBuffer sDecimal = new StringBuffer();

        int lastOffset = offset + cobolByteLength;

        /* Check that we are still within the host source range.
         * If not, consider the host optimized its payload by truncating
         * trailing nulls in which case, we just need to initialize and return.
         * TODO: This situation should not happen as packed decimals cannot
         * end with a binary zero (the last byte always holds a sign half byte) */
        if (lastOffset > hostSource.length) {
            return new BigDecimal(0).setScale(fractionDigits);
        }
        if (lastOffset < 1) {
            throw (new CobolConversionException(
                    "Invalid host byte length",
                    new HostData(hostSource), offset, cobolByteLength));
        }

        /* The leading sign is derived from the last byte */
        int s = (hostSource[lastOffset - 1]  & 0x0F);
        if (s == 0x0d) {
            sDecimal.append('-');
        } else { 
            if (s != 0x0c && s != 0x0f) {
                throw (new CobolConversionException(
                        "Host data last byte is not a valid packed decimal byte",
                        new HostData(hostSource), offset, cobolByteLength));
            }
        }

        /* Each byte holds 2 digits except for the last one. */
        int integerPart = 0;
        for (int i = offset; i < lastOffset; i++) {
            String sByte =
                Integer.toHexString(
                        hostSource[i] & 0xFF | 0x100).substring(1, 3);

            /* Last byte holds a digit and the sign */
            if (i == (lastOffset - 1)) {
                insertChar(
                        sByte.charAt(0), sDecimal, integerPart, totalDigits,
                        fractionDigits, hostSource, offset, cobolByteLength);

            } else {
                /* Insert left digit unless this is the first byte of an even
                 * number of digits in which case we can ignore that first
                 * half-byte */
                if (i != offset || (totalDigits % 2 != 0)) {
                    integerPart = insertChar(
                            sByte.charAt(0), sDecimal, integerPart, totalDigits,
                            fractionDigits, hostSource, offset, cobolByteLength);
                }
                integerPart = insertChar(
                        sByte.charAt(1), sDecimal, integerPart, totalDigits,
                        fractionDigits, hostSource, offset, cobolByteLength);
            }
        }
        return new BigDecimal(sDecimal.toString());
    }

    /**
     * Checks that the character is valid and inserts it in the string.
     * 
     * @param c the character to insert
     * @param sDecimal the decimal string representation
     * @param integerPart current digits count
     * @param totalDigits total number of digits
     * @param fractionDigits fractional number of digits
     * @param hostSource host source buffer
     * @param offset offset in host source buffer
     * @param cobolByteLength host byte length
     * @return the new integer part
     * @throws CobolConversionException if character is not a digit
     */
    private static int insertChar(
            final char c,
            final StringBuffer sDecimal,
            final int integerPart,
            final int totalDigits,
            final int fractionDigits,
            final byte[] hostSource,
            final int offset,
            final int cobolByteLength)
    throws CobolConversionException {

        /* Insert a decimal point when needed */
        if (integerPart == (totalDigits - fractionDigits)) {
            sDecimal.append('.');
        }

        /* Make sure this is a valid digit */
        if (!Character.isDigit(c)) {
            throw (new CobolConversionException(
                    "Host data contains a byte that is not a valid packed decimal byte",
                    new HostData(hostSource), offset, cobolByteLength));
        }

        sDecimal.append(c);

        return integerPart + 1;
    }

}
