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

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolArrayDoubleBinding;
import com.legstar.coxb.ICobolDoubleBinding;
import com.legstar.coxb.convert.ICobolDoubleConverter;
import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostDouble;
import com.legstar.coxb.host.HostException;

import java.util.ArrayList;
import java.util.List;

/**
 * This is a concrete implementation of marshal/unmarshal operations of java
 * double to cobol comp-2.
 *
 * @author Fady Moussallam
 * 
 */
public class CobolDoubleSimpleConverter extends CobolSimpleConverter
implements ICobolDoubleConverter {

    /**
     * @param cobolContext the Cobol compiler parameters in effect
     */
    public CobolDoubleSimpleConverter(final CobolContext cobolContext) {
        super(cobolContext);
    }

    /** {@inheritDoc} */
    public final int toHost(
            final ICobolDoubleBinding ce,
            final byte[] hostTarget,
            final int offset)
    throws HostException {
        int newOffset = 0;
        try {
            newOffset = toHostSingle(ce.getDoubleValue(),
                    hostTarget,
                    offset);
        } catch (CobolConversionException e) {
            throwHostException(ce, e);
        }
        return newOffset;
    }

    /** {@inheritDoc} */
    public final int toHost(
            final ICobolArrayDoubleBinding ce,
            final byte[] hostTarget,
            final int offset,
            final int currentOccurs)
    throws HostException {
        int newOffset = offset;
        try {
            for (Double javaSource : ce.getDoubleList()) {
                newOffset = toHostSingle(javaSource,
                        hostTarget,
                        newOffset);
            }
            /* If necessary, fill in the array with missing items */
            for (int i = ce.getDoubleList().size();
            i < currentOccurs; i++) {
                newOffset = toHostSingle(Double.valueOf(0),
                        hostTarget,
                        newOffset);
            }
        } catch (CobolConversionException e) {
            throwHostException(ce, e);
        }
        return newOffset;
    }

    /** {@inheritDoc} */
    public final int fromHost(
            final ICobolDoubleBinding ce,
            final byte[] hostSource,
            final int offset)
    throws HostException {
        int newOffset = offset;
        try {
            Double javaDouble = fromHostSingle(ce.getByteLength(),
                    hostSource,
                    newOffset);
            ce.setDoubleValue(javaDouble);
            newOffset += ce.getByteLength();
        } catch (CobolConversionException e) {
            throwHostException(ce, e);
        }
        return newOffset;
    }

    /** {@inheritDoc} */
    public final int fromHost(
            final ICobolArrayDoubleBinding ce,
            final byte[] hostSource,
            final int offset,
            final int currentOccurs)
    throws HostException {
        List < Double > lArray = new ArrayList < Double >();
        int newOffset = offset;
        try {
            for (int i = 0; i < currentOccurs; i++) {
                Double javaDouble = fromHostSingle(ce.getItemByteLength(),
                        hostSource,
                        newOffset);
                lArray.add(javaDouble);
                newOffset += ce.getItemByteLength();
            }
            ce.setDoubleList(lArray);
        } catch (CobolConversionException e) {
            throwHostException(ce, e);
        }
        return newOffset;
    }

    /**
     *  Converts a Java Double to a host comp-2.
     * 
     * @param javaDouble java double to convert
     * @param hostTarget target host buffer
     * @param offset offset in target host buffer
     * @return offset after host buffer is updated
     * @throws CobolConversionException if conversion fails
     */
    public static final int toHostSingle(
            final Double javaDouble,
            final byte[] hostTarget,
            final int offset)
    throws CobolConversionException {

        /* Comp-2 are 8 byte long */
        int cobolByteLength = 8;

        /* Check that we are still within the host target range */
        int lastOffset = offset + cobolByteLength;
        if (lastOffset > hostTarget.length) {
            throw (new CobolConversionException(
                    "Attempt to write past end of host source buffer",
                    new HostData(hostTarget), offset));
        }

        /* Provide a default if input is null */
        Double localDouble = javaDouble;
        if (localDouble == null) {
            localDouble = 0d;
        }

        /* Host doubles do not support NaN or infinite.     */
        if (localDouble.isInfinite()) {
            throw (new CobolConversionException(
                    "Infinite doubles are not supported",
                    new HostData(hostTarget), offset));
        }
        if (localDouble.isNaN()) {
            throw (new CobolConversionException(
                    "NaN doubles are not supported",
                    new HostData(hostTarget), offset));
        }

        /* Treat the zero case separatly because the bit layout is not
         *  consistent.     */
        if ((localDouble.doubleValue() == 0.0f)
                ||
                (localDouble.doubleValue() == -0.0f)) {
            for (int i = 0; i < 8; i++) {
                hostTarget[offset + i] = 0;
            }
            return offset + cobolByteLength;
        }

        /* Parse the Java double to get sign, exponent and mantissa */
        HostDouble jF = parseJavaDouble(localDouble);

        /* Create a representation of the corresponding host double */
        HostDouble hF = new HostDouble();
        hF.setSign(jF.getSign());

        /* The java exponent is a binary offset while the host exponent is an
         * hexadecimal offset. This means host exponent values are multiple
         * or 4. If the java exponent is not a multiple of 4 we then need 
         * to shift the mantissa which might result in loss of precision. */
        int r = jF.getExponent() % 4;
        int mantissaShift = 0; 
        if (r <= 0) {
            mantissaShift = -1 * r;
        } else {
            mantissaShift = 4 - r;
        }

        hF.setExponent((jF.getExponent() + mantissaShift) / 4);
        hF.setMantissa(jF.getMantissa() >> mantissaShift);

        /* The host double mantissa is stored on 56 bits while java is on 53
         * (52 + 1 plus).
         * bits so we need to padd the java mantissa with an additional 3 bits
         * to the right */
        hF.setMantissa(hF.getMantissa() << 3);

        /* Now assemble the host double 8 bytes */
        long hostLongBits = createHostDouble(hF);

        /* Store the bytes in the host buffer */
        for (int i = 0; i < 8; i++) {
            hostTarget[offset + i] =
                (byte) ((hostLongBits >>> (56 - i * 8)) & 0xff);
        }

        return offset + cobolByteLength;
    }

    /** Converts a host comp-2 to a Java Double.
     * 
     * @param cobolByteLength host byte length
     * @param hostSource source host buffer
     * @param offset offset in source host buffer
     * @return offset after host buffer is read
     * @throws CobolConversionException if conversion fails
     */
    public static final Double fromHostSingle(
            final int cobolByteLength,
            final byte[] hostSource,
            final int offset)
    throws CobolConversionException {

        int lastOffset = offset + cobolByteLength;

        /* Check that we are still within the host source range.
         * If not, consider the host optimized its payload by truncating
         * trailing nulls in which case, we just need to initialize and return. */
        if (lastOffset > hostSource.length) {
            return new Double(0);
        }

        /* Create a host double representation from the bytes we have */
        long hostLongBits = 0;
        for (int i = 0; i < 8; i++) {
            hostLongBits = hostLongBits
            | ((long) (hostSource[offset + (7 - i)] & 0xff) << (i * 8));
        }
        HostDouble hF = parseHostDouble(hostLongBits);

        /* Create a representation of the corresponding java double */
        HostDouble jF = new HostDouble();
        jF.setSign(hF.getSign());

        /* Host exponent is hexadecimal based while java is binary based.
         * There is also an additional shift for non-zero values due to
         * the 1-plus" normalized java specs.  */
        if (hF.getMantissa() != 0) {
            jF.setExponent((4 * hF.getExponent()) - 1);
        }

        /* The java mantissa is 53 bits while the host is 56. This
         * means there is a systematic loss of precision. */
        jF.setMantissa(hF.getMantissa() >>> 3);

        /* In java the 53th bit needs to be one */
        while ((jF.getMantissa() > 0L)
                &&
                (jF.getMantissa() & 0x0010000000000000L) == 0) {
            jF.setMantissa(jF.getMantissa() << 1);
            jF.setExponent(jF.getExponent() - 1);
        }

        return createJavaDouble(jF);
    }

    /**
     * Extracts the sign, exponent and mantissa from a Java double.
     * 
     * @param f the double to extract components from
     * @return a class holding the various components
     */
    public static final HostDouble parseJavaDouble(final double f) {

        HostDouble jF = new HostDouble();

        /* Zero is a special case */
        if (f == 0.0f) {
            return jF;
        }

        long javaLongBits = Double.doubleToLongBits(f);

        /* First bit left (bit 63) is the sign: 0 = positive, 1 = negative */
        jF.setSign((int) ((javaLongBits & 0x8000000000000000L) >>> 63));

        /* Bits 62-52  (11 bits) represents the exponent offset by 1023, this
         * number is called excess so you get the exponent as E= excess - 1023
         * Furthermore, the "1-plus" normalized representation has the decimal
         * point after the implicit initial 1. Here we elect to store the
         * exponent for decimal point before that initial 1.*/
        int excess = (int) ((javaLongBits >> 52) & 0x7ffL);
        jF.setExponent(excess - 1023 + 1);

        /* Bits 51-0 (52 bits) represents the mantissa in a form called
         * "1-plus" normalized. This means that the real mantissa is actually
         * 1.b(51)b(50)...b(0) where the intiial "1" is implicit.
         * This code will explicitly add 1 in front of the mantissa. */
        long orMask = 1L << 52;
        jF.setMantissa(javaLongBits & 0x000fffffffffffffL | orMask);

        return jF;
    }

    /**
     *  Reconstruct a java double from its sign, exponent and mantissa
     *  components.
     * 
     * @param jF a class holding the various components
     * @return a Java double
     */
    public static final double createJavaDouble(final HostDouble jF) {

        /* First check if this is a zero value */
        if (jF.getExponent() == 0 && jF.getMantissa() == 0) {
            return 0d;
        }

        /* Get rid of the leading 1 which needs to be implicit */
        long javaLongBits = jF.getMantissa() & 0x000fffffffffffffL;
        javaLongBits = javaLongBits | ((long) (jF.getExponent() + 1023) << 52);
        javaLongBits = javaLongBits | ((long) jF.getSign() << 63);
        return Double.longBitsToDouble(javaLongBits);
    }

    /** 
     * Extracts the sign, exponent and mantissa from a Host double .
     * 
     * @param hostLongBits the bit sequence representing the host double
     * @return a class holding the various components
     */
    public static final HostDouble parseHostDouble(final long hostLongBits) {
        HostDouble hF = new HostDouble();

        /* First bit left (bit 63) is the sign: 0 = positive, 1 = negative */
        hF.setSign((int) ((hostLongBits & 0x8000000000000000L) >>> 63));

        /* Bits 62-56 (7 bits) represents the exponent offset by 64, this
         * number is called excess so you get the exponent as
         * E= excess - 64 */
        int excess = (int) ((hostLongBits & 0x7f00000000000000L) >>> 56);
        if (excess != 0) {
            hF.setExponent((excess - 64));
        }

        /* Bits 55-0 (56 bits) represents the mantissa. */
        hF.setMantissa(hostLongBits & 0x00ffffffffffffffL);

        return hF;
    }

    /** 
     * Reconstruct a host double from its sign, exponent and mantissa
     * components.
     * 
     * @param hF a class holding the various components
     * @return the bit sequence representing the host double
     */
    public static final long createHostDouble(final HostDouble hF) {

        /* First check if this is a zero value */
        if (hF.getExponent() == 0 && hF.getMantissa() == 0) {
            return 0L;
        }

        long hostLongBits = hF.getMantissa();
        hostLongBits = hostLongBits | ((long) (hF.getExponent() + 64) << 56);
        hostLongBits = hostLongBits | ((long) hF.getSign() << 63);
        return hostLongBits;
    }
}
