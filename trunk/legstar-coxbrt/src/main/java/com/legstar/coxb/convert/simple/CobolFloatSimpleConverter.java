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
import com.legstar.coxb.ICobolArrayFloatBinding;
import com.legstar.coxb.ICobolFloatBinding;
import com.legstar.coxb.convert.ICobolFloatConverter;
import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;
import com.legstar.coxb.host.HostFloat;

import java.util.ArrayList;
import java.util.List;

/**
 * This is a concrete implementation of marshal/unmarshal operations of java
 * float to cobol comp-1.
 *
 * @author Fady Moussallam
 * 
 */
public class CobolFloatSimpleConverter extends CobolSimpleConverter
implements ICobolFloatConverter {

    /**
     * @param cobolContext the Cobol compiler parameters in effect
     */
    public CobolFloatSimpleConverter(final CobolContext cobolContext) {
        super(cobolContext);
    }

    /** {@inheritDoc} */
    public int toHost(
            final ICobolFloatBinding ce,
            final byte[] hostTarget,
            final int offset)
    throws HostException {
        int newOffset = 0;
        try {
            newOffset = toHostSingle(ce.getFloatValue(),
                    hostTarget,
                    offset);
        } catch (CobolConversionException e) {
            throwHostException(ce, e);
        }
        return newOffset;
    }

    /** {@inheritDoc} */
    public int toHost(
            final ICobolArrayFloatBinding ce,
            final byte[] hostTarget,
            final int offset,
            final int currentOccurs)
    throws HostException {
        int newOffset = offset;
        try {
            for (Float javaSource : ce.getFloatList()) {
                newOffset = toHostSingle(javaSource,
                        hostTarget,
                        newOffset);
            }
            /* If necessary, fill in the array with missing items */
            for (int i = ce.getFloatList().size();
            i < currentOccurs; i++) {
                newOffset = toHostSingle(Float.valueOf(0),
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
            final ICobolFloatBinding ce,
            final byte[] hostSource,
            final int offset)
    throws HostException {
        int newOffset = offset;
        try {
            Float javaFloat = fromHostSingle(ce.getByteLength(),
                    hostSource,
                    newOffset);
            ce.setFloatValue(javaFloat);
            newOffset += ce.getByteLength();
        } catch (CobolConversionException e) {
            throwHostException(ce, e);
        }
        return newOffset;
    }

    /** {@inheritDoc} */
    public int fromHost(
            final ICobolArrayFloatBinding ce,
            final byte[] hostSource,
            final int offset,
            final int currentOccurs)
    throws HostException {
        List < Float > lArray = new ArrayList < Float >();
        int newOffset = offset;
        try {
            for (int i = 0; i < currentOccurs; i++) {
                Float javaFloat = fromHostSingle(ce.getItemByteLength(),
                        hostSource,
                        newOffset);
                lArray.add(javaFloat);
                newOffset += ce.getItemByteLength();
            }
            ce.setFloatList(lArray);
        } catch (CobolConversionException e) {
            throwHostException(ce, e);
        }
        return newOffset;
    }

    /**
     *  Converts a Java Float to a host comp-1.
     * 
     * @param javaFloat java float to convert
     * @param hostTarget target host buffer
     * @param offset offset in target host buffer
     * @return offset after host buffer is updated
     * @throws CobolConversionException if conversion fails
     */
    public static final int toHostSingle(
            final Float javaFloat,
            final byte[] hostTarget,
            final int offset)
    throws CobolConversionException {

        /* Comp-1 are 4 byte long */
        int cobolByteLength = 4;

        /* Check that we are still within the host target range */
        int lastOffset = offset + cobolByteLength;
        if (lastOffset > hostTarget.length) {
            throw (new CobolConversionException(
                    "Attempt to write past end of host source buffer",
                    new HostData(hostTarget), offset));
        }

        /* Provide a default if input is null */
        Float localFloat = javaFloat;
        if (localFloat == null) {
            localFloat = 0f;
        }

        /* Host floats do not support NaN or infinite.     */
        if (localFloat.isInfinite()) {
            throw (new CobolConversionException(
                    "Infinite floats are not supported",
                    new HostData(hostTarget), offset));
        }
        if (localFloat.isNaN()) {
            throw (new CobolConversionException(
                    "NaN floats are not supported",
                    new HostData(hostTarget), offset));
        }

        /* Treat the zero case separatly because the bit layout is not
         * consistent.     */
        if ((localFloat.floatValue() == 0.0f)
                ||
                (localFloat.floatValue() == -0.0f)) {
            for (int i = 0; i < 4; i++) {
                hostTarget[offset + i] = 0;
            }
            return offset + cobolByteLength;
        }

        /* Parse the Java float to get sign, exponent and mantissa */
        HostFloat jF = parseJavaFloat(localFloat);

        /* Create a representation of the corresponding host float */
        HostFloat hF = new HostFloat();
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

        /* Now assemble the host float 4 bytes */
        int hostIntBits = createHostFloat(hF);

        /* Store the bytes in the host buffer */
        for (int i = 0; i < 4; i++) {
            hostTarget[offset + i]
                       = (byte) ((hostIntBits >>> (24 - i * 8)) & 0xff);
        }

        return offset + cobolByteLength;
    }

    /** Converts a host comp-1 to a Java Float.
     * 
     * @param cobolByteLength host byte length
     * @param hostSource source host buffer
     * @param offset offset in source host buffer
     * @return offset after host buffer is read
     * @throws CobolConversionException if conversion fails
     */
    public static final Float fromHostSingle(
            final int cobolByteLength,
            final byte[] hostSource,
            final int offset)
    throws CobolConversionException {

        int lastOffset = offset + cobolByteLength;

        /* Check that we are still within the host source range.
         * If not, consider the host optimized its payload by truncating
         * trailing nulls in which case, we just need to initialize and return. */
        if (lastOffset > hostSource.length) {
            return new Float(0);
        }

        /* Create a host float representation from the bytes we have */
        int hostIntBits = 0;
        for (int i = 0; i < 4; i++) {
            hostIntBits = hostIntBits
            | ((hostSource[offset + (3 - i)] & 0xff) << (i * 8));
        }
        HostFloat hF = parseHostFloat(hostIntBits);

        /* Create a representation of the corresponding java float */
        HostFloat jF = new HostFloat();
        jF.setSign(hF.getSign());

        /* Host exponent is hexadecimal based while java is binary based.
         * There is also an additional shift for non-zero values due to
         * the 1-plus" normalized java specs.  */
        if (hF.getMantissa() != 0) {
            jF.setExponent((4 * hF.getExponent()) - 1);
        }

        /* First assume same getMantissa() */
        jF.setMantissa(hF.getMantissa());

        /* In java the 24th bit needs to be one */
        while ((jF.getMantissa() > 0) && (jF.getMantissa() & 0x00800000) == 0) {
            jF.setMantissa(jF.getMantissa() << 1);
            jF.setExponent(jF.getExponent() - 1);
        }

        return createJavaFloat(jF);
    }

    /**
     * Extracts the sign, exponent and getMantissa() from a Java float.
     * 
     * @param f the float to extract components from
     * @return a class holding the various components
     */
    public static final HostFloat parseJavaFloat(final float f) {
        HostFloat jF = new HostFloat();
        /* Zero is a special case */
        if (f == 0.0f) {
            return jF;
        }

        int javaIntBits = Float.floatToIntBits(f);

        /* First bit left (bit 31) is the sign: 0 = positive, 1 = negative */
        jF.setSign((javaIntBits & 0x80000000) >>> 31);

        /* Bits 30-23 (8 bits) represents the exponent offset by 127, this
         * number is called excess so you get the exponent as
         * E= excess - 127. This is a binary exponent ( 2 pow(E)).
         * Furthermore, the "1-plus" normalized representation has the decimal
         * point after the implicit initial 1. Here we elect to store the
         * exponent for decimal point before that initial 1.*/
        int excess = (javaIntBits & 0x7f800000) >>> 23;
        jF.setExponent(excess - 127 + 1);

        /* Bits 22-0 (23 bits) represents the getMantissa() in a form called
         * "1-plus" normalized. This means that the real getMantissa() is
         * actually * 1.b(23)b(22)...b(0) where the initial "1" is implicit.
         * This code will explicitly add 1 in front of the getMantissa(). */
        int orMask = 1 << 23;
        jF.setMantissa(javaIntBits & 0x007fffff | orMask);

        return jF;
    }

    /**
     *  Reconstruct a java float from its sign, exponent and getMantissa()
     *  components.
     * 
     * @param jF a class holding the various components
     * @return a Java float
     */
    public static final float createJavaFloat(final HostFloat jF) {
        /* First check if this is a zero value */
        if (jF.getExponent() == 0 && jF.getMantissa() == 0) {
            return 0f;
        }

        /* Get rid of the leading 1 which needs to be implicit */
        int javaIntBits = jF.getMantissa() & 0x007fffff;
        javaIntBits = javaIntBits | ((jF.getExponent() + 127) << 23);
        javaIntBits = javaIntBits | (jF.getSign() << 31);
        return Float.intBitsToFloat(javaIntBits);
    }

    /** 
     * Extracts the sign, exponent and getMantissa() from a Host float .
     * 
     * @param hostIntBits the bit sequence representing the host float
     * @return a class holding the various components
     */
    public static final HostFloat parseHostFloat(final int hostIntBits) {

        HostFloat hF = new HostFloat();

        /* First bit left (bit 31) is the sign: 0 = positive, 1 = negative */
        hF.setSign((hostIntBits & 0x80000000) >>> 31);

        /* Bits 30-24 (7 bits) represents the exponent offset by 64, this
         * number is called excess so you get the exponent as
         * E= excess - 64 */
        int excess = (hostIntBits & 0x7f000000) >>> 24;
        if (excess != 0) {
            hF.setExponent((excess - 64));
        }

        /* Bits 23-0 (24 bits) represents the getMantissa(). */
        hF.setMantissa(hostIntBits & 0x00ffffff);

        return hF;
    }

    /** 
     * Reconstruct a host float from its sign, exponent and getMantissa()
     * components.
     * 
     * @param hF a class holding the various components
     * @return the bit sequence representing the host float
     */
    public static final int createHostFloat(final HostFloat hF) {

        /* First check if this is a zero value */
        if (hF.getExponent() == 0 && hF.getMantissa() == 0) {
            return 0;
        }

        int hostIntBits = hF.getMantissa();
        hostIntBits = hostIntBits | ((hF.getExponent() + 64) << 24);
        hostIntBits = hostIntBits | (hF.getSign() << 31);
        return hostIntBits;
    }
}
