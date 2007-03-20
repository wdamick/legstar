/*******************************************************************************
 * LegStar legacy Web-enablement .
 * Copyright (C)  2007 LegSem
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 * 
 *     
 *****************************************************************************/
package com.legstar.coxb.convert.simple;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolArrayFloatBinding;
import com.legstar.coxb.ICobolFloatBinding;
import com.legstar.coxb.convert.ICobolFloatConverter;
import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.host.HostData;
import com.legstar.host.HostException;
import com.legstar.host.HostFloat;

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
	public final int toHost(
			final ICobolFloatBinding ce,
			final byte[] hostTarget,
			final int offset)
		throws HostException {
		int newOffset = 0;
		try {
			newOffset = toHostSingle(ce.getValue(),
				hostTarget,
				offset);
		} catch (CobolConversionException e) {
			throwHostException(ce, e);
		}
		return newOffset;
	}
	
	/** {@inheritDoc} */
	public final int toHost(
			final ICobolArrayFloatBinding ce,
			final byte[] hostTarget,
			final int offset,
			final int currentOccurs)
		throws HostException {
		int newOffset = offset;
		try {
			for (Float javaSource : ce.getValue()) {
				newOffset = toHostSingle(javaSource,
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
			final ICobolFloatBinding ce,
			final byte[] hostSource,
			final int offset)
		throws HostException {
		int newOffset = offset;
		try {
			Float javaFloat = fromHostSingle(ce.getByteLength(),
					hostSource,
					newOffset);
			ce.setValue(javaFloat);
			newOffset += ce.getByteLength();
		} catch (CobolConversionException e) {
			throwHostException(ce, e);
		}
		return newOffset;
	}
	
	/** {@inheritDoc} */
	public final int fromHost(
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
			ce.setValue(lArray);
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
		
		/* If input Float is null, then stay on the same offset */
		if (javaFloat == null) {
			return offset;
		}
		
		/* Comp-1 are 4 byte long */
		int cobolByteLength = 4;
		
		/* Check that we are still within the host target range */
		int lastOffset = offset + cobolByteLength;
		if (lastOffset > hostTarget.length) {
			throw (new CobolConversionException(
					"Attempt to write past end of host source buffer",
					new HostData(hostTarget), offset));
		}

		/* Host floats do not support NaN or infinite.     */
		if (javaFloat.isInfinite()) {
			throw (new CobolConversionException(
					"Infinite floats are not supported",
					new HostData(hostTarget), offset));
		}
		if (javaFloat.isNaN()) {
			throw (new CobolConversionException(
					"NaN floats are not supported",
					new HostData(hostTarget), offset));
		}

		/* Treat the zero case separatly because the bit layout is not
		 * consistent.     */
		if ((javaFloat.floatValue() == 0.0f)
				||
			(javaFloat.floatValue() == -0.0f)) {
			for (int i = 0; i < 4; i++) {
				hostTarget[offset + i] = 0;
			}
			return offset + cobolByteLength;
		}
		
		/* Parse the Java float to get sign, exponent and mantissa */
		HostFloat jF = parseJavaFloat(javaFloat);

		/* Create a representation of the corresponding host float */
		HostFloat hF = new HostFloat();
		hF.setSign(jF.getSign());
		
		/* The java exponent is a binary offset while the host exponent is an
		 * hexadecimal offset.*/
		hF.setExponent(jF.getExponent() / 4 + 1);
		hF.setMantissa(jF.getMantissa()
				>> ((hF.getExponent() * 4) - jF.getExponent() - 1));

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
		
		/* Check that we are still within the host source range */
		if (lastOffset > hostSource.length) {
			throw (new CobolConversionException(
					"Attempt to read past end of host source buffer",
					new HostData(hostSource), offset, cobolByteLength));
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

		/* Host exponent is hexadecimal based while java is binary based */
		if (hF.getExponent() != 0) {
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
		 * E= excess - 127 */
		int excess = (javaIntBits & 0x7f800000) >>> 23;
		jF.setExponent((excess - 127));
		
		/* Bits 22-0 (23 bits) represents the getMantissa() in a form called
		 * "1-plus" normalized. This means that the real getMantissa() is
		 * actually * 1.b(23)b(22)...b(0) where the intiial "1" is implicit.
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
