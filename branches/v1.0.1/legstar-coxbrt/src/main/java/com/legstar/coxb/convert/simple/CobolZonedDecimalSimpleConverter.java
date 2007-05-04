/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.coxb.convert.simple;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolArrayZonedDecimalBinding;
import com.legstar.coxb.ICobolZonedDecimalBinding;
import com.legstar.coxb.convert.ICobolZonedDecimalConverter;
import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.host.HostData;
import com.legstar.host.HostException;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * This is a concrete implementation of marshal/unmarshal operations of java 
 * numerics to cobol zoned decimals.
 *
 * @author Fady Moussallam
 * 
 */
public class CobolZonedDecimalSimpleConverter extends CobolSimpleConverter
  implements ICobolZonedDecimalConverter {
	
	/** Ebcdic code point for plus sign. */
	private static final byte PLUS_EBCDIC = 0x4E; 

	/** Ebcdic code point for minus sign. */
	private static final byte MINUS_EBCDIC = 0x60; 
	
	/**
	 * @param cobolContext the Cobol compiler parameters in effect
	 */
	public CobolZonedDecimalSimpleConverter(final CobolContext cobolContext) {
		super(cobolContext);
	}
	
	/** {@inheritDoc} */
	public final int toHost(
			final ICobolZonedDecimalBinding ce,
			final byte[] hostTarget,
			final int offset) throws HostException {
		int newOffset = 0;
		try {
			newOffset = toHostSingle(ce.getValue(),
					ce.getByteLength(),
					ce.getTotalDigits(),
					ce.getFractionDigits(),
					ce.isSigned(),
					ce.isSignSeparate(),
					ce.isSignLeading(),
					hostTarget,
					offset);
		} catch (CobolConversionException e) {
			throwHostException(ce, e);
		}
		return newOffset;
	}
	
	/** {@inheritDoc} */
	public final int toHost(
			final ICobolArrayZonedDecimalBinding ce,
			final byte[] hostTarget,
			final int offset,
			final int currentOccurs)
		throws HostException {
		int newOffset = offset;
		try {
		for (BigDecimal javaSource : ce.getValue()) {
			newOffset = toHostSingle(javaSource,
					ce.getItemByteLength(),
					ce.getTotalDigits(),
					ce.getFractionDigits(),
					ce.isSigned(),
					ce.isSignSeparate(),
					ce.isSignLeading(),
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
					newOffset);
			ce.setValue(javaDecimal);
			newOffset += ce.getByteLength();
		} catch (CobolConversionException e) {
			throwHostException(ce, e);
		}
		return newOffset;
	}
	
	/** {@inheritDoc} */
	public final int fromHost(
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
						newOffset);
				lArray.add(javaDecimal);
				newOffset += ce.getItemByteLength();
			}
			ce.setValue(lArray);
		} catch (CobolConversionException e) {
			throwHostException(ce, e);
		}
		return newOffset;
	}
	
	/**
	 *  Converts a Java BigDecimal to a host zoned decimal.
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
			final int offset)
		throws CobolConversionException {
		
		/* If input BigDecimal is null, then stay on the same offset */
		if (javaDecimal == null) {
			return offset;
		}
		
		/* Check that we are still within the host target range */
		int lastOffset = offset + cobolByteLength;
		if (lastOffset > hostTarget.length) {
			throw (new CobolConversionException(
					"Attempt to write past end of host source buffer",
					new HostData(hostTarget), offset, cobolByteLength));
		}

		// Get a string representation of the decimal value
		String sDecimal = javaDecimal.toString();

		/* if the Java decimal has a different scale than target cobol field,
		 * adjust scale */
		if (javaDecimal.scale() != fractionDigits) {
			sDecimal = javaDecimal.setScale(
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
		
		int iTarget = offset;   /* points to current byte in host data */
		int bByte = 0;          /* represents de byte value to be appended to
		                         * host data */
		
		// Start by padding to the left with zeroes as necessary
		for (int i = 0; i < pad; i++) {
			hostTarget[iTarget] = (byte) 0xF0;
			iTarget++;
		}
		
		// position a potential separate leading sign
		if (isSignSeparate && isSignLeading) {
			if (javaDecimal.signum() == -1) {
				hostTarget[iTarget] = MINUS_EBCDIC;
			} else {
				hostTarget[iTarget] = PLUS_EBCDIC;
			}
			iTarget++;
		}
		
		/**
		 * Byte representation for a cobol digit is built as 0xFd where d is
		 * the corresponding digit. Exceptions to this rule apply for signed
		 * decimals when sign is not separate in which case 0xDd signals a
		 * negative decimal and 0xCd signals a positive decimal either in
		 * leading or trailing byte.
		 */
		for (int i = 0; i < sDecimal.length(); i++) {
			char sC = sDecimal.charAt(i);
			if (Character.isDigit(sC)) {
				int digit = Character.digit(sC, 10);
				// leading sign might affect the layout of the first byte
				if ((iTarget == 0) && isSigned
						&&
						isSignLeading && !isSignSeparate) {
					if (javaDecimal.signum() == -1) {
						bByte = 16 * 0x0D;
					} else {
						bByte = 16 * 0x0C;
					}
					bByte += digit;
					hostTarget[iTarget] = (byte) bByte;
					iTarget++;
					continue;
				}
				/* trailing sign might affect the layout of the last byte */
				if ((i == (sDecimal.length() - 1))
						&&
						isSigned && !isSignLeading && !isSignSeparate) {
					if (javaDecimal.signum() == -1) {
						bByte = 16 * 0x0D;
					} else {
						bByte = 16 * 0x0C;
					}
					bByte += digit;
					hostTarget[iTarget] = (byte) bByte;
					iTarget++;
					continue;
				}
				bByte = 16 * 0x0F + digit;
				hostTarget[iTarget] = (byte) bByte;
				iTarget++;
			}
		}
		
		// position a potential separate trailing sign
		if (isSignSeparate && !isSignLeading) {
			if (javaDecimal.signum() == -1) {
				hostTarget[iTarget] = MINUS_EBCDIC;
			} else {
				hostTarget[iTarget] = PLUS_EBCDIC;
			}
			iTarget++;
		}
		
		return iTarget;
	}
	
	/** Converts a host packed decimal to a Java BigDecimal.
	 * 
	 * @param cobolByteLength host byte length
	 * @param totalDigits Cobol element total number of digits
	 * @param fractionDigits Cobol element number of fractional digits
	 * @param isSigned Cobol element is signed or unsigned
	 * @param isSignSeparate Cobol element sign occupies own byte
	 * @param isSignLeading Cobol element sign in first byte
	 * @param hostSource source host buffer
	 * @param offset offset in source host buffer
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
			final int offset)
		throws CobolConversionException {
		
		/* To initialize the BigDecimal, we construct a String that represents
		 * the decimal value held in the Cobol zoned decimal */
		StringBuffer sDecimal = new StringBuffer();
		
		int lastOffset = offset + cobolByteLength;
		
		/* Check that we are still within the host source range */
		if (lastOffset > hostSource.length) {
			throw (new CobolConversionException(
					"Attempt to read past end of host source buffer",
					new HostData(hostSource), offset, cobolByteLength));
		}
		if (lastOffset < 1) {
			throw (new CobolConversionException(
					"Invalid host byte length",
					new HostData(hostSource), offset, cobolByteLength));
		}

		/* First determine the sign of this decimal */
		if (isSigned) {
			/* If sign is separate check leading or trailing byte for a minus
			 * sign */
			if (isSignSeparate) {
				if (isSignLeading)  {
					if (hostSource[offset] == MINUS_EBCDIC) {
						sDecimal.append('-');
					}
				} else {
					if (hostSource[lastOffset - 1] == MINUS_EBCDIC) {
						sDecimal.append('-');
					}
				}
			} else {
				/* If sign is imbedded check leading or trailing byte for a
				 *  minus sign */
				int s = (isSignLeading) ? (hostSource[offset] & 0xF0)
						                : (hostSource[lastOffset - 1]  & 0xF0);
				if (s == 0xd0) {
					sDecimal.append('-');
				} else {
					if (s != 0xc0 && s != 0xf0) {
						throw (new CobolConversionException(
						"Host data sign byte is not a valid zoned decimal byte",
						new HostData(hostSource), offset, cobolByteLength));
					}
				}
			}
		}
		
		/* Each byte holds 1 digit */
		int integerPart = 0;
		for (int i = offset; i < lastOffset; i++) {
			
			/* Ignore the potential leading sign */
			if ((i == offset)
					&&
					isSigned && isSignSeparate && isSignLeading) {
				continue;
			}
			
			/* Ignore the potential trailing sign */
			if ((i == (lastOffset - 1))
					&&
					isSigned && isSignSeparate && !isSignLeading) {
				continue;
			}
			
			String sByte = Integer.toHexString(
					hostSource[i] & 0xFF | 0x100).substring(1, 3);
			
			/* If the integer part is exhausted, place a decimal point */
			if (integerPart == (totalDigits - fractionDigits)) {
				sDecimal.append('.');
			}
			
			if (!Character.isDigit(sByte.charAt(1))) {
				throw (new CobolConversionException(
				"Host data contains a byte that is not a valid zoned"
				+ " decimal byte",
				new HostData(hostSource), offset, cobolByteLength));
			}
			
			sDecimal.append(sByte.charAt(1));
			integerPart += 1;
		}
		
		return new BigDecimal(sDecimal.toString());
	}
}
