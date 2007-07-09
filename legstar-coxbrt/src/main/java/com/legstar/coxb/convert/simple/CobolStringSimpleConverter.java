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

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolArrayStringBinding;
import com.legstar.coxb.ICobolStringBinding;
import com.legstar.coxb.convert.ICobolStringConverter;
import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

/**
 * This is a concrete implementation of marshal/unmarshal operations of java 
 * strings to cobol character strings.
 *
 * @author Fady Moussallam
 * 
 */
public class CobolStringSimpleConverter extends CobolSimpleConverter
             implements ICobolStringConverter {
	
	/** Ebcdic code point for space character. */
	private static final byte SPACE_EBCDIC = 0x40; 
	
	/**
	 * @param cobolContext the Cobol compiler parameters in effect
	 */
	public CobolStringSimpleConverter(final CobolContext cobolContext) {
		super(cobolContext);
	}
	
	/** {@inheritDoc} */
	public final int toHost(
			final ICobolStringBinding ce,
			final byte[] hostTarget,
			final int offset)
		throws HostException {
		int newOffset = 0;
		try {
			newOffset = toHostSingle(ce.getStringValue(),
					getCobolContext().getHostCharsetName(),
					ce.getByteLength(),
					ce.isJustifiedRight(),
					hostTarget,
					offset);
		} catch (CobolConversionException e) {
			throwHostException(ce, e);
		}
		return newOffset;
	}
	
	/** {@inheritDoc} */
	public final int toHost(
			final ICobolArrayStringBinding ce,
			final byte[] hostTarget,
			final int offset,
			final int currentOccurs)
		throws HostException {
		int newOffset = offset;
		try {
			for (String javaSource : ce.getStringList()) {
				newOffset = toHostSingle(javaSource,
						getCobolContext().getHostCharsetName(),
						ce.getByteLength(),
						ce.isJustifiedRight(),
						hostTarget,
						newOffset);
			}
			/* If necessary, fill in the array with missing items */
			for (int i = ce.getStringList().size();
							i < currentOccurs; i++) {
				newOffset = toHostSingle(new String(""),
						getCobolContext().getHostCharsetName(),
						ce.getByteLength(),
						ce.isJustifiedRight(),
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
			final ICobolStringBinding ce,
			final byte[] hostSource,
			final int offset)
		throws HostException {
		int newOffset = offset;
		try {
			String javaString = fromHostSingle(
					getCobolContext().getHostCharsetName(),
					ce.getByteLength(),
					hostSource,
					newOffset);
			ce.setStringValue(javaString);
			newOffset += ce.getByteLength();
		} catch (CobolConversionException e) {
			throwHostException(ce, e);
		}
		return newOffset;
	}
	
	/** {@inheritDoc} */
	public final int fromHost(
			final ICobolArrayStringBinding ce,
			final byte[] hostSource,
			final int offset,
			final int currentOccurs)
		throws HostException {
		List < String > lArray = new ArrayList < String >();
		int newOffset = offset;
		try {
			for (int i = 0; i < currentOccurs; i++) {
				String javaString = fromHostSingle(
						getCobolContext().getHostCharsetName(),
						ce.getByteLength(),
						hostSource,
						newOffset);
				lArray.add(javaString);
				newOffset += ce.getByteLength();
			}
			ce.setStringList(lArray);
		} catch (CobolConversionException e) {
			throwHostException(ce, e);
		}
		return newOffset;
	}
	
	/**
	 *  Converts a Java String to a host character stream within the host
	 *  character set.
	 * 
	 * @param javaString java string to convert
	 * @param hostCharsetName host character set
	 * @param cobolByteLength host byte length
	 * @param isJustifiedRight is Cobol data right justified
	 * @param hostTarget target host buffer
	 * @param offset offset in target host buffer
	 * @return offset after host buffer is updated
	 * @throws CobolConversionException if conversion fails
	 */
	public static final int toHostSingle(
			final String javaString,
			final String hostCharsetName,
			final int cobolByteLength,
			final boolean isJustifiedRight,
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

		/* HostData is obtained by converting the java String content to the 
		 * target host character set. */
		byte[] hostSource;
		
		/* See how many host bytes would be needed to hold the converted
		 * string */
		try {
			if (javaString == null) {
				hostSource =
					"".getBytes(hostCharsetName);
			} else {
				hostSource =
					javaString.getBytes(hostCharsetName);
			}
		} catch (UnsupportedEncodingException uee) {
			throw new CobolConversionException(
					"UnsupportedEncodingException:" + uee.getMessage());
		}
		
		/* The target host element might be larger than the converted java
		 * String and might have to be right or left justified. The padding
		 * character is 0x40 (space character). */
		int iSource = 0;
		int iTarget = offset;
		
		/* Pad with initial spaces if necessary */
		if (isJustifiedRight) {
			while (iTarget < (lastOffset - hostSource.length)) {
				hostTarget[iTarget] = SPACE_EBCDIC;
				iTarget++;
			}
		}
		
		/* Continue on with source content */
		while (iSource < hostSource.length && iTarget < lastOffset) {
			hostTarget[iTarget] = hostSource[iSource];
			iSource++;
			iTarget++;
		}
		
		/* Pad with final spaces if necessary */
		if (!isJustifiedRight) {
			while (iTarget < lastOffset) {
				hostTarget[iTarget] = SPACE_EBCDIC;
				iTarget++;
			}
		}
		
		return lastOffset;
	}

	/** Converts a host character string into a Java string.
	 * 
	 * @param hostCharsetName host character set
	 * @param cobolByteLength host byte length
	 * @param hostSource source host buffer
	 * @param offset offset in source host buffer
	 * @return offset after host buffer is read
	 * @throws CobolConversionException if conversion fails
	 */
	public static final String fromHostSingle(
			final String hostCharsetName,
			final int cobolByteLength,
			final byte[] hostSource,
			final int offset)
		throws CobolConversionException {
		
		String javaString = null;
		
		/* Check that we are still within the host source range */
		int lastOffset = offset + cobolByteLength;
		if (lastOffset > hostSource.length) {
			throw (new CobolConversionException(
					"Attempt to read past end of host source buffer",
					new HostData(hostSource), offset, cobolByteLength));
		}

		/* The Java String is obtained by translating from the host code page
		 * to the local code page. */
		try {
			javaString = new String(
					hostSource, offset, cobolByteLength,
					hostCharsetName);
			/* Some low-value characters may have slipped into the resulting
			 * string. */
			if (javaString.indexOf("\0") != -1) {
				javaString = javaString.replace('\0', ' ');
			}
		} catch (UnsupportedEncodingException uee) {
			throw new CobolConversionException(
					"UnsupportedEncodingException:" + uee.getMessage());
		}
		
		return javaString.trim();
	}
}
