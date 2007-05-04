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
import com.legstar.coxb.ICobolArrayNationalBinding;
import com.legstar.coxb.ICobolNationalBinding;
import com.legstar.coxb.convert.ICobolNationalConverter;
import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.host.HostData;
import com.legstar.host.HostException;

/**
 * This is a concrete implementation of marshal/unmarshal operations of java 
 * strings to cobol national strings.
 *
 * @author Fady Moussallam
 * 
 */
public class CobolNationalSimpleConverter extends CobolSimpleConverter
             implements ICobolNationalConverter {
	
	/** UTF-16 code point for space character. */
	private static final byte[] SPACE_UTF_16 = {0x00, 0x20}; 
	
	/** Encoding used for national items. */
	private static final String NATIONAL_CHARSET = "UTF-16";
	
	/**
	 * @param cobolContext the Cobol compiler parameters in effect
	 */
	public CobolNationalSimpleConverter(final CobolContext cobolContext) {
		super(cobolContext);
	}
	
	/** {@inheritDoc} */
	public final int toHost(
			final ICobolNationalBinding ce,
			final byte[] hostTarget,
			final int offset)
		throws HostException {
		int newOffset = 0;
		try {
			newOffset = toHostSingle(ce.getValue(),
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
			final ICobolArrayNationalBinding ce,
			final byte[] hostTarget,
			final int offset,
			final int currentOccurs)
		throws HostException {
		int newOffset = offset;
		try {
			for (String javaSource : ce.getValue()) {
				newOffset = toHostSingle(javaSource,
						ce.getItemByteLength(),
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
			final ICobolNationalBinding ce,
			final byte[] hostSource,
			final int offset)
		throws HostException {
		int newOffset = offset;
		try {
			String javaString = fromHostSingle(
					ce.getByteLength(),
					hostSource,
					newOffset);
			ce.setValue(javaString);
			newOffset += ce.getByteLength();
		} catch (CobolConversionException e) {
			throwHostException(ce, e);
		}
		return newOffset;
	}
	
	/** {@inheritDoc} */
	public final int fromHost(
			final ICobolArrayNationalBinding ce,
			final byte[] hostSource,
			final int offset,
			final int currentOccurs)
		throws HostException {
		List < String > lArray = new ArrayList < String >();
		int newOffset = offset;
		try {
			for (int i = 0; i < currentOccurs; i++) {
				String javaString = fromHostSingle(
						ce.getItemByteLength(),
						hostSource,
						newOffset);
				lArray.add(javaString);
				newOffset += ce.getItemByteLength();
			}
			ce.setValue(lArray);
		} catch (CobolConversionException e) {
			throwHostException(ce, e);
		}
		return newOffset;
	}
	
	/**
	 *  Converts a Java String to a host national element.
	 * 
	 * @param javaString java string to convert
	 * @param cobolByteLength host byte length
	 * @param isJustifiedRight is Cobol data right justified
	 * @param hostTarget target host buffer
	 * @param offset offset in target host buffer
	 * @return offset after host buffer is updated
	 * @throws CobolConversionException if conversion fails
	 */
	public static final int toHostSingle(
			final String javaString,
			final int cobolByteLength,
			final boolean isJustifiedRight,
			final byte[] hostTarget,
			final int offset)
		throws CobolConversionException {
		
		/* If input String is null, then stay on the same offset */
		if (javaString == null) {
			return offset;
		}

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
			hostSource =
				javaString.getBytes(NATIONAL_CHARSET);
		} catch (UnsupportedEncodingException uee) {
			throw new CobolConversionException(
					"UnsupportedEncodingException:" + uee.getMessage());
		}
		
		int iSource = 0;
		/* z/OS UTF-16 is not expecting a BOM so we ignore it */
		int hsl = hostSource.length;
		if (hsl > 1 && hostSource[0] == -2 && hostSource[1] == -1) {
			iSource += 2;
			hsl -= 2;
		}
		
		/* The target host element might be larger than the converted java
		 * String and might have to be right or left justified. The padding
		 * code point is 0x0020 (space character). */
		int iTarget = offset;
		boolean flip = true;
		
		/* Pad with initial spaces if necessary */
		if (isJustifiedRight) {
			while (iTarget < (lastOffset - hsl)) {
				hostTarget[iTarget] = SPACE_UTF_16[(flip) ? 0 : 1];
				iTarget++;
				flip = !flip;
			}
		}
		
		/* Continue on with source content */
		while (iSource < hostSource.length && iTarget < lastOffset) {
			hostTarget[iTarget] = hostSource[iSource];
			iSource++;
			iTarget++;
		}
		
		/* Pad with final spaces if necessary */
		flip = true;
		if (!isJustifiedRight) {
			while (iTarget < lastOffset) {
				hostTarget[iTarget] = SPACE_UTF_16[(flip) ? 0 : 1];
				iTarget++;
				flip = !flip;
			}
		}
		
		return lastOffset;
	}

	/** Converts a host national string into a Java string.
	 * 
	 * @param cobolByteLength host byte length
	 * @param hostSource source host buffer
	 * @param offset offset in source host buffer
	 * @return offset after host buffer is read
	 * @throws CobolConversionException if conversion fails
	 */
	public static final String fromHostSingle(
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
					NATIONAL_CHARSET);
			/* Some low-value characters may have slipped into the resulting
			 * string. */
			if (javaString.indexOf("\0") != -1) {
				javaString = javaString.replace('\0', ' ');
			}
		} catch (UnsupportedEncodingException uee) {
			throw new CobolConversionException(
					"UnsupportedEncodingException:" + uee.getMessage());
		}
		
		return javaString;
	}
}
