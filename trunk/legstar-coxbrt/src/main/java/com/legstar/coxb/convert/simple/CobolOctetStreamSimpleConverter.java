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

import java.util.ArrayList;
import java.util.List;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolArrayOctetStreamBinding;
import com.legstar.coxb.ICobolOctetStreamBinding;
import com.legstar.coxb.convert.ICobolOctetStreamConverter;
import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

/**
 * This is a concrete implementation of marshal/unmarshal operations of java
 * byte arrays to cobol octet streams.
 *
 * @author Fady Moussallam
 * 
 */
public class CobolOctetStreamSimpleConverter extends CobolSimpleConverter
  implements ICobolOctetStreamConverter {
	
	/**
	 * @param cobolContext the Cobol compiler parameters in effect
	 */
	public CobolOctetStreamSimpleConverter(final CobolContext cobolContext) {
		super(cobolContext);
	}
	
	/** {@inheritDoc} */
	public final int toHost(
			final ICobolOctetStreamBinding ce,
			final byte[] hostTarget,
			final int offset)
		throws HostException {
		int newOffset = 0;
		try {
			newOffset = toHostSingle(ce.getByteArrayValue(),
				ce.getByteLength(),
				hostTarget,
				offset);
		} catch (CobolConversionException e) {
			throwHostException(ce, e);
		}
		return newOffset;
	}
	
	/** {@inheritDoc} */
	public final int toHost(
			final ICobolArrayOctetStreamBinding ce,
			final byte[] hostTarget,
			final int offset,
			final int currentOccurs)
		throws HostException {
		int newOffset = offset;
		try {
			for (byte[] javaSource : ce.getByteArrayList()) {
				newOffset = toHostSingle(javaSource,
						ce.getByteLength(),
						hostTarget,
						newOffset);
			}
			/* If necessary, fill in the array with missing items */
			for (int i = ce.getByteArrayList().size();
							i < currentOccurs; i++) {
				newOffset = toHostSingle(new byte[] { 0 },
						ce.getByteLength(),
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
			final ICobolOctetStreamBinding ce,
			final byte[] hostSource,
			final int offset)
		throws HostException {
		int newOffset = offset;
		try {
			byte[] javaBytes = fromHostSingle(ce.getByteLength(),
					hostSource,
					newOffset);
			ce.setByteArrayValue(javaBytes);
			newOffset += ce.getByteLength();
		} catch (CobolConversionException e) {
			throwHostException(ce, e);
		}
		return newOffset;
	}
	
	/** {@inheritDoc} */
	public final int fromHost(
			final ICobolArrayOctetStreamBinding ce,
			final byte[] hostSource,
			final int offset,
			final int currentOccurs)
		throws HostException {
		List < byte[] > lArray = new ArrayList < byte[] >();
		int newOffset = offset;
		try {
			for (int i = 0; i < currentOccurs; i++) {
				byte[] javaBytes = fromHostSingle(ce.getByteLength(),
						hostSource,
						newOffset);
				lArray.add(javaBytes);
				newOffset += ce.getByteLength();
			}
			ce.setByteArrayList(lArray);
		} catch (CobolConversionException e) {
			throwHostException(ce, e);
		}
		return newOffset;
	}

	/**
	 *  Converts a Java byte array to a host octet stream.
	 * 
	 * @param javaBytes java byte array to convert
	 * @param cobolByteLength host byte length
	 * @param hostTarget target host buffer
	 * @param offset offset in target host buffer
	 * @return offset after host buffer is updated
	 * @throws CobolConversionException if conversion fails
	 */
	public static final int toHostSingle(
			final byte[] javaBytes,
			final int cobolByteLength,
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
		byte[] localBytes = javaBytes;
		if (localBytes == null) {
			localBytes = new byte[cobolByteLength];
		}
		/**
		 * HostData is obtained by moving the java byte array content to the
		 * target host buffer. If there are more java bytes than space in the
		 * target buffer, data is truncated.
		 */
		for (int i = 0; i < localBytes.length && i < cobolByteLength; i++) {
			hostTarget[offset + i] = localBytes[i];
		}
		return lastOffset;
	}

	/** Converts a host octet stream to a java byte array.
	 * 
	 * @param cobolByteLength host byte length
	 * @param hostSource source host buffer
	 * @param offset offset in source host buffer
	 * @return offset after host buffer is read
	 * @throws CobolConversionException if conversion fails
	 */
	public static final byte[] fromHostSingle(
			final int cobolByteLength,
			final byte[] hostSource,
			final int offset) throws CobolConversionException {
		
		/* Check that we are still within the host source range */
		int lastOffset = offset + cobolByteLength;
		if (lastOffset > hostSource.length) {
			throw (new CobolConversionException(
					"Attempt to read past end of host source buffer",
					new HostData(hostSource), offset, cobolByteLength));
		}

		/* The Java byte array is the exact byte by byte copy of the host octet
		 * stream. */
		byte[] javaBytes = new byte[cobolByteLength];
		for (int i = 0; i < cobolByteLength; i++) {
			javaBytes[i] = hostSource[offset + i];
		}
		
		return javaBytes;
	}
}
