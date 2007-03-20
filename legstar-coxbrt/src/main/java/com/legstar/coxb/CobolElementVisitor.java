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
package com.legstar.coxb;
import java.math.BigDecimal;
import java.util.Hashtable;

import com.legstar.coxb.convert.CobolConverters;
import com.legstar.host.HostException;

/**
 * This class is a super-class of all visitors (visitor pattern) involved in
 * converting java object trees instances to host data buffers.
 *
 * @author Fady Moussallam
 * 
*/
public abstract class CobolElementVisitor {
	
	/** The current host buffer used by visitor. */
	private byte[] mHostBytes;
	
	/** The current offset in host buffer. */
	private int mOffset;
	
	/** The set of converters to use for cobol elements. */
	private CobolConverters mCobolConverters;
	
	/** Children elements marked as custom variables. */
	private Hashtable < String, Object > mVariablesMap;

	/** Children elements giving variable size arrays dimensions. */
	private Hashtable < String, BigDecimal > mODOMap;
	
	/** No-arg constructor.
	 */
	public CobolElementVisitor() {
		mHostBytes = null;
		mOffset = 0;
		mCobolConverters = null;
		mVariablesMap = new Hashtable < String, Object >();
		mODOMap = new Hashtable < String, BigDecimal >();
	}

	/** Constructor for a given host buffer and converters set.
	 * @param hostBytes host buffer used by visitor
	 * @param offset offset in host buffer
	 * @param cobolConverters set of converters to use for cobol elements
	 */
	public CobolElementVisitor(final byte[] hostBytes,
			final int offset,
			final CobolConverters cobolConverters) {
		mHostBytes = hostBytes;
		mOffset = offset;
		mCobolConverters = cobolConverters;
		mVariablesMap = new Hashtable < String, Object >();
		mODOMap = new Hashtable < String, BigDecimal >();
	}

	/**
	 * Visit method of visitor pattern for complex elements. 
	 * @param ce complex element descriptor
	 * @throws HostException error while visiting
	 */
	public abstract void visit(ICobolComplexBinding ce)
		throws HostException;

	/**
	 * Visit method of visitor pattern for choice elements. 
	 * @param ce choice element descriptor
	 * @throws HostException error while visiting
	 */
	public abstract void visit(ICobolChoiceBinding ce)
		throws HostException;

	/**
	 * Visit method of visitor pattern for arrays of complex elements. 
	 * @param ce complex array element descriptor
	 * @throws HostException error while visiting
	 */
	public abstract void visit(ICobolArrayComplexBinding ce)
		throws HostException;

	/**
	 * Visit method of visitor pattern for single Strings. 
	 * @param ce String element descriptor
	 * @throws HostException error while visiting
	 */
	public abstract void visit(ICobolStringBinding ce)
		throws HostException;
	/**
	 * Visit method of visitor pattern for String arrays. 
	 * @param ce String array element descriptor
	 * @throws HostException error while visiting
	 */
	public abstract void visit(ICobolArrayStringBinding ce)
		throws HostException;
	
	/**
	 * Visit method of visitor pattern for single Nationals. 
	 * @param ce National element descriptor
	 * @throws HostException error while visiting
	 */
	public abstract void visit(ICobolNationalBinding ce)
		throws HostException;
	/**
	 * Visit method of visitor pattern for National arrays. 
	 * @param ce National array element descriptor
	 * @throws HostException error while visiting
	 */
	public abstract void visit(ICobolArrayNationalBinding ce)
		throws HostException;
	
	/**
	 * Visit method of visitor pattern for single zoned decimals. 
	 * @param ce Zoned decimal element descriptor
	 * @throws HostException error while visiting
	 */
	public abstract void visit(ICobolZonedDecimalBinding ce)
		throws HostException;
	/**
	 * Visit method of visitor pattern for zoned decimal arrays. 
	 * @param ce Zoned decimal array element descriptor
	 * @throws HostException error while visiting
	 */
	public abstract void visit(ICobolArrayZonedDecimalBinding ce)
		throws HostException;
	
	/**
	 * Visit method of visitor pattern for single packed decimals. 
	 * @param ce Packed decimal element descriptor
	 * @throws HostException error while visiting
	 */
	public abstract void visit(ICobolPackedDecimalBinding ce)
		throws HostException;
	/**
	 * Visit method of visitor pattern for packed decimal arrays. 
	 * @param ce Packed dcimal array element descriptor
	 * @throws HostException error while visiting
	 */
	public abstract void visit(ICobolArrayPackedDecimalBinding ce)
		throws HostException;
	
	/**
	 * Visit method of visitor pattern for single Binary elements. 
	 * @param ce Binary element descriptor
	 * @throws HostException error while visiting
	 */
	public abstract void visit(ICobolBinaryBinding ce)
		throws HostException;
	/**
	 * Visit method of visitor pattern for Binary arrays. 
	 * @param ce Binary array element descriptor
	 * @throws HostException error while visiting
	 */
	public abstract void visit(ICobolArrayBinaryBinding ce)
		throws HostException;
	
	/**
	 * Visit method of visitor pattern for single Float elements. 
	 * @param ce Float element descriptor
	 * @throws HostException error while visiting
	 */
	public abstract void visit(ICobolFloatBinding ce)
		throws HostException;
	/**
	 * Visit method of visitor pattern for Float arrays. 
	 * @param ce Float array element descriptor
	 * @throws HostException error while visiting
	 */
	public abstract void visit(ICobolArrayFloatBinding ce)
		throws HostException;
	
	/**
	 * Visit method of visitor pattern for single Double elements. 
	 * @param ce Double element descriptor
	 * @throws HostException error while visiting
	 */
	public abstract void visit(ICobolDoubleBinding ce)
		throws HostException;
	/**
	 * Visit method of visitor pattern for Double arrays. 
	 * @param ce Double array element descriptor
	 * @throws HostException error while visiting
	 */
	public abstract void visit(ICobolArrayDoubleBinding ce)
		throws HostException;
	
	/**
	 * Visit method of visitor pattern for single Octet streams. 
	 * @param ce Octet stream element descriptor
	 * @throws HostException error while visiting
	 */
	public abstract void visit(ICobolOctetStreamBinding ce)
		throws HostException;
	/**
	 * Visit method of visitor pattern for Octet stream arrays. 
	 * @param ce Octet stream element descriptor
	 * @throws HostException error while visiting
	 */
	public abstract void visit(ICobolArrayOctetStreamBinding ce)
		throws HostException;
	
	/**
	 * @return Returns the CobolConverters.
	 */
	public final CobolConverters getCobolConverters() {
		return mCobolConverters;
	}

	/**
	 * @param cobolConverters The CobolConverters to set.
	 */
	public final void setCobolConverters(
			final CobolConverters cobolConverters) {
		mCobolConverters = cobolConverters;
	}

	/**
	 * @return Returns the mHostBytes.
	 */
	public final byte[] getHostBytes() {
		return mHostBytes;
	}

	/**
	 * @param hostBytes The mHostBytes to set.
	 */
	public final void setHostBytes(final byte[] hostBytes) {
		mHostBytes = hostBytes;
	}

	/**
	 * @return Returns the mOffset.
	 */
	public final int getOffset() {
		return mOffset;
	}

	/**
	 * @param offset The mOffset to set.
	 */
	public final void setOffset(final int offset) {
		mOffset = offset;
	}

	/**
	 * @return the current variables map.
	 */
	public final Hashtable < String, Object > getVariablesMap() {
		return mVariablesMap;
	}

	/**
	 * @param variablesMap the variables map to set
	 */
	public final void setVariablesMap(
			final Hashtable < String, Object > variablesMap) {
		mVariablesMap = variablesMap;
	}

	/**
	 * @return the children elements giving variable size arrays dimensions
	 */
	public final Hashtable < String, BigDecimal > getODOMap() {
		return mODOMap;
	}

	/**
	 * @param map children elements giving variable size arrays dimensions
	 */
	public final void setODOMap(
			final Hashtable < String, BigDecimal > map) {
		mODOMap = map;
	}
	/**
	 * Returns the current number of occurences for arrays.
	 * @param ce cobol binding description for an array
	 * @return current number of items in this array.
	 * @throws HostException if current number of items cannot be determined
	 */
	public final int getCurrentOccurs(
			final ICobolBinding ce) throws HostException {
		
		/* If this is a variable size array, its current dimension is given by
		 * an ODO object. We do a lookup in the ODO map to get the current
		 * size.       */
		if (ce.getDependingOn() != null && ce.getDependingOn().length() > 0) {
			java.math.BigDecimal value =
				mODOMap.get(ce.getDependingOn());
			if (value == null) {
				/* If we can't find the object we depend on, we behave as a
				 * fixed size array */
				return ce.getMaxOccurs();
			}
			return value.intValue();
		}
		return ce.getMaxOccurs();
	}
}
