/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb;
import java.util.Hashtable;

import com.legstar.coxb.convert.CobolConverters;
import com.legstar.coxb.host.HostException;

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

	/** No-arg constructor.
	 */
	public CobolElementVisitor() {
		mHostBytes = null;
		mOffset = 0;
		mCobolConverters = null;
		mVariablesMap = new Hashtable < String, Object >();
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
	 * Store the value of a binding in the custom variables map for
	 * later referral by custom code.
	 * @param binding the current binding
	 * @throws HostException if value cannot be stored
	 */
	public final void storeCustomVariable(
			final ICobolBinding binding) throws HostException {
		getVariablesMap().put(
				binding.getBindingName(),
				binding.getObjectValue(binding.getJaxbType()));
		
	}

}
