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
package com.legstar.cixs.coxb;

import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.convert.CobolConverters;

/**
 * This class groups parameters used by CIXWInvoker invoke method. A parameter
 * represents either the input or the output and is bound to a complex
 * element.
 * 
 * @author Fady Moussallam
 * 
 */
public class CIXSParameter {

	/** Set of Cobol to Java converters. */
	private CobolConverters mCobolConverters;
	
	/** A binding for a complex element.*/
	private ICobolComplexBinding mComplexBinding;
	
	/**
	 * Constructor from a given cobol context.
	 * @param cobolConverters set of Cobol to Java converters
	 */
	public CIXSParameter(final CobolConverters cobolConverters) {
		/* Select a conversion strategy */ 
		mCobolConverters =	cobolConverters;
		/* Not bound to a complex element yet */
		mComplexBinding = null;
	}

	/**
	 * Constructor for CIXW parameter set.
	 * 
	 * @param complexBinding binding for a complex element
	 * @param cobolConverters set of Cobol to Java converters
	 */
	public CIXSParameter(
			final ICobolComplexBinding  complexBinding,
			final CobolConverters cobolConverters) {
		mComplexBinding = complexBinding;
		mCobolConverters = cobolConverters;
	}
	
	/**
	 * @return Returns the set of Cobol Converters.
	 */
	public final CobolConverters getCobolConverters() {
		return mCobolConverters;
	}

	/**
	 * @param cobolConverters The set of Cobol Converters to set.
	 */
	public final void setCobolConverters(
			final CobolConverters cobolConverters) {
		mCobolConverters = cobolConverters;
	}

	/**
	 * @return the complex element binding for this parameter
	 */
	public final ICobolComplexBinding getComplexBinding() {
		return mComplexBinding;
	}

	/**
	 * @param complexBinding the complex element binding to set
	 */
	public final void setComplexBinding(
			final ICobolComplexBinding complexBinding) {
		mComplexBinding = complexBinding;
	}

}
