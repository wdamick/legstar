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
package com.legstar.coxb.rt;

import java.math.BigDecimal;

import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolMarshalChoiceStrategy;
import com.legstar.coxb.ICobolUnmarshalChoiceStrategy;
import com.legstar.host.HostException;

/**
 * This class is a superclass of choice element types (redefines) implementing
 * binding between a java type and cobol.
 *
 * @author Fady Moussallam
 * 
*/
public abstract class CChoiceBinding
	extends CBinding implements ICobolChoiceBinding {

	/** Ordered list of alternative elements. */
	private java.util.List < ICobolBinding > mAlternatives;

	/** When element bound belongs to a hierachy, this references the parent 
	 * binding. */
	private CComplexBinding mParentBinding;
	
    /** Instance of a class providing additional logic to select an alternative
     * within a choice element at marshaling (Java to Host) time. */
	private ICobolMarshalChoiceStrategy mMarshalChoiceStrategy;
    
    /** Instance of a class providing additional logic to select an alternative
     * within a choice element at unmarshaling (Host to Java) time. */
	private ICobolUnmarshalChoiceStrategy mUnmarshalChoiceStrategy;
    
	/**
	 * Constructor for a cobol simple element to java binding.
	 * 
	 * @param javaName the name of the bound java property
	 * @param javaType the type of the bound java property
	 * @param parentBinding a reference to the parent binding if any
	 */
	public CChoiceBinding(
			final String javaName,
			final String javaType,
			final CComplexBinding parentBinding) {
		super(javaName, javaType);
		mAlternatives = new java.util.ArrayList < ICobolBinding >();
		mParentBinding = parentBinding;
	}
	
	/** {@inheritDoc} */
	public final void accept(final CobolElementVisitor cev)
		throws HostException {
		cev.visit(this);
	}
	
	/** {@inheritDoc} */
	public final int getByteLength() throws HostException {
		int byteLength = 0;
		for (ICobolBinding alt : mAlternatives) {
			int altByteLength = alt.getByteLength();
			if (altByteLength > byteLength) {
				byteLength = altByteLength;
			}
		}
		return byteLength;
	}
	
	/** {@inheritDoc} */
	public final java.util.List < ICobolBinding > getAlternativesList() {
		return mAlternatives;
	}

	/** {@inheritDoc} */
	public final void addAlternative(final ICobolBinding ce) {
		mAlternatives.add(ce);
	}
	
	/**
	 * @param alternatives the alternatives list to set
	 */
	public final void setlternativesList(
			final java.util.List < ICobolBinding > alternatives) {
		mAlternatives = alternatives;
	}

	/** {@inheritDoc} */
	public final ICobolBinding getAlternativeByJavaName(
			final String javaName) {
		/* Select the alternative from the list of alternatives */
		for (ICobolBinding alt : mAlternatives) {
			if (alt.getJavaName().compareTo(javaName) == 0) {
				return alt;
			}
		}
		return null;
	}

	/** {@inheritDoc} */
	public final ICobolBinding getAlternativeByCobolName(
			final String cobolName) {
		/* Select the alternative from the list of alternatives */
		for (ICobolBinding alt : mAlternatives) {
			if (alt.getCobolName().compareTo(cobolName) == 0) {
				return alt;
			}
		}
		return null;
	}

	/**
	 * @return the parent binding element
	 */
	public final CComplexBinding getParentBinding() {
		return mParentBinding;
	}

	/**
	 * @param parentBinding the arent binding element to set
	 */
	public final void setParentBinding(
			final CComplexBinding parentBinding) {
		mParentBinding = parentBinding;
	}

	/** {@inheritDoc} */
	public final BigDecimal getNumericValue() throws HostException {
		throw (new HostException("Element " + getJavaName()
				+ "cannot return a numeric value"));
	}
    /** {@inheritDoc} */
    public final ICobolMarshalChoiceStrategy getMarshalChoiceStrategy() {
    	return mMarshalChoiceStrategy;
    	
    }
    
    /** {@inheritDoc} */
	public final void setMarshalChoiceStrategy(
				final ICobolMarshalChoiceStrategy strategy) {
		mMarshalChoiceStrategy = strategy;
	}

	/** {@inheritDoc} */
    public final ICobolUnmarshalChoiceStrategy getUnmarshalChoiceStrategy() {
    	return mUnmarshalChoiceStrategy;
    	
    }
    
    /** {@inheritDoc} */
	public final void setUnmarshalChoiceStrategy(
				final ICobolUnmarshalChoiceStrategy strategy) {
		mUnmarshalChoiceStrategy = strategy;
	}
}
