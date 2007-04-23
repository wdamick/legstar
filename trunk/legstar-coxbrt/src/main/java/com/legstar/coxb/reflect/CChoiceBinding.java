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
package com.legstar.coxb.reflect;

import java.math.BigDecimal;

import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.annotation.CobolElement;
import com.legstar.host.HostException;
import com.legstar.coxb.ICobolMarshalChoiceStrategy;
import com.legstar.coxb.ICobolUnmarshalChoiceStrategy;

/**
 * Represents a choice between 2 or more elements. A choice results from a cobol
 * REDEFINES clause exposed as an xs:choice in the corresponding XML schema
 *
 * @author Fady Moussallam
 * 
*/
public class CChoiceBinding
		extends CBinding
		implements ICobolChoiceBinding {

	/** List of alternative elements, all redefining the same cobol field. */
	private java.util.List < ICobolBinding > mAlternatives =
		new java.util.ArrayList < ICobolBinding >();
	
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
	 * A choice element gets created when a redefined item is encountered.
	 * The constructor gets invoked with the redefined item characteristics.
	 * 
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param jaxbObject the concrete JAXB object instance bound to this element
	 * @param parentBinding a reference to the parent binding if any
	 * @param cobolAnnotations the cobol annotations for this element
	 * @throws HostException if construction fails
	 */
	public CChoiceBinding(
			final String jaxbName,
			final String jaxbType,
			final Object jaxbObject,
			final CComplexBinding parentBinding,
			final CobolElement cobolAnnotations)
		throws HostException {
		
		super(jaxbName, jaxbType, jaxbObject, cobolAnnotations);
		mParentBinding = parentBinding;
	}

	/** {@inheritDoc} */
	public final void addAlternative(final ICobolBinding ce) {
		mAlternatives.add(ce);
	}
	
	/** {@inheritDoc} */
	public final java.util.List < ICobolBinding > getAlternativesList() {
		/* This version of a choice binding has no internal knowledge of
		 * the alternatives. The list must be built using addAlternative.*/
		return mAlternatives;
	}
	
	/** {@inheritDoc} */
	public final void prepareChildren() throws HostException {
	}
	
	/** {@inheritDoc} */
	public final void getValuesFromBoundObject() throws HostException {
	}
	
	/** {@inheritDoc} */
	public final void accept(final CobolElementVisitor cev)
		throws HostException {
		cev.visit(this);
	}
	
	/** {@inheritDoc} */
	public final void setBoundObjectValue(
			final int index) throws HostException {
	}
	
	/** The host length of an alternative is given by the maximum size of each
	 *  of the alternatives. */
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

	/**
	 * @return Returns the alternatives list.
	 */
	public final java.util.List < ICobolBinding > getAlternatives() {
		return mAlternatives;
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

	/** {@inheritDoc} */
	public final BigDecimal getNumericValue() throws HostException {
		throw (new HostException("Element " + getJavaName()
				+ "cannot return a numeric value"));
	}

	/** {@inheritDoc} */
	public final Object getValue() throws HostException {
		return getJaxbObject();
	}

	/**
	 * @return the parent binding element
	 */
	public final CComplexBinding getParentBinding() {
		return mParentBinding;
	}

	/** {@inheritDoc}  */
    public final ICobolMarshalChoiceStrategy getMarshalChoiceStrategy()
    		throws HostException {
    	if (mMarshalChoiceStrategy != null) {
    	   	return mMarshalChoiceStrategy;
    	}
    	/* See if an annotation describes the choice strategy */
    	String strategy = getCobolAnnotations().
    			marshalChoiceStrategyClassName();
    	if (strategy != null &&	strategy.length() > 0) {
    		/* Try to load the proposed strategy*/
			try {
				Class clazz = getClass().getClassLoader().loadClass(strategy);
				return (ICobolMarshalChoiceStrategy) clazz.newInstance();
			} catch (ClassNotFoundException e) {
				e.printStackTrace();
				throw new HostException(e.getMessage());
			} catch (IllegalAccessException e) {
				e.printStackTrace();
				throw new HostException(e.getMessage());
			} catch (InstantiationException e) {
				e.printStackTrace();
				throw new HostException(e.getMessage());
			}
    	}
    	return mMarshalChoiceStrategy;
    	
    }
 
    /** {@inheritDoc} */
	public final void setMarshalChoiceStrategy(
				final ICobolMarshalChoiceStrategy strategy) {
		mMarshalChoiceStrategy = strategy;
	}

	/** {@inheritDoc}  */
    public final ICobolUnmarshalChoiceStrategy getUnmarshalChoiceStrategy()
    		throws HostException {
    	if (mUnmarshalChoiceStrategy != null) {
    	   	return mUnmarshalChoiceStrategy;
    	}
    	/* See if an annotation describes the choice strategy */
    	String strategy = getCobolAnnotations().
    			unmarshalChoiceStrategyClassName();
    	if (strategy != null &&	strategy.length() > 0) {
    		/* Try to load the proposed strategy*/
			try {
				Class clazz = getClass().getClassLoader().loadClass(strategy);
				return (ICobolUnmarshalChoiceStrategy) clazz.newInstance();
			} catch (ClassNotFoundException e) {
				e.printStackTrace();
				throw new HostException(e.getMessage());
			} catch (IllegalAccessException e) {
				e.printStackTrace();
				throw new HostException(e.getMessage());
			} catch (InstantiationException e) {
				e.printStackTrace();
				throw new HostException(e.getMessage());
			}
    	}
    	return mUnmarshalChoiceStrategy;
    	
    }
 
    /** {@inheritDoc} */
	public final void setUnmarshalChoiceStrategy(
				final ICobolUnmarshalChoiceStrategy strategy) {
		mUnmarshalChoiceStrategy = strategy;
	}
}
