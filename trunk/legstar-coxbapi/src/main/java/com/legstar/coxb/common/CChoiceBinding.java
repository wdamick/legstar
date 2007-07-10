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
package com.legstar.coxb.common;

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.ICobolMarshalChoiceStrategy;
import com.legstar.coxb.ICobolUnmarshalChoiceStrategy;
import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.host.HostException;

/**
 * Represents a choice between 2 or more elements. A choice results from a cobol
 * REDEFINES clause exposed as an xs:choice in the corresponding XML schema
 *
 * @author Fady Moussallam
 * 
*/
public abstract class CChoiceBinding
		extends CBinding
		implements ICobolChoiceBinding {

	/** List of alternative elements, all redefining the same cobol field. */
	private java.util.List < ICobolBinding > mAlternatives =
		new java.util.ArrayList < ICobolBinding >();
	
    /** Instance of a class providing additional logic to select an alternative
     * within a choice element at marshaling (Java to Host) time. */
	private ICobolMarshalChoiceStrategy mMarshalChoiceStrategy;
    
    /** Instance of a class providing additional logic to select an alternative
     * within a choice element at unmarshaling (Host to Java) time. */
	private ICobolUnmarshalChoiceStrategy mUnmarshalChoiceStrategy;
    
	/**
	 * A choice element gets created when a redefined item is encountered.
	 * 
	 * @param bindingName the identifier for this binding
	 * @param cobolAnnotations the cobol annotations for the first alternative
	 * @param parentBinding a reference to the parent binding if any
	 */
	public CChoiceBinding(
			final String bindingName,
			final CobolElement cobolAnnotations,
			final ICobolComplexBinding parentBinding) {
		
		super(bindingName, null, null, cobolAnnotations, parentBinding);
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
	public final void accept(final CobolElementVisitor cev)
		throws HostException {
		cev.visit(this);
	}
	
	/** {@inheritDoc} */
	public final int calcByteLength() throws HostException {
		
		int byteLength = 0;
		for (ICobolBinding alt : mAlternatives) {
			int altByteLength = alt.calcByteLength();
			if (altByteLength > byteLength) {
				byteLength = altByteLength;
			}
		}
		return byteLength;
	}

	/** {@inheritDoc} */
	public final ICobolBinding getAlternativeByName(
			final String name) {
		/* Select the alternative from the list of alternatives */
		for (ICobolBinding alt : mAlternatives) {
			if (alt.getBindingName().compareTo(name) == 0) {
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

	/** {@inheritDoc}  */
    public final ICobolMarshalChoiceStrategy getMarshalChoiceStrategy()
    		throws HostException {
    	if (mMarshalChoiceStrategy == null) {
	    	mMarshalChoiceStrategy = (ICobolMarshalChoiceStrategy)
	    		loadStrategy(getMarshalChoiceStrategyClassName());
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
    	if (mUnmarshalChoiceStrategy == null) {
	    	mUnmarshalChoiceStrategy = (ICobolUnmarshalChoiceStrategy)
				loadStrategy(getUnmarshalChoiceStrategyClassName());
    	}
    	return mUnmarshalChoiceStrategy;
    }
    
    /** {@inheritDoc} */
	public final void setUnmarshalChoiceStrategy(
				final ICobolUnmarshalChoiceStrategy strategy) {
		mUnmarshalChoiceStrategy = strategy;
	}

    /** {@inheritDoc} */
    public final Object getParentJaxbObject() throws HostException {
    	return getParentBinding().getObjectValue(
				getParentBinding().getJaxbType());
    }
 
	/**
     * Try to load the proposed strategy.
     * @param strategyClassName the class name for the strategy implementation
     * @return an instance of the class
     * @throws HostException if loading fails
     */
    private Object loadStrategy(
    		final String strategyClassName) throws HostException {
    	if (strategyClassName != null && strategyClassName.length() > 0) {
			try {
				Class clazz = getClass().getClassLoader().
				loadClass(strategyClassName);
				return (ICobolUnmarshalChoiceStrategy) clazz.newInstance();
			} catch (ClassNotFoundException e) {
				throw new HostException(e);
			} catch (IllegalAccessException e) {
				throw new HostException(e);
			} catch (InstantiationException e) {
				throw new HostException(e);
			}
    	} else {
    		return null;
    	}
    }
 
}
