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

import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.CobolType;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.annotation.CobolElement;
import com.legstar.host.HostException;

/**
 * This class is a superclass of all other JAXB element types. It
 * holds a reference to a JAXB concrete object.
 *
 * @author Fady Moussallam
 * 
*/
public abstract class CBinding implements ICobolBinding {

	/** This is a reference to a concrete JAXB object bound to this cobol
	 *  element. */
	private Object mJaxbObject;

	/** This is the name of the property as it appears in the getter/setter
	 *  methods of the JAXB object. */
	private String mJaxbName;

	/** This is the java type of the JAXB property. */
	private String mJaxbType;
	
	/** These are the cobol annotations describing this element. */
	private CobolElement mCobolAnnotations;
	
	/**
	 * Constructor for a Cobol element bound to a JAXB object or property.
	 * @param jaxbName the name of the associated JAXB property
	 * @param jaxbType the java type of the associated JAXB property
	 * @param jaxbObject the concrete JAXB object instance bound to this element
	 * @param cobolAnnotations the cobol annotations for this element
	 * @throws HostException if construction fails
	 */
	public CBinding(
			final String jaxbName,
			final String jaxbType,
			final Object jaxbObject,
			final CobolElement cobolAnnotations)
		throws HostException {
		
		mJaxbObject = jaxbObject;
		mJaxbName = jaxbName;
		mJaxbType = jaxbType;
		mCobolAnnotations = cobolAnnotations;
	}

	/** {@inheritDoc} */
	public abstract void accept(CobolElementVisitor cev) throws HostException;

	/** {@inheritDoc} */
	public abstract int getByteLength() throws HostException;

	/**
	 * Returns the name of the associated JAXB property.
	 * @return the JAXB property name 
	 */
	public final String getJavaName() {
		return mJaxbName;
	}

	/**
	 * Sets the JAXB property name.
	 * @param jaxbName the name of the associated JAXB property 
	 */
	public final void setName(final String jaxbName) {
		mJaxbName = jaxbName;
	}

	/**
	 * Returns the java type of the associated JAXB property.
	 * @return the JAXB property type
	 */
	public final String getType() {
		return mJaxbType;
	}

	/** 
	 * Sets the JAXB property type.
	 * @param jaxbType the type of the associated JAXB property 
	 */
	public final void setType(final String jaxbType) {
		mJaxbType = jaxbType;
	}

	/**
	 *  Returns the reference to the JAXB object.
	 *  @return the associated JAXB object
	 */
	public final Object getJaxbObject() {
		return mJaxbObject;
	}

	/** 
	 * Sets the reference to the JAXB object.
	 * @param jaxbObject the associated JAXB object
	 */
	public final void setJaxbObject(final Object jaxbObject) {
		mJaxbObject = jaxbObject;
	}

	/**
	 * @return the cobol annotations for this element.
	 */
	public final CobolElement getCobolAnnotations() {
		return mCobolAnnotations;
	}

	/** {@inheritDoc} */
	public final String getCobolName() {
		return mCobolAnnotations.cobolName();
	}
	
	/** {@inheritDoc} */
	public final CobolType getCobolType() {
		return mCobolAnnotations.type();
	}
	
	/** {@inheritDoc} */
	public final int getTotalDigits() {
		return mCobolAnnotations.totalDigits();
	}
	
	/** {@inheritDoc} */
	public final int getFractionDigits() {
		return mCobolAnnotations.fractionDigits();
	}
	
	/** {@inheritDoc} */
	public final boolean isSigned() {
		return mCobolAnnotations.isSigned();
	}
	
	/** {@inheritDoc} */
	public final boolean isSignLeading() {
		return mCobolAnnotations.isSignLeading();
	}
	
	/** {@inheritDoc} */
	public final boolean isSignSeparate() {
		return mCobolAnnotations.isSignSeparate();
	}

	/** {@inheritDoc} */
	public final boolean isJustifiedRight() {
		return mCobolAnnotations.isJustifiedRight();
	}
	/** {@inheritDoc} */
	public final int getMinOccurs() {
		return mCobolAnnotations.minOccurs();
	}

	/** {@inheritDoc} */
	public final int getMaxOccurs() throws HostException {
		return mCobolAnnotations.maxOccurs();
	}

	/** {@inheritDoc} */
	public final String getDependingOn() {
		return mCobolAnnotations.dependingOn();
	}

	/** {@inheritDoc} */
	public final boolean isODOObject() {
		return mCobolAnnotations.isODOObject();
	}

	/** {@inheritDoc} */
	public final String getRedefines() {
		return mCobolAnnotations.redefines();
	}

	/** {@inheritDoc} */
	public final boolean isRedefined() {
		return mCobolAnnotations.isRedefined();
	}

	/** {@inheritDoc} */
	public final boolean isCustomVariable() {
		return mCobolAnnotations.isCustomVariable();
	}

	/** {@inheritDoc} */
	public final String getUnmarshalChoiceStrategyClassName() {
		return mCobolAnnotations.unmarshalChoiceStrategyClassName();
	}

	/** {@inheritDoc} */
	public final String getMarshalChoiceStrategyClassName() {
		return mCobolAnnotations.marshalChoiceStrategyClassName();
	}

	/** {@inheritDoc} */
	public final String getJavaType() {
		return mJaxbType;
	}

	/**
	 * @param jaxbType java type of the property bound to the cobol element
	 */
	public final void setJavaType(
			final String jaxbType) {
		mJaxbType = jaxbType;
	}

}
