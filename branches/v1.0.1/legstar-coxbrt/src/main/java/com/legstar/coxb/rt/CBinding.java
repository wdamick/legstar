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

import com.legstar.coxb.CobolType;
import com.legstar.coxb.ICobolBinding;
import com.legstar.host.HostException;

import java.math.BigDecimal;

/**
 * This class is a superclass of all other element types implementing
 * binding between a java type and cobol.
 *
 * @author Fady Moussallam
 * 
*/
public abstract class CBinding implements ICobolBinding {

	/** Cobol element name. */
	private String mCobolName = "";
	
	/** Cobol element type. */
	private CobolType mCobolType = CobolType.ALPHANUMERIC_ITEM;
	
	/** String justification. */
	private boolean mIsJustifiedRight = false;
	
	/** Numerics total number of digits (including fractional). */
	private int mTotalDigits = 0;
	
	/** Numerics fractional number of digits. */
	private int mFractionDigits = 0;

	/** Numerics signed or unsigned. */
	private boolean mIsSigned = false;

	/** Numerics sign in leading byte or trailing byte. */
	private boolean mIsSignLeading = true;

	/** Numerics sign occupies a separate byte. */
	private boolean mIsSignSeparate = false;
	
	/** Arrays minimum number of occurences. */
	private int mMinOccurs = 0;

	/** Arrays maximum number of occurences. */
	private int mMaxOccurs = 0;

	/** Cobol element giving array actual size. */
	private String mDependingOn = "";

	/** Determines the size of a variable size array. */
	private boolean mIsODOObject = false;

	/** Cobol element sharing same memory location. */
	private String mRedefines = "";

	/** Element is redefined by at least one other element. */
	private boolean mIsRedefined = false;

	/** True if this element is used in custom code. */
	private boolean mIsCustomVariable = false;

	/** Java property name bound to this cobol element. */
	private String mJavaName;

	/** Java property type bound to this cobol element. */
	private String mJavaType;

	/** Name of class providing logic to help with alternative selection. 
	 *  Host to Java. */
	private String mUnmarshalChoiceStrategyClassName;

	/** Name of class providing logic to help with alternative selection. 
	 *  Java to Host. */
	private String mMarshalChoiceStrategyClassName;
	
	/**
	 * Constructor for a cobol java binding.
	 * 
	 * @param javaName the name of the bound java property
	 * @param javaType the type of the bound java property
	 */
	public CBinding(
			final String javaName,
			final String javaType) {
		setJavaName(javaName);
		setJavaType(javaType);
	}

	/**
	 * @return the Cobol element name
	 */
	public final String getCobolName() {
		return mCobolName;
	}

	/**
	 * @param cobolName the Cobol element name to set
	 */
	public final void setCobolName(final String cobolName) {
		mCobolName = cobolName;
	}

	/**
	 * @return the fractional number of digits
	 */
	public final int getFractionDigits() {
		return mFractionDigits;
	}

	/**
	 * @param fractionDigits the fractional number of digits to set
	 */
	public final void setFractionDigits(final int fractionDigits) {
		mFractionDigits = fractionDigits;
	}

	/**
	 * @return true if element is used in custom code
	 */
	public final boolean isCustomVariable() {
		return mIsCustomVariable;
	}

	/**
	 * @param isCustomVariable true if element is used in custom code
	 */
	public final void setIsCustomVariable(
			final boolean isCustomVariable) {
		mIsCustomVariable = isCustomVariable;
	}

	/**
	 * @return true if String is right justified
	 */
	public final boolean isJustifiedRight() {
		return mIsJustifiedRight;
	}

	/**
	 * @param isJustifiedRight true if String is right justified
	 */
	public final void setIsJustifiedRight(
			final boolean isJustifiedRight) {
		mIsJustifiedRight = isJustifiedRight;
	}

	/**
	 * @return true if array is variable size
	 */
	public final boolean isODOObject() {
		return mIsODOObject;
	}

	/**
	 * @param isODOObject true if array is variable size
	 */
	public final void setIsODOObject(final boolean isODOObject) {
		mIsODOObject = isODOObject;
	}

	/**
	 * @return true if redefined by at least one other element
	 */
	public final boolean isRedefined() {
		return mIsRedefined;
	}

	/**
	 * @param isRedefined true if redefined by at least one other element
	 */
	public final void setIsRedefined(final boolean isRedefined) {
		mIsRedefined = isRedefined;
	}

	/**
	 * @return true if signed
	 */
	public final boolean isSigned() {
		return mIsSigned;
	}

	/**
	 * @param isSigned true if signed
	 */
	public final void setIsSigned(final boolean isSigned) {
		mIsSigned = isSigned;
	}

	/**
	 * @return true if sign in leading byte
	 */
	public final boolean isSignLeading() {
		return mIsSignLeading;
	}

	/**
	 * @param isSignLeading true if sign in leading byte
	 */
	public final void setIsSignLeading(final boolean isSignLeading) {
		mIsSignLeading = isSignLeading;
	}

	/**
	 * @return true if sign occupies a separate byte
	 */
	public final boolean isSignSeparate() {
		return mIsSignSeparate;
	}

	/**
	 * @param isSignSeparate true if sign occupies a separate byte
	 */
	public final void setIsSignSeparate(final boolean isSignSeparate) {
		mIsSignSeparate = isSignSeparate;
	}

	/**
	 * @return the maximum number of occurences
	 */
	public final int getMaxOccurs() {
		return mMaxOccurs;
	}

	/**
	 * @param maxOccurs the maximum number of occurences to set
	 */
	public final void setMaxOccurs(final int maxOccurs) {
		mMaxOccurs = maxOccurs;
	}

	/**
	 * @return the Cobol element giving array actual size
	 */
	public final String getDependingOn() {
		return mDependingOn;
	}

	/**
	 * @param dependingOn the Cobol element giving array actual size to set
	 */
	public final void setDependingOn(final String dependingOn) {
		mDependingOn = dependingOn;
	}

	/**
	 * @return the minimum number of occurences
	 */
	public final int getMinOccurs() {
		return mMinOccurs;
	}

	/**
	 * @param minOccurs the minimum number of occurences to set
	 */
	public final void setMinOccurs(final int minOccurs) {
		mMinOccurs = minOccurs;
	}

	/**
	 * @return the Cobol element sharing same memory location
	 */
	public final String getRedefines() {
		return mRedefines;
	}

	/**
	 * @param redefines Cobol element sharing same memory location to set
	 */
	public final void setRedefines(final String redefines) {
		mRedefines = redefines;
	}

	/**
	 * @return the total number of digits
	 */
	public final int getTotalDigits() {
		return mTotalDigits;
	}

	/**
	 * @param totalDigits the total number of digits to set
	 */
	public final void setTotalDigits(final int totalDigits) {
		mTotalDigits = totalDigits;
	}

	/**
	 * @return the Cobol element type
	 */
	public final CobolType getCobolType() {
		return mCobolType;
	}

	/**
	 * @param type the Cobol element type to set
	 */
	public final void setCobolType(final CobolType type) {
		mCobolType = type;
	}

	/**
	 * @return the Java property name bound to this cobol element
	 */
	public final String getJavaName() {
		return mJavaName;
	}

	/**
	 * @param javaName the Java property name bound to this cobol element to set
	 */
	public final void setJavaName(final String javaName) {
		mJavaName = javaName;
	}
	
	/**
	 * All types that are able to return a numeric value share this method. This
	 * allows variable size arrays dimensions to be determined by any type of
	 * numeric element.
	 * 
	 * @return the current numeric value as a BigDecimal
	 * @throws HostException if this element cannot produce a numeric value
	 */
	public abstract BigDecimal getNumericValue() throws HostException;

	/**
	 * In a general fashion, most elements are associated with values.
	 * 
	 * @return the current value as an Object
	 * @throws HostException if this element cannot produce a value
	 */
	public abstract Object getValue() throws HostException;

	/**
	 * @return the Java property type bound to this cobol element
	 */
	public final String getJavaType() {
		return mJavaType;
	}

	/**
	 * @param javaType the Java property type bound to this cobol element to set
	 */
	public final void setJavaType(final String javaType) {
		mJavaType = javaType;
	}

	/**
	 * @return the name of a class providing additional logic to select an
	 * alternative within a choice element at marshaling (Java to Host) time.
	 */
	public final String getMarshalChoiceStrategyClassName() {
		return mMarshalChoiceStrategyClassName;
	}

	/**
	 * @param marshalChoiceStrategyClassName the name of a class providing
	 * additional logic to select an alternative within a choice element at
	 * marshaling (Java to Host) time.
	 */
	public final void setMarshalChoiceStrategyClassName(
			final String marshalChoiceStrategyClassName) {
		mMarshalChoiceStrategyClassName = marshalChoiceStrategyClassName;
	}

	/**
	 * @return the name of a class providing additional logic to select an
	 * alternative within a choice element at unmarshaling (Host to Java) time.
	 */
	public final String getUnmarshalChoiceStrategyClassName() {
		return mUnmarshalChoiceStrategyClassName;
	}

	/**
	 * @param unmarshalChoiceStrategyClassName the the name of a class providing
	 * additional logic to select an alternative within a choice element at
	 * unmarshaling (Host to Java) time.
	 */
	public final void setUnmarshalChoiceStrategyClassName(
			final String unmarshalChoiceStrategyClassName) {
		mUnmarshalChoiceStrategyClassName = unmarshalChoiceStrategyClassName;
	}
}
