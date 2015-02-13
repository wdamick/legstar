/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.common;

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.util.PictureUtil;

/**
 * This class is a superclass of all other element types implementing binding
 * between a java type and cobol. The Cobol properties are the exact equivalent
 * of the CobolElement annotation.
 * 
 * @author Fady Moussallam
 * 
 */
public abstract class CBinding implements ICobolBinding {

    /** Name of this binding element. */
    private String mBindingName;

    /** Parent binding (null if binding is root). */
    private ICobolComplexBinding mParentBinding;

    /** Java property name bound to this cobol element. */
    private String mJaxbName;

    /** Java property type bound to this cobol element. */
    private Class < ? > mJaxbType;

    /** Cobol element name. */
    private String mCobolName = "";

    /** Cobol element type. */
    private CobolType mCobolType = CobolType.ALPHANUMERIC_ITEM;

    /** Cobol element length in bytes (unknown by default). */
    private int mByteLength = 0;

    /** String justification. */
    private boolean mIsJustifiedRight = false;

    /** Numerics total number of digits (including fractional). */
    private int mTotalDigits = 0;

    /** Numerics fractional number of digits. */
    private int mFractionDigits = 0;

    /** Numerics signed or unsigned. */
    private boolean mIsSigned = false;

    /** Numerics sign in leading byte or trailing byte. */
    private boolean mIsSignLeading = false;

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

    /**
     * Name of class providing logic to help with alternative selection. Host to
     * Java.
     */
    private String mUnmarshalChoiceStrategyClassName;

    /**
     * Name of class providing logic to help with alternative selection. Java to
     * Host.
     */
    private String mMarshalChoiceStrategyClassName;

    /** Level in the hierarchy this element was parsed from. */
    private int mLevelNumber = 1;

    /** Cobol picture clause. */
    private String mPicture = "";

    /** Cobol usage. */
    private String mUsage = "";

    /** Cobol default value. */
    private String mDefaultValue = "";

    /** Line number in the original source file this element was parsed from. */
    private int mSrceLine = 1;

    /**
     * Bindings can be dynamically built or generated. Generated bindings are
     * faster but they lack some metadata such as picture or usage.
     */
    private boolean mGeneratedBinding = false;

    /**
     * Constructor for a cobol element to java binding. Dynamic bindings are
     * built from cobol annotations.
     * 
     * @param name the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param cobolAnnotations the cobol annotations for this element
     * @param parentBinding a reference to the parent binding
     */
    public CBinding(final String name, final String jaxbName,
            final Class < ? > jaxbType, final CobolElement cobolAnnotations,
            final ICobolComplexBinding parentBinding) {
        setBindingName(name);
        setJaxbName(jaxbName);
        setJaxbType(jaxbType);
        setParentBinding(parentBinding);
        if (cobolAnnotations == null) {
            mGeneratedBinding = true;
            return;
        }
        setCobolName(cobolAnnotations.cobolName());
        setCobolType(cobolAnnotations.type());
        setIsJustifiedRight(cobolAnnotations.isJustifiedRight());
        setTotalDigits(cobolAnnotations.totalDigits());
        setFractionDigits(cobolAnnotations.fractionDigits());
        setIsSigned(cobolAnnotations.isSigned());
        setIsSignLeading(cobolAnnotations.isSignLeading());
        setIsSignSeparate(cobolAnnotations.isSignSeparate());
        setMinOccurs(cobolAnnotations.minOccurs());
        setMaxOccurs(cobolAnnotations.maxOccurs());
        setDependingOn(cobolAnnotations.dependingOn());
        setIsODOObject(cobolAnnotations.isODOObject());
        setRedefines(cobolAnnotations.redefines());
        setIsRedefined(cobolAnnotations.isRedefined());
        setIsCustomVariable(cobolAnnotations.isCustomVariable());
        setUnmarshalChoiceStrategyClassName(cobolAnnotations
                .unmarshalChoiceStrategyClassName());
        setMarshalChoiceStrategyClassName(cobolAnnotations
                .marshalChoiceStrategyClassName());
        setLevelNumber(cobolAnnotations.levelNumber());
        setPicture(PictureUtil.preparePicture(cobolAnnotations.picture()));
        setUsage(cobolAnnotations.usage());
        setDefaultValue(cobolAnnotations.value());
        setSrceLine(cobolAnnotations.srceLine());

    }

    /**
     * @return the Cobol element name
     */
    public String getCobolName() {
        return mCobolName;
    }

    /**
     * @param cobolName the Cobol element name to set
     */
    public void setCobolName(final String cobolName) {
        mCobolName = cobolName;
    }

    /**
     * @return the Cobol element type
     */
    public CobolType getCobolType() {
        return mCobolType;
    }

    /**
     * @param type the Cobol element type to set
     */
    public void setCobolType(final CobolType type) {
        mCobolType = type;
    }

    /**
     * The host byte length calculation is not performed at construction time.
     * This is because some information such as children of complex elements or
     * dimension of arrays might not be known at construction time.
     * 
     * @return the Cobol element length in bytes
     */
    public int getByteLength() {
        if (mByteLength == 0) {
            mByteLength = calcByteLength();
        }
        return mByteLength;
    }

    /**
     * @param byteLength the Cobol element length in bytes to set
     */
    public void setByteLength(final int byteLength) {
        mByteLength = byteLength;
    }

    /**
     * @return true if String is right justified
     */
    public boolean isJustifiedRight() {
        return mIsJustifiedRight;
    }

    /**
     * @param isJustifiedRight true if String is right justified
     */
    public void setIsJustifiedRight(final boolean isJustifiedRight) {
        mIsJustifiedRight = isJustifiedRight;
    }

    /**
     * @return the total number of digits
     */
    public int getTotalDigits() {
        return mTotalDigits;
    }

    /**
     * @param totalDigits the total number of digits to set
     */
    public void setTotalDigits(final int totalDigits) {
        mTotalDigits = totalDigits;
    }

    /**
     * @return the fractional number of digits
     */
    public int getFractionDigits() {
        return mFractionDigits;
    }

    /**
     * @param fractionDigits the fractional number of digits to set
     */
    public void setFractionDigits(final int fractionDigits) {
        mFractionDigits = fractionDigits;
    }

    /**
     * @return true if signed
     */
    public boolean isSigned() {
        return mIsSigned;
    }

    /**
     * @param isSigned true if signed
     */
    public void setIsSigned(final boolean isSigned) {
        mIsSigned = isSigned;
    }

    /**
     * @return true if sign in leading byte
     */
    public boolean isSignLeading() {
        return mIsSignLeading;
    }

    /**
     * @param isSignLeading true if sign in leading byte
     */
    public void setIsSignLeading(final boolean isSignLeading) {
        mIsSignLeading = isSignLeading;
    }

    /**
     * @return true if sign occupies a separate byte
     */
    public boolean isSignSeparate() {
        return mIsSignSeparate;
    }

    /**
     * @param isSignSeparate true if sign occupies a separate byte
     */
    public void setIsSignSeparate(final boolean isSignSeparate) {
        mIsSignSeparate = isSignSeparate;
    }

    /**
     * @return the minimum number of occurences
     */
    public int getMinOccurs() {
        return mMinOccurs;
    }

    /**
     * @param minOccurs the minimum number of occurences to set
     */
    public void setMinOccurs(final int minOccurs) {
        mMinOccurs = minOccurs;
    }

    /**
     * @return the maximum number of occurences
     */
    public int getMaxOccurs() {
        return mMaxOccurs;
    }

    /**
     * @param maxOccurs the maximum number of occurences to set
     */
    public void setMaxOccurs(final int maxOccurs) {
        mMaxOccurs = maxOccurs;
    }

    /**
     * @return the Cobol element giving array actual size
     */
    public String getDependingOn() {
        return mDependingOn;
    }

    /**
     * @param dependingOn the Cobol element giving array actual size to set
     */
    public void setDependingOn(final String dependingOn) {
        mDependingOn = dependingOn;
    }

    /**
     * @return true if array is variable size
     */
    public boolean isODOObject() {
        return mIsODOObject;
    }

    /**
     * @param isODOObject true if array is variable size
     */
    public void setIsODOObject(final boolean isODOObject) {
        mIsODOObject = isODOObject;
    }

    /**
     * @return the Cobol element sharing same memory location
     */
    public String getRedefines() {
        return mRedefines;
    }

    /**
     * @param redefines Cobol element sharing same memory location to set
     */
    public void setRedefines(final String redefines) {
        mRedefines = redefines;
    }

    /**
     * @return true if redefined by at least one other element
     */
    public boolean isRedefined() {
        return mIsRedefined;
    }

    /**
     * @param isRedefined true if redefined by at least one other element
     */
    public void setIsRedefined(final boolean isRedefined) {
        mIsRedefined = isRedefined;
    }

    /**
     * @return true if element is used in custom code
     */
    public boolean isCustomVariable() {
        return mIsCustomVariable;
    }

    /**
     * @param isCustomVariable true if element is used in custom code
     */
    public void setIsCustomVariable(final boolean isCustomVariable) {
        mIsCustomVariable = isCustomVariable;
    }

    /**
     * @return the name of a class providing additional logic to select an
     *         alternative within a choice element at unmarshaling (Host to
     *         Java) time.
     */
    public String getUnmarshalChoiceStrategyClassName() {
        return mUnmarshalChoiceStrategyClassName;
    }

    /**
     * @param unmarshalChoiceStrategyClassName the the name of a class providing
     *            additional logic to select an alternative within a choice
     *            element at unmarshaling (Host to Java) time.
     */
    public void setUnmarshalChoiceStrategyClassName(
            final String unmarshalChoiceStrategyClassName) {
        mUnmarshalChoiceStrategyClassName = unmarshalChoiceStrategyClassName;
    }

    /**
     * @return the name of a class providing additional logic to select an
     *         alternative within a choice element at marshaling (Java to Host)
     *         time.
     */
    public String getMarshalChoiceStrategyClassName() {
        return mMarshalChoiceStrategyClassName;
    }

    /**
     * @param marshalChoiceStrategyClassName the name of a class providing
     *            additional logic to select an alternative within a choice
     *            element at marshaling (Java to Host) time.
     */
    public void setMarshalChoiceStrategyClassName(
            final String marshalChoiceStrategyClassName) {
        mMarshalChoiceStrategyClassName = marshalChoiceStrategyClassName;
    }

    /**
     * @return the Java property name bound to this cobol element
     */
    public String getJaxbName() {
        return mJaxbName;
    }

    /**
     * @param jaxbName the Java property name bound to this cobol element to set
     */
    public void setJaxbName(final String jaxbName) {
        mJaxbName = jaxbName;
    }

    /**
     * @return the Java property type bound to this cobol element
     */
    public Class < ? > getJaxbType() {
        return mJaxbType;
    }

    /**
     * @param jaxbType the Java property type bound to this cobol element to set
     */
    public void setJaxbType(final Class < ? > jaxbType) {
        mJaxbType = jaxbType;
    }

    /**
     * @return the Level in the hierarchy
     */
    public int getLevelNumber() {
        return mLevelNumber;
    }

    /**
     * @param levelNumber the Level in the hierarchy to set
     */
    public void setLevelNumber(final int levelNumber) {
        mLevelNumber = levelNumber;
    }

    /**
     * @return the Cobol picture clause
     */
    public String getPicture() {
        return mPicture;
    }

    /**
     * @param picture the Cobol picture clause to set
     */
    public void setPicture(final String picture) {
        mPicture = picture;
    }

    /**
     * @return the Cobol default value
     */
    public String getDefaultValue() {
        return mDefaultValue;
    }

    /**
     * @return the Cobol usage
     */
    public String getUsage() {
        return mUsage;
    }

    /**
     * @param usage the Cobol usage to set
     */
    public void setUsage(final String usage) {
        mUsage = usage;
    }

    /**
     * @param defaultValue the Cobol default value to set
     */
    public void setDefaultValue(final String defaultValue) {
        mDefaultValue = defaultValue;
    }

    /**
     * @return the Line number in the original source file
     */
    public int getSrceLine() {
        return mSrceLine;
    }

    /**
     * @param srceLine the Line number in the original source file to set
     */
    public void setSrceLine(final int srceLine) {
        mSrceLine = srceLine;
    }

    /**
     * Determines if this Cobol element is bound to a Jaxb property.
     * 
     * @return true if element is bound to a jaxb property which name and type
     *         are known
     */
    public boolean isBound() {
        return (mJaxbName != null && mJaxbType != null);
    }

    /**
     * @return the name of this binding element
     */
    public String getBindingName() {
        return mBindingName;
    }

    /**
     * @param bindingName the binding element name to set
     */
    public void setBindingName(final String bindingName) {
        mBindingName = bindingName;
    }

    /**
     * @return the parent binding
     */
    public ICobolComplexBinding getParentBinding() {
        return mParentBinding;
    }

    /**
     * @param parentBinding the parent binding to set
     */
    public void setParentBinding(final ICobolComplexBinding parentBinding) {
        mParentBinding = parentBinding;
    }

    /**
     * This utility method is used to suppress the need for @SuppressWarnings
     * when we cast objects to List < ? >.
     * 
     * @param <T> the list type
     * @param x the object to cast
     * @return a list object
     */
    @SuppressWarnings("unchecked")
    public <T> T cast(final Object x) {
        return (T) x;
    }

    /**
     * @return true if this is a generated binding
     */
    public boolean isGeneratedBinding() {
        return mGeneratedBinding;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return "CBinding [mBindingName="
                + mBindingName
                + ", mParentBindingName="
                + ((mParentBinding == null) ? "" : mParentBinding
                        .getBindingName()) + ", mJaxbName=" + mJaxbName
                + ", mJaxbType=" + mJaxbType + ", mCobolName=" + mCobolName
                + ", mCobolType=" + mCobolType + ", mByteLength=" + mByteLength
                + ", mIsJustifiedRight=" + mIsJustifiedRight
                + ", mTotalDigits=" + mTotalDigits + ", mFractionDigits="
                + mFractionDigits + ", mIsSigned=" + mIsSigned
                + ", mIsSignLeading=" + mIsSignLeading + ", mIsSignSeparate="
                + mIsSignSeparate + ", mMinOccurs=" + mMinOccurs
                + ", mMaxOccurs=" + mMaxOccurs + ", mDependingOn="
                + mDependingOn + ", mIsODOObject=" + mIsODOObject
                + ", mRedefines=" + mRedefines + ", mIsRedefined="
                + mIsRedefined + ", mIsCustomVariable=" + mIsCustomVariable
                + ", mUnmarshalChoiceStrategyClassName="
                + mUnmarshalChoiceStrategyClassName
                + ", mMarshalChoiceStrategyClassName="
                + mMarshalChoiceStrategyClassName + ", mLevelNumber="
                + mLevelNumber + ", mPicture=" + mPicture + ", mUsage="
                + mUsage + ", mDefaultValue=" + mDefaultValue + ", mSrceLine="
                + mSrceLine + ", mGeneratedBinding=" + mGeneratedBinding + "]";
    }
}
