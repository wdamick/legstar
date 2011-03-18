/*******************************************************************************
 * Copyright (c) 2011 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb;

/**
 * This interface describes a cobol element. It is almost identical to the
 * annotation interface com.legstar.binding.CobolElement but it is recommended
 * not to use annotation interfaces as super interfaces.
 *
 * @author Fady Moussallam
 * 
 */
public interface ICobolElement {

    /** @return Cobol element name. */
    String getCobolName();

    /**
     * @param cobolName the Cobol element name to set
     */
    void setCobolName(String cobolName);

    /** @return Cobol element type. */
    CobolType getCobolType();

    /**
     * @param type the Cobol element type to set
     */
    void setCobolType(CobolType type);

    /** @return Cobol element length in bytes.
     */
    int getByteLength();

    /**
     * @param byteLength the Cobol element length in bytes to set
     */
    void setByteLength(int byteLength);

    /** @return String justification. */
    boolean isJustifiedRight();

    /**
     * @param isJustifiedRight true if String is right justified
     */
    void setIsJustifiedRight(boolean isJustifiedRight);

    /** @return Numerics total number of digits (including fractional). */
    int getTotalDigits();

    /**
     * @param totalDigits the total number of digits to set
     */
    void setTotalDigits(int totalDigits);

    /** @return Numerics fractional number of digits. */
    int getFractionDigits();

    /**
     * @param fractionDigits the fractional number of digits to set
     */
    void setFractionDigits(int fractionDigits);

    /** @return Numerics signed or unsigned. */
    boolean isSigned();

    /**
     * @param isSigned true if signed
     */
    void setIsSigned(boolean isSigned);

    /** @return Numerics sign in leading byte or trailing byte. */
    boolean isSignLeading();

    /**
     * @param isSignLeading true if sign in leading byte
     */
    void setIsSignLeading(boolean isSignLeading);

    /** @return Numerics sign occupies a separate byte. */
    boolean isSignSeparate();

    /**
     * @param isSignSeparate true if sign occupies a separate byte
     */
    void setIsSignSeparate(boolean isSignSeparate);

    /** @return Arrays minimum number of occurences. */
    int getMinOccurs();

    /**
     * @param minOccurs the minimum number of occurences to set
     */
    void setMinOccurs(int minOccurs);

    /** @return Arrays maximum number of occurences.
     */
    int getMaxOccurs();

    /**
     * @param maxOccurs the maximum number of occurences to set
     */
    void setMaxOccurs(int maxOccurs);

    /** @return Cobol element giving array actual size. */
    String getDependingOn();

    /**
     * @param dependingOn the Cobol element giving array actual size to set
     */
    void setDependingOn(String dependingOn);

    /** @return Determines the size of a variable size array. */
    boolean isODOObject();

    /**
     * @param isODOObject true if array is variable size
     */
    void setIsODOObject(boolean isODOObject);

    /** @return Cobol element share the same memory location as object. */
    String getRedefines();

    /**
     * @param redefines Cobol element sharing same memory location to set
     */
    void setRedefines(String redefines);

    /** @return Element is redefined by at least one other element. */
    boolean isRedefined();

    /**
     * @param isRedefined true if redefined by at least one other element
     */
    void setIsRedefined(boolean isRedefined);

    /** @return True if this element is used in custom code. */
    boolean isCustomVariable();

    /**
     * @param isCustomVariable true if element is used in custom code
     */
    void setIsCustomVariable(boolean isCustomVariable);

    /** @return Name of class providing logic to help with alternative
     * selection (Host to Java). */
    String getUnmarshalChoiceStrategyClassName();

    /**
     * @param unmarshalChoiceStrategyClassName the the name of a class providing
     * additional logic to select an alternative within a choice element at
     * unmarshaling (Host to Java) time.
     */
    void setUnmarshalChoiceStrategyClassName(
            String unmarshalChoiceStrategyClassName);

    /** @return Name of class providing logic to help with alternative
     * selection (Java to Host). */
    String getMarshalChoiceStrategyClassName();

    /**
     * @param marshalChoiceStrategyClassName the name of a class providing
     * additional logic to select an alternative within a choice element at
     * marshaling (Java to Host) time.
     */
    void setMarshalChoiceStrategyClassName(
            String marshalChoiceStrategyClassName);

    /** @return Level in the hierarchy this element was parsed from. */
    int getLevelNumber();

    /**
     * @param levelNumber the Level in the hierarchy to set
     */
    void setLevelNumber(int levelNumber);

    /** @return Cobol picture clause. */
    String getPicture();

    /**
     * @param picture the Cobol picture clause to set
     */
    void setPicture(String picture);

    /** @return Cobol usage. */
    String getUsage();

    /**
     * @param usage the Cobol usage to set
     */
    void setUsage(String usage);

    /** @return Cobol default value. */
    String getDefaultValue();

    /**
     * @param defaultValue the Cobol default value to set
     */
    void setDefaultValue(String defaultValue);

    /** @return Line number in the original source file this element
     *  was parsed from. */
    int getSrceLine();

    /**
     * @param srceLine the Line number in the original source file  to set
     */
    void setSrceLine(int srceLine);
}
