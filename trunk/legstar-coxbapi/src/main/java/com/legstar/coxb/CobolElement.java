/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;


/** Cobol annotations persist at execution time. */
@Retention(RetentionPolicy.RUNTIME)

//This annotation is associated with a field
@Target(ElementType.FIELD)

public @interface CobolElement {
    /** Cobol element name. */
    String cobolName();

    /** Cobol element type. */
    CobolType type() default CobolType.ALPHANUMERIC_ITEM;

    /** Cobol element length in bytes. */
    int byteLength() default 0;

    /** String justification. */
    boolean isJustifiedRight() default false;

    /** Numerics total number of digits (including fractional). */
    int totalDigits() default 0;

    /** Numerics fractional number of digits. */
    int fractionDigits() default 0;

    /** Numerics signed or unsigned. */
    boolean isSigned() default false;

    /** Numerics sign in leading byte or trailing byte. */
    boolean isSignLeading() default false;

    /** Numerics sign occupies a separate byte. */
    boolean isSignSeparate() default false;

    /** Arrays minimum number of occurences. */
    int minOccurs() default 0;

    /** Arrays maximum number of occurences. */
    int maxOccurs() default 0;

    /** Cobol element giving array actual size. */
    String dependingOn() default "";

    /** Determines the size of a variable size array. */
    boolean isODOObject() default false;

    /** Cobol element sharing same memory location. */
    String redefines() default "";

    /** Element is redefined by at least one other element. */
    boolean isRedefined() default false;

    /** True if this element is used in custom code. */
    boolean isCustomVariable() default false;

    /** Name of class providing logic to help with alternative selection. 
     *  Host to Java. */
    String unmarshalChoiceStrategyClassName() default "";

    /** Name of class providing logic to help with alternative selection. 
     *  Java to Host. */
    String marshalChoiceStrategyClassName() default "";

    /** Level in the hierarchy this element was parsed from. */
    int levelNumber() default 1;

    /** Cobol picture clause. */
    String picture() default "";

    /** Cobol usage. */
    String usage() default "";

    /** Cobol default value. */
    String value() default "";

    /** Line number in the original source file this element was parsed from. */
    int srceLine() default 1;
}
