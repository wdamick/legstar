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
package com.legstar.coxb.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import com.legstar.coxb.CobolType;

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
	boolean isSignLeading() default true;

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
}
