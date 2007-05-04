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
package com.legstar.coxb;

import com.legstar.host.HostException;

/**
 * This interface describes a cobol element.
 *
 * @author Fady Moussallam
 * 
*/
public interface ICobolElement {
	
	/** @return Cobol element name. */
	String getCobolName();
	
	/** @return Cobol element type. */
	CobolType getCobolType();
	
	/** @return Cobol element length in bytes.
	 * @throws HostException host byte length cannot be computed
	 */
	int getByteLength() throws HostException;
	
	/** @return String justification. */
	boolean isJustifiedRight();
	
	/** @return Numerics total number of digits (including fractional). */
	int getTotalDigits();
	
	/** @return Numerics fractional number of digits. */
	int getFractionDigits();

	/** @return Numerics signed or unsigned. */
	boolean isSigned();

	/** @return Numerics sign in leading byte or trailing byte. */
	boolean isSignLeading();

	/** @return Numerics sign occupies a separate byte. */
	boolean isSignSeparate();
	
	/** @return Arrays minimum number of occurences. */
	int getMinOccurs();

	/** @return Arrays maximum number of occurences.
	 * @throws HostException number of items cannot be computed
     */
	int getMaxOccurs() throws HostException;

	/** @return Cobol element giving array actual size. */
	String getDependingOn();

	/** @return Determines the size of a variable size array. */
	boolean isODOObject();

	/** @return Cobol element share the same memory location as object. */
	String getRedefines();

	/** @return Element is redefined by at least one other element. */
	boolean isRedefined();

	/** @return True if this element is used in custom code. */
	boolean isCustomVariable();

	/** @return Name of class providing logic to help with alternative
	 * selection (Host to Java). */
	String getUnmarshalChoiceStrategyClassName();

	/** @return Name of class providing logic to help with alternative
	 * selection (Java to Host). */
	String getMarshalChoiceStrategyClassName();
}
