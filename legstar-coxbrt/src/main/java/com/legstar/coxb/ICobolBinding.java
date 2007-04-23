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

import java.math.BigDecimal;

import com.legstar.host.HostException;

/**
 * This interface represents the binding between a java type and a cobol
 * element.
 *
 * @author Fady Moussallam
 * 
*/
public interface ICobolBinding extends ICobolElement {

	/** Returns the java name bound to this Cobol element.
	 * @return the cobol element java name */
	String getJavaName();

	/** Returns the java type of the property bound to this Cobol element.
	 * @return the cobol element java type */
	String getJavaType();

	/**
	 * Visitor pattern accept method.
	 * @param cev The visitor
	 * @throws HostException visitor request cannot be accepted
	 */
	void accept(CobolElementVisitor cev) throws HostException;

	/** Returns the corresponding host byte size of this cobol element.
	 * @return Returns the host byte length 
	 * @throws HostException host byte length cannot be computed
	 */
	int getByteLength() throws HostException;
	
	/**
	 * All types that are able to return a numeric value share this method. This
	 * allows variable size arrays dimensions to be determined by any type of
	 * numeric element.
	 * 
	 * @return the current numeric value as a BigDecimal
	 * @throws HostException if this element cannot produce a numeric value
	 */
	BigDecimal getNumericValue() throws HostException;

	/**
	 * Current value associated with this element.
	 * 
	 * @return the current value as an Object
	 * @throws HostException if this element cannot produce a value
	 */
	Object getValue() throws HostException;

}
