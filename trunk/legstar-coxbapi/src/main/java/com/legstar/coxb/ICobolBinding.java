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

import com.legstar.coxb.host.HostException;

/**
 * This interface represents the binding between a java type and a cobol
 * element.
 *
 * @author Fady Moussallam
 * 
*/
public interface ICobolBinding extends ICobolElement {

	/** Returns the name of this binding element.
	 * @return the name of this binding element */
	String getBindingName();
	
	/**
	 * A binding element might belong to another complex element . This method
	 * returns a reference to the complex parent binding if any,
	 * null otherwise.
	 * 
	 * @return a reference to the parent binding
	 *  */
	ICobolComplexBinding getParentBinding();

	/** Returns the java name of the property bound to this Cobol element.
	 * @return the java name of the property bound to this Cobol element */
	String getJaxbName();

	/** Returns the java type of the property bound to this Cobol element.
	 * @return the java type of the property bound to this Cobol element */
	Class < ? > getJaxbType();

	/**
	 * Visitor pattern accept method.
	 * @param cev The visitor
	 * @throws HostException visitor request cannot be accepted
	 */
	void accept(CobolElementVisitor cev) throws HostException;

	/**
	 * The method calculates the exact host byte length for this
	 * Cobol binding.
	 * @return the host byte length of this binding
	 * @throws HostException if byte length cannot be computed
	 */
	int calcByteLength() throws HostException;
	
	/**
	 * Sets the binding internal value. This method is used when
	 * the value type is not known in advance. More strongly typed
	 * methods exist in each binding category.
	 * @param value the value to set the binding
	 * @throws HostException if value is not compatible with binding type
	 */
	void setObjectValue(Object value) throws HostException;
	
	/**
	 * Retrieves the binding value as an object of the requested type.
	 * @param type the type of object to produce
	 * @return an object of the requested type
	 * @throws HostException if value cannot be converted to the requested type
	 */
	Object getObjectValue(Class < ? > type) throws HostException;
	
	/**
	 * When a binding value has been explicitly set, this method
	 * will return true. This is useful for the choice default
	 * strategy that only considers alternatives that have been 
	 * explicitly set.
	 * @return true if this binding value has been set
	 */
	boolean isSet();
	
	
	/**
	 * Determines if this Cobol element is bound to a Jaxb property.
	 * @return true if element is bound to a jaxb property which name and type
	 * are known
	 */
	boolean isBound();

}
