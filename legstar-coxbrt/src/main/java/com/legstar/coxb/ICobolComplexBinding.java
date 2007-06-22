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
 * This interface groups methods that are common to complex elements that are
 * not arrays.
 * 
 * @author Fady Moussallam
 * 
*/
public interface ICobolComplexBinding extends ICobolBinding {
	
	/**
	 * Requests complex binding to create an instance of its bound
	 * object.
	 * 
	 * @throws HostException if initialization fails
	 */
	void createJaxbObject() throws HostException;
	
	/**
	 * Requests complex binding to set its children values from bound
	 * Jaxb objects values.
	 * 
	 * @throws HostException if reading from bound object values fails
	 */
	void setChildrenValues() throws HostException;

	/**
	 * Request complex binding to set a property of the bound Jaxb object
	 * to the value of a corresponding child.
	 * 
	 * @param index the position of the child in the complex element child list
	 * @throws HostException if bound object values cannot be set
	 */
	void setJaxbPropertyValue(int index) throws HostException;
	
	/**
	 * Request a list of children from this complex binding.
	 * 
	 * @return Ordered list of children
	 * @throws HostException if list cannot be created
	 */
	java.util.List < ICobolBinding > getChildrenList()
		throws HostException;
	
	/**
	 * A complex element might belong to another complex element . This method
	 * returns a reference to the complex parent binding if any,
	 * null otherwise.
	 * 
	 * @return a reference to the parent binding
	 *  */
	ICobolComplexBinding getParentBinding();

	/**
     * @return the java object factory for objects creation
     */
    Object getObjectFactory();

    /**
     * @param jaxbObjectFactory the java object factory for objects creation 
     */
    void setObjectFactory(Object jaxbObjectFactory);
}
