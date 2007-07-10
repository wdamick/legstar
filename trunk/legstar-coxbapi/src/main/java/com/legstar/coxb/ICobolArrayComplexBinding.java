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

import java.util.List;

import com.legstar.coxb.host.HostException;

/**
 * This interface groups methods that are common to arrays of complex elements.
 * 
 * @author Fady Moussallam
 * 
*/
public interface ICobolArrayComplexBinding extends ICobolArrayBinding {
	
	/**
	 * Requests array to create a JAXB complex array.
	 * 
	 * @throws HostException if initialization fails
	 */
	void createJaxbObject() throws HostException;
	
	/**
	 * Requests complex array binding to set its item values from the
	 * JAXB item identified by index.
	 * 
	 * @param index identifies which item to set values for
	 * @throws HostException if reading from bound item values fails
	 */
	void setItemValue(int index) throws HostException;

	/**
	 * Request complex binding item to add a value to a jaxb bound object
	 * of the List type. The value is taken at index index in this binding
	 * array of values.
	 * 
	 * @param index item index in this array.
	 * @throws HostException if bound object values cannot be set
	 */
	void addJaxbPropertyValue(int index) throws HostException;

	/**
	 * Returns the complex binding used for items of this array.
	 * @return the items complex binding
	 */
	ICobolComplexBinding getComplexItemBinding();

	/**
	 * @return the List of items
	 */
	List getObjectList();

	/**
	 * @param list the items List to set
	 */
	void setObjectList(List list);
}
