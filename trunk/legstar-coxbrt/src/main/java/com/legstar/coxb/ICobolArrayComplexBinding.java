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
package com.legstar.coxb;

import com.legstar.host.HostException;

/**
 * This interface groups methods that are common to arrays of complex elements.
 * 
 * @author Fady Moussallam
 * 
*/
public interface ICobolArrayComplexBinding extends ICobolBinding {
	
	/**
	 * Requests array to create a JAXB complex array.
	 * 
	 * @throws HostException if initialization fails
	 */
	void createBoundObject() throws HostException;
	
	/**
	 * With late binding of items, they might need to get passed context
	 * data such as JAXB object factories.
	 * 
	 * @throws HostException if initialization fails
	 */
	void prepareChildren() throws HostException;
	
	/**
	 * Requests complex array binding to set its item values from the
	 * JAXB item identified by index.
	 * 
	 * @param index identifies which item to set values for
	 * @throws HostException if reading from bound item values fails
	 */
	void getValuesFromBoundItem(int index) throws HostException;

	/**
	 * Requests a complex binding item to perform any initialization of an
	 * item for visiting.
	 * 
	 * @param index identifies which item to initialize
	 * @throws HostException if initialization fails
	 */
	void initBoundItem(int index) throws HostException;
	
	/**
	 * Request complex binding item to set bound object values from its children
	 * values.
	 * 
	 * @param index item index in this array.
	 * @throws HostException if bound object values cannot be set
	 */
	void setBoundItemValues(int index) throws HostException;

	/** Gets a Cobol binding description for a given item of this array.
	 * 
	 * @param index item index in this array.
	 * @return a new Cobol binding
	 * @throws HostException if creating the Cobol binding fails
	 */
	ICobolBinding getItem(int index)
		throws HostException;
}
