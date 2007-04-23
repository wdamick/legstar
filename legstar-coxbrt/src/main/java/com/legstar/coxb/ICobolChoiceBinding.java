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
 * This interface groups methods that are common to choice elements.
 * 
 * @author Fady Moussallam
 * 
*/
public interface ICobolChoiceBinding extends ICobolBinding {
	
	/**
	 * With late binding of alternatives, they might need to get passed context
	 * data such as JAXB object factories.
	 * 
	 * @throws HostException if initialization fails
	 */
	void prepareChildren() throws HostException;
	
	/**
	 * Requests choice binding to set its alternatives values from bound
	 * objects values.
	 * 
	 * @throws HostException if reading from bound object values fails
	 */
	void getValuesFromBoundObject() throws HostException;

	/**
	 * Request choice binding to set a bound object value from a corresponding
	 * child value.
	 * 
	 * @param index the position of the child in the alternatives list
	 * @throws HostException if bound object values cannot be set
	 */
	void setBoundObjectValue(int index) throws HostException;
	
	/**
	 * Returns the current list of alternatives for this choice element.
	 * 
	 * @return Ordered list of alternatives
	 * @throws HostException if list cannot be created
	 */
	java.util.List < ICobolBinding > getAlternativesList()
		throws HostException;
	
	/**
	 * Retrieves an alternative identified by its java type name.
	 * @param javaName the type name
	 * @return corresponding alternative or null if not found
	 */
	ICobolBinding getAlternativeByJavaName(String javaName);
	
	/**
	 * Retrieves an alternative identified by its cobol field name.
	 * @param cobolName the field name
	 * @return corresponding alternative or null if not found
	 */
	ICobolBinding getAlternativeByCobolName(String cobolName);

	/**
	 * When alternatives are discovered dynamically, this method allows
	 * other classes to participate in the alternative list buildup.
	 * 
	 * @param ce redefined or redefining cobol element description
	 *  */
	void addAlternative(final ICobolBinding ce);
	
	/**
	 * A choice element always belongs to a complex element (roots can never
	 * be choices). This method returns a reference to the complex parent
	 * binding.
	 * 
	 * @return a reference to the parent binding
	 *  */
	ICobolComplexBinding getParentBinding();
	
	/**
	 * Sets a instance of a class providing additional logic to select an
	 * alternative within a choice element at marshaling (Java to Host) time.
	 * @param strategy a choice strategy implementation
	 */
	void setMarshalChoiceStrategy(ICobolMarshalChoiceStrategy strategy);
	
	/**
	 * Gets an instance of a class providing additional logic to select an
	 * alternative within a choice element at marshaling (Java to Host) time.
	 * @return a choice strategy implementation
	 * @throws HostException if strategy cannot be loaded
	 */
	ICobolMarshalChoiceStrategy getMarshalChoiceStrategy()
			throws HostException;

	/**
	 * Sets a instance of a class providing additional logic to select an
	 * alternative within a choice element at unmarshaling (Host to Java) time.
	 * @param strategy a choice strategy implementation
	 */
	void setUnmarshalChoiceStrategy(ICobolUnmarshalChoiceStrategy strategy);
	
	/**
	 * Gets an instance of a class providing additional logic to select an
	 * alternative within a choice element at unmarshaling (Host to Java) time.
	 * @return a choice strategy implementation
	 * @throws HostException if strategy cannot be loaded
	 */
	ICobolUnmarshalChoiceStrategy getUnmarshalChoiceStrategy()
			throws HostException;
}
