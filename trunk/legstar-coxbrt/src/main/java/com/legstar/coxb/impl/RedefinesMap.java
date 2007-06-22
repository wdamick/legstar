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
package com.legstar.coxb.impl;

import java.util.Hashtable;

import com.legstar.coxb.ICobolChoiceBinding;

/**
 * Encapsulates a singleton HashTable to track REDEFINES relationships.
 * The map holds an entry for each redefined object and the associated
 * choice element.
 */
public class RedefinesMap {
	
	/** 
	 * Redefines choice element. This table is populated when an element is
	 * being redefined by others (cobol REDEFINES clause). When redefining
	 * elements are found, this table will give direct access to the
	 * corresponding choice element.              */
	private static
	  ThreadLocal < Hashtable < String, ICobolChoiceBinding > > mapHolder
	  = new ThreadLocal < Hashtable < String, ICobolChoiceBinding > >();
	
	/**
	 * Gets the current choice element for a redefining object.
	 * 
	 * @param redefines The cobol element name that is redefined
	 * @return the cobol description of the choice element
	 */
	public final synchronized ICobolChoiceBinding getChoiceElement(
			final String redefines)	{
		
		ICobolChoiceBinding value = getRedefinesMap().get(redefines);
		return value;
	}
	
	/**
	 * Changes the choice element for this redefining object.
	 */
	/**
	 * @param redefines cobol variable redefining an element
	 * @param ce the cobol description of the redefined element
	 */
	public final synchronized void updateChoiceElement(
			final String redefines,
			final ICobolChoiceBinding ce) {
		
		getRedefinesMap().put(redefines, ce);
	}

	/**
	 * Removes the choice element from the map.
	 * 
	 * @param redefines cobol variable redefining an element
	 */
	public final synchronized void removeChoiceElement(
			final String redefines)	{
		
		getRedefinesMap().remove(redefines);
	}
	
	/**
	 * Getter for the redefines map.
	 * 
	 * @return the redefines map
	 */
	public final Hashtable < String, ICobolChoiceBinding >
		getRedefinesMap()	{

		Hashtable < String, ICobolChoiceBinding > result = mapHolder.get();
		if (result == null) {
			mapHolder.set(new Hashtable < String, ICobolChoiceBinding >());
		}
		return mapHolder.get();
	}
	
}
