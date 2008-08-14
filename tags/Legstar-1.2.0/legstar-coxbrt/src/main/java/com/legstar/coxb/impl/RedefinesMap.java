/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
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
