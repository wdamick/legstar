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

import com.legstar.host.HostException;

/**
 * This interface groups methods that are common to all cobol array string
 * elements (PIC X OCCURS).
 *
 * @author Fady Moussallam
 * 
*/
public interface ICobolArrayStringBinding extends ICobolBinding {
	
	
	/**
	 * @return Returns an ArrayList of Strings 
	 * @throws HostException if list cannot be returned
	 */
	List < String > getValue() throws HostException;
	
	/**
	 * Set a string array element.
	 * @param iArray set array to set
	 * @throws HostException if list cannot be set
	 */
	void setValue(List < String > iArray) throws HostException;
	
	/**
	 * Corresponding cobol field must be right justified.
	 * @return True if field is right justified
	 */
	boolean isJustifiedRight();

	/** Returns the corresponding host byte size of a single item.
	 * @return the host byte length of an item
	 * @throws HostException byte length cannot be computed
	 */
	int getItemByteLength() throws HostException;


}
