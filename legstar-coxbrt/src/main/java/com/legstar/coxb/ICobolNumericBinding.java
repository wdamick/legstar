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

import java.math.BigDecimal;

/**
 * This interface groups methods that are common to all cobol numeric elements
 * that are not arrays.
 * 
 * @author Fady Moussallam
 * 
*/
public interface ICobolNumericBinding extends ICobolBinding {
	
	/**
	 * Returns this element value.
	 * @return Element value
	 * @throws HostException if value cannot be retrieved
	 */
	BigDecimal getValue() throws HostException;

	/**
	 * Sets the element value.
	 * @param value Value to set
	 * @throws HostException if value cannot be set
	 */
	void setValue(BigDecimal value) throws HostException;
	
}
