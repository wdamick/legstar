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

import java.math.BigDecimal;
import java.util.List;

/**
 * This interface groups methods that are common to double array elements.
 * 
 * @author Fady Moussallam
 * 
*/
public interface ICobolArrayDoubleBinding extends ICobolArrayBinding {
	
	/**
	 * @return Returns an ArrayList of Doubles 
	 * @throws HostException list cannot be created
	 */
	List < Double > getDoubleList() throws HostException;
	
	/**
	 * Sets a list of Doubles.
	 * @param iArray set array to set
	 * @throws HostException list cannot be set
	 */
	void setDoubleList(List < Double > iArray) throws HostException;

	/**
	 * @return Returns an ArrayList of BigDecimals 
	 * @throws HostException list cannot be created
	 */
	List < BigDecimal > getBigDecimalList() throws HostException;
	
	/**
	 * Sets a list of BigDecimals.
	 * @param iArray set array to set
	 * @throws HostException list cannot be set
	 */
	void setBigDecimalList(List < BigDecimal > iArray) throws HostException;
}
