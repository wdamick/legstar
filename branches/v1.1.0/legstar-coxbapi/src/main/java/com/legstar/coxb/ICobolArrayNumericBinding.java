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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;

/**
 * This interface groups methods that are common to all array of numeric
 * elements.
 * 
 * @author Fady Moussallam
 * 
*/
public interface ICobolArrayNumericBinding extends ICobolArrayBinding {
	
	/**
	 * @return the numeric list as a List of Bytes
	 */
	List < Byte > getByteList();

	/**
	 * @param list the numeric list as a List of Bytes to set
	 */
	void setByteList(List < Byte > list);

	/**
	 * @return the numeric list as a List of Shorts
	 */
	List < Short > getShortList();

	/**
	 * @param list the numeric list as a List of Shorts to set
	 */
	void setShortList(List < Short > list);

	/**
	 * @return the numeric list as a List of Integers
	 */
	List < Integer > getIntegerList();

	/**
	 * @param list the numeric list as a List of Integers to set
	 */
	void setIntegerList(List < Integer > list);

	/**
	 * @return the numeric list as a List of Longs
	 */
	List < Long > getLongList();

	/**
	 * @param list the numeric list as a List of Longs to set
	 */
	void setLongList(List < Long > list);

	/**
	 * @return the numeric list as a List of BigDecimals
	 */
	List < BigDecimal > getBigDecimalList();

	/**
	 * @param list the numeric list as a List of BigDecimals to set
	 */
	void setBigDecimalList(List < BigDecimal > list);

	/**
	 * @return the numeric list as a List of BigIntegers
	 */
	List < BigInteger > getBigIntegerList();

	/**
	 * @param list the numeric list as a List of BigIntegers to set
	 */
	void setBigIntegerList(List < BigInteger > list);
}
