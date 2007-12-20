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

import com.legstar.coxb.host.HostException;

/**
 * This interface groups methods that are common to all cobol numeric
 * elements.
 *
 * @author Fady Moussallam
 * 
*/
public interface ICobolNumericBinding
	     extends ICobolBinding {
	
	/**
	 * @return the current numeric value as a Byte
	 * @throws HostException if this element cannot produce a numeric value
	 */
	Byte getByteValue() throws HostException;
	
	/**
	 * Set the value associated with this element.
	 * 
	 * @param value the value as a Byte
	 * @throws HostException if this setting value fails
	 */
	 void setByteValue(Byte value) throws HostException;

	/**
	 * @return the current numeric value as a Short
	 * @throws HostException if this element cannot produce a numeric value
	 */
	Short getShortValue() throws HostException;
	
	/**
	 * Set the value associated with this element.
	 * 
	 * @param value the value as a Short
	 * @throws HostException if this setting value fails
	 */
	 void setShortValue(Short value) throws HostException;

	/**
	 * @return the current numeric value as an Integer
	 * @throws HostException if this element cannot produce a numeric value
	 */
	Integer getIntegerValue() throws HostException;
	
	/**
	 * Set the value associated with this element.
	 * 
	 * @param value the value as an Integer
	 * @throws HostException if this setting value fails
	 */
	 void setIntegerValue(Integer value) throws HostException;

	/**
	 * @return the current numeric value as a Long
	 * @throws HostException if this element cannot produce a numeric value
	 */
	Long getLongValue() throws HostException;
	
	/**
	 * Set the value associated with this element.
	 * 
	 * @param value the value as a Long
	 * @throws HostException if this setting value fails
	 */
	 void setLongValue(Long value) throws HostException;

	/**
	 * @return the current numeric value as a BigDecimal
	 * @throws HostException if this element cannot produce a numeric value
	 */
	BigDecimal getBigDecimalValue() throws HostException;
  
	/**
	 * Set the value associated with this element.
	 * 
	 * @param value the value as a BigDecimal
	 * @throws HostException if this setting value fails
	 */
	 void setBigDecimalValue(BigDecimal value) throws HostException;

	 /**
	 * @return the current numeric value as a BigInteger
	 * @throws HostException if this element cannot produce a numeric value
	 */
	BigInteger getBigIntegerValue() throws HostException;

	/**
	 * Set the value associated with this element.
	 * 
	 * @param value the value as a BigInteger
	 * @throws HostException if this setting value fails
	 */
	 void setBigIntegerValue(BigInteger value) throws HostException;
}
