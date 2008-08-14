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
