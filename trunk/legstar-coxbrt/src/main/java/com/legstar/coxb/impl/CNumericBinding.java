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

import java.math.BigDecimal;
import java.math.BigInteger;

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.ICobolNumericBinding;
import com.legstar.coxb.common.CBinding;
import com.legstar.coxb.host.HostException;

/**
 * A generic binding for numeric elements. All numeric values are
 * stored in a BigDecimal.
 */
public abstract class CNumericBinding extends CBinding
		implements ICobolNumericBinding {

	/** The current value for this element. */
	private BigDecimal mValue = null;
	
	/**
	 * Constructor for a cobol element to java binding.
	 * 
	 * @param bindingName the identifier for this binding
	 * @param jaxbName the name of the bound java property
	 * @param jaxbType the type of the bound java property
	 * @param cobolAnnotations the cobol annotations for this element
	 * @param parentBinding a reference to the parent binding
	 */
	public CNumericBinding(
			final String bindingName,
			final String jaxbName,
			final Class jaxbType,
			final CobolElement cobolAnnotations,
			final ICobolComplexBinding parentBinding) {
		super(bindingName, jaxbName, jaxbType, cobolAnnotations, parentBinding);
	}


	/** {@inheritDoc} */
	public final void setByteValue(
			final Byte value) throws HostException {
		mValue = new BigDecimal(value);
	}

	/** {@inheritDoc} */
	public final Byte getByteValue() throws HostException {
		return mValue.byteValue();
	}
	
	/** {@inheritDoc} */
	public final void setShortValue(
			final Short value) throws HostException {
		mValue = new BigDecimal(value);
	}

	/** {@inheritDoc} */
	public final Short getShortValue() throws HostException {
		return mValue.shortValue();
	}
	
	/** {@inheritDoc} */
	public final void setIntegerValue(
			final Integer value) throws HostException {
		mValue = new BigDecimal(value);
	}

	/** {@inheritDoc} */
	public final Integer getIntegerValue() throws HostException {
		return mValue.intValue();
	}
	
	/** {@inheritDoc} */
	public final void setLongValue(
			final Long value) throws HostException {
		mValue = new BigDecimal(value);
	}

	/** {@inheritDoc} */
	public final Long getLongValue() throws HostException {
		return mValue.longValue();
	}
	
	/** {@inheritDoc} */
	public final void setBigDecimalValue(
			final BigDecimal value) throws HostException {
		mValue = value;
	}

	/** {@inheritDoc} */
	public final BigDecimal getBigDecimalValue() throws HostException {
		return mValue;
	}
	
	/** {@inheritDoc} */
	public final void setBigIntegerValue(
			final BigInteger value) throws HostException {
		mValue = new BigDecimal(value);
	}

	/** {@inheritDoc} */
	public final BigInteger getBigIntegerValue() throws HostException {
		return mValue.toBigInteger();
	}
	
	/** {@inheritDoc} */
	public final int calcByteLength() throws HostException {
		return getByteLength();
	}

	/** {@inheritDoc} */
	public final Object getObjectValue(final Class type) throws HostException {
		if (type.equals(BigDecimal.class)) {
			return mValue;
		} else if (type.equals(BigInteger.class)) {
			return getBigIntegerValue();
		} else if (type.equals(Long.class) || type.equals(long.class)) {
			return getLongValue();
		} else if (type.equals(Integer.class) || type.equals(int.class)) {
			return getIntegerValue();
		} else if (type.equals(Short.class) || type.equals(short.class)) {
			return getShortValue();
		} else if (type.equals(Byte.class) || type.equals(byte.class)) {
			return getByteValue();
		} else {
			throw new HostException("Attempt to get binding " + getBindingName()
					+ " as an incompatible type " + type);
		}
	}

	/** {@inheritDoc} */
	public final void setObjectValue(final Object value) throws HostException {
		if (value == null) {
			mValue = null;
			return;
		}
		if (value instanceof BigDecimal) {
			 mValue = (BigDecimal) value;
		} else if (value instanceof BigInteger) {
			setBigIntegerValue((BigInteger) value);
		} else if (value instanceof Long) {
			setLongValue((Long) value);
		} else if (value instanceof Integer) {
			setIntegerValue((Integer) value);
		} else if (value instanceof Short) {
			setShortValue((Short) value);
		} else if (value instanceof Byte) {
			setByteValue((Byte) value);
		} else {
			throw new HostException("Attempt to set binding " + getBindingName()
					+ " from an incompatible value " + value);
		}
	}

	/** {@inheritDoc} */
	public final boolean isSet() {
		return (mValue != null);
	}
}
