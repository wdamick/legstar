package com.legstar.coxb.impl;

import java.math.BigDecimal;
import java.math.BigInteger;

import com.legstar.binding.CobolElement;
import com.legstar.coxb.ICobolNumericBinding;
import com.legstar.host.HostException;

/**
 * A generic binding for numeric elements. All numeric values are
 * stored in a BigDecimal.
 */
public abstract class CNumericBinding extends CBinding
		implements ICobolNumericBinding {

	/** The current value for this element. */
	private BigDecimal mValue = null;
	
	/**
	 * Creates an empty binding between a Cobol numeric element and a java
	 *  BigDecimal.
	 * 
	 * @param javaName the name of the bound java property
	 * @param javaType the type of the bound java property
	 */
	public CNumericBinding(
			final String javaName,
			final Class javaType) {
		
		super(javaName, javaType);
	}
	
	/**
	 * Creates a binding between a Cobol numeric element and a java BigDecimal.
	 * 
	 * @param javaName the name of the bound java property
	 * @param javaType the type of the bound java property
	 * @param cobolAnnotations the cobol annotations for this element
	 */
	public CNumericBinding(
			final String javaName,
			final Class javaType,
			final CobolElement cobolAnnotations) {
		
		super(javaName, javaType, cobolAnnotations);
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
			throw new HostException("Attempt to get binding " + getJavaName()
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
			throw new HostException("Attempt to set binding " + getJavaName()
					+ " from an incompatible value " + value);
		}
	}

	/** {@inheritDoc} */
	public final boolean isSet() {
		return (mValue != null);
	}
}
