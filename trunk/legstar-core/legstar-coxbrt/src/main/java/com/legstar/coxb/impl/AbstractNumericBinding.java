/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.impl;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.math.BigInteger;

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.common.CBinding;
import com.legstar.coxb.host.HostException;

/**
 * A generic binding for numeric bindings. All numeric values are stored in a
 * BigDecimal.
 */
public abstract class AbstractNumericBinding extends CBinding {

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
    public AbstractNumericBinding(final String bindingName,
            final String jaxbName, final Class < ? > jaxbType,
            final CobolElement cobolAnnotations,
            final ICobolComplexBinding parentBinding) {
        super(bindingName, jaxbName, jaxbType, cobolAnnotations, parentBinding);
    }

    /** {@inheritDoc} */
    public void setByteValue(final Byte value) throws HostException {
        mValue = new BigDecimal(value);
    }

    /** {@inheritDoc} */
    public Byte getByteValue() throws HostException {
        return mValue.byteValue();
    }

    /** {@inheritDoc} */
    public void setShortValue(final Short value) throws HostException {
        mValue = new BigDecimal(value);
    }

    /** {@inheritDoc} */
    public Short getShortValue() throws HostException {
        return mValue.shortValue();
    }

    /** {@inheritDoc} */
    public void setIntegerValue(final Integer value) throws HostException {
        mValue = new BigDecimal(value);
    }

    /** {@inheritDoc} */
    public Integer getIntegerValue() throws HostException {
        return mValue.intValue();
    }

    /** {@inheritDoc} */
    public void setLongValue(final Long value) throws HostException {
        mValue = new BigDecimal(value);
    }

    /** {@inheritDoc} */
    public Long getLongValue() throws HostException {
        return mValue.longValue();
    }

    /** {@inheritDoc} */
    public void setFloatValue(final Float value) throws HostException {
        mValue = new BigDecimal(value);
    }

    /** {@inheritDoc} */
    public Float getFloatValue() throws HostException {
        return mValue.floatValue();
    }

    /** {@inheritDoc} */
    public void setDoubleValue(final Double value) throws HostException {
        mValue = new BigDecimal(value);
    }

    /** {@inheritDoc} */
    public Double getDoubleValue() throws HostException {
        return mValue.doubleValue();
    }

    /** {@inheritDoc} */
    public void setBigDecimalValue(final BigDecimal value) throws HostException {
        mValue = value;
    }

    /** {@inheritDoc} */
    public BigDecimal getBigDecimalValue() throws HostException {
        return mValue;
    }

    /** {@inheritDoc} */
    public void setBigIntegerValue(final BigInteger value) throws HostException {
        mValue = new BigDecimal(value);
    }

    /** {@inheritDoc} */
    public BigInteger getBigIntegerValue() throws HostException {
        return mValue.toBigInteger();
    }

    /** {@inheritDoc} */
    public void setBooleanValue(Boolean value) throws HostException {
        mValue = new BigDecimal((value) ? "1" : "0");
    }

    /** {@inheritDoc} */
    public Boolean getBooleanValue() throws HostException {
        if (mValue.intValue() == 0) {
            return false;
        }
        return true;
    }

    /**
     * With enumerations we need to recover the actual value type using
     * reflection.
     * 
     * @param type the enum member type
     * @return an enum member
     * @throws HostException if something wrong with the Enum methods
     */
    public Object getEnumValue(final Class < ? > type) throws HostException {
        try {
            Method value = type.getMethod("value");
            Method fromValue = type.getMethod("fromValue",
                    value.getReturnType());
            return fromValue
                    .invoke(null, getObjectValue(value.getReturnType()));
        } catch (SecurityException e) {
            throw new HostException(e);
        } catch (IllegalArgumentException e) {
            throw new HostException(e);
        } catch (IllegalAccessException e) {
            throw new HostException(e);
        } catch (NoSuchMethodException e) {
            throw new HostException(e);
        } catch (InvocationTargetException e) {
            throw new HostException(e);
        }
    }

    /**
     * Enums cannot be manipulated directly because we have no idea what the
     * values types actually are. Here we use reflection to recover the real
     * value type.
     * 
     * @param value
     * @throws HostException
     */
    public void setEnumValue(Enum < ? > value) throws HostException {
        try {
            Method valueMethod = value.getClass().getMethod("value");
            Object numValue = valueMethod.invoke(value);
            setObjectValue(numValue);
        } catch (SecurityException e) {
            throw new HostException(e);
        } catch (IllegalArgumentException e) {
            throw new HostException(e);
        } catch (IllegalAccessException e) {
            throw new HostException(e);
        } catch (NoSuchMethodException e) {
            throw new HostException(e);
        } catch (InvocationTargetException e) {
            throw new HostException(e);
        }
    }

    /** {@inheritDoc} */
    public Object getObjectValue(final Class < ? > type) throws HostException {
        if (type.equals(BigDecimal.class)) {
            return mValue;
        } else if (type.equals(BigInteger.class)) {
            return getBigIntegerValue();
        } else if (type.equals(Long.class) || type.equals(long.class)) {
            return getLongValue();
        } else if (type.equals(Float.class) || type.equals(float.class)) {
            return getFloatValue();
        } else if (type.equals(Double.class) || type.equals(double.class)) {
            return getDoubleValue();
        } else if (type.equals(Integer.class) || type.equals(int.class)) {
            return getIntegerValue();
        } else if (type.equals(Short.class) || type.equals(short.class)) {
            return getShortValue();
        } else if (type.equals(Byte.class) || type.equals(byte.class)) {
            return getByteValue();
        } else if (type.equals(Boolean.class) || type.equals(boolean.class)) {
            return getBooleanValue();
        } else if (type.isEnum()) {
            return getEnumValue(type);
        } else {
            throw new HostException("Attempt to get binding "
                    + getBindingName() + " as an incompatible type " + type);
        }
    }

    /** {@inheritDoc} */
    public void setObjectValue(final Object value) throws HostException {
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
        } else if (value instanceof Float) {
            setFloatValue((Float) value);
        } else if (value instanceof Double) {
            setDoubleValue((Double) value);
        } else if (value instanceof Integer) {
            setIntegerValue((Integer) value);
        } else if (value instanceof Short) {
            setShortValue((Short) value);
        } else if (value instanceof Byte) {
            setByteValue((Byte) value);
        } else if (value instanceof Boolean) {
            setBooleanValue((Boolean) value);
        } else if (value instanceof Enum) {
            setEnumValue((Enum < ? >) value);
        } else {
            throw new HostException("Attempt to set binding "
                    + getBindingName() + " from an incompatible value " + value);
        }
    }

    /** {@inheritDoc} */
    public boolean isSet() {
        return (mValue != null);
    }
}
