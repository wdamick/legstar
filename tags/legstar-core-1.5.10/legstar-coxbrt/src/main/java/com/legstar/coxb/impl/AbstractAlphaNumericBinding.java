/*******************************************************************************
 * Copyright (c) 2011 LegSem.
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
import java.sql.Timestamp;
import java.util.Date;

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.common.CBinding;
import com.legstar.coxb.host.HostException;

/**
 * A generic binding for alphanumeric bindings. All alphanumeric values are
 * stored in a String.
 */
public abstract class AbstractAlphaNumericBinding extends CBinding {

    /** The current value for this element. */
    private String mValue = null;

    /**
     * Constructor for a cobol element to java binding.
     * 
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param cobolAnnotations the cobol annotations for this element
     * @param parentBinding a reference to the parent binding
     */
    public AbstractAlphaNumericBinding(final String bindingName,
            final String jaxbName, final Class < ? > jaxbType,
            final CobolElement cobolAnnotations,
            final ICobolComplexBinding parentBinding) {
        super(bindingName, jaxbName, jaxbType, cobolAnnotations, parentBinding);
    }

    /** {@inheritDoc} */
    public String getStringValue() throws HostException {
        return mValue;
    }

    /** {@inheritDoc} */
    public void setStringValue(final String value) throws HostException {
        mValue = value;
    }

    /** {@inheritDoc} */
    public Object getObjectValue(final Class < ? > type) throws HostException {
        if (type.equals(String.class)) {
            return mValue;
        } else if (type.equals(Date.class)) {
            return new Date(Timestamp.valueOf(mValue).getTime());
        } else if (type.isEnum()) {
            /*
             * If result is request to be an enum, return instance corresponding
             * to this value
             */
            try {
                Method fromValue = getFromValueMethod(type);
                return fromValue.invoke(null, mValue.trim());
            } catch (SecurityException e) {
                throw new HostException(e);
            } catch (IllegalArgumentException e) {
                throw new HostException(e);
            } catch (IllegalAccessException e) {
                throw new HostException(e);
            } catch (InvocationTargetException e) {
                throw new HostException(e);
            }

        } else {
            throw new HostException("Attempt to get binding "
                    + getBindingName() + " as an incompatible type " + type);
        }
    }

    /**
     * Enum classes can be JAXB Enum wrappers with a "fromValue" method or plain
     * POJO Enums.
     * <p/>
     * Here we lookup a method that takes a String representation of the Enum
     * and returns the Enum item.
     * 
     * @param type an Enum type
     * @return the method that turns a String into an Enum item
     * @throws HostException if method cannot be found
     */
    protected Method getFromValueMethod(Class < ? > type) throws HostException {
        try {
            Method fromValueMethod = null;
            try {
                fromValueMethod = type.getMethod("fromValue", String.class);
            } catch (NoSuchMethodException e) {
                fromValueMethod = type.getMethod("valueOf", String.class);
            }
            return fromValueMethod;
        } catch (SecurityException e) {
            throw new HostException(e);
        } catch (NoSuchMethodException e) {
            throw new HostException(e);
        }
    }

    /** {@inheritDoc} */
    public void setObjectValue(final Object value) throws HostException {
        if (value == null) {
            mValue = null;
            return;
        }
        if (value instanceof String) {
            mValue = (String) value;
        } else if (value instanceof Date) {
            mValue = new Timestamp(((Date) value).getTime()).toString();
        } else if (value instanceof Enum < ? >) {
            try {
                Method valueMethod = getValueMethod(value.getClass());
                mValue = (String) valueMethod.invoke(value);
            } catch (SecurityException e) {
                throw new HostException(e);
            } catch (IllegalArgumentException e) {
                throw new HostException(e);
            } catch (IllegalAccessException e) {
                throw new HostException(e);
            } catch (InvocationTargetException e) {
                throw new HostException(e);
            }
        } else {
            throw new HostException("Attempt to set binding "
                    + getBindingName() + " from an incompatible value " + value);
        }
    }

    /**
     * Enum classes can be JAXB Enum wrappers with a "value" method or plain
     * POJO Enums.
     * <p/>
     * Here we lookup a method that returns a String representation of the Enum
     * value by first trying the JAXB type, then the POJO type.
     * 
     * @param type the Enum type
     * @return the method that returns a String representation of that enum item
     * @throws HostException if method cannot be found
     */
    protected Method getValueMethod(Class < ? > type) throws HostException {
        try {
            Method valueMethod = null;
            try {
                valueMethod = type.getMethod("value");
            } catch (NoSuchMethodException e) {
                valueMethod = type.getMethod("toString");
            }
            return valueMethod;
        } catch (SecurityException e) {
            throw new HostException(e);
        } catch (NoSuchMethodException e) {
            throw new HostException(e);
        }
    }

    /** {@inheritDoc} */
    public boolean isSet() {
        return (mValue != null);
    }
}
