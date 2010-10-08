/*******************************************************************************
 * Copyright (c) 2010 LegSem.
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
    public AbstractAlphaNumericBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
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
    public Object getObjectValue(
            final Class < ? > type) throws HostException {
        if (type.equals(String.class)) {
            return mValue;
        } else if (type.isEnum()) {
            /* If result is request to be an enum, return instance corresponding
             * to this value */
            try {
                Method fromValue = type.getMethod("fromValue", String.class);
                return fromValue.invoke(null, mValue.trim());
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

        } else {
            throw new HostException("Attempt to get binding " + getBindingName()
                    + " as an incompatible type " + type);
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
        } else if (value instanceof Enum < ? >) {
            try {
                Method valueMethod = value.getClass().getMethod("value");
                mValue = (String) valueMethod.invoke(value);
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
        } else {
            throw new HostException("Attempt to set binding " + getBindingName()
                    + " from an incompatible value " + value);
        }
    }

    /** {@inheritDoc} */
    public boolean isSet() {
        return (mValue != null);
    }
}
