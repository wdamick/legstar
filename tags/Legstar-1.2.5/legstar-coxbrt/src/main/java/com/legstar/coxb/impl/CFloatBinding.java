/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.impl;

import java.math.BigDecimal;

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.ICobolFloatBinding;
import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.common.CBinding;
import com.legstar.coxb.host.HostException;

/**
 * This class implements the behavior of a comp-1 cobol element bound to
 * a JAXB Float property.
 *
 * @author Fady Moussallam
 * 
 */
public class CFloatBinding extends CBinding
implements ICobolFloatBinding {

    /** The current value for this element. */
    private Float mValue = null;

    /** Floats have a fixed host byte length. */
    public static final int BYTE_LENGTH = 4;
    
    /**
     * Constructor for a cobol element to java binding.
     * 
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param cobolAnnotations the cobol annotations for this element
     * @param parentBinding a reference to the parent binding
     */
    public CFloatBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final CobolElement cobolAnnotations,
            final ICobolComplexBinding parentBinding) {
        super(bindingName, jaxbName, jaxbType, cobolAnnotations, parentBinding);
    }

    /** {@inheritDoc} */
    public void accept(final CobolElementVisitor cev)
    throws HostException {
        cev.visit(this);
    }

    /** {@inheritDoc} */
    public Float getFloatValue() throws HostException {
        return mValue;
    }

    /** {@inheritDoc} */
    public void setFloatValue(final Float value) throws HostException {
        mValue = value;
    }

    /** {@inheritDoc} */
    public BigDecimal getBigDecimalValue() throws HostException {
        return new BigDecimal(mValue);
    }

    /** {@inheritDoc} */
    public void setBigDecimalValue(
            final BigDecimal value) throws HostException {
        mValue = value.floatValue();
    }

    /** {@inheritDoc} */
    public int calcByteLength() {
        return calcFloatByteLength();
    }

    /**
     * Calculates the host byte length for a COMP-1.
     * @return the host byte length for a COMP-1
     */
    public static int calcFloatByteLength() {
        return BYTE_LENGTH;
    }

    /** {@inheritDoc} */
    public Object getObjectValue(
            final Class < ? > type) throws HostException {
        if (type.equals(Float.class) || type.equals(float.class)) {
            return mValue;
        } else if (type.equals(BigDecimal.class)) {
            return getBigDecimalValue();
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
        if (value instanceof Float) {
            mValue = (Float) value;
        } else if (value instanceof BigDecimal) {
            setBigDecimalValue((BigDecimal) value);
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
