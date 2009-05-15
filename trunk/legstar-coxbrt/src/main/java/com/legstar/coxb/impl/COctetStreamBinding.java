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

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolUsage;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.ICobolOctetStreamBinding;
import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.common.CBinding;
import com.legstar.coxb.host.HostException;
import com.legstar.coxb.util.PictureUtil;

/**
 * This class implements the behavior of a binary cobol element bound to
 * a JAXB byte array property.
 * COBOL elements could be PIC X which were explicitly changed to OCTET_STREAM_ITEM
 * or COBOL special types such as INDEX or POINTERS that we would not be able to
 * map to anything meaningful in Java.
 *
 * @author Fady Moussallam
 * 
 */
public class COctetStreamBinding extends CBinding
implements ICobolOctetStreamBinding {

    /** The current value for this element. */
    private byte[] mValue =  null;

    /**
     * Constructor for a cobol element to java binding.
     * 
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param cobolAnnotations the cobol annotations for this element
     * @param parentBinding a reference to the parent binding
     */
    public COctetStreamBinding(
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
    public byte[] getByteArrayValue() throws HostException {
        return mValue;
    }

    /** {@inheritDoc} */
    public void setByteArrayValue(final byte[] value)
    throws HostException {
        mValue = value;
    }

    /** {@inheritDoc} */
    public int calcByteLength() {
        return calcOctetStreamByteLength(getPicture(), getUsage());
    }

    /**
     * Calculates the host byte length for a PIC X(n).
     * @param picture the picture clause
     * @param usage the usage clause
     * @return the host byte length for a PIC X(n)
     */
    public static int calcOctetStreamByteLength(
            final String picture, final String usage) {
        if (usage.equals(CobolUsage.INDEX)) {
            return 4;
        } else if (usage.equals(CobolUsage.POINTER)) {
            return 4;
        } else if (usage.equals(CobolUsage.PROCEDURE_POINTER)) {
            return 8;
        } else if (usage.equals(CobolUsage.FUNCTION_POINTER)) {
            return 4;
        }
        return PictureUtil.getSymbolsNumber('X', picture);
    }

    /** {@inheritDoc} */
    public Object getObjectValue(
            final Class < ? > type) throws HostException {
        if (type.equals(byte[].class)) {
            return mValue;
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
        if (value instanceof byte[]) {
            mValue = (byte[]) value;
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
