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
package com.legstar.coxb.common;

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.ICobolArrayBinding;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.ICobolNumericBinding;
import com.legstar.coxb.host.HostException;

/**
 * This generic class implements behavior common to all array bindings.  
 *
 */
public abstract class CArrayBinding extends CBinding
implements ICobolArrayBinding {
    
    /** A reference to a counter for variable size arrays. */
    private ICobolNumericBinding mCounter;
    
    /** Array individual item length in bytes (unknown by default). */
    private int mItemByteLength = 0;
    
    /**
     * Constructor for a cobol element to java binding.
     * 
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param cobolAnnotations the cobol annotations for this element
     * @param parentBinding a reference to the parent binding if any
     */
    public CArrayBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final CobolElement cobolAnnotations,
            final ICobolComplexBinding parentBinding) {
        super(bindingName, jaxbName, jaxbType, cobolAnnotations, parentBinding);
    }

    /** 
     * {@inheritDoc}
     */
    public int getCurrentOccurs() throws HostException {
        /* If this is a variable size array, ask ancestors for the current
         * value of the counter we depend on. */
        if (isVariableSize()) {
            return getCounter().getBigIntegerValue().intValue();
        }
        return this.getMaxOccurs();
    }

    /** {@inheritDoc} */
    public int calcByteLength() {
        return getMaxOccurs() * getItemByteLength();
    }

    /**
     * This test cannot be done at construction time because the depending on
     * property can be added later.
     * @return true if this is a variable size array
     */
    public boolean isVariableSize() {
        return (getMinOccurs() < getMaxOccurs() 
                && getDependingOn() != null
                && getDependingOn().length() > 0) ? true : false;
    }

    /**
     * The first time around, this will seek the counter from the parent binding.
     * This is an expensive operation so we cache the result to speed up next
     * referrals.
     * @return the counter
     * @throws HostException if something goes wrong
     */
    private ICobolNumericBinding getCounter() throws HostException {
        if (mCounter == null) {
            mCounter = getParentBinding().getCounter(getDependingOn());
        }
        return mCounter;
    }

    /**
     * Pre version 1.2.4 generated simple arrays did not support the ItemByteLength
     * attribute. Their ByteLength attribute was actually the ItemByteLength instead
     * of being the size of the entire array. We need to continue supporting this old
     * model. Hence the size adjustment here.
     * @return the Cobol element length in bytes
     */
    public int getByteLength() {
        if (isGeneratedBinding()) {
            if (!(this instanceof CArrayComplexBinding)) {
                if (mItemByteLength == 0) {
                    /* Binding was generated before 1.2.4. Adjust lengths.*/
                    int itemByteLength = super.getByteLength();
                    setItemByteLength(itemByteLength);
                    setByteLength(itemByteLength * getMaxOccurs());
                }
            }
        }
        return super.getByteLength();
    }

    /**
     * {@inheritDoc}
     */
    public int getItemByteLength() {
        if (mItemByteLength == 0) {
            if (isGeneratedBinding() && !(this instanceof CArrayComplexBinding)) {
                /* Binding was generated before 1.2.4. Adjust lengths.*/
                mItemByteLength = super.getByteLength();
                setByteLength(mItemByteLength * getMaxOccurs());
            } else {
                mItemByteLength = calcItemByteLength();
            }
        }
        return mItemByteLength;
    }

    /**
     * {@inheritDoc}
     */
    public void setItemByteLength(final int itemByteLength) {
        mItemByteLength = itemByteLength;
    }


}
