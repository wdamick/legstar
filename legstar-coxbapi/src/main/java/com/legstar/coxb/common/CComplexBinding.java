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
package com.legstar.coxb.common;

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolNumericBinding;
import com.legstar.coxb.host.HostException;

/**
 * This class is a superclass of all other complex element types implementing
 * binding between a java type and cobol.
 *
 * @author Fady Moussallam
 */
public abstract class CComplexBinding extends CBinding implements ICobolComplexBinding {

    /** Ordered list of child elements. */
    private java.util.List < ICobolBinding > mChildren;

    /** Variable size arrays or lists, without an explicit depending on clause,
     * have a dynamic counter which gets dynamically created and added to this
     * complex binding children. Because Cobol does not like such counters to
     * be variably located we store them as the first children. This counter
     * keeps track of how many such dynamic counters we already have. */
    private int mDynamicCountersCount = 0;

    /**
     * Complex bindings can be bound to JAXB objects or arbitrary POJOs
     * jointly referred to as value objects.
     * This property is the fully qualified java class name of the bound
     * value object.
     */
    private String mValueObjectClassName;

    /**
     * Optional factory class name used to create bound value objects.
     * If null, value objects are assumed to have a no-argument constructor.
     */
    private String mValueObjectsFactoryClassName;


    /**
     * Constructor for a cobol complex element to java binding.
     * 
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param cobolAnnotations the cobol annotations for this element
     * @param parentBinding a reference to the parent binding if any
     */
    public CComplexBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final CobolElement cobolAnnotations,
            final ICobolComplexBinding parentBinding) {
        super(bindingName, jaxbName, jaxbType, cobolAnnotations, parentBinding);
        mChildren = new java.util.LinkedList < ICobolBinding >();
    }

    /** {@inheritDoc} */
    public final void accept(final CobolElementVisitor cev)
    throws HostException {
        cev.visit(this);
    }

    /**
     * {@inheritDoc}
     */
    public final int calcByteLength() {
        int byteLength = 0;
        for (ICobolBinding child : mChildren) {
            byteLength += child.getByteLength();
        }
        return byteLength;
    }

    /** {@inheritDoc} */
    public final java.util.List < ICobolBinding > getChildrenList() {
        return mChildren;
    }

    /**
     * @param children the children list to set
     */
    public final void setChildrenList(
            final java.util.List < ICobolBinding > children) {
        mChildren = children;
    }

    /**
     * Store a new counter as a child of the root complex element.
     * If this complex element is root (has no parent), the counter
     * is inserted at the beginning of the children list in order to
     * ensure that it is not variably located in the corresponding
     * Cobol layout. If this is not a root element, the request is
     * propagated up to parent.
     * @param counter the counter to add to children list
     */
    public final void storeCounter(
            final ICobolNumericBinding counter) {
        if (getParentBinding() == null) {
            /* If there is already a child, use the same cobol level number
             * for the inserted counter, otherwise it might have a lower level
             * than the next child which would create an unwanted hierarchy */
            if (getChildrenList().size() > 0) {
                counter.setLevelNumber(
                        getChildrenList().get(0).getLevelNumber());
            }
            getChildrenList().add(mDynamicCountersCount, counter);
            mDynamicCountersCount++;
        } else {
            getParentBinding().storeCounter(counter);
        }
    }
    
    /** {@inheritDoc} */
    public final void storeCounter(final ICobolBinding counter) {
        storeCounter((ICobolNumericBinding) counter); 
    }

    /**
     * When a list size is known, this method updates the corresponding counter
     * (if any). Counters are kept at the root level so if this complex binding
     * is not root (has a parent), the request is propagated up.
     * @param cobolName cobol name of the counter
     * @param count the array or list size
     * @throws HostException if counter cannot be updated
     */
    public final void setCounterValue(
            final String cobolName,
            final int count) throws HostException {
        /* Look for a counter with the corresponding cobol name */
        for (ICobolBinding child : getChildrenList()) {
            if (child.getCobolName().compareTo(cobolName) == 0) {
                child.setObjectValue(Integer.valueOf(count));
                return;
            }
        }
        /* We don't own this counter, see if ancestors know of it */
        if (getParentBinding() != null) {
            getParentBinding().setCounterValue(cobolName, count);
        } else {
            throw new HostException("Cannot locate counter " + cobolName);
        }
    }

    /** {@inheritDoc} */
    public final ICobolNumericBinding getCounter(
            final String cobolName) throws HostException {
        /* Look for a counter with the corresponding cobol name */
        for (ICobolBinding child : getChildrenList()) {
            if (child.getCobolName().compareTo(cobolName) == 0) {
                return (ICobolNumericBinding) child;
            }
        }
        /* We don't own this counter, see if ancestors know of it */
        if (getParentBinding() != null) {
            return getParentBinding().getCounter(cobolName);
        } else {
            throw new HostException("Cannot locate counter " + cobolName);
        }
    }

    /** {@inheritDoc} */
    public final int getCounterValue(
            final String cobolName) throws HostException {
        /* Look for a counter with the corresponding cobol name */
        for (ICobolBinding child : getChildrenList()) {
            if (child.getCobolName().compareTo(cobolName) == 0) {
                return (Integer) child.getObjectValue(Integer.class);
            }
        }
        /* We don't own this counter, see if ancestors know of it */
        if (getParentBinding() != null) {
            return getParentBinding().getCounterValue(cobolName);
        } else {
            throw new HostException("Cannot locate counter " + cobolName);
        }
    }

    /**
     * @return the number of dynamic counters this complex element is handling
     */
    public final int getDynamicCountersCount() {
        return mDynamicCountersCount;
    }

    /**
     * This method is meant to be overridden. If it is not, then we are dealing
     * with an previous version of a binding object which did not implement
     * this method. For backward compatibility, we route the call to the now
     * deprecated <code>createJaxbObject</code>
     * {@inheritDoc}
     */
    public void createValueObject() throws HostException {
        createJaxbObject();
    }

    /**
     * This method is meant to be overridden. If it is not, then we are dealing
     * with an previous version of a binding object which did not implement
     * this method. For backward compatibility, we route the call to the now
     * deprecated <code>setJaxbPropertyValue</code>
     * {@inheritDoc}
     */
    public void setPropertyValue(final int index) throws HostException {
        setJaxbPropertyValue(index);
    }

    /**
     * {@inheritDoc}
     * @deprecated
     */
    public void createJaxbObject() throws HostException {
    }
    /**
     * {@inheritDoc}
     * @deprecated
     */
    public void setJaxbPropertyValue(final int index) throws HostException {
    }

    /**
     * {@inheritDoc}
     */
    public final String getValueObjectClassName() {
        return mValueObjectClassName;
    }

    /**
     * {@inheritDoc}
     */
    public final void setValueObjectClassName(
            final String valueObjectClassName) {
        mValueObjectClassName = valueObjectClassName;
    }

    /**
     * {@inheritDoc}
     */
    public final String getValueObjectsFactoryClassName() {
        return mValueObjectsFactoryClassName;
    }

    /**
     * {@inheritDoc}
     */
    public final void setValueObjectsFactoryClassName(
            final String valueObjectsFactoryClassName) {
        mValueObjectsFactoryClassName = valueObjectsFactoryClassName;
    }

}
