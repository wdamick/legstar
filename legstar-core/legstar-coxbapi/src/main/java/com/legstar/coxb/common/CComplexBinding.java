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
package com.legstar.coxb.common;

import java.util.List;

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolArrayComplexBinding;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.ICobolNumericBinding;
import com.legstar.coxb.host.HostException;

/**
 * This class is a superclass of all other complex element types implementing
 * binding between a java type and cobol.
 * 
 * @author Fady Moussallam
 */
public abstract class CComplexBinding extends CBinding implements
        ICobolComplexBinding {

    /** Ordered list of child elements. */
    private java.util.List < ICobolBinding > mChildren;

    /**
     * Variable size arrays or lists, without an explicit depending on clause,
     * have a dynamic counter which gets dynamically created and added to this
     * complex binding children. Because Cobol does not like such counters to be
     * variably located we store them as the first children. This counter keeps
     * track of how many such dynamic counters we already have.
     */
    private int mDynamicCountersCount = 0;

    /**
     * Complex bindings can be bound to JAXB objects or arbitrary POJOs jointly
     * referred to as value objects. This property is the fully qualified java
     * class name of the bound value object.
     */
    private String mValueObjectClassName;

    /**
     * Optional factory class name used to create bound value objects. If null,
     * value objects are assumed to have a no-argument constructor.
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
    public CComplexBinding(final String bindingName, final String jaxbName,
            final Class < ? > jaxbType, final CobolElement cobolAnnotations,
            final ICobolComplexBinding parentBinding) {
        super(bindingName, jaxbName, jaxbType, cobolAnnotations, parentBinding);
        mChildren = new java.util.LinkedList < ICobolBinding >();
    }

    /** {@inheritDoc} */
    public void accept(final CobolElementVisitor cev) throws HostException {
        cev.visit(this);
    }

    /**
     * {@inheritDoc}
     * <p/>
     * If length exceed Integer.MAX_VALUE, returns Integer.MAX_VALUE.
     */
    public int calcByteLength() {
        long byteLength = 0;
        for (ICobolBinding child : mChildren) {
            byteLength += child.getByteLength();
        }
        return byteLength > Integer.MAX_VALUE ? Integer.MAX_VALUE
                : (int) byteLength;
    }

    /** {@inheritDoc} */
    public java.util.List < ICobolBinding > getChildrenList() {
        return mChildren;
    }

    /**
     * @param children the children list to set
     */
    public void setChildrenList(final java.util.List < ICobolBinding > children) {
        mChildren = children;
    }

    /**
     * Store a new counter as a child of the root complex element. If this
     * complex element is root (has no parent), the counter is inserted at the
     * beginning of the children list in order to ensure that it is not variably
     * located in the corresponding Cobol layout. If this is not a root element,
     * the request is propagated up to parent.
     * 
     * @param counter the counter to add to children list
     */
    public void storeCounter(final ICobolNumericBinding counter) {
        if (getParentBinding() == null) {
            /*
             * If there is already a child, use the same cobol level number for
             * the inserted counter, otherwise it might have a lower level than
             * the next child which would create an unwanted hierarchy
             */
            if (getChildrenList().size() > 0) {
                counter.setLevelNumber(getChildrenList().get(0)
                        .getLevelNumber());
            }
            getChildrenList().add(mDynamicCountersCount, counter);
            mDynamicCountersCount++;
        } else {
            getParentBinding().storeCounter(counter);
        }
    }

    /** {@inheritDoc} */
    public void storeCounter(final ICobolBinding counter) {
        storeCounter((ICobolNumericBinding) counter);
    }

    /**
     * When a list size is known, this method updates the corresponding counter
     * (if any). Counters are kept at the root level so if this complex binding
     * is not root (has a parent), the request is propagated up.
     * 
     * @param cobolName cobol name of the counter
     * @param count the array or list size
     * @throws HostException if counter cannot be updated
     */
    public void setCounterValue(final String cobolName, final int count)
            throws HostException {
        ICobolNumericBinding counter = getCounter(cobolName);
        counter.setIntegerValue(count);
    }

    /** {@inheritDoc} */
    public int getCounterValue(final String cobolName) throws HostException {
        ICobolNumericBinding counter = getCounter(cobolName);
        return counter.getIntegerValue();
    }

    /** {@inheritDoc} */
    public ICobolNumericBinding getCounter(final String cobolName)
            throws HostException {

        // When the root binding is reached, start looking for the counter in
        // its children. Otherwise, pass control to parent
        if (getParentBinding() == null) {
            ICobolNumericBinding counter = getCounterInChildren(
                    getChildrenList(), cobolName);
            if (counter == null) {
                throw new HostException("Cannot locate counter " + cobolName);
            } else {
                return counter;
            }
        } else {
            return getParentBinding().getCounter(cobolName);
        }
    }

    /**
     * Descend the children of a complex or choice element looking for a counter
     * with the corresponding COBOL name.
     * <p/>
     * First lookup numeric children for the requested counter. If not found,
     * give a chance to complex and choice children for finding the counter
     * within their children.
     * 
     * @param children the list of children element to search
     * @param cobolName the COBOL name we are looking for
     * @return the element mapping the COBOL counter or null if not found
     */
    protected ICobolNumericBinding getCounterInChildren(
            List < ICobolBinding > children, final String cobolName)
            throws HostException {

        ICobolNumericBinding counter = null;
        for (ICobolBinding child : children) {
            if (child instanceof ICobolNumericBinding
                    && child.getCobolName().equals(cobolName)) {
                return (ICobolNumericBinding) child;
            }
        }
        for (ICobolBinding child : children) {
            if (child instanceof ICobolComplexBinding) {
                counter = getCounterInChildren(
                        ((ICobolComplexBinding) child).getChildrenList(),
                        cobolName);
            } else if (child instanceof ICobolChoiceBinding) {
                counter = getCounterInChildren(
                        ((ICobolChoiceBinding) child).getAlternativesList(),
                        cobolName);
            } else if (child instanceof ICobolArrayComplexBinding) {
                counter = getCounterInChildren(
                        ((ICobolArrayComplexBinding) child)
                                .getComplexItemBinding().getChildrenList(),
                        cobolName);
            }
            if (counter != null) {
                return counter;
            }
        }
        return counter;
    }

    /**
     * @return the number of dynamic counters this complex element is handling
     */
    public int getDynamicCountersCount() {
        return mDynamicCountersCount;
    }

    /**
     * This method is meant to be overridden. If it is not, then we are dealing
     * with an previous version of a binding object which did not implement this
     * method. For backward compatibility, we route the call to the now
     * deprecated <code>createJaxbObject</code> {@inheritDoc}
     */
    public void createValueObject() throws HostException {
        createJaxbObject();
    }

    /**
     * This method is meant to be overridden. If it is not, then we are dealing
     * with an previous version of a binding object which did not implement this
     * method. For backward compatibility, we route the call to the now
     * deprecated <code>setJaxbPropertyValue</code> {@inheritDoc}
     */
    public void setPropertyValue(final int index) throws HostException {
        setJaxbPropertyValue(index);
    }

    /**
     * {@inheritDoc}
     * 
     * @deprecated
     */
    public void createJaxbObject() throws HostException {
    }

    /**
     * {@inheritDoc}
     * 
     * @deprecated
     */
    public void setJaxbPropertyValue(final int index) throws HostException {
    }

    /**
     * {@inheritDoc}
     */
    public String getValueObjectClassName() {
        return mValueObjectClassName;
    }

    /**
     * {@inheritDoc}
     */
    public void setValueObjectClassName(final String valueObjectClassName) {
        mValueObjectClassName = valueObjectClassName;
    }

    /**
     * {@inheritDoc}
     */
    public String getValueObjectsFactoryClassName() {
        return mValueObjectsFactoryClassName;
    }

    /**
     * {@inheritDoc}
     */
    public void setValueObjectsFactoryClassName(
            final String valueObjectsFactoryClassName) {
        mValueObjectsFactoryClassName = valueObjectsFactoryClassName;
    }

}
