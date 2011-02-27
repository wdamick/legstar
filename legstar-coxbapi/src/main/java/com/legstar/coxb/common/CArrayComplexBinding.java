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
import com.legstar.coxb.ICobolArrayComplexBinding;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.host.HostException;

/**
 * This class is a superclass of all arrays of complex element types
 * implementing binding between a java type and cobol.
 *
 * @author Fady Moussallam
 * 
 */
public abstract class CArrayComplexBinding extends CArrayBinding
implements ICobolArrayComplexBinding {

    /** Reference to the complex binding representing each occurrence of this 
     * array. */
    private ICobolComplexBinding mComplexItemBinding;

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
     * Constructor for a cobol element to java binding.
     * 
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param cobolAnnotations the cobol annotations for this element
     * @param parentBinding a reference to the parent binding if any
     * @param complexItemBinding a binding element for array items
     */
    public CArrayComplexBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final CobolElement cobolAnnotations,
            final ICobolComplexBinding parentBinding,
            final ICobolComplexBinding complexItemBinding) {
        super(bindingName, jaxbName, jaxbType, cobolAnnotations, parentBinding);
        mComplexItemBinding = complexItemBinding;
    }

    /** {@inheritDoc} */
    public void accept(final CobolElementVisitor cev)
    throws HostException {
        cev.visit(this);
    }

    /** {@inheritDoc} */
    public int calcItemByteLength() {
        return getComplexItemBinding().getByteLength();
    }

    /**
     * @return the Complex Binding describing an item
     */
    public ICobolComplexBinding getComplexItemBinding() {
        return mComplexItemBinding;
    }

    /**
     * @param complexItemBinding the item Complex Binding to set
     */
    public void setComplexItemBinding(
            final ICobolComplexBinding complexItemBinding) {
        mComplexItemBinding = complexItemBinding;
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
     * deprecated <code>addJaxbPropertyValue</code>
     * {@inheritDoc}
     */
    public void addPropertyValue(final int index) throws HostException {
        addJaxbPropertyValue(index);
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
    public void addJaxbPropertyValue(final int index) throws HostException {
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
    public void setValueObjectClassName(
            final String valueObjectClassName) {
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
