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
package com.legstar.coxb;

import com.legstar.coxb.host.HostException;

/**
 * This interface groups methods that are common to complex elements that are
 * not arrays.
 * 
 * @author Fady Moussallam
 * 
 */
public interface ICobolComplexBinding extends ICobolBinding {

    /**
     * @deprecated
     * Requests complex binding to create an instance of its bound
     * jaxb object. Since bindings can now be bound to arbitrary
     * value objects, use method <code>createValueObject</code>
     * instead of this one.
     * 
     * @throws HostException if initialization fails
     */

    void createJaxbObject() throws HostException;

    /**
     * Requests complex binding to create an instance of its bound
     * object.
     * 
     * @throws HostException if initialization fails
     */
    void createValueObject() throws HostException;

    /**
     * Requests complex binding to set its children values from bound
     * Jaxb objects values.
     * 
     * @throws HostException if reading from bound object values fails
     */
    void setChildrenValues() throws HostException;

    /**
     * @deprecated
     * Request complex binding to set a property of the bound Jaxb object
     * to the value of a corresponding child.  Since bindings can now be bound
     * to arbitrary value objects, use method <code>setPropertyValue</code>
     * instead of this one.
     * 
     * @param index the position of the child in the complex element child list
     * @throws HostException if bound object values cannot be set
     */
    void setJaxbPropertyValue(int index) throws HostException;

    /**
     * Request complex binding to set a property of the bound value object
     * to the value of a corresponding child.
     * 
     * @param index the position of the child in the complex element child list
     * @throws HostException if bound object values cannot be set
     */
    void setPropertyValue(int index) throws HostException;

    /**
     * Request a list of children from this complex binding.
     * 
     * @return Ordered list of children
     * @throws HostException if list cannot be created
     */
    java.util.List < ICobolBinding > getChildrenList()
    throws HostException;

    /**
     * @return the java object factory for bound objects creation
     */
    Object getObjectFactory();

    /**
     * @param objectFactory the java object factory for bound objects creation 
     */
    void setObjectFactory(Object objectFactory);

    /**
     * @return the number of dynamic counters this complex element is handling
     */
    int getDynamicCountersCount();

    /**
     * Store a new counter as a child of the root complex element.
     * If this complex element is root (has no parent), the counter
     * is inserted at the beginning of the children list in order to
     * ensure that it is not variably located in the corresponding
     * Cobol layout. If this is not a root element, the request is
     * propagated up to parent.
     * @param counter the counter to add to children list
     */
    void storeCounter(final ICobolBinding counter);

    /**
     * Set the value of a specific counter either in us or in one of our
     * ancestors.
     * @param cobolName cobol name of the counter
     * @param count the array or list size
     * @throws HostException if counter cannot be updated
     */
    void setCounterValue(String cobolName, int count) throws HostException;

    /**
     * Get the value of a specific counter either from us or one of our
     * ancestors.
     * @param cobolName cobol name of the counter
     * @return the array or list size
     * @throws HostException if counter cannot be queried
     */
    int getCounterValue(String cobolName) throws HostException;

    /**
     * Complex bindings can be bound to JAXB objects or arbitrary POJOs
     * jointly referred to as value objects.
     * This property is the fully qualified java class name of the bound
     * value object.
     * @return the fully qualified bound value object class name
     */
    String getValueObjectClassName();

    /**
     * @param valueObjectClassName the fully qualified bound value object class
     *  name to set
     */
    void setValueObjectClassName(final String valueObjectClassName);

    /**
     * Optional factory class name used to create bound value objects.
     * If null, value objects are assumed to have a no-argument constructor.
     * @return the factory class name used to create bound value objects
     */
    String getValueObjectsFactoryClassName();

    /**
     * @param valueObjectsFactoryClassName the factory class name used to create
     *  bound value objects to set
     */
    void setValueObjectsFactoryClassName(
            final String valueObjectsFactoryClassName);
}
