/*******************************************************************************
 * Copyright (c) 2008 LegSem.
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
 * This interface groups methods that are common to choice elements.
 * 
 * @author Fady Moussallam
 * 
 */
public interface ICobolChoiceBinding extends ICobolBinding {

    /**
     * Requests choice binding to set its alternatives values from bound value
     * objects.
     * 
     * @throws HostException if reading from bound object values fails
     */
    void setAlternativesValues() throws HostException;

    /**
     * @deprecated
     * Request choice binding to set to set a property of the bound Jaxb object
     * to the value of a corresponding alternative.
     * child value. Since bindings can now be bound
     * to arbitrary value objects, use method <code>setPropertyValue</code>
     * instead of this one.
     * 
     * @param index the position of the child in the alternatives list
     * @throws HostException if bound object values cannot be set
     */
    void setJaxbPropertyValue(int index) throws HostException;

    /**
     * Request choice binding to set to set a property of the bound value object
     * to the value of a corresponding alternative.
     * child value.
     * 
     * @param index the position of the child in the alternatives list
     * @throws HostException if bound object values cannot be set
     */
    void setPropertyValue(int index) throws HostException;

    /**
     * Returns the current list of alternatives for this choice element.
     * 
     * @return Ordered list of alternatives
     * @throws HostException if list cannot be created
     */
    java.util.List < ICobolBinding > getAlternativesList()
    throws HostException;

    /**
     * Retrieves an alternative identified by its name.
     * @param name the alternative name
     * @return corresponding alternative or null if not found
     */
    ICobolBinding getAlternativeByName(String name);

    /**
     * Retrieves an alternative identified by its cobol field name.
     * @param cobolName the field name
     * @return corresponding alternative or null if not found
     */
    ICobolBinding getAlternativeByCobolName(String cobolName);

    /**
     * When alternatives are discovered dynamically, this method allows
     * other classes to participate in the alternative list buildup.
     * 
     * @param ce redefined or redefining cobol element description
     *  */
    void addAlternative(final ICobolBinding ce);

    /**
     * A choice element always belongs to a complex element (roots can never
     * be choices). This method returns a reference to the complex parent
     * binding.
     * 
     * @return a reference to the parent binding
     *  */
    ICobolComplexBinding getParentBinding();

    /**
     * @deprecated
     * Retrieves the parent jaxb object. Choice bindings are not directly
     * bound to a jaxb object themselves but they are always children of a
     * complex binding. Since bindings can now be bound
     * to arbitrary value objects, use method <code>getParentValueObject</code>
     * instead of this one.
     * 
     * @return the parent bound jaxb object
     * @throws HostException if parent jaxb object cannot be retrieved
     */
    Object getParentJaxbObject() throws HostException;

    /**
     * Retrieves the parent's value object. Choice bindings are not directly
     * bound to a value object themselves but they are always children of a
     * complex binding.
     * 
     * @return the parent's bound value object
     * @throws HostException if parent's value object cannot be retrieved
     */
    Object getParentValueObject() throws HostException;

    /**
     * Sets a instance of a class providing additional logic to select an
     * alternative within a choice element at marshaling (Java to Host) time.
     * @param strategy a choice strategy implementation
     */
    void setMarshalChoiceStrategy(ICobolMarshalChoiceStrategy strategy);

    /**
     * Gets an instance of a class providing additional logic to select an
     * alternative within a choice element at marshaling (Java to Host) time.
     * @return a choice strategy implementation
     * @throws HostException if strategy cannot be loaded
     */
    ICobolMarshalChoiceStrategy getMarshalChoiceStrategy()
    throws HostException;

    /**
     * Sets a instance of a class providing additional logic to select an
     * alternative within a choice element at unmarshaling (Host to Java) time.
     * @param strategy a choice strategy implementation
     */
    void setUnmarshalChoiceStrategy(ICobolUnmarshalChoiceStrategy strategy);

    /**
     * Gets an instance of a class providing additional logic to select an
     * alternative within a choice element at unmarshaling (Host to Java) time.
     * @return a choice strategy implementation
     * @throws HostException if strategy cannot be loaded
     */
    ICobolUnmarshalChoiceStrategy getUnmarshalChoiceStrategy()
    throws HostException;

}
