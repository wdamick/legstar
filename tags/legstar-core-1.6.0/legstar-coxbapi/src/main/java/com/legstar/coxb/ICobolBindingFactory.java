/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb;

/**
 * This is an abstract representation of a binding factory used to shield
 * users from a particular binding implementation.
 */
public interface ICobolBindingFactory {

    /**
     * Create a cobol binding without a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolStringBinding createStringBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding with a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolStringBinding createStringBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding without a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolOctetStreamBinding createOctetStreamBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding with a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolOctetStreamBinding createOctetStreamBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding without a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolNationalBinding createNationalBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding with a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolNationalBinding createNationalBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding without a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolDbcsBinding createDbcsBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding with a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolDbcsBinding createDbcsBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding without a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolZonedDecimalBinding createZonedDecimalBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding with a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolZonedDecimalBinding createZonedDecimalBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding without a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolPackedDecimalBinding createPackedDecimalBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding with a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolPackedDecimalBinding createPackedDecimalBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding without a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolBinaryBinding createBinaryBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding with a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolBinaryBinding createBinaryBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding without a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolFloatBinding createFloatBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding with a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolFloatBinding createFloatBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding without a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolDoubleBinding createDoubleBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding with a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolDoubleBinding createDoubleBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding without a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolArrayStringBinding createArrayStringBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding with a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolArrayStringBinding createArrayStringBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding without a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolArrayOctetStreamBinding createArrayOctetStreamBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding with a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolArrayOctetStreamBinding createArrayOctetStreamBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding without a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolArrayNationalBinding createArrayNationalBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding with a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolArrayNationalBinding createArrayNationalBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding without a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolArrayDbcsBinding createArrayDbcsBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding with a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolArrayDbcsBinding createArrayDbcsBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding without a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolArrayZonedDecimalBinding createArrayZonedDecimalBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding with a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolArrayZonedDecimalBinding createArrayZonedDecimalBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding without a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolArrayPackedDecimalBinding createArrayPackedDecimalBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding with a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolArrayPackedDecimalBinding createArrayPackedDecimalBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding without a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolArrayBinaryBinding createArrayBinaryBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding with a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolArrayBinaryBinding createArrayBinaryBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding without a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolArrayFloatBinding createArrayFloatBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding with a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolArrayFloatBinding createArrayFloatBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding without a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolArrayDoubleBinding createArrayDoubleBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding);

    /**
     * Create a cobol binding with a bound jaxb object.
     * @param bindingName the identifier for this binding
     * @param jaxbName the name of the bound java property
     * @param jaxbType the type of the bound java property
     * @param parentBinding a reference to the parent binding (null if none)
     * @return a concrete binding implementation
     */
    ICobolArrayDoubleBinding createArrayDoubleBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding);

}
