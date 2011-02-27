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

import com.legstar.coxb.ICobolArrayBinaryBinding;
import com.legstar.coxb.ICobolArrayDbcsBinding;
import com.legstar.coxb.ICobolArrayDoubleBinding;
import com.legstar.coxb.ICobolArrayFloatBinding;
import com.legstar.coxb.ICobolArrayNationalBinding;
import com.legstar.coxb.ICobolArrayOctetStreamBinding;
import com.legstar.coxb.ICobolArrayPackedDecimalBinding;
import com.legstar.coxb.ICobolArrayStringBinding;
import com.legstar.coxb.ICobolArrayZonedDecimalBinding;
import com.legstar.coxb.ICobolBinaryBinding;
import com.legstar.coxb.ICobolBindingFactory;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.ICobolDbcsBinding;
import com.legstar.coxb.ICobolDoubleBinding;
import com.legstar.coxb.ICobolFloatBinding;
import com.legstar.coxb.ICobolNationalBinding;
import com.legstar.coxb.ICobolOctetStreamBinding;
import com.legstar.coxb.ICobolPackedDecimalBinding;
import com.legstar.coxb.ICobolStringBinding;
import com.legstar.coxb.ICobolZonedDecimalBinding;

/**
 * This is an concrete implementation of a binding factory used to shield
 * users from a particular binding implementation.
 */
public class CBindingFactory implements ICobolBindingFactory {

    /** {@inheritDoc} */
    public ICobolArrayBinaryBinding createArrayBinaryBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding) {
        return new CArrayBinaryBinding(
                bindingName, null, null, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolArrayBinaryBinding createArrayBinaryBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding) {
        return new CArrayBinaryBinding(
                bindingName, jaxbName, jaxbType, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolArrayDoubleBinding createArrayDoubleBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding) {
        return new CArrayDoubleBinding(
                bindingName, null, null, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolArrayDoubleBinding createArrayDoubleBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding) {
        return new CArrayDoubleBinding(
                bindingName, jaxbName, jaxbType, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolArrayFloatBinding createArrayFloatBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding) {
        return new CArrayFloatBinding(
                bindingName, null, null, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolArrayFloatBinding createArrayFloatBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding) {
        return new CArrayFloatBinding(
                bindingName, jaxbName, jaxbType, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolArrayNationalBinding createArrayNationalBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding) {
        return new CArrayNationalBinding(
                bindingName, null, null, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolArrayNationalBinding createArrayNationalBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding) {
        return new CArrayNationalBinding(
                bindingName, jaxbName, jaxbType, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolArrayDbcsBinding createArrayDbcsBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding) {
        return new CArrayDbcsBinding(
                bindingName, null, null, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolArrayDbcsBinding createArrayDbcsBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding) {
        return new CArrayDbcsBinding(
                bindingName, jaxbName, jaxbType, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolArrayOctetStreamBinding createArrayOctetStreamBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding) {
        return new CArrayOctetStreamBinding(
                bindingName, null, null, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolArrayOctetStreamBinding
    createArrayOctetStreamBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding) {
        return new CArrayOctetStreamBinding(
                bindingName, jaxbName, jaxbType, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolArrayPackedDecimalBinding
    createArrayPackedDecimalBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding) {
        return new CArrayPackedDecimalBinding(
                bindingName, null, null, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolArrayPackedDecimalBinding
    createArrayPackedDecimalBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding) {
        return new CArrayPackedDecimalBinding(
                bindingName, jaxbName, jaxbType, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolArrayStringBinding createArrayStringBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding) {
        return new CArrayStringBinding(
                bindingName, null, null, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolArrayStringBinding createArrayStringBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding) {
        return new CArrayStringBinding(
                bindingName, jaxbName, jaxbType, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolArrayZonedDecimalBinding createArrayZonedDecimalBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding) {
        return new CArrayZonedDecimalBinding(
                bindingName, null, null, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolArrayZonedDecimalBinding createArrayZonedDecimalBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding) {
        return new CArrayZonedDecimalBinding(
                bindingName, jaxbName, jaxbType, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolBinaryBinding createBinaryBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding) {
        return new CBinaryBinding(
                bindingName, null, null, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolBinaryBinding createBinaryBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding) {
        return new CBinaryBinding(
                bindingName, jaxbName, jaxbType, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolDoubleBinding createDoubleBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding) {
        return new CDoubleBinding(
                bindingName, null, null, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolDoubleBinding createDoubleBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding) {
        return new CDoubleBinding(
                bindingName, jaxbName, jaxbType, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolFloatBinding createFloatBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding) {
        return new CFloatBinding(
                bindingName, null, null, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolFloatBinding createFloatBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding) {
        return new CFloatBinding(
                bindingName, jaxbName, jaxbType, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolNationalBinding createNationalBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding) {
        return new CNationalBinding(
                bindingName, null, null, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolNationalBinding createNationalBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding) {
        return new CNationalBinding(
                bindingName, jaxbName, jaxbType, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolDbcsBinding createDbcsBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding) {
        return new CDbcsBinding(
                bindingName, null, null, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolDbcsBinding createDbcsBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding) {
        return new CDbcsBinding(
                bindingName, jaxbName, jaxbType, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolOctetStreamBinding createOctetStreamBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding) {
        return new COctetStreamBinding(
                bindingName, null, null, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolOctetStreamBinding createOctetStreamBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding) {
        return new COctetStreamBinding(
                bindingName, jaxbName, jaxbType, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolPackedDecimalBinding createPackedDecimalBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding) {
        return new CPackedDecimalBinding(
                bindingName, null, null, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolPackedDecimalBinding createPackedDecimalBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding) {
        return new CPackedDecimalBinding(
                bindingName, jaxbName, jaxbType, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolStringBinding createStringBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding) {
        return new CStringBinding(
                bindingName, null, null, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolStringBinding createStringBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding) {
        return new CStringBinding(
                bindingName, jaxbName, jaxbType, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolZonedDecimalBinding createZonedDecimalBinding(
            final String bindingName,
            final ICobolComplexBinding parentBinding) {
        return new CZonedDecimalBinding(
                bindingName, null, null, null, parentBinding);
    }

    /** {@inheritDoc} */
    public ICobolZonedDecimalBinding createZonedDecimalBinding(
            final String bindingName,
            final String jaxbName,
            final Class < ? > jaxbType,
            final ICobolComplexBinding parentBinding) {
        return new CZonedDecimalBinding(
                bindingName, jaxbName, jaxbType, null, parentBinding);
    }

}
