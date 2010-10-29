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
package com.legstar.coxb.impl.reflect;

import com.legstar.coxb.CobolBindingException;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.transform.AbstractHostToJavaTransformer;
import com.legstar.util.JAXBAnnotationException;
import com.legstar.util.JAXBElementDescriptor;

/**
 * This implementation of a host to java transformer dynamically binds to
 * a complex object using reflection.
 * The complex object must be a JAXB type with COBOL annotations.
 * 
 */
public class ReflectHostToJavaTransformer extends AbstractHostToJavaTransformer {

    /**
     * The JAXB element descriptor.
     */
    private JAXBElementDescriptor _jaxbElementDescriptor;

    /**
     * Construct a transformer for a particular JAXB type.
     * 
     * @param jaxbPackageName the JAXB type package name
     * @param jaxbType the JAXB type
     * @throws ReflectBindingException if JAXB type has no annotations or cannot
     *             be
     *             located from the classpath
     */
    public ReflectHostToJavaTransformer(
            final String jaxbPackageName,
            final String jaxbType) throws ReflectBindingException {
        try {
            _jaxbElementDescriptor = new JAXBElementDescriptor(jaxbPackageName,
                    jaxbType);
        } catch (JAXBAnnotationException e) {
            throw new ReflectBindingException(e);
        }
    }

    /** {@inheritDoc} */
    public ICobolComplexBinding getBinding() throws CobolBindingException {
        try {
            CComplexReflectBinding ccem = new CComplexReflectBinding(
                    getObjectFactory(), getJaxbClass());
            return ccem;
        } catch (ReflectBindingException e) {
            throw new CobolBindingException(e);
        }
    }

    /**
     * @return the JAXB type factory to use for the JAXB object type
     */
    public Object getObjectFactory() {
        return _jaxbElementDescriptor.getObjectFactory();
    }

    /**
     * @return the JAXB type class
     */
    public Class < ? > getJaxbClass() {
        return getJaxbElementDescriptor().getJaxbClass();
    }

    /**
     * @return the JAXB element descriptor
     */
    public JAXBElementDescriptor getJaxbElementDescriptor() {
        return _jaxbElementDescriptor;
    }

}
