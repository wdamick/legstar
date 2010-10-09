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
import com.legstar.coxb.transform.AbstractJavaToHostTransformer;
import com.legstar.util.JAXBAnnotationException;
import com.legstar.util.JAXBElementDescriptor;

/**
 * This implementation of a java to host transformer dynamically binds to
 * a complex object using reflection.
 * The complex object must be a JAXB type with COBOL annotations.
 *
 */
public class ReflectJavaToHostTransformer extends AbstractJavaToHostTransformer {
    
    /**
     * The JAXB element descriptor.
     */
    private JAXBElementDescriptor mJaxbElementDescriptor;

    /**
     * The JAXB type factory to use for the JAXB object type.
     */
    private Object mObjectFactory;

    /**
     * The JAXB type class.
     */
    private Class < ? > mJaxbClass;

    /**
     * Construct a transformer for a particular JAXB type.
     * @param jaxbPackageName the JAXB type package name
     * @param jaxbType the JAXB type
     * @throws ReflectBindingException if JAXB type has no annotations or cannot be 
     *  located from the classpath
     */
    public ReflectJavaToHostTransformer(
            final String jaxbPackageName,
            final String jaxbType) throws ReflectBindingException {
        try {
            mJaxbElementDescriptor = new JAXBElementDescriptor(jaxbPackageName, jaxbType);
            mObjectFactory = mJaxbElementDescriptor.createObjectFactory();
            mJaxbClass = mJaxbElementDescriptor.getJaxbClass();
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
        return mObjectFactory;
    }

    /**
     * @return the JAXB type class
     */
    public Class < ? > getJaxbClass() {
        return mJaxbClass;
    }

    /**
     * @return the JAXB element descriptor
     */
    public JAXBElementDescriptor getJaxbElementDescriptor() {
        return mJaxbElementDescriptor;
    }
}
