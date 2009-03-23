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
package com.legstar.coxb.impl.reflect;

import com.legstar.coxb.transform.AbstractHostToXmlTransformer;
import com.legstar.coxb.transform.HostTransformException;
import com.legstar.util.JAXBElementDescriptor;

/**
 * This implementation of a host to XML transformer dynamically binds to
 * a complex object using reflection.
 * The complex object must be a JAXB type with COBOL annotations.
 *
 */
public class ReflectHostToXmlTransformer extends AbstractHostToXmlTransformer {

    /**
     * Construct a transformer for a particular JAXB type.
     * @param jaxbPackageName the JAXB type package name
     * @param jaxbType the JAXB type
     * @throws HostTransformException if transformer cannot be created
     * @throws ReflectBindingException if JAXB type has no annotations or cannot be 
     *  located from the classpath
     */
    public ReflectHostToXmlTransformer(
            final String jaxbPackageName,
            final String jaxbType) throws HostTransformException, ReflectBindingException {
        super(new ReflectHostToJavaTransformer(jaxbPackageName, jaxbType));
    }

    /**
     * @return the Host to Java transformer using reflection
     */
    public JAXBElementDescriptor getJaxbElementDescriptor() {
        return ((ReflectHostToJavaTransformer) getHostToJavaTransformer()).getJaxbElementDescriptor();
    }

    /** {@inheritDoc} */
    public String getElementName() {
        return getJaxbElementDescriptor().getElementName();
    }

    /** {@inheritDoc} */
    public String getNamespace() {
        return getJaxbElementDescriptor().getNamespace();
    }

    /** {@inheritDoc} */
    public boolean isXmlRootElement() {
        return getJaxbElementDescriptor().isXmlRootElement();
    }

}
