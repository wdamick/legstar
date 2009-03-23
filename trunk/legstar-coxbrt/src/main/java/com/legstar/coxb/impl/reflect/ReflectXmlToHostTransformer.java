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

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Source;

import com.legstar.coxb.transform.AbstractXmlToHostTransformer;
import com.legstar.coxb.transform.HostTransformException;
import com.legstar.util.JAXBElementDescriptor;

/**
 * This implementation of an XML to host transformer dynamically binds to
 * a complex object using reflection.
 * The complex object must be a JAXB type with COBOL annotations.
 *
 */
public class ReflectXmlToHostTransformer extends AbstractXmlToHostTransformer {

    /** JAXB Context. */
    private JAXBContext mJaxbContext = null;

    /** JAXB Unmarshaller (XML to Object). */
    private Unmarshaller mXmlUnmarshaller = null;

    /**
     * Construct a transformer for a particular JAXB type.
     * @param jaxbPackageName the JAXB type package name
     * @param jaxbType the JAXB type
     * @throws ReflectBindingException if JAXB type has no annotations or cannot be 
     *  located from the classpath
     */
    public ReflectXmlToHostTransformer(
            final String jaxbPackageName,
            final String jaxbType) throws ReflectBindingException {
        super(new ReflectJavaToHostTransformer(jaxbPackageName, jaxbType));
        try {
            mJaxbContext = JAXBContext.newInstance(
                    getJaxbElementDescriptor().getJaxbClass());
            mXmlUnmarshaller = mJaxbContext.createUnmarshaller();
        } catch (JAXBException e) {
            throw new ReflectBindingException(e);
        }
    }

    /** {@inheritDoc} */
    public Object getObjectFromXml(final Source source) throws HostTransformException {
        try {
            return getXmlUnmarshaller().unmarshal(
                    source, getJaxbElementDescriptor().getJaxbClass()).getValue();
        } catch (JAXBException e) {
            throw new HostTransformException(e);
        }
    }


    /**
     * @return the Host to Java transformer using reflection
     */
    public JAXBElementDescriptor getJaxbElementDescriptor() {
        return ((ReflectJavaToHostTransformer) getJavaToHostTransformer()).getJaxbElementDescriptor();
    }

    /**
     * @return the JAXB Unmarshaller (Object to XML)
     */
    public Unmarshaller getXmlUnmarshaller() {
        return mXmlUnmarshaller;
    }
}
