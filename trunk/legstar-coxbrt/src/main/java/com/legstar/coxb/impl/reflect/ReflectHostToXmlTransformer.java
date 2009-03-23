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

import java.io.Writer;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.namespace.QName;

import com.legstar.coxb.transform.AbstractHostToXmlTransformer;
import com.legstar.coxb.transform.HostTransformException;
import com.legstar.util.JAXBAnnotationException;
import com.legstar.util.JAXBElementDescriptor;

/**
 * This implementation of a host to XML transformer dynamically binds to
 * a complex object using reflection.
 * The complex object must be a JAXB type with COBOL annotations.
 *
 */
public class ReflectHostToXmlTransformer extends AbstractHostToXmlTransformer {

    /** JAXB Context. */
    private JAXBContext mJaxbContext = null;

    /** JAXB Marshaller (Object to XML). */
    private Marshaller mXmlMarshaller = null;

    /**
     * Construct a transformer for a particular JAXB type.
     * @param jaxbPackageName the JAXB type package name
     * @param jaxbType the JAXB type
     * @throws ReflectBindingException if JAXB type has no annotations or cannot be 
     *  located from the classpath
     */
    public ReflectHostToXmlTransformer(
            final String jaxbPackageName,
            final String jaxbType) throws ReflectBindingException {
        super(new ReflectHostToJavaTransformer(jaxbPackageName, jaxbType));
        try {
            mJaxbContext = JAXBContext.newInstance(
                    getJaxbElementDescriptor().getJaxbClass());
            mXmlMarshaller = mJaxbContext.createMarshaller();
        } catch (JAXBException e) {
            throw new ReflectBindingException(e);
        }
    }

    /** {@inheritDoc}
     * Root elements can be marshalled directly while non-root elements must
     * be encapsulated in a JAXBElement before they can be marshalled.
     *   */
    @SuppressWarnings("unchecked")
    public void getXmlFromObject(
            final Object valueObject, final Writer writer) throws HostTransformException {
        try {
            if (getJaxbElementDescriptor().isXmlRootElement()) {
                getXmlMarshaller().marshal(valueObject, writer);
            } else {
                QName qName = new QName(
                        getJaxbElementDescriptor().getNamespace(),
                        getJaxbElementDescriptor().getElementName());
                JAXBElement < ? > jaxbElement =
                    new JAXBElement(qName, getJaxbElementDescriptor().getJaxbClass(), valueObject);
                getXmlMarshaller().marshal(jaxbElement, writer);
            }
        } catch (JAXBAnnotationException e) {
            throw new HostTransformException(e);
        } catch (JAXBException e) {
            throw new HostTransformException(e);
        }
    }

    /**
     * @return the Host to Java transformer using reflection
     */
    public JAXBElementDescriptor getJaxbElementDescriptor() {
        return ((ReflectHostToJavaTransformer) getHostToJavaTransformer()).getJaxbElementDescriptor();
    }

    /**
     * @return the JAXB Marshaller (Object to XML)
     */
    public Marshaller getXmlMarshaller() {
        return mXmlMarshaller;
    }
    
}
