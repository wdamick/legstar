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
package com.legstar.util;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import com.legstar.coxb.util.Utils;

/**
 * This immutable class holds the parameters that describe the relationship
 * between an XML element and a corresponding JAXB object. This same information
 * can be derived from a JAXB object XmlType annotation or XmlRootElement
 * annotation.
 */
public class JAXBElementDescriptor {

    /** The JAXB package name. */
    private String mJaxbPackageName;

    /** The JAXB type name. */
    private String mJaxbType;

    /** Element name extracted from XmlRootElement or XmlType annotation. */
    private String mElementName;

    /** Inferred from the presence of an XmlRootElement annotation on the JAXB
     *  type. */
    private boolean mIsXmlRootElement;

    /** JAXB Object factory for the JAXB package. */
    private Object mObjectFactory;

    /**
     * Create the element descriptor.
     * @param jaxbPackageName the JAXB package name
     * @param jaxbType the JAXB type name
     * @throws JAXBAnnotationException if JAXB annotations are incomplete 
     */
    public JAXBElementDescriptor(
            final String jaxbPackageName, final String jaxbType)
    throws JAXBAnnotationException {
        mJaxbPackageName = jaxbPackageName;
        mJaxbType = jaxbType;
        mElementName = extractElementName();
        mIsXmlRootElement = extractIsXmlRootElement();
        mObjectFactory = createObjectFactory();
    }

    /**
     * @return the element name is given either by the XmlRootElement
     * annotation or the XmlType annotation.
     * @throws JAXBAnnotationException if element name cannot be retrieved
     * from JAXB annotations
     */
    private String extractElementName() throws JAXBAnnotationException {
        try {
            Class < ? > clazz = loadJaxbClass();
            XmlRootElement xmlRootElement =
                clazz.getAnnotation(XmlRootElement.class);
            if (xmlRootElement != null) {
                String name = xmlRootElement.name();
                if (name != null && !name.equals("##default")) {
                    return name;
                }
            }
            XmlType xmlType = clazz.getAnnotation(XmlType.class);
            if (xmlType != null) {
                String name = xmlType.name();
                if (name != null && !name.equals("##default")) {
                    return name;
                }
            }
            throw new JAXBAnnotationException("Object " + getJaxbType()
                    + " in package " + getJaxbPackageName()
                    + " does not have an XmlRootElement or XmlType annotation");
        } catch (ClassNotFoundException e) {
            throw new JAXBAnnotationException(e);
        }
    }

    /**
     * @return true if the JAXB element is marked as XmlRootElement which
     * means it does not need to be encapsulated in a JAXBElement.
     * @throws JAXBAnnotationException if class is not found
     */
    private boolean extractIsXmlRootElement() throws JAXBAnnotationException {
        try {
            Class < ? > clazz = loadJaxbClass();
            XmlRootElement xmlRootElement =
                clazz.getAnnotation(XmlRootElement.class);
            if (xmlRootElement != null) {
                return true;
            }
            return false;
        } catch (ClassNotFoundException e) {
            throw new JAXBAnnotationException(e);
        }
    }

    /**
     * Returns the JAXB package Object factory.
     * @return the object factory
     * @throws JAXBAnnotationException if object factory cannot be
     *  instantiated
     */
    public final Object createObjectFactory()
    throws JAXBAnnotationException {
        try {
            Class < ? > ofClass = Utils.loadClass(getJaxbPackageName()
                    + ".ObjectFactory");
            return ofClass.newInstance();
        } catch (ClassNotFoundException e) {
            throw new JAXBAnnotationException(e);
        } catch (InstantiationException e) {
            throw new JAXBAnnotationException(e);
        } catch (IllegalAccessException e) {
            throw new JAXBAnnotationException(e);
        }
    }

    /**
     * @return a new instance of the JAXB class described
     * @throws ClassNotFoundException if jaxb class cannot be found from
     * the current thread loader
     */
    public Class < ? > loadJaxbClass() throws ClassNotFoundException {
        String className = JaxbUtil.getClassName(getJaxbPackageName(),
                getJaxbType());
        return Utils.loadClass(className);
    }

    /**
     * @return the JAXB package name
     */
    public final String getJaxbPackageName() {
        return mJaxbPackageName;
    }

    /**
     * @return the JAXB class name
     */
    public final String getJaxbType() {
        return mJaxbType;
    }

    /**
     * @return the element name is given either by the XmlRootElement
     * annotation or the XmlType annotation.
     * @throws JAXBAnnotationException if element name cannot be retrieved
     * from JAXB annotations
     */
    public final String getElementName() throws JAXBAnnotationException {
        return mElementName;
    }

    /**
     * @return true if the JAXB element is marked as XmlRootElement which
     * means it does not need to be encapsulated in a JAXBElement.
     * @throws JAXBAnnotationException if class is not found
     */
    public final boolean isXmlRootElement() throws JAXBAnnotationException {
        return mIsXmlRootElement;
    }

    /**
     * Returns the JAXB package Object factory.
     * @return the object factory
     */
    public final Object getObjectFactory() {
        return mObjectFactory;
    }

    /** {@inheritDoc} */
    public final String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("JAXB package=");
        sb.append(mJaxbPackageName);
        sb.append(", ");
        sb.append("JAXB type=");
        sb.append(mJaxbType);
        sb.append(", ");
        sb.append("XML element=");
        try {
            sb.append(getElementName());
        } catch (JAXBAnnotationException e) {
            sb.append(e.getMessage());
        }
        sb.append(", ");
        sb.append("is XmlRootElement=");
        try {
            sb.append(isXmlRootElement());
        } catch (JAXBAnnotationException e) {
            sb.append(e.getMessage());
        }
        return sb.toString();
    }

}
