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
import javax.xml.bind.annotation.XmlSchema;
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

    /** The JAXB class. */
    private Class < ? > mJaxbClass;

    /** Element name extracted from XmlRootElement or XmlType annotation. */
    private String mElementName;

    /** The XML Schema namespace. */
    private String mNamespace;

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
        try {
            mJaxbPackageName = jaxbPackageName;
            mJaxbType = jaxbType;
            mJaxbClass = loadJaxbClass();
            mElementName = extractElementName();
            mNamespace = extractNamespace();
            mIsXmlRootElement = extractIsXmlRootElement();
            mObjectFactory = createObjectFactory();
        } catch (ClassNotFoundException e) {
            throw new JAXBAnnotationException(e);
        }
    }

    /**
     * @return the element name is given either by the XmlRootElement
     * annotation or the XmlType annotation.
     * @throws JAXBAnnotationException if element name cannot be retrieved
     * from JAXB annotations
     */
    private String extractElementName() throws JAXBAnnotationException {
        XmlRootElement xmlRootElement =
            getJaxbClass().getAnnotation(XmlRootElement.class);
        if (xmlRootElement != null) {
            String name = xmlRootElement.name();
            if (name != null && !name.equals("##default")) {
                return name;
            }
        }
        XmlType xmlType = getJaxbClass().getAnnotation(XmlType.class);
        if (xmlType != null) {
            String name = xmlType.name();
            if (name != null && !name.equals("##default")) {
                return name;
            }
        }
        throw new JAXBAnnotationException("Object " + getJaxbType()
                + " in package " + getJaxbPackageName()
                + " does not have an XmlRootElement or XmlType annotation");
    }

    /**
     * Extract the XML Schema namespace associated with the JAXB package.
     * @return the XML Schema namespace
     * @throws JAXBAnnotationException if package-info class is not found
     */
    private String extractNamespace() throws JAXBAnnotationException {
        try {
            Class < ? > packageInfoClass = Utils.loadClass(getJaxbPackageName()
                    + ".package-info");
            XmlSchema xmlSchema = packageInfoClass.getAnnotation(XmlSchema.class);
            return xmlSchema.namespace();
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
        XmlRootElement xmlRootElement =
            getJaxbClass().getAnnotation(XmlRootElement.class);
        if (xmlRootElement != null) {
            return true;
        }
        return false;
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
    private Class < ? > loadJaxbClass() throws ClassNotFoundException {
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
        sb.append(getJaxbPackageName());
        sb.append(", ");
        sb.append("JAXB type=");
        sb.append(getJaxbType());
        sb.append(", ");
        sb.append("XML namespace=");
        sb.append(getNamespace());
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

    /**
     * @return the XML Schema namespace
     */
    public String getNamespace() {
        return mNamespace;
    }

    /**
     * @return the The JAXB class
     */
    public Class < ? > getJaxbClass() {
        return mJaxbClass;
    }

}
