/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.cixs.gen.model;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

import com.legstar.cixs.model.util.ModelUtil;

/**
 * This class describes a mapping between a CICS structure (either commarea or
 * container) and a JAXB object.
 */
public class CixsStructure {

    /** JAXB complex type. */
    private String mJaxbType;

    /** COXB binding type. */
    private String mCoxbType;

    /** JAXB package of complex type. */
    private String mJaxbPackageName;

    /** COXB package of binding type. */
    private String mCoxbPackageName;

    /** A CICS container mapping this structure. */
    private String mCicsContainer;

    /** Jaxb property name associated with this structure. */
    private String mJaxbPropertyName;

    /** Jaxb field name associated with this structure. */
    private String mJaxbFieldName;

    /** XML attribute representing a JAXB type. */
    public static final String CIXS_JAXB_TYPE_XML_A = "jaxbType";

    /** XML attribute representing a JAXB package name. */
    public static final String CIXS_JAXB_PKG_XML_A = "jaxbPackageName";

    /** XML attribute representing a CICS Container name. */
    public static final String CIXS_CICS_CONTAINER_XML_A
    = "cicsContainer";

    /** Input or output jaxb property name XML attribute. */
    private static final String JAXB_PROP_NAME_A = "jaxbPropertyName";

    /** Input or output jaxb field name XML attribute. */
    private static final String JAXB_FIELD_NAME_A = "jaxbFieldName";

	/** Coxb binding type names usually use this prefix.*/
    private static final String BINDING_TYPE_SUFFIX = "Binding";

	/** Coxb binding types default package name suffix. */
    private static final String BINDING_PACKAGE_SUFFIX = ".bind";

    /**
     * @return the JAXB complex type
     */
    public final String getJaxbType() {
        return mJaxbType;
    }

    /**
     * @param jaxbType the JAXB complex type to set
     */
    public final void setJaxbType(final String jaxbType) {
        mJaxbType = jaxbType;
    }

    /**
     * @return the the JAXB package of complex type
     */
    public final String getJaxbPackageName() {
        return mJaxbPackageName;
    }

    /**
     * @param jaxbPackageName the JAXB package of complex type to set
     */
    public final void setJaxbPackageName(final String jaxbPackageName) {
        mJaxbPackageName = jaxbPackageName;
    }

    /**
     * @return the COXB binding type
     */
    public final String getCoxbType() {
    	if (mCoxbType == null || mCoxbType.length() == 0) {
    		return getJaxbType() + BINDING_TYPE_SUFFIX;
    	}
        return mCoxbType;
    }

    /**
     * @param coxbType the COXB binding type to set
     */
    public final void setCoxbType(final String coxbType) {
        mCoxbType = coxbType;
    }

    /**
     * @return the the COXB package of binding type
     */
    public final String getCoxbPackageName() {
    	if (mCoxbPackageName == null || mCoxbPackageName.length() == 0) {
    		return getJaxbPackageName() + BINDING_PACKAGE_SUFFIX;
    	}
        return mCoxbPackageName;
    }

    /**
     * @param coxbPackageName the COXB package of binding type to set
     */
    public final void setCoxbPackageName(final String coxbPackageName) {
        mCoxbPackageName = coxbPackageName;
    }

    /**
     * @return the CICS container mapping this structure
     */
    public final String getCicsContainer() {
        return mCicsContainer;
    }

    /**
     * @param cicsContainer the CICS container mapping this structure
     */
    public final void setCicsContainer(final String cicsContainer) {
        mCicsContainer = cicsContainer;
    }

    /**
     * Create an XML usable as input for and ant task.
     * @param direction either input or output
     * @return the XML
     */
    public final String serialize(final String direction) {
        StringBuffer result = new StringBuffer();
        result.append("<" + direction + " " + CIXS_JAXB_TYPE_XML_A + "=" + '\"'
                + getJaxbType() + '\"');
        serializeAttribute(result, getJaxbPackageName(), CIXS_JAXB_PKG_XML_A);
        serializeAttribute(result, getCicsContainer(),
                CIXS_CICS_CONTAINER_XML_A);
        serializeAttribute(result, getJaxbPropertyName(),
                JAXB_PROP_NAME_A);
        serializeAttribute(result, getJaxbFieldName(),
                JAXB_FIELD_NAME_A);
        result.append("/>");
        return result.toString();
    }

    /**
     * Helper to serialize an attribute tag and value in XML.
     * @param result the current string being built
     * @param value the attribute value
     * @param xmlTag the attribute XML tag
     */
    private void serializeAttribute(
            final StringBuffer result,
            final String value,
            final String xmlTag) {
        if (value != null
                && value.length() > 0) {
            result.append(" " + xmlTag + "="  + '\"'
                    + value + '\"');
        }
    }

    /**
     * Loads the CIXS Structure from an XML node element.
     * @param structureNode the structure node
     * @throws CixsModelException if load fails
     */
    public final void load(final Node structureNode) throws CixsModelException {
        Element structureElement = (Element) structureNode;
        mJaxbType = structureElement.getAttribute(CIXS_JAXB_TYPE_XML_A);
        if (mJaxbType == null || mJaxbType.length() == 0) {
            throw new CixsModelException("Structure must have a JAXB type");
        }
        mJaxbPackageName = structureElement.getAttribute(
                CIXS_JAXB_PKG_XML_A);
        mCicsContainer =  structureElement.getAttribute(
                CIXS_CICS_CONTAINER_XML_A);
        mJaxbPropertyName =  structureElement.getAttribute(
                JAXB_PROP_NAME_A);
        mJaxbFieldName =  structureElement.getAttribute(
                JAXB_FIELD_NAME_A);

    }

    /**
     * @return this structure property values as a string array. This helps
     * inserting the structure as an item in an array.
     */
    public final String[] getAsStringArray() {
        String[] array = {getJaxbType(),
                getJaxbPackageName(),
                getCicsContainer()};
        return array;
    }

    /**
     * @return the Jaxb field name. If no jaxb field name was set,
     * derive one from the property name (if any) by lower casing the first
     * character
     */
    public final String getJaxbFieldName() {
    	if (mJaxbFieldName == null || mJaxbFieldName.length() == 0) {
        	return ModelUtil.fieldNameFromPropertyName(getJaxbPropertyName());
    	}
        return mJaxbFieldName;
    }

    /**
     * @param jaxbFieldName the Jaxb field name to set
     */
    public final void setJaxbFieldName(final String jaxbFieldName) {
        mJaxbFieldName = jaxbFieldName;
    }

    /**
     * @return the Jaxb property name. If no jaxb property name was set,
     * try to derive one from the field name (if any) by upper casing the first
     * character. If this still yields no property name, try to derive
     * one of the property type.
     */
    public final String getJaxbPropertyName() {
        if (mJaxbPropertyName == null || mJaxbPropertyName.length() == 0) {
        	String propertyName =
        		ModelUtil.propertyNameFromFieldName(mJaxbFieldName);
        	if (propertyName == null || propertyName.length() == 0) {
        		return ModelUtil.propertyNameFromJaxbType(mJaxbType);
        	} else {
        		return propertyName;
        	}
        }
        return mJaxbPropertyName;
    }

    /**
     * @param jaxbPropertyName the Jaxb property name to set
     */
    public final void setJaxbPropertyName(final String jaxbPropertyName) {
        mJaxbPropertyName = jaxbPropertyName;
    }
}
