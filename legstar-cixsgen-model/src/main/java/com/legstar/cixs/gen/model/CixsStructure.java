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
package com.legstar.cixs.gen.model;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

import com.legstar.codegen.CodeGenUtil;
import com.legstar.coxb.host.HostException;
import com.legstar.coxb.util.BindingUtil;

/**
 * This class describes a mapping between a CICS structure (either commarea or
 * container) and a JAXB object.
 */
public class CixsStructure {

    /** JAXB complex type. */
    private String _jaxbType;

    /** COXB binding type. */
    private String _coxbType;

    /** JAXB package of complex type. */
    private String _jaxbPackageName;

    /** COXB package of binding type. */
    private String _coxbPackageName;

    /** Custom package of custom classes. */
    private String _custPackageName;

    /** The COBOL structure root data item name. */
    private String _cobolRootDataItemName;

    /** A CICS container mapping this structure. */
    private String _cicsContainer;

    /** Jaxb property name associated with this structure. */
    private String _jaxbPropertyName;

    /** Jaxb field name associated with this structure. */
    private String _jaxbFieldName;

    /** XML attribute representing a JAXB type. */
    public static final String CIXS_JAXB_TYPE_XML_A = "jaxbType";

    /** XML attribute representing a JAXB package name. */
    public static final String CIXS_JAXB_PKG_XML_A = "jaxbPackageName";

    /** XML attribute representing a COXB type. */
    public static final String CIXS_COXB_TYPE_XML_A = "coxbType";

    /** XML attribute representing a COXB package name. */
    public static final String CIXS_COXB_PKG_XML_A = "coxbPackageName";

    /** XML attribute representing a custom package name. */
    public static final String CIXS_CUST_PKG_XML_A = "custPackageName";

    /** XML attribute representing a COBOL root data item name. */
    public static final String CIXS_COBOL_ROOT_NAME_XML_A = "cobolRootDataItemName";

    /** XML attribute representing a CICS Container name. */
    public static final String CIXS_CICS_CONTAINER_XML_A = "cicsContainer";

    /** Input or output jaxb property name XML attribute. */
    private static final String JAXB_PROP_NAME_A = "jaxbPropertyName";

    /** Input or output jaxb field name XML attribute. */
    private static final String JAXB_FIELD_NAME_A = "jaxbFieldName";

    /** Coxb binding type names usually use this prefix. */
    private static final String BINDING_TYPE_SUFFIX = "Binding";

    /** Coxb binding types default package name suffix. */
    private static final String BINDING_PACKAGE_SUFFIX = ".bind";

    /**
     * @return the JAXB complex type
     */
    public String getJaxbType() {
        return _jaxbType;
    }

    /**
     * @param jaxbType the JAXB complex type to set
     */
    public void setJaxbType(final String jaxbType) {
        _jaxbType = jaxbType;
    }

    /**
     * @return the the JAXB package of complex type
     */
    public String getJaxbPackageName() {
        return _jaxbPackageName;
    }

    /**
     * @param jaxbPackageName the JAXB package of complex type to set
     */
    public void setJaxbPackageName(final String jaxbPackageName) {
        _jaxbPackageName = jaxbPackageName;
    }

    /**
     * @return JAXB namespace of complex type
     * @throws HostException if namespace cannot be detected from annotations
     */
    public String getJaxbNamespace() throws HostException {
        return BindingUtil.getXmlNamespace(getJaxbPackageName(), getJaxbType());
    }

    /**
     * @return the COXB binding type
     */
    public String getCoxbType() {
        if (_coxbType == null || _coxbType.length() == 0) {
            return getJaxbType() + BINDING_TYPE_SUFFIX;
        }
        return _coxbType;
    }

    /**
     * @param coxbType the COXB binding type to set
     */
    public void setCoxbType(final String coxbType) {
        _coxbType = coxbType;
    }

    /**
     * @return the the COXB package of binding type
     */
    public String getCoxbPackageName() {
        if (_coxbPackageName == null || _coxbPackageName.length() == 0) {
            return getJaxbPackageName() + BINDING_PACKAGE_SUFFIX;
        }
        return _coxbPackageName;
    }

    /**
     * @param coxbPackageName the COXB package of binding type to set
     */
    public void setCoxbPackageName(final String coxbPackageName) {
        _coxbPackageName = coxbPackageName;
    }

    /**
     * @return the the custom package of custom classes
     */
    public String getCustPackageName() {
        return _custPackageName;
    }

    /**
     * @param custPackageName the custom package of custom classes to set
     */
    public void setCustPackageName(final String custPackageName) {
        _custPackageName = custPackageName;
    }

    /**
     * @return the CICS container mapping this structure
     */
    public String getCicsContainer() {
        return _cicsContainer;
    }

    /**
     * @param cicsContainer the CICS container mapping this structure
     */
    public void setCicsContainer(final String cicsContainer) {
        _cicsContainer = cicsContainer;
    }

    /**
     * Create an XML usable as input for and ant task.
     * 
     * @param direction either input or output
     * @return the XML
     */
    public String serialize(final String direction) {
        StringBuffer result = new StringBuffer();
        result.append("<" + direction);
        serializeAttribute(result, getJaxbType(), CIXS_JAXB_TYPE_XML_A);
        serializeAttribute(result, getJaxbPackageName(), CIXS_JAXB_PKG_XML_A);
        serializeAttribute(result, getCoxbType(), CIXS_COXB_TYPE_XML_A);
        serializeAttribute(result, getCoxbPackageName(), CIXS_COXB_PKG_XML_A);
        serializeAttribute(result, getCustPackageName(), CIXS_CUST_PKG_XML_A);
        serializeAttribute(result, getCobolRootDataItemName(),
                CIXS_COBOL_ROOT_NAME_XML_A);
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
     * 
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
            result.append(" " + xmlTag + "=" + '\"'
                    + value + '\"');
        }
    }

    /**
     * Loads the CIXS Structure from an XML node element.
     * 
     * @param structureNode the structure node
     * @throws CixsModelException if load fails
     */
    public void load(final Node structureNode) throws CixsModelException {
        Element structureElement = (Element) structureNode;
        _jaxbType = structureElement.getAttribute(CIXS_JAXB_TYPE_XML_A);
        if (_jaxbType == null || _jaxbType.length() == 0) {
            throw new CixsModelException("Structure must have a JAXB type");
        }
        _jaxbPackageName = loadAttribute(structureElement,
                CIXS_JAXB_PKG_XML_A);
        _coxbType = structureElement.getAttribute(CIXS_COXB_TYPE_XML_A);
        _coxbPackageName = loadAttribute(structureElement,
                CIXS_COXB_PKG_XML_A);
        _custPackageName = loadAttribute(structureElement,
                CIXS_CUST_PKG_XML_A);
        _cobolRootDataItemName = loadAttribute(structureElement,
                CIXS_COBOL_ROOT_NAME_XML_A);
        _cicsContainer = loadAttribute(structureElement,
                CIXS_CICS_CONTAINER_XML_A);
        _jaxbPropertyName = loadAttribute(structureElement,
                JAXB_PROP_NAME_A);
        _jaxbFieldName = loadAttribute(structureElement,
                JAXB_FIELD_NAME_A);

    }

    /**
     * The DOM always returns a value for a getAttribute.
     * The semantic used for velocity templates assumes null values
     * for empty strings.
     * 
     * @param element the element to get an attribute from
     * @param attributeName the attribute name
     * @return not null only if not empty
     */
    private String loadAttribute(
            final Element element, final String attributeName) {
        String value = element.getAttribute(attributeName);
        if (value.trim().length() == 0) {
            return null;
        } else {
            return value.trim();
        }

    }

    /**
     * @return this structure property values as a string array. This helps
     *         inserting the structure as an item in an array.
     */
    public String[] getAsStringArray() {
        String[] array = { getJaxbType(),
                getJaxbPackageName(),
                getCicsContainer() };
        return array;
    }

    /**
     * @return the Jaxb field name. If no jaxb field name was set,
     *         derive one from the property name (if any) by lower casing the
     *         first
     *         character
     */
    public String getJaxbFieldName() {
        if (_jaxbFieldName == null || _jaxbFieldName.length() == 0) {
            return CodeGenUtil.fieldNameFromPropertyName(getJaxbPropertyName());
        }
        return _jaxbFieldName;
    }

    /**
     * @param jaxbFieldName the Jaxb field name to set
     */
    public void setJaxbFieldName(final String jaxbFieldName) {
        _jaxbFieldName = jaxbFieldName;
    }

    /**
     * @return the Jaxb property name. If no jaxb property name was set,
     *         try to derive one from the field name (if any) by upper casing
     *         the first
     *         character. If this still yields no property name, try to derive
     *         one of the property type.
     */
    public String getJaxbPropertyName() {
        if (_jaxbPropertyName == null || _jaxbPropertyName.length() == 0) {
            String propertyName =
                    CodeGenUtil.propertyNameFromFieldName(_jaxbFieldName);
            if (propertyName == null || propertyName.length() == 0) {
                return CodeGenUtil.propertyNameFromJaxbType(_jaxbType);
            } else {
                return propertyName;
            }
        }
        return _jaxbPropertyName;
    }

    /**
     * @param jaxbPropertyName the Jaxb property name to set
     */
    public void setJaxbPropertyName(final String jaxbPropertyName) {
        _jaxbPropertyName = jaxbPropertyName;
    }

    /**
     * @return the COBOL structure root data item name
     */
    public String getCobolRootDataItemName() {
        return _cobolRootDataItemName;
    }

    /**
     * @param cobolRootDataItemName the COBOL structure root data item name to
     *            set
     */
    public void setCobolRootDataItemName(
            final String cobolRootDataItemName) {
        _cobolRootDataItemName = cobolRootDataItemName;
    }
}
