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
package com.legstar.cixs.gen;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * This class describes a mapping between a host structure and a JAXB object.
 * 
 * @author Fady Moussallam
 * 
 */
public class CixsStructure {

	/** JAXB complex type. */
	private String mJaxbType;

	/** JAXB package of complex type. */
	private String mJaxbPackageName;

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
	
	/** Suffix used for JAXB type variable names. */
	public static final String JAXB_TYPE_SUFFIX = "Type";
	
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
		/* Property name is derived from jaxb type by stripping any trailing
		 * type */
		if (getJaxbPropertyName() == null
				|| getJaxbPropertyName().length() == 0) {
			String varName = getJaxbType();
			if (varName.endsWith(JAXB_TYPE_SUFFIX)) {
				varName = varName.substring(0,
						varName.length() - JAXB_TYPE_SUFFIX.length());
			}
			setJaxbPropertyName(varName);
		}
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
			result.append(" " + xmlTag + "="	+ '\"'
					+ value + '\"');
		}
	}

	/**
	 * Loads the CIXS Structure from an XML node element.
	 * @param structureNode the structure node
	 * @throws CixsException if load fails
	 */
	public final void load(final Node structureNode) throws CixsException {
		Element structureElement = (Element) structureNode;
		mJaxbType = structureElement.getAttribute(CIXS_JAXB_TYPE_XML_A);
		if (mJaxbType == null || mJaxbType.length() == 0) {
			throw new CixsException("Structure must have a JAXB type");
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
	 * @return the Jaxb field name
	 */
	public final String getJaxbFieldName() {
		return mJaxbFieldName;
	}

	/**
	 * @param jaxbFieldName the Jaxb field name to set
	 */
	public final void setJaxbFieldName(final String jaxbFieldName) {
		mJaxbFieldName = jaxbFieldName;
		/* Property name is derived from field name by uppercasing the first
		 *  charater */
		if (getJaxbPropertyName() == null
				|| getJaxbPropertyName().length() == 0) {
			String varName = getJaxbFieldName().
					substring(0, 1).toUpperCase()
			+ getJaxbFieldName().substring(1,
					getJaxbFieldName().length());
			setJaxbPropertyName(varName);
		}
	}

	/**
	 * @return the Jaxb property name
	 */
	public final String getJaxbPropertyName() {
		return mJaxbPropertyName;
	}

	/**
	 * @param jaxbPropertyName the Jaxb property name to set
	 */
	public final void setJaxbPropertyName(final String jaxbPropertyName) {
		mJaxbPropertyName = jaxbPropertyName;
		/* Field is derived from property by lowercasing the first character */
		if (getJaxbFieldName() == null
				|| getJaxbFieldName().length() == 0) {
			String varName = getJaxbPropertyName().
					substring(0, 1).toLowerCase()
			+ getJaxbPropertyName().substring(1,
					getJaxbPropertyName().length());
			setJaxbFieldName(varName);
		}
	}
}
