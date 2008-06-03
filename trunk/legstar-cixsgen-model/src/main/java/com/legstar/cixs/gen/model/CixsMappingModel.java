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

import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.legstar.cixs.gen.model.CixsModelException;
import com.legstar.cixs.gen.model.CixsOperation;

/**
 * This class describes a mapping between list of operations and
 * their CICS programs counterparts.
 * This is a generic mapping that is usable for many generation
 * targets.
 * TODO should ultimately replace AbstractCicsService
 * 
 * @author Fady Moussallam
 * 
 */ 
public class CixsMappingModel {
	
	/** Mapping name. */
	private String mName;

    /** List of operations provided by this service. */
    private List < CixsOperation > mCixsOperations =
        new ArrayList < CixsOperation >();

	/** For compatibility with previous versions, we support this
	 * tag CIXS Jaxws service definition. */
	public static final String CIXS_COMPAT_SERVICE_XML_E = "cixsJaxwsService";

    /** XML element representing a CIXS mapping definition. */
    public static final String CIXS_MAPPING_XML_E = "cixsMapping";

	/** XML attribute representing a CIXS mapping name. */
	public static final String CIXS_MAPPING_NAME_XML_A = "name";

	/** A constant used to pretty print serialized XML. */
	private static final String CRLF = "\r\n";

	/**
	 * @return the service name
	 */
	public final String getName() {
		return mName;
	}

	/**
	 * @param name the service name to set
	 */
	public final void setName(final String name) {
		mName = name;
	}

    /**
     * @return the service list of operations
     */
    public final List<CixsOperation> getCixsOperations() {
        return mCixsOperations;
    }

    /**
     * @param cixsOperations the service list of operations to set
     */
    public final void setCixsOperations(final List<CixsOperation> cixsOperations) {
        mCixsOperations = cixsOperations;
    }

    /**
     * Operations are actually a set of uniquely named operations.
     * @param operation the operation to add
     * @throws CixsModelException if operation is a duplicate
     */
    public final void addCixsOperation(
            final CixsOperation operation) throws CixsModelException {
        /* Check that this operation is not already part of the set */
        if (mCixsOperations.contains(operation)) {
            throw new CixsModelException(
                    "This service already contains this operation");
        }
        
        mCixsOperations.add(operation);
    }

    /**
	 * Create an XML usable as input for and ant task.
	 * @return the XML
	 */
	public final String serialize() {
		StringBuffer result = new StringBuffer();
		result.append("<" + CIXS_MAPPING_XML_E + " "
				+ CIXS_MAPPING_NAME_XML_A + "="
				+ '\"' + mName + '\"');
		result.append('>' + CRLF);
		for (CixsOperation op : getCixsOperations()) {
			result.append(op.serialize());
			result.append(CRLF);
		}
		result.append("</" + CIXS_MAPPING_XML_E + ">");
		return result.toString();
	}
	
	/**
	 * Loads the CIXS Service from a serialized XML.
	 * @param serviceFile the serialized file
	 * @throws CixsModelException if load fails
	 */
	public final void load(final File serviceFile) throws CixsModelException {
    	DocumentBuilderFactory docBuilderFactory =
    		DocumentBuilderFactory.newInstance();
    	DocumentBuilder docBuilder;
		try {
			docBuilderFactory.setNamespaceAware(false);
			docBuilder = docBuilderFactory.newDocumentBuilder();
			Document doc = docBuilder.parse(serviceFile);
			load(doc);
		} catch (ParserConfigurationException e) {
			throw (new CixsModelException(e));
		} catch (SAXException e) {
			throw (new CixsModelException(e));
		} catch (IOException e) {
			throw (new CixsModelException(e));
		}
	}
	
	/**
	 * Loads the CIXS Service from a serialized XML in a string.
	 * @param serviceDesc the service description
	 * @throws CixsModelException if load fails
	 */
	public final void load(final String serviceDesc) throws CixsModelException {
    	DocumentBuilderFactory docBuilderFactory =
    		DocumentBuilderFactory.newInstance();
    	DocumentBuilder docBuilder;
		try {
			docBuilderFactory.setNamespaceAware(false);
			docBuilder = docBuilderFactory.newDocumentBuilder();
			Document doc = docBuilder.parse(
					new InputSource(new StringReader(serviceDesc)));
			load(doc);
		} catch (ParserConfigurationException e) {
			throw (new CixsModelException(e));
		} catch (SAXException e) {
			throw (new CixsModelException(e));
		} catch (IOException e) {
			throw (new CixsModelException(e));
		}
	}
	
	/**
	 * Loads the CIXS Service from an XML document.
	 * @param doc an XML document
	 * @throws CixsModelException if load fails
	 */
	public final void load(final Document doc) throws CixsModelException {
		NodeList listOfElements = doc.getElementsByTagName(
				CIXS_MAPPING_XML_E);
		if (listOfElements == null || listOfElements.getLength() == 0) {
		    /* This might be a previous version mapping file */
		    listOfElements = doc.getElementsByTagName(
		            CIXS_COMPAT_SERVICE_XML_E);
	        if (listOfElements == null || listOfElements.getLength() == 0) {
    			throw (new CixsModelException(
    					"Empty or invalid service descriptor file"));
	        }
		}
		try {
			Element serviceElement = (Element) listOfElements.item(0);
			mName = serviceElement.getAttribute(CIXS_MAPPING_NAME_XML_A);
			if (mName == null || mName.length() == 0) {
				throw new CixsModelException("Service must have a name");
			}
			getCixsOperations().clear();
			listOfElements = serviceElement.getElementsByTagName(
					CixsOperation.CIXS_OPERATION_XML_E);
			for (int i = 0; i < listOfElements.getLength(); i++) {
				CixsOperation operation = new CixsOperation();
				operation.load(listOfElements.item(i));
				addCixsOperation(operation);
			}
		} catch (CixsModelException e) {
			throw new CixsModelException(e);
		}
	}
	/**
	 * @see Object#hashCode() 
	 * {@inheritDoc}
	 */
	public final int hashCode() {
	    return getName().hashCode();
	}
	
	/**
	 * Indicates whether some other service is "equal to" this one.
	 *
	 * @param obj Object to be compared.
	 * @return true if this object is the same as the obj argument; false
	 *         otherwise..
	 */
	public final boolean equals(final Object obj) {
	    return (obj != null) && (obj.getClass() == CixsMappingModel.class)
	    	&& ((CixsMappingModel) obj).getName().equals(getName());
	}
	
	/**
	 * Compares this object with the specified object for order. Returns a
	 * negative integer, zero, or a positive integer as this object is less
	 * than, equal to, or greater than the specified object.
	 *
	 * @param o Object to be compared.
	 * @return A negative integer, zero, or a positive integer as this object
	 *         is less than, equal to, or greater than the specified object.
	 */
	public final int compareTo(final Object o) {
	    if (o.getClass() != CixsMappingModel.class) {
	        throw new ClassCastException(o.getClass().getName());
	    } else {
	        return ((CixsMappingModel) o).getName().compareTo(getName());
	    }
	}

}
