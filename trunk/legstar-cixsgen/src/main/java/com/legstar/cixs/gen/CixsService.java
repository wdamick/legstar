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

import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.util.List;
import java.util.ArrayList;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * This class describes a service.
 * 
 * @author Fady Moussallam
 * 
 */
public class CixsService {
	
	/** Service name. */
	private String mName;

	/** Service endpoint package name. */
	private String mEndpointPackageName;
	
	/** Service target namespace. */
	private String mTargetNamespace;
	
	/** Service interface class name. */
	private String mEndpointInterfaceClassName;
	
	/** Service implementation class name. */
	private String mEndpointImplementationClassName;
	
	/** Service name in WSDL. */
	private String mWsdlServiceName;
	
	/** Port type in WSDL. */
	private String mWsdlPortType;
	
	/** The service list of operations. */
	private List < CixsOperation > mCixsOperations =
		new ArrayList < CixsOperation >();

	/** XML element representing a CIXS service definition. */
	public static final String CIXS_SERVICE_XML_E = "cixsService";

	/** XML attribute representing a CIXS service name. */
	public static final String CIXS_SERVICE_NAME_XML_A = "name";

	/** XML attribute representing a CIXS endpoint package name. */
	public static final String CIXS_ENDPOINT_PKG_XML_A
			= "endpointPackageName";

	/** XML attribute representing a CIXS target namespace. */
	public static final String CIXS_NAMESPACE_XML_A = "targetNamespace";
	
	/** XML attribute representing a service interface class name. */
	public static final String CIXS_SEI_CLASS_A = "endpointInterfaceClassName";
	
	/** XML attribute representing a Service implementation class name. */
	public static final String CIXS_SEIMPL_CLASS_A =
		"endpointImplementationClassName";
	
	/** XML attribute representing a Service name in WSDL. */
	public static final String CIXS_WSDL_SERVICE_XML_A = "wsdlServiceName";
	
	/** XML attribute representing a Port type in WSDL. */
	public static final String CIXS_WSDL_PORT_TYPE_XML_A = "wsdlPortType";
	
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
	 * @return the service endpoint package name
	 */
	public final String getEndpointPackageName() {
		return mEndpointPackageName;
	}

	/**
	 * @param endpointPackageName the service endpoint package name to set
	 */
	public final void setEndpointPackageName(final String endpointPackageName) {
		mEndpointPackageName = endpointPackageName;
	}

	/**
	 * @return the service target namespace
	 */
	public final String getTargetNamespace() {
		return mTargetNamespace;
	}

	/**
	 * @param targetNamespace the service target namespace to set
	 */
	public final void setTargetNamespace(final String targetNamespace) {
		mTargetNamespace = targetNamespace;
	}

	/**
	 * @return the service list of operations
	 */
	public final List < CixsOperation > getCixsOperations() {
		return mCixsOperations;
	}

	/**
	 * @param operations the list of operations to set
	 */
	public final void setCixsOperations(
			final List < CixsOperation > operations) {
		mCixsOperations = operations;
	}

	/**
	 * Operations are actually a set of uniquely named operations.
	 * @param operation the operation to add
	 * @throws CixsException if operation is a duplicate
	 */
	public final void addCixsOperation(
			final CixsOperation operation) throws CixsException {
		/* Check that this operation is not already part of the set */
		if (mCixsOperations.contains(operation)) {
			throw new CixsException(
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
		result.append("<" + CIXS_SERVICE_XML_E + " "
				+ CIXS_SERVICE_NAME_XML_A + "="
				+ '\"' + mName + '\"');
		result.append(" " + CIXS_ENDPOINT_PKG_XML_A + "="
				+ '\"' + mEndpointPackageName + '\"');
		result.append(" " + CIXS_NAMESPACE_XML_A + "="
				+ '\"' + mTargetNamespace + '\"');
		if (getEndpointInterfaceClassName() != null
				&& getEndpointInterfaceClassName().length() > 0) {
			result.append(" " + CIXS_SEI_CLASS_A + "=" + '\"'
					+ getEndpointInterfaceClassName() + '\"');
		}
		if (getEndpointImplementationClassName() != null
				&& getEndpointImplementationClassName().length() > 0) {
			result.append(" " + CIXS_SEIMPL_CLASS_A + "=" + '\"'
					+ getEndpointImplementationClassName() + '\"');
		}
		if (getWsdlServiceName() != null 
				&& getWsdlServiceName().length() > 0) {
			result.append(" " + CIXS_WSDL_SERVICE_XML_A + "=" + '\"'
					+ getWsdlServiceName() + '\"');
		}
		if (getWsdlPortType() != null 
				&& getWsdlPortType().length() > 0) {
			result.append(" " + CIXS_WSDL_PORT_TYPE_XML_A + "=" + '\"'
					+ getWsdlPortType() + '\"');
		}
		result.append('>' + CRLF);
		for (CixsOperation op : mCixsOperations) {
			result.append(op.serialize());
			result.append(CRLF);
		}
		result.append("</" + CIXS_SERVICE_XML_E + ">");
		return result.toString();
	}
	
	/**
	 * Loads the CIXS Service from a serialized XML.
	 * @param serviceFile the serialized file
	 * @throws CixsException if load fails
	 */
	public final void load(final File serviceFile) throws CixsException {
    	DocumentBuilderFactory docBuilderFactory =
    		DocumentBuilderFactory.newInstance();
    	DocumentBuilder docBuilder;
		try {
			docBuilderFactory.setNamespaceAware(false);
			docBuilder = docBuilderFactory.newDocumentBuilder();
			Document doc = docBuilder.parse(serviceFile);
			load(doc);
		} catch (ParserConfigurationException e) {
			throw (new CixsException(e));
		} catch (SAXException e) {
			throw (new CixsException(e));
		} catch (IOException e) {
			throw (new CixsException(e));
		}
	}
	
	/**
	 * Loads the CIXS Service from a serialized XML in a string.
	 * @param serviceDesc the service description
	 * @throws CixsException if load fails
	 */
	public final void load(final String serviceDesc) throws CixsException {
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
			throw (new CixsException(e));
		} catch (SAXException e) {
			throw (new CixsException(e));
		} catch (IOException e) {
			throw (new CixsException(e));
		}
	}
	
	/**
	 * Loads the CIXS Service from an XML document.
	 * @param doc an XML document
	 * @throws CixsException if load fails
	 */
	public final void load(final Document doc) throws CixsException {
		NodeList listOfElements = doc.getElementsByTagName(
				CIXS_SERVICE_XML_E);
		if (listOfElements == null || listOfElements.getLength() == 0) {
			throw (new CixsException(
					"Empty or invalid service descriptor file"));
		}
		Element serviceElement = (Element) listOfElements.item(0);
		mName = serviceElement.getAttribute(CIXS_SERVICE_NAME_XML_A);
		if (mName == null || mName.length() == 0) {
			throw new CixsException("Service must have a name");
		}
		mEndpointPackageName = serviceElement.getAttribute(
				CIXS_ENDPOINT_PKG_XML_A);
		mTargetNamespace = serviceElement.getAttribute(
				CIXS_NAMESPACE_XML_A);
		mEndpointInterfaceClassName = serviceElement.getAttribute(
				CIXS_SEI_CLASS_A);
		mEndpointImplementationClassName = serviceElement.getAttribute(
				CIXS_SEIMPL_CLASS_A);
		mWsdlServiceName =  serviceElement.getAttribute(
				CIXS_WSDL_SERVICE_XML_A);
		mWsdlPortType =  serviceElement.getAttribute(
				CIXS_WSDL_PORT_TYPE_XML_A);
		
		mCixsOperations = new ArrayList < CixsOperation >();
		listOfElements = serviceElement.getElementsByTagName(
				CixsOperation.CIXS_OPERATION_XML_E);
		for (int i = 0; i < listOfElements.getLength(); i++) {
			CixsOperation operation = new CixsOperation();
			operation.load(listOfElements.item(i));
			mCixsOperations.add(operation);
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
	    return (obj != null) && (obj.getClass() == CixsService.class)
	    	&& ((CixsService) obj).getName().equals(getName());
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
	    if (o.getClass() != CixsService.class) {
	        throw new ClassCastException(o.getClass().getName());
	    } else {
	        return ((CixsService) o).getName().compareTo(getName());
	    }
	}

	/**
	 * @return the Service implementation class name
	 */
	public final String getEndpointImplementationClassName() {
		return mEndpointImplementationClassName;
	}

	/**
	 * @param endpointImplementationClassName the Service implementation class
	 *  name to set
	 */
	public final void setEndpointImplementationClassName(
			final String endpointImplementationClassName) {
		mEndpointImplementationClassName = endpointImplementationClassName;
	}

	/**
	 * @return the Service interface class name
	 */
	public final String getEndpointInterfaceClassName() {
		return mEndpointInterfaceClassName;
	}

	/**
	 * @param endpointInterfaceClassName the Service interface class name to set
	 */
	public final void setEndpointInterfaceClassName(
			final String endpointInterfaceClassName) {
		mEndpointInterfaceClassName = endpointInterfaceClassName;
	}

	/**
	 * @return the Port type in WSDL
	 */
	public final String getWsdlPortType() {
		return mWsdlPortType;
	}

	/**
	 * @param wsdlPortType the Port type in WSDL to set
	 */
	public final void setWsdlPortType(final String wsdlPortType) {
		mWsdlPortType = wsdlPortType;
	}

	/**
	 * @return the Service name in WSDL
	 */
	public final String getWsdlServiceName() {
		return mWsdlServiceName;
	}

	/**
	 * @param wsdlServiceName the Service name in WSDL to set
	 */
	public final void setWsdlServiceName(final String wsdlServiceName) {
		mWsdlServiceName = wsdlServiceName;
	}

}
