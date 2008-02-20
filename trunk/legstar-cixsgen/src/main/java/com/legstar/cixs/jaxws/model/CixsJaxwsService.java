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
package com.legstar.cixs.jaxws.model;

import java.io.File;
import java.io.IOException;
import java.io.StringReader;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.legstar.cixs.gen.model.AbstractCixsService;
import com.legstar.cixs.gen.model.CixsModelException;
import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.codegen.CodeGenUtil;

/**
 * This class describes a service.
 * 
 * @author Fady Moussallam
 * 
 */ 
public class CixsJaxwsService extends AbstractCixsService {
	
	/** Service name. */
	private String mName;

	/** Service endpoint package name. */
	private String mPackageName;
	
	/** Service target namespace. */
	private String mTargetNamespace;
	
	/** Service interface class name. */
	private String mInterfaceClassName;
	
	/** Service implementation class name. */
	private String mImplementationClassName;
	
	/** Service host header class name. */
	private String mHeaderClassName;
	
	/** Service name in WSDL. */
	private String mWsdlServiceName;
	
	/** Port type in WSDL. */
	private String mWsdlPortType;
	
	/** XML element representing a CIXS Jaxws service definition. */
	public static final String CIXS_SERVICE_XML_E = "cixsJaxwsService";

	/** XML attribute representing a CIXS service name. */
	public static final String CIXS_SERVICE_NAME_XML_A = "name";

	/** XML attribute representing a CIXS endpoint package name. */
	public static final String CIXS_ENDPOINT_PKG_XML_A
			= "packageName";

	/** XML attribute representing a CIXS target namespace. */
	public static final String CIXS_NAMESPACE_XML_A = "targetNamespace";
	
	/** XML attribute representing a service interface class name. */
	public static final String CIXS_SEI_CLASS_A = "interfaceClassName";
	
	/** XML attribute representing a Service implementation class name. */
	public static final String CIXS_SEIMPL_CLASS_A =
		"implementationClassName";
	
	/** XML attribute representing a Service name in WSDL. */
	public static final String CIXS_WSDL_SERVICE_XML_A = "wsdlServiceName";
	
	/** XML attribute representing a Port type in WSDL. */
	public static final String CIXS_WSDL_PORT_TYPE_XML_A = "wsdlPortType";
	
	/** Used to construct implementation class name if none is provided. */
	private static final String IMPLEMENTATION_TYPE_SUFFIX = "Impl";

	/** Will be appended to service name to form a port type name. */
	private static final String WSDL_PORT_TYPE_SUFFIX = "Port";
	
	/** Will be appended to implementation class name to form a host header
	 *  class name. */
	private static final String HOST_HEADER_SUFFIX = "HostHeader";
	
	/** A constant used to pretty print serialized XML. */
	private static final String CRLF = "\r\n";

	/** By default the web service name is built from component name and this
	 * suffix.*/
	private static final String WSDL_SERVICE_NAME_SUFFIX = "Service";
	
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
	public final String getPackageName() {
		return mPackageName;
	}

	/**
	 * @param packageName the service endpoint package name to set
	 */
	public final void setPackageName(final String packageName) {
		mPackageName = packageName;
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
	 * Create an XML usable as input for and ant task.
	 * @return the XML
	 */
	public final String serialize() {
		StringBuffer result = new StringBuffer();
		result.append("<" + CIXS_SERVICE_XML_E + " "
				+ CIXS_SERVICE_NAME_XML_A + "="
				+ '\"' + mName + '\"');
		result.append(" " + CIXS_ENDPOINT_PKG_XML_A + "="
				+ '\"' + mPackageName + '\"');
		result.append(" " + CIXS_NAMESPACE_XML_A + "="
				+ '\"' + mTargetNamespace + '\"');
		if (getInterfaceClassName() != null
				&& getInterfaceClassName().length() > 0) {
			result.append(" " + CIXS_SEI_CLASS_A + "=" + '\"'
					+ getInterfaceClassName() + '\"');
		}
		if (getImplementationClassName() != null
				&& getImplementationClassName().length() > 0) {
			result.append(" " + CIXS_SEIMPL_CLASS_A + "=" + '\"'
					+ getImplementationClassName() + '\"');
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
		for (CixsOperation op : getCixsOperations()) {
			result.append(op.serialize());
			result.append(CRLF);
		}
		result.append("</" + CIXS_SERVICE_XML_E + ">");
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
				CIXS_SERVICE_XML_E);
		if (listOfElements == null || listOfElements.getLength() == 0) {
			throw (new CixsModelException(
					"Empty or invalid service descriptor file"));
		}
		try {
			Element serviceElement = (Element) listOfElements.item(0);
			mName = serviceElement.getAttribute(CIXS_SERVICE_NAME_XML_A);
			if (mName == null || mName.length() == 0) {
				throw new CixsModelException("Service must have a name");
			}
			mPackageName = serviceElement.getAttribute(
					CIXS_ENDPOINT_PKG_XML_A);
			mTargetNamespace = serviceElement.getAttribute(
					CIXS_NAMESPACE_XML_A);
			mInterfaceClassName = serviceElement.getAttribute(
					CIXS_SEI_CLASS_A);
			mImplementationClassName = serviceElement.getAttribute(
					CIXS_SEIMPL_CLASS_A);
			mWsdlServiceName =  serviceElement.getAttribute(
					CIXS_WSDL_SERVICE_XML_A);
			mWsdlPortType =  serviceElement.getAttribute(
					CIXS_WSDL_PORT_TYPE_XML_A);
			
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
	    return (obj != null) && (obj.getClass() == CixsJaxwsService.class)
	    	&& ((CixsJaxwsService) obj).getName().equals(getName());
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
	    if (o.getClass() != CixsJaxwsService.class) {
	        throw new ClassCastException(o.getClass().getName());
	    } else {
	        return ((CixsJaxwsService) o).getName().compareTo(getName());
	    }
	}

	/**
	 * @return the Service implementation class name
	 */
	public final String getImplementationClassName() {
		if (mImplementationClassName == null
				|| mImplementationClassName.length() == 0) {
			return getInterfaceClassName() + IMPLEMENTATION_TYPE_SUFFIX;
		}
		return mImplementationClassName;
	}

	/**
	 * @param implementationClassName the Service implementation class
	 *  name to set
	 */
	public final void setImplementationClassName(
			final String implementationClassName) {
		mImplementationClassName = implementationClassName;
	}

	/**
	 * @return the Service interface class name
	 */
	public final String getInterfaceClassName() {
		if (mInterfaceClassName == null || mInterfaceClassName.length() == 0) {
			if (mName != null && mName.length() > 0) {
				return CodeGenUtil.classNormalize(mName);
			}
		}
		return mInterfaceClassName;
	}

	/**
	 * @param endpointInterfaceClassName the Service interface class name to set
	 */
	public final void setInterfaceClassName(
			final String endpointInterfaceClassName) {
		mInterfaceClassName = endpointInterfaceClassName;
	}

	/**
	 * @return the Port type in WSDL
	 */
	public final String getWsdlPortType() {
		if (mWsdlPortType == null || mWsdlPortType.length() == 0) {
			return getName() + WSDL_PORT_TYPE_SUFFIX;
		}
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
		if (mWsdlServiceName == null || mWsdlServiceName.length() == 0) {
			return getName() + WSDL_SERVICE_NAME_SUFFIX;
		}
		return mWsdlServiceName;
	}

	/**
	 * @param wsdlServiceName the Service name in WSDL to set
	 */
	public final void setWsdlServiceName(final String wsdlServiceName) {
		mWsdlServiceName = wsdlServiceName;
	}

	/**
	 * @return the host header class name
	 */
	public final String getHeaderClassName() {
		if (mHeaderClassName == null || mHeaderClassName.length() == 0) {
			return getInterfaceClassName() + HOST_HEADER_SUFFIX;
		}
		return mHeaderClassName;
	}

	/**
	 * @param headerClassName the host header class name to set
	 */
	public final void setHeaderClassName(final String headerClassName) {
		mHeaderClassName = headerClassName;
	}

}
