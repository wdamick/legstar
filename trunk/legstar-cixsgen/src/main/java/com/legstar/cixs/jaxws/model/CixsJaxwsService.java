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

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.util.List;

import com.legstar.cixs.gen.model.AbstractCixsService;
import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.gen.model.CixsStructure;
import com.legstar.cixs.jaxws.gen.CobolCodeGenException;
import com.legstar.cobc.gen.CobolGenVisitor;
import com.legstar.coxb.host.HostException;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.impl.reflect.ReflectBindingException;
import com.legstar.util.JAXBAnnotationException;
import com.legstar.util.JAXBElementDescriptor;
import com.legstar.xsdc.gen.CobolNameResolver;
import com.legstar.xsdc.gen.CobolNameResolverException;

/**
 * This class describes a Web Service which maps a set of mainframe programs
 * (each program maps with an operation).
 * This model is usable for both inbound (Mainframe calling Web Service) and
 * outbound (Web Service calling Mainframe).
 * Not all properties are useful for both inbound and outbound but is cleaner
 * a single model for both.
 * 
 * @author Fady Moussallam
 * 
 */ 
public class CixsJaxwsService extends AbstractCixsService {
	
	/** Web service WSDL service name. */
	private String mWsdlServiceName;

	/** Web service WSDL port type. */
	private String mWsdlPortType;
	
	/** Web service WSDL port name. */
	private String mWsdlPortName;

	/** The URI that a client must use to reach the Web service. */
	private String mServiceURI;

	/** User ID to present Web service. */
	private String mServiceUserId;

	/** Password to present Web service. */
	private String mServicePassword;

	/** URL locating target Web service WSDL. */
	private String mWsdlUrl;

	/** Web service target namespace. */
	private String mTargetNamespace;

	/** Service host header class name. */
	private String mHeaderClassName;
	
	/** Used for structures to determine if they are input or output. */
    private enum Direction { INPUT, OUTPUT };

    /** Helper to suggest COBOL names from Java names. */
    private CobolNameResolver mCobolNameResolver;
    
	/** Will be appended to service name to form a port type name. */
	private static final String WSDL_PORT_TYPE_SUFFIX = "Port";
	
	/** Will be appended to service name to form a port name. */
	private static final String WSDL_PORT_NAME_SUFFIX = "Port";
	
	/** Will be appended to implementation class name to form a host header
	 *  class name. */
	private static final String HOST_HEADER_SUFFIX = "HostHeader";
	
	/** By default the web service name is built from component name and this
	 * suffix.*/
	private static final String WSDL_SERVICE_NAME_SUFFIX = "Service";
	
	/**
	 * @return the Web service target namespace
	 */
	public final String getTargetNamespace() {
		return mTargetNamespace;
	}

	/**
	 * @param targetNamespace the Web service target namespace to set
	 */
	public final void setTargetNamespace(final String targetNamespace) {
		mTargetNamespace = targetNamespace;
	}

	/**
	 * @return the Web service WSDL service name
	 */
	public final String getWsdlServiceName() {
		if (mWsdlServiceName == null || mWsdlServiceName.length() == 0) {
			return getName() + WSDL_SERVICE_NAME_SUFFIX;
		}
		return mWsdlServiceName;
	}

	/**
	 * @param wsdlServiceName the Web service WSDL service name to set
	 */
	public final void setWsdlServiceName(final String wsdlServiceName) {
		mWsdlServiceName = wsdlServiceName;
	}

	/**
	 * @return the Web service WSDL port type
	 */
	public final String getWsdlPortType() {
		if (mWsdlPortType == null || mWsdlPortType.length() == 0) {
			return getName() + WSDL_PORT_TYPE_SUFFIX;
		}
		return mWsdlPortType;
	}

	/**
	 * @param wsdlPortType the Web service WSDL port type to set
	 */
	public final void setWsdlPortType(final String wsdlPortType) {
		mWsdlPortType = wsdlPortType;
	}

	/**
	 * @return the Web service WSDL port name
	 */
	public final String getWsdlPortName() {
		if (mWsdlPortName == null || mWsdlPortName.length() == 0) {
			return getName() + WSDL_PORT_NAME_SUFFIX;
		}
		return mWsdlPortName;
	}

	/**
	 * @param wsdlPortName the Web service WSDL port name to set
	 */
	public final void setWsdlPortName(final String wsdlPortName) {
		mWsdlPortName = wsdlPortName;
	}

	/**
	 * @return the URI that the host must use to reach the remote service
	 */
	public final String getServiceURI() {
		return mServiceURI;
	}

	/**
	 * @param serviceURI the URI that the host must use to reach the remote
	 *  service to set
	 */
	public final void setServiceURI(final String serviceURI) {
		mServiceURI = serviceURI;
	}

	/**
	 * @return the User ID to present remote service. If no user ID was set,
	 * this defaults to 8 space characters as a COBOL default.
	 */
	public final String getServiceUserId() {
		if (mServiceUserId == null || mServiceUserId.length() == 0) {
			return "        ";
		}
		return mServiceUserId;
	}

	/**
	 * @param serviceUserId the User ID to present remote service to set
	 */
	public final void setServiceUserId(final String serviceUserId) {
		mServiceUserId = serviceUserId;
	}

	/**
	 * @return the Password to present remote service. If no password was set,
	 * this defaults to 8 space characters as a COBOL default.
	 */
	public final String getServicePassword() {
		if (mServicePassword == null || mServicePassword.length() == 0) {
			return "        ";
		}
		return mServicePassword;
	}

	/**
	 * @param servicePassword the Password to present remote service to set
	 */
	public final void setServicePassword(final String servicePassword) {
		mServicePassword = servicePassword;
	}

	/**
	 * @return the URL locating target Web service WSDL
	 */
	public final String getWsdlUrl() {
		return mWsdlUrl;
	}

	/**
	 * @param wsdlUrl the URL locating target Web service WSDL to set
	 */
	public final void setWsdlUrl(final String wsdlUrl) {
		mWsdlUrl = wsdlUrl;
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
	/**
	 * Generates COBOL code for input data structures.
	 * @param operation an operation
	 * @return the COBOL source code for input data items.
	 * @throws CobolCodeGenException if code generation fails
	 */
	public String getInputStructuresCode(
			final CixsOperation operation) throws CobolCodeGenException {
		return getStructuresCode(operation, Direction.INPUT);
	}

	/**
	 * Generates COBOL code for output data structures.
	 * @param operation an operation
	 * @return the COBOL source code for output data items.
	 * @throws CobolCodeGenException if code generation fails
	 */
	public String getOutputStructuresCode(
			final CixsOperation operation) throws CobolCodeGenException {
		return getStructuresCode(operation, Direction.OUTPUT);
	}

	/**
	 * Generates COBOL code for data structures. The result is a a
	 * concatenation of the code for each of the inner structures.
	 * @param operation an operation
	 * @param direction either input or output
	 * @return the COBOL source code for data items
	 * @throws CobolCodeGenException if code generation fails
	 */
	private String getStructuresCode(
			final CixsOperation operation,
			final Direction direction) throws CobolCodeGenException {
		StringBuilder sb = new StringBuilder();
		List < CixsStructure > structures = null;

		if (direction == Direction.INPUT) {
			structures = operation.getInput();
		} else {
			structures = operation.getOutput();
		}
		for (CixsStructure structure : structures) {
			sb.append(getStructureCode(structure));
		}
		return sb.toString();
	}

	/**
	 * Using the <code>cobcgen</code> utility, this will use reflection
	 * to instantiate a Jaxb object corresponding to the structure
	 * received and then generate COBOL data description code using
	 * the COBOL annotations in the jaxb class. 
	 * @param structure the structure for which code is to be generated
	 * @return data description COBOL source code for the structure
	 * @throws CobolCodeGenException if code generation fails
	 */
	public String getStructureCode(
			final CixsStructure structure) throws CobolCodeGenException {
		try {
			JAXBElementDescriptor elementDescriptor = new JAXBElementDescriptor(
					structure.getJaxbPackageName(), structure.getJaxbType());
			Object objectFactory = elementDescriptor.createObjectFactory();
			Class < ? > clazz = elementDescriptor.loadJaxbClass();
			CComplexReflectBinding ccem = new CComplexReflectBinding(
					objectFactory, clazz);
			String cobolRootName = structure.getCobolRootDataItemName();
			if (cobolRootName == null || cobolRootName.length() == 0) {
				cobolRootName = getCobolNameResolver().getName(
						structure.getJaxbType());
			}
			ccem.setCobolName(cobolRootName);
			StringWriter writer = new StringWriter();
			BufferedWriter bufWriter = new BufferedWriter(writer);
			CobolGenVisitor cev = new CobolGenVisitor(5, 5, bufWriter);
			ccem.accept(cev);
			bufWriter.flush();
			return writer.toString();
		} catch (ReflectBindingException e) {
			throw new CobolCodeGenException(e);
		} catch (HostException e) {
			throw new CobolCodeGenException(e);
		} catch (IOException e) {
			throw new CobolCodeGenException(e);
		} catch (ClassNotFoundException e) {
			throw new CobolCodeGenException(e);
		} catch (CobolNameResolverException e) {
			throw new CobolCodeGenException(e);
		} catch (JAXBAnnotationException e) {
			throw new CobolCodeGenException(e);
		}
	}
	
	/**
	 * @return the current name resolver or a new one if none existed before
	 * @throws CobolNameResolverException if resolver cannot be created
	 */
	private CobolNameResolver getCobolNameResolver()
	throws CobolNameResolverException {
		if (mCobolNameResolver == null) {
			mCobolNameResolver = new CobolNameResolver();
		}
		return mCobolNameResolver;
	}
	

}
