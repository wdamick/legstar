/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.cixs.gen.model;

import com.legstar.codegen.CodeGenUtil;

/**
 * This class describes a Service which maps a set of mainframe programs
 * (each program maps with an operation).
 * This model is usable for both inbound (Mainframe calling Web Service) and
 * outbound (Web Service calling Mainframe).
 * Not all properties are useful for both inbound and outbound but it is simpler
 * to have a single model for both.
 */
public abstract class AbstractCixsService extends CixsMappingModel {

	/** Default suffix for class implementation name. */
    private static final String DEFAULT_IMPL_SUFFIX = "Impl";
    
	/** Service package name. */
	private String mPackageName;

	/** Service interface class name. */
	private String mInterfaceClassName;
	
	/** Service implementation class name. */
	private String mImplementationClassName;

	/** The URI that a client must use to reach the Web service. */
	private String mServiceURI;

	/** User ID to present Web service. */
	private String mServiceUserId;

	/** Password to present Web service. */
	private String mServicePassword;

	/** Web service WSDL service name. */
	private String mWsdlServiceName;

	/** Web service WSDL port type. */
	private String mWsdlPortType;
	
	/** Web service WSDL port name. */
	private String mWsdlPortName;

	/** URL locating target Web service WSDL. */
	private String mWsdlUrl;

	/** Web service target namespace. */
	private String mTargetNamespace;

	/** Will be appended to service name to form a port type name. */
	private static final String WSDL_PORT_TYPE_SUFFIX = "Port";
	
	/** Will be appended to service name to form a port name. */
	private static final String WSDL_PORT_NAME_SUFFIX = "Port";
	
	/** By default the web service name is built from component name and this
	 * suffix.*/
	private static final String WSDL_SERVICE_NAME_SUFFIX = "Service";
	
	/**
	 * @return the Service package name
	 */
	public final String getPackageName() {
		return mPackageName;
	}

	/**
	 * @param packageName the Service package name to set
	 */
	public final void setPackageName(final String packageName) {
		mPackageName = packageName;
	}

	/**
	 * @return the Service interface class name
	 */
	public final String getInterfaceClassName() {
		if (mInterfaceClassName == null || mInterfaceClassName.length() == 0) {
			return CodeGenUtil.classNormalize(getName());
		}
		return mInterfaceClassName;
	}

	/**
	 * @param interfaceClassName the Service interface class name to set
	 */
	public final void setInterfaceClassName(final String interfaceClassName) {
		mInterfaceClassName = interfaceClassName;
	}

	/**
	 * @return the Service implementation class name
	 */
	public final String getImplementationClassName() {
		if (mImplementationClassName == null 
				|| mImplementationClassName.length() == 0) {
			if (getName() == null) {
				return DEFAULT_IMPL_SUFFIX;
			}
			return CodeGenUtil.classNormalize(getName() + DEFAULT_IMPL_SUFFIX);
		}
		return mImplementationClassName;
	}

	/**
	 * @param implementationClassName the Service implementation class name to
	 *  set
	 */
	public final void setImplementationClassName(
			final String implementationClassName) {
		mImplementationClassName = implementationClassName;
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

}
