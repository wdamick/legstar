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

import com.legstar.cixs.gen.model.AbstractCixsService;

/**
 * This class describes a service with Mainframe access capabilities.
 * 
 * @author Fady Moussallam
 * 
 */ 
public class CixsJaxwsService extends AbstractCixsService {
	
	/** Service target namespace. */
	private String mTargetNamespace;
	
	/** Service host header class name. */
	private String mHeaderClassName;
	
	/** Service name in WSDL. */
	private String mWsdlServiceName;
	
	/** Port type in WSDL. */
	private String mWsdlPortType;
	
	/** Will be appended to service name to form a port type name. */
	private static final String WSDL_PORT_TYPE_SUFFIX = "Port";
	
	/** Will be appended to implementation class name to form a host header
	 *  class name. */
	private static final String HOST_HEADER_SUFFIX = "HostHeader";
	
	/** By default the web service name is built from component name and this
	 * suffix.*/
	private static final String WSDL_SERVICE_NAME_SUFFIX = "Service";
	
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
