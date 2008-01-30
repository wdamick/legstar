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
package com.legstar.c2ws;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import org.apache.commons.configuration.Configuration;

/**
 * This class holds the parameters that are necessary to call a remote
 * Web Service method.
 */
public class C2wsWSDescriptor {
	
	/* ====================================================================== */
	/* = Properties section                                                 = */
	/* ====================================================================== */
	/** The WSDL target namespace. */
	private String mWsdlTargetNamespace;
	
	/** The WSDL service name. */
	private String mWsdlName;

	/** The WSDL port name. */
	private String mWsdlPort;
	
	/** The WSDL URL. */
	private String mWsdlUrl;
	
	/** The JAXB request object. */
	private JaxbObjectDescriptor mJaxbRequest;
	
	/** The JAXB response object. */
	private JaxbObjectDescriptor mJaxbResponse;
	
	/** This is a JAXB Context that can be used to marshal/unmarshal the
	 * JAXB annotated classes that form the request and reply for this
	 * web service. */
	private JAXBContext mJaxbContext;
	
	/** This indicates if the JAXBContext was set by the caller or if we are
	 * responsible for automatically creating one when requested (which is
	 * the default. */
	private boolean mJaxbContextAutoGeneration = true;
	
	/** JAXB Object factory for the request package. */
	private Object mRequestObjectFactory;

	/** JAXB Object factory for the response package. */
	private Object mResponseObjectFactory;

	/* ====================================================================== */
	/* = Constants section                                                  = */
	/* ====================================================================== */
	/** Path to wsdl target namespace in configuration file. */
	private static final String WSDL_NS_KEY = "wsdl.targetNamespace";

	/** Path to wsdl service name in configuration file. */
	private static final String WSDL_SERVICE_KEY = "wsdl.name";

	/** Path to wsdl port name in configuration file. */
	private static final String WSDL_PORT_KEY = "wsdl.port";

	/** Path to wsdl url in configuration file. */
	private static final String WSDL_URL_KEY = "wsdl.url";
	
	/** Path to jaxb request package name in configuration file. */
	private static final String JAXB_REQUEST_PKG_KEY =
		"jaxb.request.packageName";

	/** Path to jaxb request type name in configuration file. */
	private static final String JAXB_REQUEST_TYPE_KEY =
		"jaxb.request.typeName";

	/** Path to jaxb request element name in configuration file. */
	private static final String JAXB_REQUEST_ELEMENT_KEY =
		"jaxb.request.elementName";

	/** Path to jaxb response package name in configuration file. */
	private static final String JAXB_RESPONSE_PKG_KEY =
		"jaxb.response.packageName";

	/** Path to jaxb response type name in configuration file. */
	private static final String JAXB_RESPONSE_TYPE_KEY =
		"jaxb.response.typeName";

	/** Path to jaxb response element name in configuration file. */
	private static final String JAXB_RESPONSE_ELEMENT_KEY =
		"jaxb.response.elementName";

	/** No-Argument constructor. */
	public C2wsWSDescriptor() {
		
	}

	/**
	 * Create a descriptor from a configuration.
	 * @param wsConfig a configuration fragment describing this service
	 */
	public C2wsWSDescriptor(final Configuration wsConfig) {
		mWsdlTargetNamespace =
				(String) wsConfig.getString(WSDL_NS_KEY);
		mWsdlName =
				(String) wsConfig.getString(WSDL_SERVICE_KEY);
		mWsdlPort =
				(String) wsConfig.getString(WSDL_PORT_KEY);
		mWsdlUrl =
				(String) wsConfig.getString(WSDL_URL_KEY);
		mJaxbRequest = new JaxbObjectDescriptor();
		mJaxbRequest.setPackageName(
				(String) wsConfig.getString(JAXB_REQUEST_PKG_KEY));
		mJaxbRequest.setTypeName(
				(String) wsConfig.getString(JAXB_REQUEST_TYPE_KEY));
		mJaxbRequest.setElementName(
				(String) wsConfig.getString(JAXB_REQUEST_ELEMENT_KEY));
		mJaxbResponse = new JaxbObjectDescriptor();
		mJaxbResponse.setPackageName(
				(String) wsConfig.getString(JAXB_RESPONSE_PKG_KEY));
		mJaxbResponse.setTypeName(
				(String) wsConfig.getString(JAXB_RESPONSE_TYPE_KEY));
		mJaxbResponse.setElementName(
				(String) wsConfig.getString(JAXB_RESPONSE_ELEMENT_KEY));
		
	}
	
	/**
	 * @return the JAXB package name
	 */
	public final JaxbObjectDescriptor getJaxbRequest() {
		return mJaxbRequest;
	}

	/**
	 * @param jaxbRequest the JAXB package name to set
	 */
	public final void setJaxbRequest(final JaxbObjectDescriptor jaxbRequest) {
		/* If we are responsible for the JAXBContext, make sure it gets
		 * regenerated since caller is changing the request here. */
		if (mJaxbContextAutoGeneration) {
			mJaxbContext = null;
		}
		/* Invalidate the associated object factory so its get recreated next
		 * time it is queried. */
		mRequestObjectFactory = null;

		mJaxbRequest = jaxbRequest;
	}

	/**
	 * @return the JAXB package name
	 */
	public final JaxbObjectDescriptor getJaxbResponse() {
		return mJaxbResponse;
	}

	/**
	 * @param jaxbResponse the JAXB package name to set
	 */
	public final void setJaxbResponse(final JaxbObjectDescriptor jaxbResponse) {
		/* If we are responsible for the JAXBContext, make sure it gets
		 * regenerated since caller is changing the response here. */
		if (mJaxbContextAutoGeneration) {
			mJaxbContext = null;
		}
		/* Invalidate the associated object factory so its get recreated next
		 * time it is queried. */
		mResponseObjectFactory = null;

		mJaxbResponse = jaxbResponse;
	}

	/**
	 * @return the WSDL service name
	 */
	public final String getWsdlName() {
		return mWsdlName;
	}

	/**
	 * @param wsdlName the WSDL service name to set
	 */
	public final void setWsdlName(final String wsdlName) {
		mWsdlName = wsdlName;
	}

	/**
	 * @return the WSDL port name
	 */
	public final String getWsdlPort() {
		return mWsdlPort;
	}

	/**
	 * @param wsdlPort the WSDL port name to set
	 */
	public final void setWsdlPort(final String wsdlPort) {
		mWsdlPort = wsdlPort;
	}

	/**
	 * @return the WSDL target namespace
	 */
	public final String getWsdlTargetNamespace() {
		return mWsdlTargetNamespace;
	}

	/**
	 * @param wsdlTargetNamespace the WSDL target namespace to set
	 */
	public final void setWsdlTargetNamespace(final String wsdlTargetNamespace) {
		mWsdlTargetNamespace = wsdlTargetNamespace;
	}

	/**
	 * @return the WSDL URL
	 */
	public final String getWsdlUrl() {
		return mWsdlUrl;
	}

	/**
	 * @param wsdlUrl the WSDL URL to set
	 */
	public final void setWsdlUrl(final String wsdlUrl) {
		mWsdlUrl = wsdlUrl;
	}
	
	/** {@inheritDoc} */
	public final String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("Wsdl service name=");
		sb.append(mWsdlName);
		sb.append(", ");
		sb.append("Wsdl target namespace=");
		sb.append(mWsdlTargetNamespace);
		sb.append(", ");
		sb.append("Wsdl port=");
		sb.append(mWsdlPort);
		sb.append(", ");
		sb.append("Wsdl Url=");
		sb.append(mWsdlUrl);
		sb.append(", ");
		sb.append("JAXB request=[");
		sb.append(mJaxbRequest.toString());
		sb.append("], ");
		sb.append("JAXB response=[");
		sb.append(mJaxbResponse.toString());
		sb.append("]");
		return sb.toString();
	}

	/**
	 * This method is used to lazily create the JAXB context which is an
	 * expensive operation.
	 * @return the JAXB context
	 * @throws C2wsConfigurationException if the JAXB context cannot be created
	 */
	public final JAXBContext getJaxbContext()
						throws C2wsConfigurationException {
		if (mJaxbContext == null) {
			mJaxbContextAutoGeneration = true;
			try {
				if (mJaxbResponse.getPackageName().compareTo(
						mJaxbRequest.getPackageName()) == 0) {
					mJaxbContext = JAXBContext.newInstance(
							getRequestObjectFactory().getClass());
				} else {
					mJaxbContext = JAXBContext.newInstance(
							getRequestObjectFactory().getClass(),
							getResponseObjectFactory().getClass());
				}
			} catch (JAXBException e) {
				throw new C2wsConfigurationException(e);
			}
		}
		return mJaxbContext;
	}

	/**
	 * @param jaxbContext the JAXB context to set
	 */
	public final void setJaxbContext(final JAXBContext jaxbContext) {
		mJaxbContextAutoGeneration = false;
		mJaxbContext = jaxbContext;
	}

	/**
	 * Returns the request package JAXB Object factory.
	 * @return the object factory
	 * @throws C2wsConfigurationException if object factory cannot be
	 *  instanciated
	 */
	public final Object getRequestObjectFactory()
	            throws C2wsConfigurationException {
		if (mRequestObjectFactory == null) {
			try {
				Class < ? > ofClass = Class.forName(
						mJaxbRequest.getPackageName() + ".ObjectFactory");
				return ofClass.newInstance();
			} catch (ClassNotFoundException e) {
				throw new C2wsConfigurationException(e);
			} catch (InstantiationException e) {
				throw new C2wsConfigurationException(e);
			} catch (IllegalAccessException e) {
				throw new C2wsConfigurationException(e);
			}
		}
		return mRequestObjectFactory;
	}
	
	/**
	 * Returns the response package JAXB Object factory.
	 * @return the object factory
	 * @throws C2wsConfigurationException if object factory cannot be
	 *  instanciated
	 */
	public final Object getResponseObjectFactory()
				throws C2wsConfigurationException {
		if (mResponseObjectFactory == null) {
			try {
				Class < ? > ofClass = Class.forName(
						mJaxbResponse.getPackageName() + ".ObjectFactory");
				return ofClass.newInstance();
			} catch (ClassNotFoundException e) {
				throw new C2wsConfigurationException(e);
			} catch (InstantiationException e) {
				throw new C2wsConfigurationException(e);
			} catch (IllegalAccessException e) {
				throw new C2wsConfigurationException(e);
			}
		}
		return mResponseObjectFactory;
	}
	

}
