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
package com.legstar.c2ws.reflect;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import javax.xml.bind.JAXBElement;
import javax.xml.namespace.QName;
import javax.xml.ws.Dispatch;
import javax.xml.ws.Service;
import javax.xml.ws.WebServiceException;
import javax.xml.ws.soap.SOAPBinding;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.c2ws.C2wsAdapter;
import com.legstar.c2ws.C2wsAdapterException;
import com.legstar.c2ws.C2wsConfigurationException;
import com.legstar.c2ws.HostMarshalException;
import com.legstar.c2ws.HostUnmarshalException;
import com.legstar.c2ws.C2wsWSDescriptor;
import com.legstar.c2ws.util.C2wsLog;
import com.legstar.c2ws.util.C2wsUtil;
import com.legstar.coxb.CobolContext;
import com.legstar.coxb.convert.CobolConverters;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.host.HostException;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.visitor.CobolMarshalVisitor;
import com.legstar.coxb.visitor.CobolUnmarshalVisitor;

/**
 * This class uses a combination of jaxws and jaxb to formulate a SOAP
 * request on behalf of the host. The host data is being translated into
 * jaxb objects using the dynamic reflective cobol binding classes from
 * coxbrt.
 */
public class C2wsReflectAdapter implements C2wsAdapter {

	/** Logger. */
	private static final Log LOG =	LogFactory.getLog(C2wsReflectAdapter.class);
	
	/** Enhanced logger with correlation id. */
	private C2wsLog mLog = new C2wsLog(LOG);
	
	/** A set of cobol converters. */
	private CobolConverters mCc;
	
	/** Default no-arg constructor. */
	public C2wsReflectAdapter() {
		/* Create a default cobol converter using the default host character
		 *  set */
		mCc = new CobolSimpleConverters(new CobolContext());
	}
	
	/**
	 * Invokes a target Web Service with a one input/one output sybchronous
	 * exchange pattern.
	 * @param wsd the web service descriptor
	 * @param hostBytes the inbound host request data
	 * @return the outbound host reply data
	 * @throws C2wsAdapterException in invoke fails
	 */
	public final byte[] invoke(
			final C2wsWSDescriptor wsd,
			final byte[] hostBytes) throws C2wsAdapterException {
		try {
			if (mLog.isDebugEnabled()) {
				mLog.debug("Entered invoke for " + wsd.getWsdlName());
			}
			
			/* Create jaxb request object, dispatch and process reply */
			Object oRequest = getRequestObject(wsd, hostBytes);
			Object oResponse = invokeDispatch(wsd, oRequest);
			byte[] responseHostBytes = getResponseBytes(wsd, oResponse);

			if (mLog.isDebugEnabled()) {
				mLog.debug("invoke returned " + responseHostBytes.length
						+ " bytes");
			}
			return responseHostBytes;
		} catch (WebServiceException e) {
			throw new C2wsAdapterException(e);
		}
	}
	
	/**
	 * Buils a request JAXB object.
	 * @param wsd the web service descriptor
	 * @param hostBytes the inbound request host data 
	 * @return a JAXB request object
	 * @throws C2wsAdapterException if object cannot be instanciated
	 */
	private Object getRequestObject(
			final C2wsWSDescriptor wsd,
			final byte[] hostBytes) throws C2wsAdapterException {
		try {
			Class requestClass = Class.forName(
					wsd.getJaxbRequest().getPackageName() + '.'
					+ wsd.getJaxbRequest().getTypeName());
			return unmarshalReflect(
					wsd.getRequestObjectFactory(), requestClass, hostBytes);
		} catch (ClassNotFoundException e) {
			throw new C2wsAdapterException(e);
		} catch (HostUnmarshalException e) {
			throw new C2wsAdapterException(e);
		} catch (C2wsConfigurationException e) {
			throw new C2wsAdapterException(e);
		}
	}
	
	/**
	 * The response can be encapsulated in a JAXBElement. If this is the case,
	 * this code will extract the imbedded annotated response.
	 * @param wsd the web service descriptor
	 * @param oResponse the response object returned from dispatch (potentially
	 * a JAXBElement).
	 * @return the marshaled data ready to be sent to the host
	 * @throws C2wsAdapterException if response construction fails
	 */
	private byte[] getResponseBytes(
			final C2wsWSDescriptor wsd,
			final Object oResponse) throws C2wsAdapterException {
		try {
			if (wsd.getJaxbResponse().isXmlRootElement()) {
				return marshalReflect(wsd.getResponseObjectFactory(),
						oResponse);
			} else {
				return marshalReflect(wsd.getResponseObjectFactory(),
						((JAXBElement) oResponse).getValue());
			}
		} catch (HostMarshalException e) {
			throw new C2wsAdapterException(e);
		} catch (C2wsConfigurationException e) {
			throw new C2wsAdapterException(e);
		}
	}

	/**
	 * Call the JAXWS dispatch method.
	 * @param wsd the web service descriptor
	 * @param oRequest the request object
	 * @return the object response
	 * @throws C2wsAdapterException if dispatch fails
	 */
	private Object invokeDispatch(
			final C2wsWSDescriptor wsd,
			final Object oRequest) throws C2wsAdapterException {
		
		if (mLog.isDebugEnabled()) {
			mLog.debug("Entered invokeDispatch for " + wsd.getWsdlName());
		}
		/* Create jaxws service and port */
		QName serviceQname = new QName(
				wsd.getWsdlTargetNamespace(), wsd.getWsdlName());
		QName portQname = new QName(
				wsd.getWsdlTargetNamespace(), wsd.getWsdlPort());
		Service service = Service.create(serviceQname);
		service.addPort(portQname, SOAPBinding.SOAP11HTTP_BINDING,
				wsd.getWsdlUrl());
		
		try {
			Dispatch < Object > dispatcher = service.createDispatch(
					portQname, wsd.getJaxbContext(), Service.Mode.PAYLOAD);
			/* The parameters passed and received from dispatch.invoke must
			 * be of XmlRootElement type. */
			Object oResponse;
			if (wsd.getJaxbRequest().isXmlRootElement()) {
				oResponse = dispatcher.invoke(oRequest);
			} else {
				JAXBElement jeRequest = getJAXBElement(
						wsd.getRequestObjectFactory(),
						wsd.getJaxbRequest().getElementName(), oRequest);
				oResponse = dispatcher.invoke(jeRequest);
			}
			if (mLog.isDebugEnabled()) {
				mLog.debug("invokeDispatch returned " + oResponse);
			}
			return oResponse;
		} catch (C2wsConfigurationException e) {
			throw new C2wsAdapterException(e);
		}
	}
	
	/**
	 * Root elements can be created with special methods in the JAXB object 
	 * factory. This method will use reflection to create a JAXBElement.
	 * @param objectFactory the JAXB object factory
	 * @param elementName the element name
	 * @param type an occurence of the element type
	 * @return a JAXBElement
	 * @throws C2wsAdapterException if the JAXBElement cannot be created
	 */
	private JAXBElement getJAXBElement(
			final Object objectFactory,
			final String elementName,
			final Object type) throws C2wsAdapterException {
		try {
			String createName = "create" + elementName;
			Method creator = objectFactory.getClass().getMethod(
					createName, type.getClass());
			return (JAXBElement) creator.invoke(objectFactory, type);
		} catch (IllegalAccessException e) {
			throw new C2wsAdapterException(e);
		} catch (SecurityException e) {
			throw new C2wsAdapterException(e);
		} catch (NoSuchMethodException e) {
			throw new C2wsAdapterException(e);
		} catch (IllegalArgumentException e) {
			throw new C2wsAdapterException(e);
		} catch (InvocationTargetException e) {
			throw new C2wsAdapterException(e);
		}
	}
	
	/**
	 * Unmarshals host data into a JAXB object using reflection.
	 * @param objectFactory the JAXB object factory
	 * @param requestClass the JAXB request class
	 * @param hostBytes the host data
	 * @return a JAXB object
	 * @throws HostUnmarshalException if unmarshaling fails
	 */
	public final Object unmarshalReflect(
			final Object objectFactory,
			final Class requestClass,
			final byte[] hostBytes) throws HostUnmarshalException {
		try {
			if (mLog.isDebugEnabled()) {
				mLog.debug("Entered unmarshalReflect with "
						+ hostBytes.length + " bytes:");
				C2wsUtil.traceData(mLog.getCorrelationId(),
						hostBytes, hostBytes.length);
			}
			
			CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(
					hostBytes, 0, mCc);
			CComplexReflectBinding ccem = new CComplexReflectBinding(
					objectFactory, requestClass);
			ccem.accept(uv);
			Object request = ccem.getObjectValue(requestClass);
			
			if (mLog.isDebugEnabled()) {
				mLog.debug("unmarshalReflect produced object " + request);
			}
			return request;
		} catch (HostException e) {
			throw new HostUnmarshalException(e);
		}
	}

	/**
	 * Marhals a JAXB to host data using reflection.
	 * @param objectFactory the JAXB object factory
	 * @param response the JAXB response object
	 * @return hostBytes the host data
	 * @throws HostMarshalException if marshaling fails
	 */
	public final byte[] marshalReflect(
			final Object objectFactory,
			final Object response) throws HostMarshalException {
		try {
			byte[] hostBytes;
			if (mLog.isDebugEnabled()) {
				mLog.debug("Entered marshalReflect with object " + response);
			}
			CComplexReflectBinding ccem = new CComplexReflectBinding(
					objectFactory, response);
			byte[] maxHostBytes = new byte[ccem.calcByteLength()];
			CobolMarshalVisitor mv = new CobolMarshalVisitor(
					maxHostBytes, 0, mCc);
			ccem.accept(mv);
			if (mv.getOffset() == 0) {
				return null;
			}
			
			/* calcByteLength returns the largest host buffer length. Since we
			 * now know what the real size is, avoid sending too much data
			 * back.*/
			if (mv.getOffset() < maxHostBytes.length) {
				if (mLog.isDebugEnabled()) {
					mLog.debug("allocating smaller byte array");
				}
				byte[] shortHostBytes = new byte[mv.getOffset()];
				System.arraycopy(maxHostBytes, 0,
						shortHostBytes, 0, mv.getOffset());
				hostBytes = shortHostBytes;
			} else {
				hostBytes = maxHostBytes;
			}
			if (mLog.isDebugEnabled()) {
				mLog.debug("marshalReflect produced "
						+ hostBytes.length + " bytes:");
				C2wsUtil.traceData(mLog.getCorrelationId(),
						hostBytes, hostBytes.length);
			}
			return hostBytes;
		} catch (HostException e) {
			throw new HostMarshalException(e);
		}
	}

	/** {@inheritDoc}*/
	public final void setCorrelationId(final String cxid) {
		mLog.setCorrelationId(cxid);
	}

	/** {@inheritDoc}*/
	public final void setHostCharset(final String hostCharset) {
		/* Setup cobol converters with the target character set */
		CobolContext cobolContext = new CobolContext();
		cobolContext.setHostCharsetName(hostCharset);
		mCc = new CobolSimpleConverters(cobolContext);
	}

}
