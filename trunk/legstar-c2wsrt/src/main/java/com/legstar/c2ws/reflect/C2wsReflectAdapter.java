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
package com.legstar.c2ws.reflect;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Iterator;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.namespace.QName;
import javax.xml.soap.Detail;
import javax.xml.soap.DetailEntry;
import javax.xml.soap.SOAPFault;
import javax.xml.ws.Dispatch;
import javax.xml.ws.Service;
import javax.xml.ws.WebServiceException;
import javax.xml.ws.soap.SOAPBinding;
import javax.xml.ws.soap.SOAPFaultException;

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
import com.legstar.coxb.convert.ICobolConverters;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.host.HostException;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.util.JAXBAnnotationException;
import com.legstar.util.JAXBElementDescriptor;
import com.legstar.util.NameUtil;
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
    private static final Log LOG = LogFactory.getLog(C2wsReflectAdapter.class);

    /** Enhanced logger with correlation id. */
    private C2wsLog mLog = new C2wsLog(LOG);

    /** A set of cobol converters. */
    private ICobolConverters mCc;

    /** The target web service descriptor. */
    private C2wsWSDescriptor mC2wsWSDescriptor;

    /** This is a JAXB Context that can be used to marshal/unmarshal the
     * JAXB annotated classes that form the request and reply for this
     * web service. */
    private JAXBContext mJaxbContext;

    /** Instance of jaxws dynamic client invoker. This instance is lazily
     *  created once and reused each time this action is processed. */
    private Dispatch < Object > mDispatcher;

    /** The request object class. */
    Class < ? > mRequestClass;

    /** Default no-arg constructor. */
    public C2wsReflectAdapter() {
    }

    /** {@inheritDoc} */
    public void init(
            final C2wsWSDescriptor wsd,
            final String hostCharset) throws C2wsConfigurationException {
        try {
            mC2wsWSDescriptor = wsd;
            setHostCharset(hostCharset);
            mRequestClass = getRequestElementDescriptor().loadJaxbClass();
        } catch (ClassNotFoundException e) {
            throw new C2wsConfigurationException(e);
        }
    }

    /**
     * Invokes a target Web Service with a one input/one output sybchronous
     * exchange pattern.
     * @param hostBytes the inbound host request data
     * @return the outbound host reply data
     * @throws C2wsAdapterException in invoke fails
     */
    public final byte[] invoke(
            final byte[] hostBytes) throws C2wsAdapterException {
        try {
            if (mLog.isDebugEnabled()) {
                mLog.debug("Entered invoke for " + getWsdlServiceName());
            }
            if (getCobolConverters() == null) {
                throw new C2wsAdapterException(
                "Method init must be called prior to invoke");
            }

            /* Create jaxb request object, dispatch and process reply */
            Object oRequest = getRequestObject(hostBytes);
            Object oResponse = invokeDispatch(oRequest);
            byte[] responseHostBytes = getResponseBytes(oResponse);

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
     * Builds a request JAXB object.
     * @param hostBytes the inbound request host data 
     * @return a JAXB request object
     * @throws C2wsAdapterException if object cannot be instantiated
     */
    private Object getRequestObject(
            final byte[] hostBytes) throws C2wsAdapterException {
        try {
            return unmarshalReflect(
                    getRequestElementDescriptor().getObjectFactory(),
                    getRequestClass(), hostBytes);
        } catch (HostUnmarshalException e) {
            throw new C2wsAdapterException(e);
        }
    }

    /**
     * The response can be encapsulated in a JAXBElement. If this is the case,
     * this code will extract the imbedded annotated response.
     * @param oResponse the response object returned from dispatch (potentially
     * a JAXBElement).
     * @return the marshaled data ready to be sent to the host
     * @throws C2wsAdapterException if response construction fails
     */
    private byte[] getResponseBytes(
            final Object oResponse) throws C2wsAdapterException {
        try {
            if (oResponse instanceof JAXBElement) {
                return marshalReflect(
                        getResponseElementDescriptor().getObjectFactory(),
                        ((JAXBElement < ? >) oResponse).getValue());
            } else {
                return marshalReflect(
                        getResponseElementDescriptor().getObjectFactory(),
                        oResponse);
            }
        } catch (HostMarshalException e) {
            throw new C2wsAdapterException(e);
        }
    }

    /**
     * Call the JAXWS dispatch method.
     * <p/>
     * The parameters passed to dispatch.invoke must have an XmlRootElement
     * annotation or otherwise must be wrapped in a JAXBElement.
     * <p/>
     * Similarly, the reply could be wrapped in a JAXBElement in which
     * case we return its inner content model.
     * @param oRequest the request object
     * @return the object response
     * @throws C2wsAdapterException if dispatch fails
     */
    private Object invokeDispatch(
            final Object oRequest) throws C2wsAdapterException {

        try {
            Object oResponse;
            if (getRequestElementDescriptor().isXmlRootElement()) {
                oResponse = getDispatcher().invoke(oRequest);
            } else {
                JAXBElement < ? > jeRequest = getJAXBElement(
                        getRequestElementDescriptor().getObjectFactory(),
                        getRequestElementDescriptor().getElementName(),
                        oRequest);
                oResponse = getDispatcher().invoke(jeRequest);
            }
            if (LOG.isDebugEnabled()) {
                LOG.debug("invokeDispatch returned " + oResponse);
            }
            if (getResponseElementDescriptor().isXmlRootElement()) {
                return oResponse;
            } else {
                return ((JAXBElement < ? >) oResponse).getValue();
            }

        } catch (JAXBAnnotationException e) {
            throw new C2wsAdapterException(e);
        } catch (SOAPFaultException e) {
            throw new C2wsAdapterException(getFaultReasonText(e));
        }
    }

    /**
     * Try to extract something meaningful from a SOAP Fault.
     * @param e the SOAP Fault exception
     * @return a fault description
     */
    @SuppressWarnings("unchecked")
    public String getFaultReasonText(final SOAPFaultException e) {
        if (LOG.isDebugEnabled()) {
            SOAPFault fault = e.getFault();
            if (fault != null) {
                QName code = fault.getFaultCodeAsQName();
                String string = fault.getFaultString();
                String actor = fault.getFaultActor();
                LOG.debug("SOAP fault contains: ");
                LOG.debug("  Fault code = " + code.toString());
                LOG.debug("  Local name = " + code.getLocalPart());
                LOG.debug("  Namespace prefix = "
                        + code.getPrefix() + ", bound to "
                        + code.getNamespaceURI());
                LOG.debug("  Fault string = " + string);
                if (actor != null) {
                    LOG.debug("  Fault actor = " + actor);
                }
                Detail detail = fault.getDetail();
                if (detail != null) {
                    Iterator entries = detail.getDetailEntries();
                    while (entries.hasNext()) {
                        DetailEntry newEntry = (DetailEntry) entries.next();
                        String value = newEntry.getValue();
                        LOG.debug("  Detail entry = " + value);
                    }
                } 
            } else {
                LOG.debug(e);
            }
        }
        SOAPFault fault = e.getFault();
        if (fault != null) {
            String faultMessage = e.getFault().getFaultString();
            Detail detail = fault.getDetail();
            if (detail != null) {
                Iterator entries = detail.getDetailEntries();
                while (entries.hasNext()) {
                    DetailEntry newEntry = (DetailEntry) entries.next();
                    faultMessage += " [" + newEntry.getValue() + "]";
                }
            }
            return faultMessage;
        } else {
            return e.getMessage();
        }

    }

    /**
     * Gets or creates a JAX-WS dispatcher.
     * <p/>
     * If no dispatcher has been created yet, creates a Dispatch instance for
     * use with JAXB generated objects.
     * @return the instance of jaxws dynamic client invoker. 
     * @throws C2wsAdapterException if attempt to instantiate dispatcher
     *  fails
     */
    public Dispatch < Object > getDispatcher() throws C2wsAdapterException {
        try {
            if (mDispatcher == null) {
                QName serviceQname = new QName(
                        getWsdlTargetNamespace(), getWsdlServiceName());
                QName portQname = new QName(
                        getWsdlTargetNamespace(), getWsdlPortName());
                Service service = Service.create(serviceQname);
                service.addPort(portQname, SOAPBinding.SOAP11HTTP_BINDING,
                        getWsdlUrl());
                mDispatcher = service.createDispatch(
                        portQname, getJaxbContext(), Service.Mode.PAYLOAD);
                if (LOG.isDebugEnabled()) {
                    LOG.debug("New javax.xml.ws.Dispatch created for "
                            + getWsdlServiceName());
                }
            }
            return mDispatcher;
        } catch (JAXBException e) {
            throw new C2wsAdapterException(e);
        }
    }

    /**
     * This method is used to lazily create the JAXB context which is an
     * expensive operation.
     * @return the JAXB context
     * @throws JAXBException if the JAXB context cannot be created
     */
    public final JAXBContext getJaxbContext() throws JAXBException {
        if (mJaxbContext == null) {
            if (getResponseJaxbPackageName().compareTo(
                    getRequestJaxbPackageName()) == 0) {
                mJaxbContext = JAXBContext.newInstance(
                        getRequestObjectFactoryClass());
            } else {
                mJaxbContext = JAXBContext.newInstance(
                        getRequestObjectFactoryClass(),
                        getResponseObjectFactoryClass());
            }
        }
        return mJaxbContext;
    }

    /**
     * Root elements can be created with special methods in the JAXB object 
     * factory. This method will use reflection to create a JAXBElement.
     * @param objectFactory the JAXB object factory
     * @param elementName the XML element name
     * @param type an occurence of the element type
     * @return a JAXBElement
     * @throws C2wsAdapterException if the JAXBElement cannot be created
     */
    private JAXBElement < ? > getJAXBElement(
            final Object objectFactory,
            final String elementName,
            final Object type) throws C2wsAdapterException {
        try {
            String createName = "create" + NameUtil.toClassName(elementName);
            Method creator = objectFactory.getClass().getMethod(
                    createName, type.getClass());
            return (JAXBElement < ? >) creator.invoke(objectFactory, type);
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
            final Class < ? > requestClass,
            final byte[] hostBytes) throws HostUnmarshalException {
        try {
            if (mLog.isDebugEnabled()) {
                mLog.debug("Entered unmarshalReflect with "
                        + hostBytes.length + " bytes:");
                C2wsUtil.traceData(mLog.getCorrelationId(),
                        hostBytes, hostBytes.length);
            }

            CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(
                    hostBytes, 0, getCobolConverters());
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
                    maxHostBytes, 0, getCobolConverters());
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

    /**
     * @return the current set of converters
     */
    public ICobolConverters getCobolConverters() {
        return mCc;
    }

    /**
     * @return the WSDL service name
     */
    public final C2wsWSDescriptor getC2wsWSDescriptor() {
        return mC2wsWSDescriptor;
    }

    /**
     * @return the WSDL service name
     */
    public final String getWsdlServiceName() {
        return getC2wsWSDescriptor().getWsdlName();
    }

    /**
     * @return the WSDL port name
     */
    public final String getWsdlPortName() {
        return getC2wsWSDescriptor().getWsdlPort();
    }

    /**
     * @return the WSDL target namespace
     */
    public final String getWsdlTargetNamespace() {
        return getC2wsWSDescriptor().getWsdlTargetNamespace();
    }

    /**
     * @return the WSDL URL
     */
    public final String getWsdlUrl() {
        return getC2wsWSDescriptor().getWsdlUrl();
    }

    /**
     * @return the request element descriptor
     */
    public final JAXBElementDescriptor getRequestElementDescriptor() {
        return getC2wsWSDescriptor().getRequestElementDescriptor();
    }

    /**
     * @return the response element descriptor
     */
    public final JAXBElementDescriptor getResponseElementDescriptor() {
        return getC2wsWSDescriptor().getResponseElementDescriptor();
    }

    /**
     * @return the request JAXB package name
     */
    public final String getRequestJaxbPackageName() {
        return getRequestElementDescriptor().getJaxbPackageName();
    }

    /**
     * @return the response JAXB package name
     */
    public final String getResponseJaxbPackageName() {
        return getResponseElementDescriptor().getJaxbPackageName();
    }

    /**
     * @return the request JAXB object factory class
     */
    public final Class < ? > getRequestObjectFactoryClass() {
        return getRequestElementDescriptor().getObjectFactory().getClass();
    }

    /**
     * @return the response JAXB object factory class
     */
    public final Class < ? > getResponseObjectFactoryClass() {
        return getResponseElementDescriptor().getObjectFactory().getClass();
    }

    /**
     * @return the request object class
     */
    public Class < ? > getRequestClass() {
        return mRequestClass;
    }
}
