package com.legstar.proxy.invoke.jaxws;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.namespace.QName;
import javax.xml.soap.Detail;
import javax.xml.soap.DetailEntry;
import javax.xml.soap.SOAPFault;
import javax.xml.ws.Dispatch;
import javax.xml.ws.Service;
import javax.xml.ws.soap.SOAPBinding;
import javax.xml.ws.soap.SOAPFaultException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.proxy.invoke.IProxyInvoker;
import com.legstar.proxy.invoke.ProxyInvokerException;
import com.legstar.proxy.invoke.ReflectOperationProxy;
import com.legstar.util.JAXBAnnotationException;
import com.legstar.util.JAXBElementDescriptor;
import com.legstar.util.NameUtil;

/**
 * This provides a direct Web Service invoker via {@link java.xml.ws.Dispatch}.
 * <p/>
 * The class is immutable. All parameters are passed at construction time.
 * Limitations:
 * <ul>
 *<li>SOAPBinding.SOAP11HTTP_BINDING there is no support for MTOM or SOAP 1.2</li>
 *<li>Service.Mode.PAYLOAD JAXB Objects are used to create the SOAP payload</li>
 *<li>No support for authentication against target Web Service</li>
 * </ul> 
 *
 */
public class WebServiceInvoker implements IProxyInvoker {

    /* ====================================================================== */
    /* = Constants section                                                  = */
    /* ====================================================================== */
    /** URL locating target Web service WSDL. */ 
    public static final String WSDL_URL_PROPERTY = "wsdlUrl";

    /** Target Web service WSDL namespace. */ 
    public static final String WSDL_TARGET_NAMESPACE_PROPERTY = "wsdlTargetNamespace";

    /** Target Web service WSDL service name. */ 
    public static final String WSDL_SERVICE_NAME_PROPERTY = "wsdlServiceName";

    /** Target Web service WSDL port name. */ 
    public static final String WSDL_PORT_NAME_PROPERTY = "wsdlPortName";

    /* The following set of parameters is the same as ReflectOperationProxy */

    /** Request JAXB type configuration parameter. */ 
    public static final String REQUEST_JAXB_TYPE_PROPERTY =
        ReflectOperationProxy.REQUEST_JAXB_TYPE_PROPERTY;

    /** Request JAXB package name configuration parameter. */ 
    public static final String REQUEST_JAXB_PACKAGE_NAME_PROPERTY =
        ReflectOperationProxy.REQUEST_JAXB_PACKAGE_NAME_PROPERTY;

    /** Response JAXB type. */ 
    public static final String RESPONSE_JAXB_TYPE_PROPERTY =
        ReflectOperationProxy.RESPONSE_JAXB_TYPE_PROPERTY;

    /** Response JAXB package name. */ 
    public static final String RESPONSE_JAXB_PACKAGE_NAME_PROPERTY =
        ReflectOperationProxy.RESPONSE_JAXB_PACKAGE_NAME_PROPERTY;

    /* ====================================================================== */
    /* = Properties section                                                 = */
    /* ====================================================================== */
    /** The WSDL URL. */
    private String mWsdlUrl;

    /** The WSDL target namespace. */
    private String mWsdlTargetNamespace;

    /** The WSDL service name. */
    private String mWsdlServiceName;

    /** The WSDL port name. */
    private String mWsdlPortName;

    /** The request descriptor. */
    private JAXBElementDescriptor mRequestElementDescriptor;

    /** The response descriptor. */
    private JAXBElementDescriptor mResponseElementDescriptor;

    /** This is a JAXB Context that can be used to marshal/unmarshal the
     * JAXB annotated classes that form the request and reply for this
     * web service. */
    private JAXBContext mJaxbContext;

    /** Instance of jaxws dynamic client invoker. This instance is lazily created once
     * and reused each time this action is processed. */
    private Dispatch < Object > mDispatcher;

    /**
     * Current set of configuration parameters.
     */
    private Map < String, String > mConfig;

    /** Logger. */
    private static final Log LOG = LogFactory.getLog(WebServiceInvoker.class);

    /**
     * Standard constructor. The configuration parameters supported are:
     * <ul>
     *  <li>wsdlUrl: Target web service wsdl URL</li>
     *  <li>wsdlTargetNamespace: Target web service WSDL namespace</li>
     *  <li>wsdlServiceName: Target web service name</li>
     *  <li>wsdlPortName: Target web service port name</li>
     *  <li>requestJaxbType: JAXB request type</li>
     *  <li>requestJaxbPackageName: JAXB request package name</li>
     *  <li>responseJaxbType: JAXB response type</li>
     *  <li>responseJaxbPackageName: JAXB response package name</li>
     * </ul>
     * @param config parameters setup in jboss-esb.xml
     * @throws WebServiceInvokerException if configuration is wrong 
     */
    public WebServiceInvoker(
            final Map < String, String > config) throws WebServiceInvokerException {
        mWsdlUrl = config.get(WSDL_URL_PROPERTY);
        if (mWsdlUrl == null || mWsdlUrl.length() == 0) {
            throw new WebServiceInvokerException("You must specify a wsdl URL using the "
                    + WSDL_URL_PROPERTY + " attribute");
        }
        mWsdlTargetNamespace = config.get(WSDL_TARGET_NAMESPACE_PROPERTY);
        if (mWsdlTargetNamespace == null || mWsdlTargetNamespace.length() == 0) {
            throw new WebServiceInvokerException("You must specify a wsdl target namespace using the "
                    + WSDL_TARGET_NAMESPACE_PROPERTY + " attribute");
        }
        mWsdlServiceName = config.get(WSDL_SERVICE_NAME_PROPERTY);
        if (mWsdlServiceName == null || mWsdlServiceName.length() == 0) {
            throw new WebServiceInvokerException("You must specify a service name using the "
                    + WSDL_SERVICE_NAME_PROPERTY + " attribute");
        }
        mWsdlPortName = config.get(WSDL_PORT_NAME_PROPERTY);
        if (mWsdlPortName == null || mWsdlPortName.length() == 0) {
            throw new WebServiceInvokerException("You must specify a port name using the "
                    + WSDL_PORT_NAME_PROPERTY + " attribute");
        }

        String requestJaxbType = config.get(REQUEST_JAXB_TYPE_PROPERTY);
        if (requestJaxbType == null || requestJaxbType.length() == 0) {
            throw new WebServiceInvokerException("You must specify a jaxb request type using the "
                    + REQUEST_JAXB_TYPE_PROPERTY + " attribute");
        }
        String requestJaxbPackageName = config.get(REQUEST_JAXB_PACKAGE_NAME_PROPERTY);
        if (requestJaxbPackageName == null || requestJaxbPackageName.length() == 0) {
            throw new WebServiceInvokerException("You must specify a jaxb request package name using the "
                    + REQUEST_JAXB_PACKAGE_NAME_PROPERTY + " attribute");
        }
        try {
            mRequestElementDescriptor = new JAXBElementDescriptor(requestJaxbPackageName, requestJaxbType);
        } catch (JAXBAnnotationException e) {
            throw new WebServiceInvokerException(e);
        }

        String responseJaxbType = config.get(RESPONSE_JAXB_TYPE_PROPERTY);
        if (responseJaxbType == null || responseJaxbType.length() == 0) {
            throw new WebServiceInvokerException("You must specify a jaxb response type using the "
                    + RESPONSE_JAXB_TYPE_PROPERTY + " attribute");
        }
        String responseJaxbPackageName = config.get(RESPONSE_JAXB_PACKAGE_NAME_PROPERTY);
        if (responseJaxbPackageName == null || responseJaxbPackageName.length() == 0) {
            throw new WebServiceInvokerException("You must specify a jaxb response package name using the "
                    + RESPONSE_JAXB_PACKAGE_NAME_PROPERTY + " attribute");
        }
        try {
            mResponseElementDescriptor = new JAXBElementDescriptor(responseJaxbPackageName, responseJaxbType);
        } catch (JAXBAnnotationException e) {
            throw new WebServiceInvokerException(e);
        }

        if (LOG.isDebugEnabled()) {
            LOG.debug("WebServiceInvokerAction setup configuration:");
            LOG.debug("Wsdl Url=" + getWsdlUrl());
            LOG.debug("Wsdl service name=" + getWsdlServiceName());
            LOG.debug("Wsdl target namespace=" + getWsdlTargetNamespace());
            LOG.debug("Wsdl port=" + getWsdlPortName());
            LOG.debug("Request element=[" + getRequestElementDescriptor().toString() + "]");
            LOG.debug("Response element=[" + getResponseElementDescriptor().toString() + "]");
        }
        mConfig = new HashMap < String, String >();
        mConfig.putAll(config);
    }

    /** {@inheritDoc} */
    @SuppressWarnings("unchecked")
    public < T > T invoke(final String requestID, final Object oRequest) throws ProxyInvokerException {
        if (LOG.isDebugEnabled()) {
            LOG.debug("About to call invokeDispatch for service="
                    + getWsdlServiceName() + " request ID=" + requestID);
        }
        Object replyObject = invokeDispatch(oRequest);
        if (LOG.isDebugEnabled()) {
            LOG.debug("Returned from invokeDispatch for service="
                    + getWsdlServiceName() + " request ID=" + requestID);
        }
        return (T) replyObject;
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
     * @throws WebServiceInvokerException if dispatch fails
     */
    private Object invokeDispatch(
            final Object oRequest) throws WebServiceInvokerException {

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
            throw new WebServiceInvokerException(e);
        } catch (SOAPFaultException e) {
            throw new WebServiceInvokerException(getFaultReasonText(e));
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
                        + code.getPrefix() + ", bound to " + code.getNamespaceURI());
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
            StringBuffer faultMessage = new StringBuffer(e.getFault().getFaultString());
            Detail detail = fault.getDetail();
            if (detail != null) {
                Iterator entries = detail.getDetailEntries();
                while (entries.hasNext()) {
                    DetailEntry newEntry = (DetailEntry) entries.next();
                    faultMessage.append(" [" + newEntry.getValue() + "]");
                }
            }
            return faultMessage.toString();
        } else {
            return e.getMessage();
        }

    }

    /**
     * Gets or creates a JAXWS dispatcher.
     * <p/>
     * If no dispatcher has been created yet, creates a Dispatch instance for use with JAXB
     * generated objects.
     * @return the instance of jaxws dynamic client invoker. 
     * @throws WebServiceInvokerException if attempt to instantiate dispatcher fails
     */
    public Dispatch < Object > getDispatcher() throws WebServiceInvokerException {
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
                LOG.debug("New javax.xml.ws.Dispatch created for " + getWsdlServiceName());
            }
        }
        return mDispatcher;
    }

    /**
     * This method is used to lazily create the JAXB context which is an
     * expensive operation.
     * @return the JAXB context
     * @throws WebServiceInvokerException if the JAXB context cannot be created
     */
    public final JAXBContext getJaxbContext() throws WebServiceInvokerException {
        try {
            if (mJaxbContext == null) {
                if (getResponseElementDescriptor().getJaxbPackageName().compareTo(
                        getRequestElementDescriptor().getJaxbPackageName()) == 0) {
                    mJaxbContext = JAXBContext.newInstance(
                            getRequestElementDescriptor().getObjectFactory().getClass());
                } else {
                    mJaxbContext = JAXBContext.newInstance(
                            getRequestElementDescriptor().getObjectFactory().getClass(),
                            getResponseElementDescriptor().getObjectFactory().getClass());
                }
            }
            return mJaxbContext;
        } catch (JAXBException e) {
            throw new WebServiceInvokerException(e);
        }
    }

    /**
     * Root elements can be created with special methods in the JAXB object 
     * factory. This method will use reflection to create a JAXBElement.
     * @param objectFactory the JAXB object factory
     * @param elementName the XML element name
     * @param type an occurrence of the element type
     * @return a JAXBElement
     * @throws WebServiceInvokerException if the JAXBElement cannot be created
     */
    private JAXBElement < ? > getJAXBElement(
            final Object objectFactory,
            final String elementName,
            final Object type) throws WebServiceInvokerException {
        try {
            String createName = "create" + NameUtil.toClassName(elementName);
            Method creator = objectFactory.getClass().getMethod(
                    createName, type.getClass());
            return (JAXBElement < ? >) creator.invoke(objectFactory, type);
        } catch (IllegalAccessException e) {
            throw new WebServiceInvokerException(e);
        } catch (SecurityException e) {
            throw new WebServiceInvokerException(e);
        } catch (NoSuchMethodException e) {
            throw new WebServiceInvokerException(e);
        } catch (IllegalArgumentException e) {
            throw new WebServiceInvokerException(e);
        } catch (InvocationTargetException e) {
            throw new WebServiceInvokerException(e);
        }
    }

    /**
     * @return the request element descriptor
     */
    public final JAXBElementDescriptor getRequestElementDescriptor() {
        return mRequestElementDescriptor;
    }

    /**
     * @return the response element descriptor
     */
    public final JAXBElementDescriptor getResponseElementDescriptor() {
        return mResponseElementDescriptor;
    }

    /**
     * @return the WSDL service name
     */
    public final String getWsdlServiceName() {
        return mWsdlServiceName;
    }

    /**
     * @return the WSDL port name
     */
    public final String getWsdlPortName() {
        return mWsdlPortName;
    }

    /**
     * @return the WSDL target namespace
     */
    public final String getWsdlTargetNamespace() {
        return mWsdlTargetNamespace;
    }

    /**
     * @return the WSDL URL
     */
    public final String getWsdlUrl() {
        return mWsdlUrl;
    }

    /** {@inheritDoc} */
    public Map < String, String > getConfig() {
        return mConfig;
    }

    /**
     * These are the conditions applied:
     * <ul>
     * <li>If no configuration exists yet this will return false.</li>
     * <li>If the new configuration is null, returns false.</li>
     * <li>All keys from current configuration must yield the same value from
     * both existing configuration and new one for this to return true.</li>
     * <li>Both old and new config must have the same number of keys for
     * this to return true.</li>
     * </ul>
     * @param newConfig a proposed new configuration
     * @return if new configuration is identical to the one already used
     */
    public boolean isSameConfig(final Map < String, String > newConfig) {
        if (getConfig() == null || newConfig == null) {
            return false;
        }
        for (String key : getConfig().keySet()) {
            if (!getConfig().get(key).equals(newConfig.get(key))) {
                return false;
            }
        }
        if (getConfig().keySet().size() != newConfig.keySet().size()) {
            return false;
        }
        return true;
    }

}
