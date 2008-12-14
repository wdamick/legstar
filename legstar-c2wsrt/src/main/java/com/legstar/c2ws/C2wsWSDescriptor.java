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
package com.legstar.c2ws;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import org.apache.commons.configuration.Configuration;

import com.legstar.util.JAXBAnnotationException;
import com.legstar.util.JAXBElementDescriptor;

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

    /** The request descriptor. */
    private JAXBElementDescriptor mRequestElementDescriptor;

    /** The response descriptor. */
    private JAXBElementDescriptor mResponseElementDescriptor;

    /** This is a JAXB Context that can be used to marshal/unmarshal the
     * JAXB annotated classes that form the request and reply for this
     * web service. */
    private JAXBContext mJaxbContext;

    /** This indicates if the JAXBContext was set by the caller or if we are
     * responsible for automatically creating one when requested (which is
     * the default. */
    private boolean mJaxbContextAutoGeneration = true;

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

    /** Path to jaxb response package name in configuration file. */
    private static final String JAXB_RESPONSE_PKG_KEY =
        "jaxb.response.packageName";

    /** Path to jaxb response type name in configuration file. */
    private static final String JAXB_RESPONSE_TYPE_KEY =
        "jaxb.response.typeName";

    /** No-Argument constructor. */
    public C2wsWSDescriptor() {

    }

    /**
     * Create a descriptor from a configuration.
     * @param wsConfig a configuration fragment describing this service
     * @throws C2wsConfigurationException if configuration is invalid
     */
    public C2wsWSDescriptor(
            final Configuration wsConfig) throws C2wsConfigurationException {
        try {
            mWsdlTargetNamespace =
                (String) wsConfig.getString(WSDL_NS_KEY);
            mWsdlName =
                (String) wsConfig.getString(WSDL_SERVICE_KEY);
            mWsdlPort =
                (String) wsConfig.getString(WSDL_PORT_KEY);
            mWsdlUrl =
                (String) wsConfig.getString(WSDL_URL_KEY);
            mRequestElementDescriptor = new JAXBElementDescriptor(
                    (String) wsConfig.getString(JAXB_REQUEST_PKG_KEY),
                    (String) wsConfig.getString(JAXB_REQUEST_TYPE_KEY));
            mResponseElementDescriptor = new JAXBElementDescriptor(
                    (String) wsConfig.getString(JAXB_RESPONSE_PKG_KEY),
                    (String) wsConfig.getString(JAXB_RESPONSE_TYPE_KEY));
        } catch (JAXBAnnotationException e) {
            throw new C2wsConfigurationException(e);
        }
    }

    /**
     * @return the request element descriptor
     */
    public final JAXBElementDescriptor getRequestElementDescriptor() {
        return mRequestElementDescriptor;
    }

    /**
     * @param requestElementDescriptor the request element descriptor to set
     */
    public final void setRequestElementDescriptor(
            final JAXBElementDescriptor requestElementDescriptor) {
        /* If we are responsible for the JAXBContext, make sure it gets
         * regenerated since caller is changing the request here. */
        if (mJaxbContextAutoGeneration) {
            mJaxbContext = null;
        }
        mRequestElementDescriptor = requestElementDescriptor;
    }

    /**
     * @return the response element descriptor
     */
    public final JAXBElementDescriptor getResponseElementDescriptor() {
        return mResponseElementDescriptor;
    }

    /**
     * @param responseElementDescriptor the response element descriptor to set
     */
    public final void setResponseElementDescriptor(
            final JAXBElementDescriptor responseElementDescriptor) {
        /* If we are responsible for the JAXBContext, make sure it gets
         * regenerated since caller is changing the response here. */
        if (mJaxbContextAutoGeneration) {
            mJaxbContext = null;
        }
        mResponseElementDescriptor = responseElementDescriptor;
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
        sb.append("Request element=[");
        sb.append(mRequestElementDescriptor.toString());
        sb.append("], ");
        sb.append("Response element=[");
        sb.append(mResponseElementDescriptor.toString());
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
                if (mResponseElementDescriptor.getJaxbPackageName().compareTo(
                        mRequestElementDescriptor.getJaxbPackageName()) == 0) {
                    mJaxbContext = JAXBContext.newInstance(
                            mRequestElementDescriptor.getObjectFactory()
                            .getClass());
                } else {
                    mJaxbContext = JAXBContext.newInstance(
                            mRequestElementDescriptor.getObjectFactory()
                            .getClass(),
                            mResponseElementDescriptor.getObjectFactory()
                            .getClass());
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

}
