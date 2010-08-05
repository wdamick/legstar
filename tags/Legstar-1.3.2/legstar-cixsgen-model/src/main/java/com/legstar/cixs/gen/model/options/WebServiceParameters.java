/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.cixs.gen.model.options;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Map;

import com.legstar.codegen.CodeGenMakeException;

/**
 * Set of parameters describing a Web Service target.
 */
public class WebServiceParameters {

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
    
    /**
     * Web Service parameters are expected by templates to come from a parameters map.
     * @param parameters a parameters map to which Web Service parameters must be added
     */
    public void add(final Map < String, Object > parameters) {
        parameters.put(WSDL_URL_PROPERTY, getWsdlUrl());
        parameters.put(WSDL_TARGET_NAMESPACE_PROPERTY, getWsdlTargetNamespace());
        parameters.put(WSDL_SERVICE_NAME_PROPERTY, getWsdlServiceName());
        parameters.put(WSDL_PORT_NAME_PROPERTY, getWsdlPortName());
    }

    /**
     * When target is an ESB service, check that corresponding parameters are set correctly.
     * @throws CodeGenMakeException if parameters are missing or wrong
     */
    public void check() throws CodeGenMakeException {
        if (getWsdlUrl() == null || getWsdlUrl().length() == 0) {
            throw new CodeGenMakeException("Missing target Web service WSDL URL");
        }
        if (getWsdlTargetNamespace() == null || getWsdlTargetNamespace().length() == 0) {
            throw new CodeGenMakeException("Missing target Web service namespace");
        }
        if (getWsdlServiceName() == null || getWsdlServiceName().length() == 0) {
            throw new CodeGenMakeException("Missing target Web service name");
        }
        if (getWsdlPortName() == null || getWsdlPortName().length() == 0) {
            throw new CodeGenMakeException("Missing target Web service port name");
        }
        try {
            new URI(getWsdlUrl());
        } catch (URISyntaxException e) {
            throw new CodeGenMakeException(e);
        }
    }

    /**
     * @return the WSDL service name
     */
    public String getWsdlServiceName() {
        return mWsdlServiceName;
    }

    /**
     * @param wsdlServiceName the WSDL service name to set
     */
    public void setWsdlServiceName(final String wsdlServiceName) {
        mWsdlServiceName = wsdlServiceName;
    }

    /**
     * @return the WSDL port name
     */
    public String getWsdlPortName() {
        return mWsdlPortName;
    }

    /**
     * @param wsdlPortName the WSDL port name to set
     */
    public void setWsdlPortName(final String wsdlPortName) {
        mWsdlPortName = wsdlPortName;
    }

    /**
     * @return the WSDL target namespace
     */
    public String getWsdlTargetNamespace() {
        return mWsdlTargetNamespace;
    }

    /**
     * @param wsdlTargetNamespace the WSDL target namespace to set
     */
    public void setWsdlTargetNamespace(final String wsdlTargetNamespace) {
        mWsdlTargetNamespace = wsdlTargetNamespace;
    }

    /**
     * @return the WSDL URL
     */
    public String getWsdlUrl() {
        return mWsdlUrl;
    }

    /**
     * @param wsdlUrl the WSDL URL to set
     */
    public void setWsdlUrl(final String wsdlUrl) {
        mWsdlUrl = wsdlUrl;
    }

}
