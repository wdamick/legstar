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
import java.util.Properties;

import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.models.AbstractPropertiesModel;

/**
 * Set of parameters describing a Web Service target.
 */
public class WebServiceParameters extends AbstractPropertiesModel {

    /* ====================================================================== */
    /* Following are key identifiers for this model persistence. = */
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
    /* Following are this class fields that are persistent. = */
    /* ====================================================================== */
    /** The WSDL URL. */
    private String _wsdlUrl;

    /** The WSDL target namespace. */
    private String _wsdlTargetNamespace;

    /** The WSDL service name. */
    private String _wsdlServiceName;

    /** The WSDL port name. */
    private String _wsdlPortName;

    /**
     * A no-Arg constructor.
     */
    public WebServiceParameters() {
        super();
    }

    /**
     * Construct from a properties file.
     * 
     * @param props the property file
     */
    public WebServiceParameters(final Properties props) {
        super(props);
        setWsdlServiceName(getString(props, WSDL_SERVICE_NAME_PROPERTY, null));
        setWsdlPortName(getString(props, WSDL_PORT_NAME_PROPERTY, null));
        setWsdlTargetNamespace(getString(props, WSDL_TARGET_NAMESPACE_PROPERTY,
                null));
        setWsdlUrl(getString(props, WSDL_URL_PROPERTY, null));
    }

    /**
     * Web Service parameters are expected by templates to come from a
     * parameters map.
     * 
     * @param parameters a parameters map to which Web Service parameters must
     *            be added
     */
    public void add(final Map < String, Object > parameters) {
        parameters.put(WSDL_URL_PROPERTY, getWsdlUrl());
        parameters
                .put(WSDL_TARGET_NAMESPACE_PROPERTY, getWsdlTargetNamespace());
        parameters.put(WSDL_SERVICE_NAME_PROPERTY, getWsdlServiceName());
        parameters.put(WSDL_PORT_NAME_PROPERTY, getWsdlPortName());
    }

    /**
     * When target is an ESB service, check that corresponding parameters are
     * set correctly.
     * 
     * @throws CodeGenMakeException if parameters are missing or wrong
     */
    public void check() throws CodeGenMakeException {
        if (getWsdlUrl() == null || getWsdlUrl().length() == 0) {
            throw new CodeGenMakeException(
                    "Missing target Web service WSDL URL");
        }
        if (getWsdlTargetNamespace() == null
                || getWsdlTargetNamespace().length() == 0) {
            throw new CodeGenMakeException(
                    "Missing target Web service namespace");
        }
        if (getWsdlServiceName() == null || getWsdlServiceName().length() == 0) {
            throw new CodeGenMakeException("Missing target Web service name");
        }
        if (getWsdlPortName() == null || getWsdlPortName().length() == 0) {
            throw new CodeGenMakeException(
                    "Missing target Web service port name");
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
        return _wsdlServiceName;
    }

    /**
     * @param wsdlServiceName the WSDL service name to set
     */
    public void setWsdlServiceName(final String wsdlServiceName) {
        _wsdlServiceName = wsdlServiceName;
    }

    /**
     * @return the WSDL port name
     */
    public String getWsdlPortName() {
        return _wsdlPortName;
    }

    /**
     * @param wsdlPortName the WSDL port name to set
     */
    public void setWsdlPortName(final String wsdlPortName) {
        _wsdlPortName = wsdlPortName;
    }

    /**
     * @return the WSDL target namespace
     */
    public String getWsdlTargetNamespace() {
        return _wsdlTargetNamespace;
    }

    /**
     * @param wsdlTargetNamespace the WSDL target namespace to set
     */
    public void setWsdlTargetNamespace(final String wsdlTargetNamespace) {
        _wsdlTargetNamespace = wsdlTargetNamespace;
    }

    /**
     * @return the WSDL URL
     */
    public String getWsdlUrl() {
        return _wsdlUrl;
    }

    /**
     * @param wsdlUrl the WSDL URL to set
     */
    public void setWsdlUrl(final String wsdlUrl) {
        _wsdlUrl = wsdlUrl;
    }

    /**
     * @return a properties file holding the values of this object fields
     */
    public Properties toProperties() {
        Properties props = super.toProperties();
        putString(props, WSDL_SERVICE_NAME_PROPERTY, getWsdlServiceName());
        putString(props, WSDL_PORT_NAME_PROPERTY, getWsdlPortName());
        putString(props, WSDL_TARGET_NAMESPACE_PROPERTY,
                getWsdlTargetNamespace());
        putString(props, WSDL_URL_PROPERTY, getWsdlUrl());
        return props;
    }
}
