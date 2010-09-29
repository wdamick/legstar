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
package com.legstar.cixs.jaxws.model;

import java.io.File;
import java.util.Properties;

import com.legstar.cixs.gen.model.options.CobolHttpClientType;
import com.legstar.cixs.gen.model.options.HttpTransportParameters;
import com.legstar.cixs.gen.model.options.PojoParameters;
import com.legstar.cixs.gen.model.options.ProxyTargetType;
import com.legstar.cixs.gen.model.options.WebServiceParameters;

/**
 * This model groups parameters needed to generate artifacts for a proxy
 * mainframe operation. One such artifacts is a CICS COBOL source that showcases
 * how a remote service can be called from CICS.
 */
public class AntBuildCixs2JaxwsModel extends AbstractAntBuildCixsJaxwsModel {

    /** This generator name. */
    public static final String CIXS2JAXWS_GENERATOR_NAME =
            "Web Service proxy for Mainframe generator";

    /**
     * This velocity template that creates an ant build which in turn
     * generates the target web service proxy.
     */
    public static final String CIXS2JAXWS_VELOCITY_MACRO_NAME =
            "vlc/build-cixs2jws-xml.vm";

    /** The default port number on which the HTTP server listens. */
    public static final int DEFAULT_HTTP_PORT = 8080;

    /** The default proxy target type. */
    public static final ProxyTargetType DEFAULT_PROXY_TARGET_TYPE = ProxyTargetType.WEBSERVICE;

    /** The default COBOL sample HTTP client type. */
    public static final CobolHttpClientType DEFAULT_COBOL_HTTP_CLIENT_TYPE = CobolHttpClientType.DFHWBCLI;

    /* ====================================================================== */
    /* Following are key identifiers for this model persistence. = */
    /* ====================================================================== */
    /** Target source directory. */
    public static final String PROXY_TARGET_TYPE = "proxyTargetType";

    /** Target source directory. */
    public static final String TARGET_COBOL_DIR = "targetCobolDir";

    /** Target source directory. */
    public static final String SAMPLE_COBOL_HTTP_CLIENT_TYPE = "sampleCobolHttpClientType";

    /* ====================================================================== */
    /* Following are this class fields that are persistent. = */
    /* ====================================================================== */

    /** The type of target that the generated proxy service will invoke. */
    private ProxyTargetType _proxyTargetType = DEFAULT_PROXY_TARGET_TYPE;

    /** Set of parameters needed to invoke a POJO. */
    private PojoParameters _pojoTargetParameters;

    /** Set of parameters needed to invoke a Web Service. */
    private WebServiceParameters _webServiceTargetParameters;

    /** The target directory where COBOL files will be created. */
    private File _targetCobolDir;

    /** The type of HTTP sample Cobol client to generate. */
    private CobolHttpClientType _sampleCobolHttpClientType = DEFAULT_COBOL_HTTP_CLIENT_TYPE;

    /** HTTP parameters used by cobol client to reach the proxy over HTTP. */
    private HttpTransportParameters _httpTransportParameters;

    /*
     * Following are this class fields that are persistent.
     */

    /**
     * Construct an empty model.
     */
    public AntBuildCixs2JaxwsModel() {
        super(CIXS2JAXWS_GENERATOR_NAME, CIXS2JAXWS_VELOCITY_MACRO_NAME);
        _pojoTargetParameters = new PojoParameters();
        _webServiceTargetParameters = new WebServiceParameters();
        _httpTransportParameters = new HttpTransportParameters(
                DEFAULT_HTTP_PORT);
    }

    /**
     * Construct from a properties file.
     * 
     * @param props the property file
     */
    public AntBuildCixs2JaxwsModel(final Properties props) {
        super(CIXS2JAXWS_GENERATOR_NAME, CIXS2JAXWS_VELOCITY_MACRO_NAME, props);
        setProxyTargetType(getString(props, PROXY_TARGET_TYPE,
                DEFAULT_PROXY_TARGET_TYPE.toString()));
        setTargetCobolDir(getFile(props, TARGET_COBOL_DIR, null));
        setSampleCobolHttpClientType(getString(props,
                SAMPLE_COBOL_HTTP_CLIENT_TYPE, DEFAULT_COBOL_HTTP_CLIENT_TYPE
                        .toString()));
        _pojoTargetParameters = new PojoParameters(props);
        _webServiceTargetParameters = new WebServiceParameters(props);
        _httpTransportParameters = new HttpTransportParameters(props,
                DEFAULT_HTTP_PORT);
    }

    /**
     * @return the directory where COBOL files will be created
     */
    public File getTargetCobolDir() {
        return _targetCobolDir;
    }

    /**
     * @param targetCobolDir the directory where COBOL files will be created to
     *            set
     */
    public void setTargetCobolDir(final File targetCobolDir) {
        _targetCobolDir = targetCobolDir;
    }

    /**
     * @return The type of Http sample Cobol client to generate
     */
    public CobolHttpClientType getSampleCobolHttpClientType() {
        return _sampleCobolHttpClientType;
    }

    /**
     * @param sampleCobolHttpClientType The type of Http sample Cobol client to
     *            generate
     */
    public void setSampleCobolHttpClientType(
            final CobolHttpClientType sampleCobolHttpClientType) {
        _sampleCobolHttpClientType = sampleCobolHttpClientType;
    }

    /**
     * @param sampleCobolHttpClientType The type of Http sample Cobol client to
     *            generate
     */
    public void setSampleCobolHttpClientType(
            final String sampleCobolHttpClientType) {
        _sampleCobolHttpClientType = CobolHttpClientType
                .valueOf(sampleCobolHttpClientType);
    }

    /**
     * @return the type of target that the generated proxy service will invoke
     */
    public ProxyTargetType getProxyTargetType() {
        return _proxyTargetType;
    }

    /**
     * @param proxyTargetType the type of target that the generated proxy
     *            service will invoke
     */
    public void setProxyTargetType(final ProxyTargetType proxyTargetType) {
        _proxyTargetType = proxyTargetType;
    }

    /**
     * @param proxyTargetType the type of target that the generated proxy
     *            service will invoke
     */
    public void setProxyTargetType(final String proxyTargetType) {
        _proxyTargetType = ProxyTargetType.valueOf(proxyTargetType);
    }

    /**
     * @return the set of parameters needed to invoke a POJO
     */
    public PojoParameters getPojoTargetParameters() {
        return _pojoTargetParameters;
    }

    /**
     * @param pojoTargetParameters the set of parameters needed to invoke a POJO
     *            to set
     */
    public void setPojoTargetParameters(
            final PojoParameters pojoTargetParameters) {
        _pojoTargetParameters = pojoTargetParameters;
    }

    /**
     * @return the set of parameters needed to invoke a Web Service
     */
    public WebServiceParameters getWebServiceTargetParameters() {
        return _webServiceTargetParameters;
    }

    /**
     * @param webServiceTargetParameters the set of parameters needed to invoke
     *            a Web Service to set
     */
    public void setWebServiceTargetParameters(
            final WebServiceParameters webServiceTargetParameters) {
        _webServiceTargetParameters = webServiceTargetParameters;
    }

    /**
     * @return the parameters used by cobol client to reach the proxy over HTTP
     */
    public HttpTransportParameters getHttpTransportParameters() {
        return _httpTransportParameters;
    }

    /**
     * @param httpTransportParameters the parameters used by cobol client to
     *            reach the proxy over HTTP to set
     */
    public void setHttpTransportParameters(
            final HttpTransportParameters httpTransportParameters) {
        _httpTransportParameters = httpTransportParameters;
    }

    /**
     * @return a properties file holding the values of this object fields
     */
    public Properties toProperties() {
        Properties props = super.toProperties();
        putString(props, PROXY_TARGET_TYPE, getProxyTargetType().toString());
        putFile(props, TARGET_COBOL_DIR, getTargetCobolDir());
        putString(props, SAMPLE_COBOL_HTTP_CLIENT_TYPE,
                getSampleCobolHttpClientType().toString());
        props.putAll(getWebServiceTargetParameters().toProperties());
        props.putAll(getPojoTargetParameters().toProperties());
        props.putAll(getHttpTransportParameters().toProperties());
        return props;
    }
}
