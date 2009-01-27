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

import com.legstar.cixs.gen.ant.model.AbstractAntBuildCixsModel;

/**
 * This model groups parameters needed to generate artifacts for a proxy
 * mainframe operation. One such artifacts is a CICS COBOL source that showcases
 * how a remote service can be called from CICS.
 */
public class AntBuildCixs2JaxwsModel extends AbstractAntBuildCixsModel {

    /** This generator name. */
    public static final String CIXS2JAXWS_GENERATOR_NAME =
        "Web Service proxy for Mainframe generator";

    /** The type of target that the generated proxy service will invoke. */
    private ProxyTargetType mProxyTargetType = ProxyTargetType.WEBSERVICE;

    /** Set of parameters needed to invoke a POJO. */
    private PojoParameters mPojoTargetParameters;

    /** Set of parameters needed to invoke a Web Service. */
    private WebServiceParameters mWebServiceTargetParameters;

    /** Target location for web deployment descriptors. */
    private File mTargetWDDDir;

    /** The deployment location for jaxws war files. */
    private File mTargetWarDir;

    /** The target directory where COBOL files will be created. */
    private File mTargetCobolDir;

    /** The type of HTTP sample Cobol client to generate. */
    private CobolHttpClientType mSampleCobolHttpClientType = CobolHttpClientType.DFHWBCLI;
    
    /** HTTP parameters used by cobol client to reach the proxy over HTTP.     */
    private HttpTransportParameters mHttpTransportParameters;
    
    /** This velocity template that creates an ant build which in turn
     * generates the target web service proxy. */
    public static final String CIXS2JAXWS_VELOCITY_MACRO_NAME =
        "vlc/build-cixs2jws-xml.vm";

    /**
     * Construct an empty model.
     */
    public AntBuildCixs2JaxwsModel() {
        super(CIXS2JAXWS_GENERATOR_NAME, CIXS2JAXWS_VELOCITY_MACRO_NAME);
        mPojoTargetParameters = new PojoParameters();
        mWebServiceTargetParameters = new WebServiceParameters();
        mHttpTransportParameters = new HttpTransportParameters();
    }

    /**
     * @return the web service mapping description
     */
    public final CixsJaxwsService getCixsJaxwsService() {
        return (CixsJaxwsService) getCixsService();
    }

    /**
     * @param cixsJaxwsService the web service mapping description to set
     */
    public final void setCixsJaxwsService(
            final CixsJaxwsService cixsJaxwsService) {
        setCixsService(cixsJaxwsService);
    }

    /**
     * @return the Target location for web deployment descriptors
     */
    public final File getTargetWDDDir() {
        return mTargetWDDDir;
    }

    /**
     * @param targetWDDDir the Target location for web deployment descriptors to
     *  set
     */
    public final void setTargetWDDDir(final File targetWDDDir) {
        mTargetWDDDir = targetWDDDir;
    }

    /**
     * @return the deployment location for jaxws war files
     */
    public final File getTargetWarDir() {
        return mTargetWarDir;
    }

    /**
     * @param targetWarDir the deployment location for jaxws war files to set
     */
    public final void setTargetWarDir(final File targetWarDir) {
        mTargetWarDir = targetWarDir;
    }

    /**
     * @return the directory where COBOL files will be created
     */
    public final File getTargetCobolDir() {
        return mTargetCobolDir;
    }

    /**
     * @param targetCobolDir the directory where COBOL files will be created to
     *  set
     */
    public final void setTargetCobolDir(final File targetCobolDir) {
        mTargetCobolDir = targetCobolDir;
    }

    /**
     * @return The type of Http sample Cobol client to generate
     */
    public CobolHttpClientType getSampleCobolHttpClientType() {
        return mSampleCobolHttpClientType;
    }

    /**
     * @param sampleCobolHttpClientType The type of Http sample Cobol client to generate
     */
    public void setSampleCobolHttpClientType(
            final CobolHttpClientType sampleCobolHttpClientType) {
        mSampleCobolHttpClientType = sampleCobolHttpClientType;
    }

    /**
     * @return the type of target that the generated proxy service will invoke
     */
    public ProxyTargetType getProxyTargetType() {
        return mProxyTargetType;
    }

    /**
     * @param proxyTargetType the type of target that the generated proxy service will invoke
     */
    public void setProxyTargetType(final ProxyTargetType proxyTargetType) {
        mProxyTargetType = proxyTargetType;
    }

    /**
     * @return the set of parameters needed to invoke a POJO
     */
    public PojoParameters getPojoTargetParameters() {
        return mPojoTargetParameters;
    }

    /**
     * @param pojoTargetParameters the set of parameters needed to invoke a POJO to set
     */
    public void setPojoTargetParameters(
            final PojoParameters pojoTargetParameters) {
        mPojoTargetParameters = pojoTargetParameters;
    }

    /**
     * @return the set of parameters needed to invoke a Web Service
     */
    public WebServiceParameters getWebServiceTargetParameters() {
        return mWebServiceTargetParameters;
    }

    /**
     * @param webServiceTargetParameters the set of parameters needed to invoke a Web Service to set
     */
    public void setWebServiceTargetParameters(
            final WebServiceParameters webServiceTargetParameters) {
        mWebServiceTargetParameters = webServiceTargetParameters;
    }

    /**
     * @return the parameters used by cobol client to reach the proxy over HTTP
     */
    public HttpTransportParameters getHttpTransportParameters() {
        return mHttpTransportParameters;
    }

    /**
     * @param httpTransportParameters the parameters used by cobol client to reach the proxy over HTTP to set
     */
    public void setHttpTransportParameters(
            final HttpTransportParameters httpTransportParameters) {
        mHttpTransportParameters = httpTransportParameters;
    }

}
