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
package com.legstar.cixs.jaxws.model;

import java.io.File;

import com.legstar.cixs.gen.ant.model.AbstractAntBuildCixsModel;

/**
 * This is a model for Jaxws to Cixs component generation. The generated 
 * component runs under Jaxws and wraps a CICS transaction.
 *
 */
public class AntBuildJaxws2CixsModel extends AbstractAntBuildCixsModel {

    /** This generator name. */
    public static final String JAXWS2CIXS_GENERATOR_NAME =
        "Jaxws adapter Web Service generator";

    /** Adapter is exposed as a Web Service using these parameters. */
    private WebServiceParameters mWebServiceParameters;

    /** Target location for web deployment descriptors. */
    private File mTargetWDDDir;

    /** The deployment location for jaxws war files. */
    private File mTargetWarDir;

    /** This velocity template that creates an ant build which in turn
     * generates the target web service. */
    public static final String JAXWS2CIXS_VELOCITY_MACRO_NAME =
        "vlc/build-jws2cixs-xml.vm";

    /**
     * Construct the model.
     */
    public AntBuildJaxws2CixsModel() {
        super(JAXWS2CIXS_GENERATOR_NAME, JAXWS2CIXS_VELOCITY_MACRO_NAME);
        mWebServiceParameters = new WebServiceParameters();
    }

    /**
     * @return the Jaxws web service being generated
     */
    public final CixsJaxwsService getCixsJaxwsService() {
        return (CixsJaxwsService) getCixsService();
    }

    /**
     * @param cixsJaxwsService the Jaxws web service being generated
     *  to set
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
     * @return the set of parameters needed to expose a Web Service
     */
    public WebServiceParameters getWebServiceParameters() {
        return mWebServiceParameters;
    }

    /**
     * @param webServiceParameters the set of parameters needed to expose a Web Service to set
     */
    public void setWebServiceParameters(
            final WebServiceParameters webServiceParameters) {
        mWebServiceParameters = webServiceParameters;
    }
}
