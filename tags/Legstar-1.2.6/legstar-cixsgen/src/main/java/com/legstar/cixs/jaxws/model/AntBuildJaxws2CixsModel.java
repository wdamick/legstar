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

import com.legstar.cixs.gen.model.options.WebServiceParameters;

/**
 * This is a model for Jaxws to Cixs component generation. The generated 
 * component runs under Jaxws and wraps a CICS transaction.
 *
 */
public class AntBuildJaxws2CixsModel extends AbstractAntBuildCixsJaxwsModel {

    /** This generator name. */
    public static final String JAXWS2CIXS_GENERATOR_NAME =
        "Jaxws adapter Web Service generator";

    /** Adapter is exposed as a Web Service using these parameters. */
    private WebServiceParameters mWebServiceParameters;

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
