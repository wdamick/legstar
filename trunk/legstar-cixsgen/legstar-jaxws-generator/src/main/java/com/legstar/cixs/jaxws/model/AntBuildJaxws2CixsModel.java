/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.cixs.jaxws.model;

import java.util.Properties;

import com.legstar.cixs.gen.model.options.WebServiceParameters;

/**
 * This is a model for Jaxws to Cixs component generation. The generated
 * component runs under Jaxws and wraps a CICS transaction.
 * 
 */
public class AntBuildJaxws2CixsModel extends AbstractAntBuildCixsJaxwsModel {

    /** This generator name. */
    public static final String JAXWS2CIXS_GENERATOR_NAME = "Jaxws adapter Web Service generator";

    /**
     * This velocity template that creates an ant build which in turn generates
     * the target web service.
     */
    public static final String JAXWS2CIXS_VELOCITY_MACRO_NAME = "vlc/build-jws2cixs-xml.vm";

    /* ====================================================================== */
    /* Following are default field values. = */
    /* ====================================================================== */

    /** Default value for no package-info generation. */
    public static final boolean DEFAULT_NOPACKAGEINFO = false;

    /* ====================================================================== */
    /* Following are key identifiers for this model persistence. = */
    /* ====================================================================== */

    /** Don't generate package-info.java. */
    public static final String JAXWS_NOPACKAGEINFO = "noPackageInfo";

    /* ====================================================================== */
    /* Following are this class fields that are persistent. = */
    /* ====================================================================== */

    /** Adapter is exposed as a Web Service using these parameters. */
    private WebServiceParameters _webServiceParameters;

    /** Whether we should not generate package-info.java. */
    private boolean _noPackageInfo = DEFAULT_NOPACKAGEINFO;

    /**
     * Construct the model.
     */
    public AntBuildJaxws2CixsModel() {
        super(JAXWS2CIXS_GENERATOR_NAME, JAXWS2CIXS_VELOCITY_MACRO_NAME);
        _webServiceParameters = new WebServiceParameters();
    }

    /**
     * Construct from a properties file.
     * 
     * @param props the property file
     */
    public AntBuildJaxws2CixsModel(final Properties props) {
        super(JAXWS2CIXS_GENERATOR_NAME, JAXWS2CIXS_VELOCITY_MACRO_NAME, props);
        _webServiceParameters = new WebServiceParameters(props);
        setNoPackageInfo(getBoolean(props, JAXWS_NOPACKAGEINFO,
                DEFAULT_NOPACKAGEINFO));
    }

    /**
     * @return the set of parameters needed to expose a Web Service
     */
    public WebServiceParameters getWebServiceParameters() {
        return _webServiceParameters;
    }

    /**
     * @param webServiceParameters the set of parameters needed to expose a Web
     *            Service to set
     */
    public void setWebServiceParameters(
            final WebServiceParameters webServiceParameters) {
        _webServiceParameters = webServiceParameters;
    }

    /**
     * Prevents generation of package-info.java which does not compile under JDK
     * 1.5.
     * 
     * @return true if we should not generate package-info.java
     */
    public boolean isNoPackageInfo() {
        return _noPackageInfo;
    }

    /**
     * Prevents generation of package-info.java which does not compile under JDK
     * 1.5.
     * 
     * @param noPackageInfo true if we should not generate package-info.java
     */
    public void setNoPackageInfo(final boolean noPackageInfo) {
        this._noPackageInfo = noPackageInfo;
    }

    /**
     * @return a properties file holding the values of this object fields
     */
    public Properties toProperties() {
        Properties props = super.toProperties();
        props.putAll(getWebServiceParameters().toProperties());
        putBoolean(props, JAXWS_NOPACKAGEINFO, isNoPackageInfo());
        return props;
    }
}
