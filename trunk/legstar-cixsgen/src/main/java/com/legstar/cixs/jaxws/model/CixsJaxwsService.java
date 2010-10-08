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

import com.legstar.cixs.gen.model.AbstractCixsService;

/**
 * This class adds to <code>AbstractCixsService</code> those properties that
 * are very specific to Jaxws and would have no meaning for other targets.
 * 
 * @author Fady Moussallam
 * 
 */
public class CixsJaxwsService extends AbstractCixsService {

    /**
     * Will be appended to implementation class name to form a host header
     * class name.
     */
    private static final String HOST_HEADER_SUFFIX = "HostHeader";

    /* ====================================================================== */
    /* Following are key identifiers for this model persistence. = */
    /* ====================================================================== */

    /** Service host header class name. */
    public static final String HEADER_CLASSNAME = "headerClassName";

    /* ====================================================================== */
    /* Following are this class fields that are persistent. = */
    /* ====================================================================== */

    /** Service host header class name. */
    private String _headerClassName;

    /**
     * Construct an empty model.
     */
    public CixsJaxwsService() {
        super();
    }

    /**
     * Construct from a properties file.
     * 
     * @param props the property file
     */
    public CixsJaxwsService(final Properties props) {
        super(props);
        setHeaderClassName(getString(props, HEADER_CLASSNAME, null));
    }

    /**
     * @return the host header class name
     */
    public String getHeaderClassName() {
        if (_headerClassName == null || _headerClassName.length() == 0) {
            return getInterfaceClassName() + HOST_HEADER_SUFFIX;
        }
        return _headerClassName;
    }

    /**
     * @param headerClassName the host header class name to set
     */
    public void setHeaderClassName(final String headerClassName) {
        _headerClassName = headerClassName;
    }

    /**
     * @return a properties file holding the values of this object fields
     */
    public Properties toProperties() {
        Properties props = super.toProperties();
        putString(props, HEADER_CLASSNAME, getHeaderClassName());
        return props;
    }
}
