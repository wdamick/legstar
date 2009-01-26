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

import com.legstar.cixs.gen.model.AbstractCixsService;

/**
 * This class adds to <code>AbstractCixsService</code> those properties that
 * are very specific to Jaxws and would have no meaning for other targets.
 * 
 * @author Fady Moussallam
 * 
 */ 
public class CixsJaxwsService extends AbstractCixsService {

    /** Service host header class name. */
    private String mHeaderClassName;

    /** Will be appended to implementation class name to form a host header
     *  class name. */
    private static final String HOST_HEADER_SUFFIX = "HostHeader";

    /**
     * @return the host header class name
     */
    public final String getHeaderClassName() {
        if (mHeaderClassName == null || mHeaderClassName.length() == 0) {
            return getInterfaceClassName() + HOST_HEADER_SUFFIX;
        }
        return mHeaderClassName;
    }

    /**
     * @param headerClassName the host header class name to set
     */
    public final void setHeaderClassName(final String headerClassName) {
        mHeaderClassName = headerClassName;
    }

}
