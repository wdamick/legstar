/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.util;

import com.legstar.coxb.host.HostException;

/**
 * Dynamic manipulation of methods failures.
 * 
 */
public class ClassMethodException extends HostException {

    /** Default serial ID. */
    private static final long serialVersionUID = -2524516962618906044L;

    /**
     * Constructor from an error message.
     * 
     * @param message the text message
     * */
    public ClassMethodException(final String message) {
        super(message);
    }

    /**
     * Constructor from an inner exception.
     * 
     * @param e the inner exception
     * */
    public ClassMethodException(final Exception e) {
        super(e);
    }

}
