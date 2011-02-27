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
package com.legstar.coxb.util;

import com.legstar.coxb.host.HostException;

/**
 * Class loading exceptions.
 * 
 */
public class ClassLoadingException extends HostException {

    /** Default serial ID. */
    private static final long serialVersionUID = 3875441381578348742L;

    /**
     * Constructor from an error message.
     * 
     * @param message the text message
     * */
    public ClassLoadingException(final String message) {
        super(message);
    }

    /**
     * Constructor from an inner exception.
     * 
     * @param e the inner exception
     * */
    public ClassLoadingException(final Exception e) {
        super(e);
    }

}
