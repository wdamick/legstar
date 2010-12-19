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
package com.legstar.proxy.invoke.pojo;

import com.legstar.proxy.invoke.ProxyInvokerException;

/**
 * Something went wrong while trying to invoke a remote POJO.
 */
public class PojoInvokerException extends ProxyInvokerException {

    /** Serial ID. */
    private static final long serialVersionUID = -8754220666255281166L;

    /** 
     * Constructor from an error message. 
     * @param message the text message 
     * */
    public PojoInvokerException(final String message) {
        super(message);
    }

    /** 
     * Constructor from an inner exception. 
     * @param e the inner exception 
     * */
    public PojoInvokerException(final Exception e) {
        super(e);
    }
}
