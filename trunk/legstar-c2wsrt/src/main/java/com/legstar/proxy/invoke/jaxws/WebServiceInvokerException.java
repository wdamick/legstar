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
package com.legstar.proxy.invoke.jaxws;

import com.legstar.proxy.invoke.ProxyInvokerException;

/**
 * Something went wrong while trying to invoke a remote web service.
 */
public class WebServiceInvokerException extends ProxyInvokerException {

    /** Serial ID. */
    private static final long serialVersionUID = -5224938880900687601L;

    /** 
     * Constructor from an error message. 
     * @param message the text message 
     * */
    public WebServiceInvokerException(final String message) {
        super(message);
    }

    /** 
     * Constructor from an inner exception. 
     * @param e the inner exception 
     * */
    public WebServiceInvokerException(final Exception e) {
        super(e);
    }
}
