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
package com.legstar.proxy.invoke;

/**
 * Something went wrong while trying to invoke a remote process on behalf of a mainframe program.
 */
public class ProxyInvokerException extends Exception {

    /** Serial ID. */
    private static final long serialVersionUID = 1655098057512257829L;

    /** 
     * Constructor from an error message. 
     * @param message the text message 
     * */
    public ProxyInvokerException(final String message) {
        super(message);
    }

    /** 
     * Constructor from an inner exception. 
     * @param e the inner exception 
     * */
    public ProxyInvokerException(final Exception e) {
        super(e);
    }

    /** 
     * Constructor from an inner exception. 
     * @param e the inner exception 
     * */
    public ProxyInvokerException(final Throwable e) {
        super(e);
    }

    /** 
     * Constructor from an inner exception. 
     * @param message the text message 
     * @param e the inner exception 
     * */
    public ProxyInvokerException(final String message, final Throwable e) {
        super(message, e);
    }
}
