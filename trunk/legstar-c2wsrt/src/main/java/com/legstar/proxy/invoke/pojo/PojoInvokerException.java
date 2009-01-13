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
