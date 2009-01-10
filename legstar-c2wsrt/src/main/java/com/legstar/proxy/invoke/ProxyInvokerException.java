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
}
