package com.legstar.test.client;

/**
 * The Proxy Host simulator was unable to run the target Proxy service.
  */
public class ProxyClientException extends Exception {

    /** Serial ID. */
    private static final long serialVersionUID = 5581346492640545019L;

    /** 
     * Constructor from an error message. 
     * @param message the text message 
     * */
    public ProxyClientException(final String message) {
        super(message);
    }

    /** 
     * Constructor from an inner exception. 
     * @param e the inner exception 
     * */
    public ProxyClientException(final Exception e) {
        super(e);
    }
}
