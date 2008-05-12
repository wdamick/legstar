package com.legstar.eclipse.plugin.schemagen.util;

/**
 * Exception raised while doing an http access.
 */
public class HttpClientHelperException extends Exception {

	/** Serial ID. */
    private static final long serialVersionUID = -1496098769172853687L;

    /** 
	 * Constructor from an error message. 
	 * @param message the text message 
	 * */
	public HttpClientHelperException(final String message) {
		super(message);
	}

	/** 
	 * Constructor from an inner exception. 
	 * @param e the inner exception 
	 * */
	public HttpClientHelperException(final Exception e) {
		super(e);
	}
}
