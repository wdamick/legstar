package com.legstar.messaging;

/**
 * Exception raised if a request execution fails.
 */
public class RequestException extends Exception {

	/** Serial ID. */
	private static final long serialVersionUID = 1887640077182671863L;

	/** 
	 * Constructor from an error message. 
	 * @param message the text message 
	 * */
	public RequestException(final String message) {
		super(message);
	}

	/** 
	 * Constructor from an inner exception. 
	 * @param e the inner exception 
	 * */
	public RequestException(final Exception e) {
		super(e);
	}
}
