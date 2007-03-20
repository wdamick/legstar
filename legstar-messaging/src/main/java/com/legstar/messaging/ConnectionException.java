package com.legstar.messaging;

/**
 * Exception raised if host connection fails.
 */
public class ConnectionException extends Exception {

	/** Serial ID. */
	private static final long serialVersionUID = 28391066252414541L;

	/** 
	 * Constructor from an error message. 
	 * @param message the text message 
	 * */
	public ConnectionException(final String message) {
		super(message);
	}

	/** 
	 * Constructor from an inner exception. 
	 * @param e the inner exception 
	 * */
	public ConnectionException(final Exception e) {
		super(e);
	}
}
