package com.legstar.messaging;

/**
 * Exception raised if a header part is invalid.
 */
public class HeaderPartException extends Exception {

	/** Serial ID. */
	private static final long serialVersionUID = 8706638423491378355L;

	/** 
	 * Constructor from an error message. 
	 * @param message the text message 
	 * */
	public HeaderPartException(final String message) {
		super(message);
	}

	/** 
	 * Constructor from an inner exception. 
	 * @param e the inner exception 
	 * */
	public HeaderPartException(final Exception e) {
		super(e);
	}
}
