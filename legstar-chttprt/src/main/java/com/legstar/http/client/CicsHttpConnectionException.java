package com.legstar.http.client;

import com.legstar.messaging.ConnectionException;

/**
 * Exception raised if CICS HTTP operation fails.
 */
public class CicsHttpConnectionException extends ConnectionException {

	/** Serial ID. */
	private static final long serialVersionUID = -5138850618273151616L;

	/** 
	 * Constructor from an error message. 
	 * @param message the text message 
	 * */
	public CicsHttpConnectionException(final String message) {
		super(message);
	}

	/** 
	 * Constructor from an inner exception. 
	 * @param e the inner exception 
	 * */
	public CicsHttpConnectionException(final Exception e) {
		super(e);
	}
}
