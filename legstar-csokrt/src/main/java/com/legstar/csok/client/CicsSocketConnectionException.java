package com.legstar.csok.client;

import com.legstar.messaging.ConnectionException;

/**
 * Exception raised if CICS Socket pool operation fails.
 */
public class CicsSocketConnectionException extends ConnectionException {

	/** Serial ID. */
	private static final long serialVersionUID = -6577458666897908542L;

	/** 
	 * Constructor from an error message. 
	 * @param message the text message 
	 * */
	public CicsSocketConnectionException(final String message) {
		super(message);
	}

	/** 
	 * Constructor from an inner exception. 
	 * @param e the inner exception 
	 * */
	public CicsSocketConnectionException(final Exception e) {
		super(e);
	}
}
