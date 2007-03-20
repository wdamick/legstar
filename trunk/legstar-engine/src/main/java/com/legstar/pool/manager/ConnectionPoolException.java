package com.legstar.pool.manager;

/** Any failure related with connection pool handling.*/
public class ConnectionPoolException extends Exception {

	/** Serial ID. */
	private static final long serialVersionUID = 1887640077182671863L;

	/** 
	 * Constructor from an error message. 
	 * @param message the text message 
	 * */
	public ConnectionPoolException(final String message) {
		super(message);
	}

	/** 
	 * Constructor from an inner exception. 
	 * @param e the inner exception 
	 * */
	public ConnectionPoolException(final Exception e) {
		super(e);
	}
}
