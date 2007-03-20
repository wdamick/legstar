package com.legstar.host.access;

/**
 * Exception raised if host execution fails.
 */
public class HostAccessStrategyException extends Exception {

	/** Serial ID. */
	private static final long serialVersionUID = 8735374918077768947L;

	/** 
	 * Constructor from an error message. 
	 * @param message the text message 
	 * */
	public HostAccessStrategyException(final String message) {
		super(message);
	}

	/** 
	 * Constructor from an inner exception. 
	 * @param e the inner exception 
	 * */
	public HostAccessStrategyException(final Exception e) {
		super(e);
	}
}
