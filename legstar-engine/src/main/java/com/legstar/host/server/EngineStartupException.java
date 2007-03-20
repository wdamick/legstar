package com.legstar.host.server;

/**
 * Exception raised if engine startup fails.
 */
public class EngineStartupException extends Exception {

	/** Serial ID. */
	private static final long serialVersionUID = -4272564269034554325L;

	/** 
	 * Constructor from an error message. 
	 * @param message the text message 
	 * */
	public EngineStartupException(final String message) {
		super(message);
	}

	/** 
	 * Constructor from an inner exception. 
	 * @param e the inner exception 
	 * */
	public EngineStartupException(final Exception e) {
		super(e);
	}
}
