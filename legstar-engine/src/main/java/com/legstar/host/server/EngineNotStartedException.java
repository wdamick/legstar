package com.legstar.host.server;

/**
 * Exception raised for attempts to access the engine while
 * it is not started.
 */
public class EngineNotStartedException extends Exception {

	/** Serial ID. */
	private static final long serialVersionUID = -4272564269034554325L;

	/** 
	 * Constructor from an error message. 
	 * @param message the text message 
	 * */
	public EngineNotStartedException(final String message) {
		super(message);
	}

	/** 
	 * Constructor from an inner exception. 
	 * @param e the inner exception 
	 * */
	public EngineNotStartedException(final Exception e) {
		super(e);
	}
}
