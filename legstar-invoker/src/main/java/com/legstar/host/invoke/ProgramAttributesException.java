package com.legstar.host.invoke;

/**
 * Exception raised if program attributes cannot be recovered.
 */
public class ProgramAttributesException extends HostInvokeException {

	/** Serial ID. */
	private static final long serialVersionUID = 6751206958592325835L;

	/** 
	 * Constructor from an error message. 
	 * @param message the text message 
	 * */
	public ProgramAttributesException(final String message) {
		super(message);
	}

	/** 
	 * Constructor from an inner exception. 
	 * @param e the inner exception 
	 * */
	public ProgramAttributesException(final Exception e) {
		super(e);
	}
}
