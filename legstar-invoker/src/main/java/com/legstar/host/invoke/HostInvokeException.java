package com.legstar.host.invoke;

/**
 * Exception raised if invoke execution fails.
 */
public class HostInvokeException extends Exception {

	/** Serial ID. */
	private static final long serialVersionUID = -4069744829224499341L;

	/** 
	 * Constructor from an error message. 
	 * @param message the text message 
	 * */
	public HostInvokeException(final String message) {
		super(message);
	}

	/** 
	 * Constructor from an inner exception. 
	 * @param e the inner exception 
	 * */
	public HostInvokeException(final Exception e) {
		super(e);
	}
}
