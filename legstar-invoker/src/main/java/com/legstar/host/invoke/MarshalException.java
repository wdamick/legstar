package com.legstar.host.invoke;

/**
 * Exception raised if marshaling java object tree to host format fails.
 */
public class MarshalException extends HostInvokeException {

	/** Serial ID. */
	private static final long serialVersionUID = 1370316440084576265L;

	/** 
	 * Constructor from an error message. 
	 * @param message the text message 
	 * */
	public MarshalException(final String message) {
		super(message);
	}

	/** 
	 * Constructor from an inner exception. 
	 * @param e the inner exception 
	 * */
	public MarshalException(final Exception e) {
		super(e);
	}
}
