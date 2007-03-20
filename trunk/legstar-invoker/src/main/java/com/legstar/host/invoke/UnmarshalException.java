package com.legstar.host.invoke;

/**
 * Exception raised if unmarshaling host data to java object tree fails.
 */
public class UnmarshalException extends HostInvokeException {

	/** Serial ID. */
	private static final long serialVersionUID = 1370316440084576265L;

	/** 
	 * Constructor from an error message. 
	 * @param message the text message 
	 * */
	public UnmarshalException(final String message) {
		super(message);
	}

	/** 
	 * Constructor from an inner exception. 
	 * @param e the inner exception 
	 * */
	public UnmarshalException(final Exception e) {
		super(e);
	}
}
