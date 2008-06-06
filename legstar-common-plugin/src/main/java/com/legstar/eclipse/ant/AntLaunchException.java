package com.legstar.eclipse.ant;

/**
 * Exception raised when ant generation fails.
 */
public class AntLaunchException extends Exception {

	/** Serial ID. */
	private static final long serialVersionUID = -3132180726919792224L;

	/** 
	 * Constructor from an error message. 
	 * @param message the text message 
	 * */
	public AntLaunchException(final String message) {
		super(message);
	}

	/** 
	 * Constructor from an inner exception. 
	 * @param e the inner exception 
	 * */
	public AntLaunchException(final Exception e) {
		super(e);
	}
}
