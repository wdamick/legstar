package com.legstar.host.server;

/**
 * Exception raised if the configuration contains invalid data.
 */
public class EngineConfigurationException extends Exception {

	/** Serial ID. */
	private static final long serialVersionUID = -4272564269034554325L;

	/** 
	 * Constructor from an error message. 
	 * @param message the text message 
	 * */
	public EngineConfigurationException(final String message) {
		super(message);
	}

	/** 
	 * Constructor from an inner exception. 
	 * @param e the inner exception 
	 * */
	public EngineConfigurationException(final Exception e) {
		super(e);
	}
}
