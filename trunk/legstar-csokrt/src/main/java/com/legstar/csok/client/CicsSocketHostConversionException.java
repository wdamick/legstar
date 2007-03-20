package com.legstar.csok.client;

/** Exception raised if host to java conversion error occurs. */
public class CicsSocketHostConversionException extends Exception {

	/** Serial ID. */
	private static final long serialVersionUID = -363780096326770311L;

	/** 
	 * Constructor from an error message. 
	 * @param message the text message 
	 * */
	public CicsSocketHostConversionException(final String message) {
		super(message);
	}

	/** 
	 * Constructor from an inner exception. 
	 * @param e the inner exception 
	 * */
	public CicsSocketHostConversionException(final Exception e) {
		super(e);
	}
}
