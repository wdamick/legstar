package com.legstar.eclipse.plugin.schemagen.util;

/**
 * Exception raised while parsing or formatting Xml.
 */
public class XmlDocumentHelperException extends Exception {

	/** Serial ID. */
	private static final long serialVersionUID = -7109200198756195977L;

	/** 
	 * Constructor from an error message. 
	 * @param message the text message 
	 * */
	public XmlDocumentHelperException(final String message) {
		super(message);
	}

	/** 
	 * Constructor from an inner exception. 
	 * @param e the inner exception 
	 * */
	public XmlDocumentHelperException(final Exception e) {
		super(e);
	}
}
