package com.legstar.coxb.gen;

/**
 * Represent a simple inner element for the ant task.
 */
public class AlternativeClassName {
	
	/** Jaxb class name. */
	private String mJaxbClassName;
	
	/** Alternative class name. */
	private String mToClassName;
	
	/** Needs to be a public constructor. */
	public AlternativeClassName() { }
	
	/**
	 * @param name the Jaxb class name
	 */
	public final void setJaxbClassName(final String name) {
		mJaxbClassName = name;
	}
	
	/**
	 * @return the Jaxb class name
	 */
	public final String getJaxbClassName() {
		return mJaxbClassName;
	}

	/**
	 * @param name the alternative class name
	 */
	public final void setToClassName(final String name) {
		mToClassName = name;
	}
	
	/**
	 * @return the alternative class name
	 */
	public final String getToClassName() {
		return mToClassName;
	}
}
