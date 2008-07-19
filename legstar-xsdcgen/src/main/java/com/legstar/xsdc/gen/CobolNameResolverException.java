/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.xsdc.gen;

/**
 * Cobol name resolution exceptions.
 */
public class CobolNameResolverException extends Exception {
	
	/** Default serial ID. */
	private static final long serialVersionUID = 5089981853019789786L;
	/** 
	 * Constructor from an error message. 
	 * @param message the text message 
	 * */
	public CobolNameResolverException(final String message) {
		super(message);
	}
	/** 
	 * Constructor from an inner exception. 
	 * @param e the inner exception 
	 * */
	public CobolNameResolverException(final Exception e) {
		super(e);
	}

}
