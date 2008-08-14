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
package com.legstar.messaging.impl;

/**
 * Error formatting data for the host.
 */
public class HostMarshalException extends Exception {
	
	/** Default serial ID. */
	private static final long serialVersionUID = -1119229067501693999L;
	/** 
	 * Constructor from an error message. 
	 * @param message the text message 
	 * */
	public HostMarshalException(final String message) {
		super(message);
	}
	/** 
	 * Constructor from an inner exception. 
	 * @param e the inner exception 
	 * */
	public HostMarshalException(final Exception e) {
		super(e);
	}

}
