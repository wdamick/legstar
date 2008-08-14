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
 package com.legstar.mq.client;

import com.legstar.messaging.ConnectionException;

/**
 * Exception raised if CICS MQ operation fails.
 */
public class CicsMQConnectionException extends ConnectionException {

	/** Serial ID. */
	private static final long serialVersionUID = 5435794123153633650L;

	/** 
	 * Constructor from an error message. 
	 * @param message the text message 
	 * */
	public CicsMQConnectionException(final String message) {
		super(message);
	}

	/** 
	 * Constructor from an inner exception. 
	 * @param e the inner exception 
	 * */
	public CicsMQConnectionException(final Exception e) {
		super(e);
	}
}
