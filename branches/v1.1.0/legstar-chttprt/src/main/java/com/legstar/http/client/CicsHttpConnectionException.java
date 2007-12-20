/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
 package com.legstar.http.client;

import com.legstar.messaging.ConnectionException;

/**
 * Exception raised if CICS HTTP operation fails.
 */
public class CicsHttpConnectionException extends ConnectionException {

	/** Serial ID. */
	private static final long serialVersionUID = -5138850618273151616L;

	/** 
	 * Constructor from an error message. 
	 * @param message the text message 
	 * */
	public CicsHttpConnectionException(final String message) {
		super(message);
	}

	/** 
	 * Constructor from an inner exception. 
	 * @param e the inner exception 
	 * */
	public CicsHttpConnectionException(final Exception e) {
		super(e);
	}
}
