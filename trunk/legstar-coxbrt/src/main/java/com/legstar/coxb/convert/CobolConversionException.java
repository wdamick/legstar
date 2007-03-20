/*******************************************************************************
 * LegStar legacy Web-enablement .
 * Copyright (C)  2007 LegSem
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 * 
 *     
 *****************************************************************************/
package com.legstar.coxb.convert;
import com.legstar.host.HostData;
import com.legstar.host.HostException;

/**
 * Java element to host conversion issue.
 *
 * @author Fady Moussallam
 * 
 */
public class CobolConversionException extends HostException {

	/** Simple serial ID. */
	private static final long serialVersionUID = 1L;

	/**
	 * Contructor used when no host field exists.
	 * 
	 * @param message text describing the issue
	 */
	public CobolConversionException(final String message) {
		super(message);
	}

	/**
	 * Contructor used when host field exist.
	 * 
	 * @param message text describing the issue
	 * @param hostField host field on which the error occured
	 * @param errorOffset position in host data where faulty field starts
	 */
	public CobolConversionException(final String message,
			final HostData hostField,
			final int errorOffset) {
		super(message, hostField, errorOffset);
	}
	
	/**
	 * Contructor used when host field exist.
	 * 
	 * @param message text describing the issue
	 * @param hostField host field on which the error occured
	 * @param errorOffset position in host data where faulty field starts
	 * @param errorLength length of faulty field
	 */
	public CobolConversionException(final String message,
			final HostData hostField,
			final int errorOffset,
			final int errorLength) {
		super(message, hostField, errorOffset, errorLength);
	}
}
