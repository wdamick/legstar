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
package com.legstar.host;

/**
 * Host exceptions trace java to host conversions issues.
 *
 * @author Fady Moussallam
 * 
 */
public class HostException extends Exception {

	/** Simple serial ID. */
	private static final long serialVersionUID = 1L;
	
	/** The host data associated with this exception (if any). */
	private HostData mhostField = null;
	
	/** The offset within the host data where the faulty field starts. */
	private int mhostFieldErrorOffset = 0;
	
	/** The length of the host field. */
	private int mhostFieldErrorLength = 0;
	
	/**
	 * Contructor used when no host field exists.
	 * 
	 * @param message text describing the issue
	 */
	public HostException(final String message) {
		super(message);
	}

	/**
	 * Contructor used when host field exist.
	 * 
	 * @param message text describing the issue
	 * @param hostField host field on which the error occured
	 * @param errorOffset position in host field where error occured
	 */
	public HostException(final String message,
			final HostData hostField,
			final int errorOffset) {
		super(message);
		mhostField = hostField;
		mhostFieldErrorOffset = errorOffset;
	}

	/**
	 * Contructor used when host field exist and size is known.
	 * 
	 * @param message text describing the issue
	 * @param hostField host field on which the error occured
	 * @param errorOffset position in host data where faulty field starts
	 * @param errorLength length of faulty field
	 */
	public HostException(final String message,
			final HostData hostField,
			final int errorOffset,
			final int errorLength) {
		super(message);
		mhostField = hostField;
		mhostFieldErrorOffset = errorOffset;
		mhostFieldErrorLength = errorLength;
	}

	/**
	 * @see java.lang.Throwable#getMessage()
	 * @return String describes the exception
	 */
	public final String getMessage() {
		String message = super.getMessage();
		if (mhostField != null) {
			/* If the offset is off range, just print all the host buffer*/
			if (mhostFieldErrorOffset > mhostField.length()) {
				message += ". Host data=0x" + mhostField.toHexString();
			} else {
				/* If the field length is invalid, print data starting
				 * at offset till the end of the host buffer. */
				int restl = mhostField.length()	- mhostFieldErrorOffset;
				if (mhostFieldErrorLength == 0 
						|| mhostFieldErrorLength > restl) {
					byte[] restOfBuffer = new byte[restl];
					System.arraycopy(mhostField.getHostData(),
							mhostFieldErrorOffset, restOfBuffer, 0, restl);
					message += ". Host data at offset " + mhostFieldErrorOffset
					+ "=0x" + HostData.toHexString(restOfBuffer);
				} else {
					byte[] errorBuffer = new byte[mhostFieldErrorLength];
					System.arraycopy(mhostField.getHostData(),
							mhostFieldErrorOffset, errorBuffer, 0,
							mhostFieldErrorLength);
					message += ". Host data at offset " + mhostFieldErrorOffset
					+ "=0x" + HostData.toHexString(errorBuffer);
				}
			}
		}
		return  message;
	}

	/**
	 * @see java.lang.Object#toString()
	 * @return String describes the exception
	 */
	public final String toString() {
		return getMessage();
	}

}
