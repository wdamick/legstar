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
package com.legstar.coxb.host;

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
	 * Constructor from an inner exception. 
	 * @param e the inner exception 
	 * */
	public HostException(final Exception e) {
		super(e);
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
