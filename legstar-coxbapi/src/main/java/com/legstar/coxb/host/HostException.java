/*******************************************************************************
 * Copyright (c) 2009 LegSem.
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
    private String mHostFieldMessage = null;

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
        mHostFieldMessage = getMessage(
                hostField, errorOffset, hostField.length());
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
        mHostFieldMessage = getMessage(hostField, errorOffset, errorLength);
    }

    /** 
     * Constructor from an inner exception. 
     * @param e the inner exception 
     * */
    public HostException(final Exception e) {
        super(e);
    }

    /**
     * Construct an error message showing where in the host data the error lies.
     * @param hostField the field holding host data
     * @param hostFieldErrorOffset offset where error is detected
     * @param hostFieldErrorLength the field length
     * @return a message showing the erroneous data
     */
    private String getMessage(
            final HostData hostField,
            final int hostFieldErrorOffset,
            final int hostFieldErrorLength) {
        String message = super.getMessage();
        if (hostField != null) {
            /* If the offset is off range, just print all the host buffer*/
            if (hostFieldErrorOffset > hostField.length()) {
                message += ". Host data=0x" + hostField.toHexString();
            } else {
                /* If the field length is invalid, print data starting
                 * at offset till the end of the host buffer. */
                int restl = hostField.length() - hostFieldErrorOffset;
                if (hostFieldErrorLength == 0 
                        || hostFieldErrorLength > restl) {
                    byte[] restOfBuffer = new byte[restl];
                    System.arraycopy(hostField.getHostData(),
                            hostFieldErrorOffset, restOfBuffer, 0, restl);
                    message += ". Host data at offset " + hostFieldErrorOffset
                    + "=0x" + HostData.toHexString(restOfBuffer);
                } else {
                    byte[] errorBuffer = new byte[hostFieldErrorLength];
                    System.arraycopy(hostField.getHostData(),
                            hostFieldErrorOffset, errorBuffer, 0,
                            hostFieldErrorLength);
                    message += ". Host data at offset " + hostFieldErrorOffset
                    + "=0x" + HostData.toHexString(errorBuffer);
                }
            }
        }
        return  message;
    }
    /**
     * @see java.lang.Throwable#getMessage()
     * @return String describes the exception
     */
    public String getMessage() {
        if (mHostFieldMessage != null) {
            return mHostFieldMessage;
        } else {
            return super.getMessage();
        }
    }

    /**
     * @see java.lang.Object#toString()
     * @return String describes the exception
     */
    public String toString() {
        return getMessage();
    }

}
