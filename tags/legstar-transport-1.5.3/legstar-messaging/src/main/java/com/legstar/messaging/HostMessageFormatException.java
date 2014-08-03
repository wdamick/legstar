/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.messaging;

/**
 * Exception raised if a message part cannot be constructed from its
 * constituents.
 */
public class HostMessageFormatException extends Exception {

    /** Serial ID. */
    private static final long serialVersionUID = -4204182561865789103L;

    /** 
     * Constructor from an error message. 
     * @param message the text message 
     * */
    public HostMessageFormatException(final String message) {
        super(message);
    }

    /** 
     * Constructor from an inner exception. 
     * @param e the inner exception 
     * */
    public HostMessageFormatException(final Exception e) {
        super(e);
    }
}
