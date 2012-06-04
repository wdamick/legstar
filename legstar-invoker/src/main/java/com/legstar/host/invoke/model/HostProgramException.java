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
package com.legstar.host.invoke.model;

/**
 * Exception raised if host program attributes are abnormal.
 */
public class HostProgramException extends Exception {

    /** Serial ID. */
    private static final long serialVersionUID = 6216279285549588643L;

    /** 
     * Constructor from an error message. 
     * @param message the text message 
     * */
    public HostProgramException(final String message) {
        super(message);
    }

    /** 
     * Constructor from an inner exception. 
     * @param e the inner exception 
     * */
    public HostProgramException(final Throwable e) {
        super(e);
    }

    /** 
     * Constructor from an inner exception. 
     * @param message the text message 
     * @param e the inner exception 
     * */
    public HostProgramException(final String message, final Throwable e) {
        super(message, e);
    }
}
