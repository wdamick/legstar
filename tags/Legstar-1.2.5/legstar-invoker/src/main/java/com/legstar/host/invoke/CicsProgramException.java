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
package com.legstar.host.invoke;

/**
 * Exception raised if program attributes cannot be recovered.
 */
public class CicsProgramException extends HostInvokerException {

    /** Serial ID. */
    private static final long serialVersionUID = 6751206958592325835L;

    /** 
     * Constructor from an error message. 
     * @param message the text message 
     * */
    public CicsProgramException(final String message) {
        super(message);
    }

    /** 
     * Constructor from an inner exception. 
     * @param e the inner exception 
     * */
    public CicsProgramException(final Exception e) {
        super(e);
    }
}
