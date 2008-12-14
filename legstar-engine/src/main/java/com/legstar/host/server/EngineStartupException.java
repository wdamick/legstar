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
package com.legstar.host.server;

/**
 * Exception raised if engine startup fails.
 */
public class EngineStartupException extends Exception {

    /** Serial ID. */
    private static final long serialVersionUID = -4272564269034554325L;

    /** 
     * Constructor from an error message. 
     * @param message the text message 
     * */
    public EngineStartupException(final String message) {
        super(message);
    }

    /** 
     * Constructor from an inner exception. 
     * @param e the inner exception 
     * */
    public EngineStartupException(final Exception e) {
        super(e);
    }
}
