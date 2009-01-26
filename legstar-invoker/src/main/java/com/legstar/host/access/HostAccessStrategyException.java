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
package com.legstar.host.access;

/**
 * Exception raised if host execution fails.
 */
public class HostAccessStrategyException extends Exception {

    /** Serial ID. */
    private static final long serialVersionUID = 8735374918077768947L;

    /** 
     * Constructor from an error message. 
     * @param message the text message 
     * */
    public HostAccessStrategyException(final String message) {
        super(message);
    }

    /** 
     * Constructor from an inner exception. 
     * @param e the inner exception 
     * */
    public HostAccessStrategyException(final Exception e) {
        super(e);
    }
}
