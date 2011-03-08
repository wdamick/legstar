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
package com.legstar.config;

/**
 * Exception raised when a configuration is invalid.
 */
public class LegStarConfigurationException extends Exception {

    /** Serial ID. */
    private static final long serialVersionUID = -3896871818652001314L;

    /** 
     * Constructor from an error message. 
     * @param message the text message 
     * */
    public LegStarConfigurationException(final String message) {
        super(message);
    }

    /** 
     * Constructor from an inner exception. 
     * @param e the inner exception 
     * */
    public LegStarConfigurationException(final Throwable e) {
        super(e);
    }

    /** 
     * Constructor from an inner exception. 
     * @param message the text message 
     * @param e the inner exception 
     * */
    public LegStarConfigurationException(final String message, final Throwable e) {
        super(e);
    }
}
