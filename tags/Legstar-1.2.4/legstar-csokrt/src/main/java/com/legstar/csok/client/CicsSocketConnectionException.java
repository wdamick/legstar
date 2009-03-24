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
package com.legstar.csok.client;

import com.legstar.messaging.ConnectionException;

/**
 * Exception raised if CICS Socket pool operation fails.
 */
public class CicsSocketConnectionException extends ConnectionException {

    /** Serial ID. */
    private static final long serialVersionUID = -6577458666897908542L;

    /** 
     * Constructor from an error message. 
     * @param message the text message 
     * */
    public CicsSocketConnectionException(final String message) {
        super(message);
    }

    /** 
     * Constructor from an inner exception. 
     * @param e the inner exception 
     * */
    public CicsSocketConnectionException(final Exception e) {
        super(e);
    }
}
