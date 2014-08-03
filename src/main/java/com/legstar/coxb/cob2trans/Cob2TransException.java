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
package com.legstar.coxb.cob2trans;

/**
 * Exception when generating binding classes.
 */
public class Cob2TransException extends Exception {

    /** Unique serial ID. */
    private static final long serialVersionUID = -1;

    /**
     * Build Exception from message.
     * 
     * @param message exception description
     */
    public Cob2TransException(final String message) {
        super(message);
    }

    /**
     * Build Exception from inner exception.
     * 
     * @param e the inner exception
     */
    public Cob2TransException(final Exception e) {
        super(e);
    }
}
