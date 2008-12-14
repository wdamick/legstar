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
package com.legstar.coxb.gen;

/**
 * Exception when generating binding classes.
 */
public class CodeGenException extends Exception {

    /** Unique serial ID. */
    private static final long serialVersionUID = 4933554196026963137L;

    /**
     * Build Exception from message.
     * @param message exception description
     */
    public CodeGenException(final String message) {
        super(message);
    }

    /**
     * Build Exception from inner exception.
     * @param e the inner exception
     */
    public CodeGenException(final Exception e) {
        super(e);
    }
}

