/*******************************************************************************
 * Copyright (c) 2011 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.codegen;

/**
 * Exception related to syntax errors or execution errors using cixsmake.
 */
public class CodeGenMakeException extends Exception {

    /** Unique serial ID. */
    private static final long serialVersionUID = 6490029804547733908L;

    /**
     * Build Exception from message.
     * @param message exception description
     */
    public CodeGenMakeException(final String message) {
        super(message);
    }

  /**
   * Build Exception from inner exception.
   * @param e the inner exception
   */
    public CodeGenMakeException(final Exception e) {
        super(e);
    }
}

