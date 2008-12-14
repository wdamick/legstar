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
package com.legstar.j2sc.gen;

/**
 * Exception raised if conversion to Xsd fails.
 */
public class JavaToXsdCobolException extends Exception {

    /** Serial ID. */
    private static final long serialVersionUID = -3700780876757624957L;

    /** 
     * Constructor from an error message. 
     * @param message the text message 
     * */
    public JavaToXsdCobolException(final String message) {
        super(message);
    }

    /** 
     * Constructor from an inner exception. 
     * @param e the inner exception 
     * */
    public JavaToXsdCobolException(final Exception e) {
        super(e);
    }
}
