/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.transform;

/**
 * Error transforming data to or from the host.
 */
public class HostTransformException extends Exception {

    /** Default serial ID. */
    private static final long serialVersionUID = 6819180164913893489L;
    /** 
     * Constructor from an error message. 
     * @param message the text message 
     * */
    public HostTransformException(final String message) {
        super(message);
    }
    /** 
     * Constructor from an inner exception. 
     * @param e the inner exception 
     * */
    public HostTransformException(final Exception e) {
        super(e);
    }

}
