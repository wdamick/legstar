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
package com.legstar.c2ws;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Error while processing host request.
 */
public class C2wsInvokeException extends Exception {

    /** Default serial ID. */
    private static final long serialVersionUID = -7634449834844672823L;

    /** Logger. */
    private static final Log LOG = LogFactory.getLog(
            C2wsInvokeException.class);

    /** 
     * Constructor from an error message. 
     * @param message the text message 
     * */
    public C2wsInvokeException(final String message) {
        super(message);
        LOG.error(message);
    }
    /** 
     * Constructor from an inner exception. 
     * @param e the inner exception 
     * */
    public C2wsInvokeException(final Exception e) {
        super(e);
        LOG.error(e);
    }

}
