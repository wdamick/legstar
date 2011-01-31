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
package com.legstar.coxb.convert;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;

/**
 * Java element to host conversion issue.
 *
 * @author Fady Moussallam
 * 
 */
public class CobolConversionException extends HostException {

    /** Simple serial ID. */
    private static final long serialVersionUID = 1L;

    /**
     * Contructor used when no host field exists.
     * 
     * @param message text describing the issue
     */
    public CobolConversionException(final String message) {
        super(message);
    }

    /**
     * Contructor used when host field exist.
     * 
     * @param message text describing the issue
     * @param hostField host field on which the error occured
     * @param errorOffset position in host data where faulty field starts
     */
    public CobolConversionException(final String message,
            final HostData hostField,
            final int errorOffset) {
        super(message, hostField, errorOffset);
    }

    /**
     * Contructor used when host field exist.
     * 
     * @param message text describing the issue
     * @param hostField host field on which the error occured
     * @param errorOffset position in host data where faulty field starts
     * @param errorLength length of faulty field
     */
    public CobolConversionException(final String message,
            final HostData hostField,
            final int errorOffset,
            final int errorLength) {
        super(message, hostField, errorOffset, errorLength);
    }
}
