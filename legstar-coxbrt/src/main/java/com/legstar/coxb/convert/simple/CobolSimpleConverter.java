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
package com.legstar.coxb.convert.simple;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.coxb.host.HostException;

/**
 * An abstract class representing simple cobol version methods.
 *
 * @author Fady Moussallam
 * 
 */
public abstract class CobolSimpleConverter {

    /** Cobol compiler parameters. */
    private CobolContext mCobolContext;

    /**
     * @param cobolContext the Cobol compiler parameters in effect
     */
    public CobolSimpleConverter(final CobolContext cobolContext) {
        mCobolContext = cobolContext;
    }

    /**
     * @return Returns the CobolContext.
     */
    public CobolContext getCobolContext() {
        return mCobolContext;
    }

    /**
     * @param cobolContext The CobolContext to set.
     */
    public void setCobolContext(final CobolContext cobolContext) {
        mCobolContext = cobolContext;
    }

    /**
     * Formats a meaningful error message to help track conversion errors.
     * @param ce the faulty binding element 
     * @param e the conversion exception
     * @throws HostException the resulting host exception
     */
    public void throwHostException(
            final ICobolBinding ce, 
            final CobolConversionException e)
    throws HostException {
        throw (new HostException("ConversionException for element:"
                + ce.getBindingName()
                + " Cobol name:" + ce.getCobolName()
                + " Reason:" + e.getMessage()));

    }
}
