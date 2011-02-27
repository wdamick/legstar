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
package com.legstar.coxb;

import com.legstar.coxb.host.HostContext;

/**
 * 
 * This class encapsulates all Cobol compiler options that might influence
 * the conversion from Cobol representation to Java representation.
 *
 * @author Fady Moussallam
 * 
 */
public class CobolContext extends HostContext {

    /** True if cobol option for extended arithmetics (31 digits) is set. */
    private boolean mArithExtend = false;

    /** True if PIC N items are to be handled as DBCS items. */
    private boolean mNsymbolDbcs = false;

    /**
     * This enumeration class represents how binary data is truncated depending
     * on the number of digits specified in the PICTURE clause.
     *
     */
    public enum Trunc {
        /** TRUNC(OPT) Leaves it up to the compiler to decide to truncate or
         *             not, based on performance considerations. */
        OPT,
        /** TRUNC(BIN) No truncation occurs, this is equivalent to COMP-5. */
        BIN,
        /** TRUNC(STD) Data truncated to the number of digits in the 
         *             PICTURE clause. */
        STD 
    }

    /**
     * @return Returns the arithExtend.
     */
    public boolean isArithExtend() {
        return mArithExtend;
    }

    /**
     * @param arithExtend The arithExtend to set.
     */
    public void setArithExtend(final boolean arithExtend) {
        mArithExtend = arithExtend;
    }

    /**
     * @return Returns the symbol Dbcs.
     */
    public boolean isSymbolDbcs() {
        return mNsymbolDbcs;
    }

    /**
     * @param nsymbolDbcs The symbol Dbcs to set.
     */
    public void setSymbolDbcs(final boolean nsymbolDbcs) {
        mNsymbolDbcs = nsymbolDbcs;
    }

}
