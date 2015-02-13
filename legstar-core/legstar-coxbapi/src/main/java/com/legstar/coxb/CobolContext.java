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
package com.legstar.coxb;

import com.legstar.coxb.host.HostContext;

/**
 * 
 * This class encapsulates all Cobol compiler options that might influence the
 * conversion from Cobol representation to Java representation.
 * 
 * @author Fady Moussallam
 * 
 */
public class CobolContext extends HostContext {

    /** True if cobol option for extended arithmetics (31 digits) is set. */
    private boolean _arithExtend = false;

    /** True if PIC N items are to be handled as DBCS items. */
    private boolean _nsymbolDbcs = false;

    /**
     * Incomplete alphanumeric data items need to be padded before they are sent
     * to the mainframe. If this is not null, then the specified byte will be
     * used for padding.
     */
    private Byte _alphanumPaddingChar = null;

    /**
     * Fail if a java string content is too long for the target alphanumeric
     * COBOL data item.
     */
    private boolean _failOnAlphanumOverflow = false;

    /**
     * This enumeration class represents how binary data is truncated depending
     * on the number of digits specified in the PICTURE clause.
     * 
     */
    public enum Trunc {
        /**
         * TRUNC(OPT) Leaves it up to the compiler to decide to truncate or not,
         * based on performance considerations.
         */
        OPT,
        /** TRUNC(BIN) No truncation occurs, this is equivalent to COMP-5. */
        BIN,
        /**
         * TRUNC(STD) Data truncated to the number of digits in the PICTURE
         * clause.
         */
        STD
    }

    /**
     * @return Returns the arithExtend.
     */
    public boolean isArithExtend() {
        return _arithExtend;
    }

    /**
     * @param arithExtend The arithExtend to set.
     */
    public void setArithExtend(final boolean arithExtend) {
        _arithExtend = arithExtend;
    }

    /**
     * @return Returns the symbol Dbcs.
     */
    public boolean isSymbolDbcs() {
        return _nsymbolDbcs;
    }

    /**
     * @param nsymbolDbcs The symbol Dbcs to set.
     */
    public void setSymbolDbcs(final boolean nsymbolDbcs) {
        _nsymbolDbcs = nsymbolDbcs;
    }

    /**
     * @return the alphanumerics padding character
     */
    public Byte getAlphanumPaddingChar() {
        return _alphanumPaddingChar;
    }

    /**
     * @param alphanumPaddingChar the alphanumerics padding character to set
     */
    public void setAlphanumPaddingChar(final Byte alphanumPaddingChar) {
        _alphanumPaddingChar = alphanumPaddingChar;
    }

    /**
     * Fail if a java string content is too long for the target alphanumeric
     * COBOL data item.
     * 
     * @return true when failure is requested if a java string content is too
     *         long for the target alphanumeric COBOL data item
     */
    public boolean failOnAlphanumOverflow() {
        return _failOnAlphanumOverflow;
    }

    /**
     * Fail if a java string content is too long for the target alphanumeric
     * COBOL data item.
     * 
     * @param failOnAlphanumOverflow true when failure is requested if a java
     *            string content is too long for the target alphanumeric COBOL
     *            data item
     */
    public void setFailOnAlphanumOverflow(boolean failOnAlphanumOverflow) {
        this._failOnAlphanumOverflow = failOnAlphanumOverflow;
    }

}
