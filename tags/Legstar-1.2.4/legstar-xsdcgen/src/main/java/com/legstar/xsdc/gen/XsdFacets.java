/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.xsdc.gen;

/**
 * This class collects all facet values of interest in a hierarchy of XML
 * schema types.
 */
public class XsdFacets {

    /** Size of string elements. */
    private int mLength = -1;

    /** Regular expression pattern for elements values. */
    private String mPattern = null;

    /** Total number of digits for numeric elements. */
    private int mTotalDigits = -1;

    /** Number of fractional digits for numeric elements. */
    private int mFractionDigits = -1;

    /**
     * @return the fractional digits
     */
    public final int getFractionDigits() {
        return mFractionDigits;
    }

    /**
     * @param fractionalDigits the fractional digits to set
     */
    public final void setFractionDigits(
            final int fractionalDigits) {
        mFractionDigits = fractionalDigits;
    }

    /**
     * @return the length
     */
    public final int getLength() {
        return mLength;
    }

    /**
     * @param length the length to set
     */
    public final void setLength(final int length) {
        mLength = length;
    }

    /**
     * @return the regular expression pattern
     */
    public final String getPattern() {
        return mPattern;
    }

    /**
     * @param pattern the regular expression pattern to set
     */
    public final void setPattern(final String pattern) {
        mPattern = pattern;
    }

    /**
     * @return the total number of digits
     */
    public final int getTotalDigits() {
        return mTotalDigits;
    }

    /**
     * @param totalDigits the total number of digits to set
     */
    public final void setTotalDigits(final int totalDigits) {
        mTotalDigits = totalDigits;
    }
}
