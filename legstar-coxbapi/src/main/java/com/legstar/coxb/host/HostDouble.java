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
package com.legstar.coxb.host;

/**
 * Helper class to hold components of a double.
 * 
 * @author Fady Moussallam
 * 
 */
public class HostDouble {
    /** 0 means positive and 1 negative. */
    private int mSign = 0;

    /** The exponent value (not the excess). */
    private int mExponent = 0;

    /** The mantissa value. */
    private long mMantissa = 0;

    /**
     * @return Returns the exponent.
     */
    public int getExponent() {
        return mExponent;
    }

    /**
     * @param exponent The exponent to set.
     */
    public void setExponent(final int exponent) {
        mExponent = exponent;
    }

    /**
     * @return Returns the mantissa.
     */
    public long getMantissa() {
        return mMantissa;
    }

    /**
     * @param mantissa The mantissa to set.
     */
    public void setMantissa(final long mantissa) {
        mMantissa = mantissa;
    }

    /**
     * @return Returns the sign.
     */
    public int getSign() {
        return mSign;
    }

    /**
     * @param sign The sign to set.
     */
    public void setSign(final int sign) {
        mSign = sign;
    }

}
