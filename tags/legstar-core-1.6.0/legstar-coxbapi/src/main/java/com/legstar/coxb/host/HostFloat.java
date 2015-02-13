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
package com.legstar.coxb.host;

/**
 * Helper class to hold components of a float.
 * 
 * @author Fady Moussallam
 * 
 */
public class HostFloat {
    /** 0 means positive and 1 negative. */
    private int mSign = 0;

    /** The exponent value (not the excess). */
    private int mExponent = 0;

    /** The mantissa value. */
    private int mMantissa = 0;

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
    public int getMantissa() {
        return mMantissa;
    }

    /**
     * @param mantissa The mantissa to set.
     */
    public void setMantissa(final int mantissa) {
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
