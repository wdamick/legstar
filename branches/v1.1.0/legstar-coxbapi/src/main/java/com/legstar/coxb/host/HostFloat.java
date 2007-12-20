/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
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
	public final int getExponent() {
		return mExponent;
	}

	/**
	 * @param exponent The exponent to set.
	 */
	public final void setExponent(final int exponent) {
		mExponent = exponent;
	}

	/**
	 * @return Returns the mantissa.
	 */
	public final int getMantissa() {
		return mMantissa;
	}

	/**
	 * @param mantissa The mantissa to set.
	 */
	public final void setMantissa(final int mantissa) {
		mMantissa = mantissa;
	}

	/**
	 * @return Returns the sign.
	 */
	public final int getSign() {
		return mSign;
	}

	/**
	 * @param sign The sign to set.
	 */
	public final void setSign(final int sign) {
		mSign = sign;
	}

}
