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
package com.legstar.xsdc.test.cases.cultureinfo;

import java.math.BigDecimal;

import javax.xml.bind.annotation.XmlType;

@XmlType(name = "cultureInfoParameters")
public class CultureInfoRequest {
	
	private BigDecimal mDecimalNumber;
	private String mCultureCode;
	/**
	 * @return the Culture Code
	 */
	public final String getCultureCode() {
		return mCultureCode;
	}
	/**
	 * @param culturecode the Culture Code to set
	 */
	public final void setCultureCode(String culturecode) {
		mCultureCode = culturecode;
	}
	/**
	 * @return the DecimalNumber
	 */
	public final BigDecimal getDecimalNumber() {
		return mDecimalNumber;
	}
	/**
	 * @param DecimalNumber the DecimalNumber to set
	 */
	public final void setDecimalNumber(BigDecimal DecimalNumber) {
		mDecimalNumber = DecimalNumber;
	}

}
