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
package com.legstar.c2ws.sample;

public class CultureInfoReply {
	
	private String mDisplayLanguage;
	private String mDisplayCountry;
	private String mFormattedDate;
	private String mCurrencySymbol;
	private String mFormattedDecimalNumber;
	private ServerCultureInfo mServerCultureInfo;

	/**
	 * @return the Currency Symbol
	 */
	public final String getCurrencySymbol() {
		return mCurrencySymbol;
	}

	/**
	 * @param currencySymbol the Currency Symbol to set
	 */
	public final void setCurrencySymbol(String currencySymbol) {
		mCurrencySymbol = currencySymbol;
	}

	/**
	 * @return the Formatted Date
	 */
	public final String getFormattedDate() {
		return mFormattedDate;
	}

	/**
	 * @param formattedDate the Formatted Date to set
	 */
	public final void setFormattedDate(String formattedDate) {
		mFormattedDate = formattedDate;
	}

	/**
	 * @return the Display Country
	 */
	public final String getDisplayCountry() {
		return mDisplayCountry;
	}

	/**
	 * @param displayCountry the Display Country to set
	 */
	public final void setDisplayCountry(String displayCountry) {
		mDisplayCountry = displayCountry;
	}

	/**
	 * @return the Display Language
	 */
	public final String getDisplayLanguage() {
		return mDisplayLanguage;
	}

	/**
	 * @param displayLanguage the Display Language to set
	 */
	public final void setDisplayLanguage(String displayLanguage) {
		mDisplayLanguage = displayLanguage;
	}

	/**
	 * @return the Formatted Decimal Number
	 */
	public final String getFormattedDecimalNumber() {
		return mFormattedDecimalNumber;
	}

	/**
	 * @param formattedDecimalNumber the Formatted Decimal Number to set
	 */
	public final void setFormattedDecimalNumber(String formattedDecimalNumber) {
		mFormattedDecimalNumber = formattedDecimalNumber;
	}

	/**
	 * @return the Server Culture Info
	 */
	public final ServerCultureInfo getServerCultureInfo() {
		return mServerCultureInfo;
	}

	/**
	 * @param serverCultureInfo the Server Culture Info to set
	 */
	public final void setServerCultureInfo(ServerCultureInfo serverCultureInfo) {
		mServerCultureInfo = serverCultureInfo;
	}

}
