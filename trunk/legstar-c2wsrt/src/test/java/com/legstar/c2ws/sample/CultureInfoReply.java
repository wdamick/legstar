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
