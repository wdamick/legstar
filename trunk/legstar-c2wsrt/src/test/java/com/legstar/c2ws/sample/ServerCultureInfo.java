package com.legstar.c2ws.sample;

public class ServerCultureInfo {
	private String mCultureCode;
	private String mDisplayLanguage;
	private String mDisplayCountry;

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

}
