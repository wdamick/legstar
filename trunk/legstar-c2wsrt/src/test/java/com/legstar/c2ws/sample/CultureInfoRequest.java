package com.legstar.c2ws.sample;

import java.math.BigDecimal;

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
