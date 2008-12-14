/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.xsdc.test.cases.cultureinfo;

/**
 * Another data object. Child of another.
 *
 */
public class ServerCultureInfo {
    
    /** The Locale culture code. */
    private String mCultureCode;
    /** The Locale display language. */
    private String mDisplayLanguage;
    /** The Locale display country. */
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
    public final void setCultureCode(final String culturecode) {
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
    public final void setDisplayCountry(final String displayCountry) {
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
    public final void setDisplayLanguage(final String displayLanguage) {
        mDisplayLanguage = displayLanguage;
    }

}
