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
package com.legstar.xsdc.test.cases.jvmquery;

import java.util.ArrayList;
import java.util.List;

/**
 * POJO replies by providing values for requested environment variables
 * as well as other JVM parameters.
 */
public class JVMQueryReply {
    private List <String> mEnvVarValues = new ArrayList <String>();
    private String mFormattedDate;
    private String mCountry;
    private String mLanguage;
    private String mCurrencySymbol;

    /**
     * @return the environment variable values to get
     */
    public final List <String> getEnvVarValues() {
        return mEnvVarValues;
    }

    /**
     * @param envVarValues the the environment variable values to set
     */
    public final void setEnvVarValues(List<String> envVarValues) {
        mEnvVarValues = envVarValues;
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
     * @return the country
     */
    public final String getCountry() {
        return mCountry;
    }

    /**
     * @param country the country to set
     */
    public final void setCountry(String country) {
        mCountry = country;
    }

    /**
     * @return the language
     */
    public final String getLanguage() {
        return mLanguage;
    }

    /**
     * @param mlanguage the language to set
     */
    public final void setLanguage(String mlanguage) {
        this.mLanguage = mlanguage;
    }

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

}
