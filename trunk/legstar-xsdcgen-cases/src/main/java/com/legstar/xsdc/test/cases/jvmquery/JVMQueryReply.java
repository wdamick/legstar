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
