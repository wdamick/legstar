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

import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Currency;
import java.util.Date;
import java.util.List;
import java.util.Locale;

/**
 * A simple POJO with a single method. It takes a list of environment variable 
 * names as input and returns their value along with other JVM parameters.
 */
public class JVMQuery {
    
    public JVMQueryReply execute(JVMQueryRequest request) throws JVMQueryException {
        
        JVMQueryReply reply = new JVMQueryReply();
        
        List <String> envVarValues = new ArrayList <String>();
        try {
			for (String envVarName : request.getEnvVarNames()) {
			    envVarValues.add(System.getenv(envVarName));
			}
		} catch (RuntimeException e) {
			throw new JVMQueryException(e);
		}
        reply.setEnvVarValues(envVarValues);
        Locale locale = Locale.getDefault();
        reply.setCountry(locale.getDisplayCountry());
        reply.setLanguage(locale.getDisplayLanguage());
        reply.setFormattedDate(
                DateFormat.getDateTimeInstance(DateFormat.FULL, DateFormat.FULL,
                        locale).format(new Date()));
        reply.setCurrencySymbol(Currency.getInstance(locale).getSymbol());
        return reply;
    }

}
