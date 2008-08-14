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
    
    public JVMQueryReply queryJvm(JVMQueryRequest request) throws JVMQueryException {
        
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
