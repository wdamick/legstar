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
import java.util.Locale;

import junit.framework.TestCase;

/**
 * Test JVMQuery POJO.
 *
 */
public class JVMQueryTest extends TestCase {

    /**
     * Test with a japanese locale.
     * @throws Exception if test fails
     */
    public void testQuery() throws Exception {
        Locale.setDefault(new Locale("jp", "JP"));
        List < String > envVarNames = new ArrayList < String >();
        JVMQueryRequest request = new JVMQueryRequest();
        request.setEnvVarNames(envVarNames);
        JVMQuery query = new JVMQuery();
        JVMQueryReply reply = query.queryJvm(request);
        assertEquals("Japan", reply.getCountry());
        assertEquals("jp", reply.getLanguage());
        assertEquals("JPY", reply.getCurrencySymbol());
        assertTrue(reply.getFormattedDate().length() > 0);
        assertEquals(0, reply.getEnvVarValues().size());

        envVarNames.add("JAVA_HOME");
        reply = query.queryJvm(request);
        assertEquals(1, reply.getEnvVarValues().size());
        assertTrue(reply.getEnvVarValues().get(0).contains("jre")
                || reply.getEnvVarValues().get(0).contains("jdk"));
    }

}
