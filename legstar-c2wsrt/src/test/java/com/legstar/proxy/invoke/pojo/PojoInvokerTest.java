/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.proxy.invoke.pojo;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import com.legstar.proxy.invoke.ProxyInvokerException;
import com.legstar.xsdc.test.cases.JvmqueryCases;
import com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply;

import junit.framework.TestCase;

/**
 * Test POJO Invoker.
 *
 */
public class PojoInvokerTest extends TestCase {
    
    /**
     * Test configuration and constructor.
     */
    public void testConfig() {
       Map < String, String > config = new HashMap < String, String >();
       try {
            new PojoInvoker(config);
        } catch (PojoInvokerException e) {
            assertEquals("You must specify a POJO class name using the pojoClassName attribute",
                    e.getMessage());
        }
        config.put("pojoClassName", "tt.TT");
        try {
            new PojoInvoker(config);
        } catch (PojoInvokerException e) {
            assertEquals("You must specify a POJO method name using the pojoMethodName attribute",
                    e.getMessage());
        }
        config.put("pojoMethodName", "go");
        try {
            new PojoInvoker(config);
        } catch (PojoInvokerException e) {
            assertEquals("java.lang.ClassNotFoundException: tt.TT",
                    e.getMessage());
        }
        config.put("pojoClassName", "com.legstar.xsdc.test.cases.jvmquery.JVMQuery");
        try {
            new PojoInvoker(config);
        } catch (PojoInvokerException e) {
            assertEquals("Class com.legstar.xsdc.test.cases.jvmquery.JVMQuery does not implement method go",
                    e.getMessage());
        }
        config.put("pojoMethodName", "queryJvm");
        try {
            PojoInvoker pojoInvoker = new PojoInvoker(config);
            assertEquals("com.legstar.xsdc.test.cases.jvmquery.JVMQuery", pojoInvoker.getPojoClass().getName());
            assertEquals("queryJvm", pojoInvoker.getPojoMethod().getName());
            
        } catch (PojoInvokerException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test configuration and constructor.
     */
    public void testInvoke() {
        Map < String, String > config = JVMQueryPojoCases.getReflectConfig();
        try {
            PojoInvoker pojoInvoker = new PojoInvoker(config);
            pojoInvoker.invoke(null, "string");
        } catch (ProxyInvokerException e) {
            assertEquals("java.lang.IllegalArgumentException: argument type mismatch",
                    e.getMessage());
        }
        try {
            Locale.setDefault(Locale.FRANCE);
            PojoInvoker pojoInvoker = new PojoInvoker(config);
            Object reply = pojoInvoker.invoke(null, JvmqueryCases.getJavaObjectRequest());
            assertTrue(reply instanceof JVMQueryReply);
            JvmqueryCases.checkJavaObjectReplyFrance((JVMQueryReply) reply);
        } catch (ProxyInvokerException e) {
            fail(e.getMessage());
        }
        
    }
}
