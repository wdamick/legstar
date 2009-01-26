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
package com.legstar.proxy.invoke;

import java.util.Map;

import com.legstar.proxy.invoke.jaxws.MSNSearchJaxwsCases;

import junit.framework.TestCase;

/**
 * Test the AbstractProxyInvoker class.
 *
 */
public class AbstractProxyInvokerTest extends TestCase {

    /**
     * Check the code that compares current config to another one.
     */
    public void testSameConfig() {
        MockProxyInvoker invoker = new MockProxyInvoker(MSNSearchJaxwsCases.getReflectConfig());
        assertTrue(invoker.isSameConfig(MSNSearchJaxwsCases.getReflectConfig()));
        Map < String, String > newConfig = MSNSearchJaxwsCases.getReflectConfig();
        newConfig.put("oneMoreKey", "value");
        assertFalse(invoker.isSameConfig(newConfig));
        newConfig = MSNSearchJaxwsCases.getReflectConfig();
        newConfig.remove(IProxyInvoker.PROXY_INVOKER_CLASS_NAME_PROPERTY);
        assertFalse(invoker.isSameConfig(newConfig));
        newConfig.put(IProxyInvoker.PROXY_INVOKER_CLASS_NAME_PROPERTY, "different");
        assertFalse(invoker.isSameConfig(newConfig));
            
    }
}
