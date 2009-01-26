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

import java.util.HashMap;
import java.util.Map;

import com.legstar.coxb.host.HostData;
import com.legstar.proxy.invoke.jaxws.CultureinfoJaxwsCases;
import com.legstar.proxy.invoke.jaxws.MSNSearchJaxwsCases;
import com.legstar.test.coxb.MSNSearchCases;

import junit.framework.TestCase;

/**
 * Test ReflectOperationProxy.
 *
 */
public class ReflectOperationProxyTest extends TestCase {
    
    /**
     * Check instantiation.
     */
    public void testInstantiation() {
        try {
            Map < String, String > config = new HashMap < String, String >();
            new ReflectOperationProxy(config);
        } catch (ProxyConfigurationException e) {
            assertEquals("Missing configuration parameter requestJaxbPackageName",
                    e.getMessage());
        }
        try {
            
            ReflectOperationProxy operationProxy =
                new ReflectOperationProxy(CultureinfoJaxwsCases.getReflectConfig());
            assertEquals("com.legstar.coxb.impl.reflect.ReflectTransformers",
                    operationProxy.getRequestTransformers().getClass().getName());
            assertEquals("com.legstar.coxb.impl.reflect.ReflectTransformers",
                    operationProxy.getResponseTransformers().getClass().getName());
        } catch (ProxyConfigurationException e) {
            fail(e.getMessage());
        }
    }
    
    /**
     * Test invoke.
     */
    public void testInvoke() {
        try {
            ReflectOperationProxy operationProxy =
                new ReflectOperationProxy(MSNSearchJaxwsCases.getDirectConfig());
            byte[] replyBytes = operationProxy.invoke(
                    MSNSearchJaxwsCases.getDirectConfig(), getName(),
                    HostData.toByteArray(MSNSearchCases.getHostBytesHexRequest()));
            MSNSearchJaxwsCases.checkHostBytesResponse(replyBytes);
        } catch (ProxyConfigurationException e) {
            fail(e.getMessage());
        } catch (ProxyInvokerException e) {
            fail(e.getMessage());
        }
    }

}
