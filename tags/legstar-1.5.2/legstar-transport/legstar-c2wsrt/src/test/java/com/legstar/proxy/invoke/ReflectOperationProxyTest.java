/*******************************************************************************
 * Copyright (c) 2010 LegSem.
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
import com.legstar.test.coxb.CultureinfoCases;

/**
 * Test ReflectOperationProxy.
 * 
 */
public class ReflectOperationProxyTest extends AbstractWebServiceTest {

    /**
     * Check instantiation.
     */
    public void testInstantiation() {
        try {
            Map < String, String > config = new HashMap < String, String >();
            new ReflectOperationProxy(config);
        } catch (ProxyConfigurationException e) {
            assertEquals(
                    "com.legstar.proxy.invoke.ProxyInvokerException:"
                            + " com.legstar.proxy.invoke.jaxws.WebServiceInvokerException:"
                            + " You must specify a wsdl URL using the wsdlUrl attribute",
                    e.getMessage());
        }
        try {

            ReflectOperationProxy operationProxy = new ReflectOperationProxy(
                    CultureinfoJaxwsCases.getReflectConfig());
            assertEquals("com.legstar.coxb.impl.reflect.ReflectTransformers",
                    operationProxy.getRequestTransformers().getClass()
                            .getName());
            assertEquals("com.legstar.coxb.impl.reflect.ReflectTransformers",
                    operationProxy.getResponseTransformers().getClass()
                            .getName());
        } catch (ProxyConfigurationException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test invoke.
     * 
     * @throws Exception if something abnormal
     */
    public void testInvoke() throws Exception {

        ReflectOperationProxy operationProxy = new ReflectOperationProxy(
                CultureinfoJaxwsCases.getDirectConfig());
        byte[] replyBytes = operationProxy.invoke(CultureinfoJaxwsCases
                .getDirectConfig(), getName(), HostData
                .toByteArray(CultureinfoCases.getHostBytesHexRequestFr()));
        CultureinfoCases.checkHostBytesReplyFr(replyBytes);
    }

}
