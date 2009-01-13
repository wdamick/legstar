package com.legstar.proxy.invoke;

import java.util.HashMap;
import java.util.Map;

import com.legstar.coxb.host.HostData;
import com.legstar.proxy.invoke.jaxws.CultureinfoJaxwsCases;
import com.legstar.proxy.invoke.jaxws.MSNSearchJaxwsCases;
import com.legstar.test.coxb.MSNSearchCases;

import junit.framework.TestCase;

/**
 * Test DirectOperationProxy.
 *
 */
public class DirectOperationProxyTest extends TestCase {


    /**
     * Check constructors.
     */
    public void testProxyInvokerInstantiate() {
        try {
            new DirectOperationProxy(new HashMap < String, String >());
            fail();
        } catch (ProxyConfigurationException e) {
            assertEquals("Missing configuration parameter requestTransformersClassName", e.getMessage());
        }
        try {
            Map < String, String > config = CultureinfoJaxwsCases.getDirectConfig();
            config.put(DirectOperationProxy.REQUEST_TRANSFORMERS_CLASS_NAME_PROPERTY,
                    "titi.toto.TATA");
            config.put(DirectOperationProxy.RESPONSE_TRANSFORMERS_CLASS_NAME_PROPERTY,
                    "titi.toto.TATA");
            new DirectOperationProxy(config);
            fail();
        } catch (ProxyConfigurationException e) {
            assertEquals("java.lang.ClassNotFoundException: titi.toto.TATA", e.getMessage());
        }
        try {
            DirectOperationProxy proxyInvoker =
                new DirectOperationProxy(CultureinfoJaxwsCases.getDirectConfig());
            assertTrue(null != proxyInvoker.getRequestTransformers());
            assertTrue(null != proxyInvoker.getResponseTransformers());
        } catch (ProxyConfigurationException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test actual invoke using the proxy classes.
     */
    public void testProxyInvokeDirectMSNSearch() {
        try {
            DirectOperationProxy invoker =
                new DirectOperationProxy(MSNSearchJaxwsCases.getDirectConfig());
            byte[] replyBytes = invoker.invoke(
                    MSNSearchJaxwsCases.getDirectConfig(), getName(),
                    HostData.toByteArray(MSNSearchCases.getHostBytesHexRequest()));
            MSNSearchJaxwsCases.checkHostBytesResponse(replyBytes);
        } catch (ProxyInvokerException e) {
            fail(e.getMessage());
        } catch (ProxyConfigurationException e) {
            fail(e.getMessage());
        }

    }

}
