package com.legstar.proxy.invoke;

import java.util.HashMap;
import java.util.Map;

import com.legstar.coxb.host.HostData;
import com.legstar.proxy.invoke.jaxws.CultureinfoJaxwsCases;
import com.legstar.test.client.ProxyClientException;
import com.legstar.test.client.ProxyClientHttp;
import com.legstar.test.coxb.CultureinfoCases;

import junit.framework.TestCase;

/**
 * Test DirectOperationProxy.
 *
 */
public class DirectOperationProxyTest extends TestCase {

    /** When deployed, the proxy will be there. */
    public static final String CULTUREINFO_PROXY_URL =
        "http://${host}:8080/c2ws-cultureinfo/cultureinfoProxy";

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
    public void testProxyInvokeDirect() {
        try {
            DirectOperationProxy invoker =
                new DirectOperationProxy(CultureinfoJaxwsCases.getDirectConfig());
            byte[] replyBytes = invoker.invoke(CultureinfoJaxwsCases.getDirectConfig(), getName(),
                    HostData.toByteArray(CultureinfoCases.getHostBytesHexRequestFr()));
            CultureinfoCases.checkHostBytesReplyFr(replyBytes);
        } catch (ProxyInvokerException e) {
            fail(e.getMessage());
        } catch (ProxyConfigurationException e) {
            fail(e.getMessage());
        }

    }

    /**
     * Assuming the servlet has been deployed. Test remotely.
     */
    public void testProxyInvokeRemote() {
        try {
            ProxyClientHttp client = new ProxyClientHttp(CULTUREINFO_PROXY_URL);
            String response = client.invoke(CultureinfoCases.getHostBytesHexRequestFr(),
                    CultureinfoCases.getHostBytesHexReplyFr().length() / 2);
            CultureinfoCases.checkHostBytesReplyFr(HostData.toByteArray(response));
        } catch (ProxyClientException e) {
            fail(e.getMessage());
        }

    }
    
}
