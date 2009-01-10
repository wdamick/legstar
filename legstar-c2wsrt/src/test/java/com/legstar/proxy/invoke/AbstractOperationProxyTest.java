package com.legstar.proxy.invoke;

import java.util.HashMap;
import java.util.Map;

import com.legstar.proxy.invoke.jaxws.CultureinfoJaxwsCases;
import com.legstar.proxy.invoke.jaxws.WebServiceInvoker;

import junit.framework.TestCase;

/**
 * Test AbstractOperationProxy.
 *
 */
public class AbstractOperationProxyTest extends TestCase {

    /**
     * Check the creation of the proxy invoker.
     */
    public void testGetProxyInvoker() {
        /* Without configuration parameters, a default host invoker is picked up
         * but then it fails to find its own configuration parameters. */
        try {
            MockOperationProxy invoker = new MockOperationProxy(
                    CultureinfoJaxwsCases.getReflectConfig());
            invoker.getProxyInvoker(new HashMap < String, String >());
        } catch (ProxyInvokerException e) {
            assertEquals("com.legstar.proxy.invoke.jaxws.WebServiceInvokerException:"
                    + " You must specify a wsdl URL using the wsdlUrl attribute",
                    e.getMessage());
        } catch (ProxyConfigurationException e) {
            fail(e.getMessage());
        }
        /* Make sure proxy invoker is reused */
        try {
            MockOperationProxy invoker =
                new MockOperationProxy(CultureinfoJaxwsCases.getReflectConfig());
            IProxyInvoker proxyInvoker = invoker.getProxyInvoker(CultureinfoJaxwsCases.getReflectConfig());
            int id = proxyInvoker.hashCode();
            IProxyInvoker proxyInvoker2 = invoker.getProxyInvoker(CultureinfoJaxwsCases.getReflectConfig());
            assertEquals(id, proxyInvoker2.hashCode());

        } catch (ProxyInvokerException e) {
            fail(e.getMessage());
        } catch (ProxyConfigurationException e) {
            fail(e.getMessage());
        }
        /* Make sure proxy invoker is renewed if configuration changes */
        try {
            MockOperationProxy invoker =
                new MockOperationProxy(CultureinfoJaxwsCases.getReflectConfig());
            Map < String, String > config = CultureinfoJaxwsCases.getReflectConfig();
            IProxyInvoker proxyInvoker = invoker.getProxyInvoker(config);
            int id = proxyInvoker.hashCode();
            config.put(WebServiceInvoker.WSDL_URL_PROPERTY,
            "http://localhost:8080/different-cultureinfo/getinfo?wsdl");
            IProxyInvoker proxyInvoker2 = invoker.getProxyInvoker(config);
            assertFalse(id == proxyInvoker2.hashCode());

        } catch (ProxyInvokerException e) {
            fail(e.getMessage());
        } catch (ProxyConfigurationException e) {
            fail(e.getMessage());
        }

    }

}
