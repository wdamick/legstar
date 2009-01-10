package com.legstar.proxy.invoke;

import java.util.HashMap;
import java.util.Map;

import com.legstar.proxy.invoke.jaxws.CultureinfoJaxwsCases;

import junit.framework.TestCase;

/**
 * Test ProxyInvokerFactory.
 *
 */
public class ProxyInvokerFactoryTest extends TestCase {
    
    /**
     * Test that proxy invoker can be created and if not, we get a friendly error.
     */
    public void testCreateProxyInvoker() {
        try {
            /* A default proxy invoker should be picked up but then it does not
             * have enough parameters...*/
            ProxyInvokerFactory.createProxyInvoker(new HashMap < String, String >());
            fail();
        } catch (ProxyInvokerException e) {
            assertEquals("com.legstar.proxy.invoke.jaxws.WebServiceInvokerException:"
                    + " You must specify a wsdl URL using the wsdlUrl attribute",
                    e.getMessage());
        }
        try {
            Map < String, String > config = new HashMap < String, String >();
            config.put("proxyInvokerClassName", "does.not.Exist");
            ProxyInvokerFactory.createProxyInvoker(config);
            fail();
        } catch (ProxyInvokerException e) {
            assertEquals("java.lang.ClassNotFoundException: does.not.Exist",
                    e.getMessage());
        }
        try {
            Map < String, String > config = new HashMap < String, String >();
            config.put("proxyInvokerClassName", "com.legstar.proxy.invoke.jaxws.WebServiceInvoker");
            ProxyInvokerFactory.createProxyInvoker(config);
            fail();
        } catch (ProxyInvokerException e) {
            assertEquals("com.legstar.proxy.invoke.jaxws.WebServiceInvokerException:"
                    + " You must specify a wsdl URL using the wsdlUrl attribute",
                    e.getMessage());
        }
        try {
            Map < String, String > config = CultureinfoJaxwsCases.getDirectConfig();
            IProxyInvoker proxyInvoker = ProxyInvokerFactory.createProxyInvoker(config);
            assertEquals("http://localhost:8080/jaxws-cultureinfo/getinfo?wsdl",
                    proxyInvoker.getConfig().get("wsdlUrl"));
        } catch (ProxyInvokerException e) {
            fail(e.getMessage());
        }
        
    }

}
