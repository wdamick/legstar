package com.legstar.proxy.invoke;

import java.util.HashMap;
import java.util.Map;

import com.legstar.coxb.host.HostData;
import com.legstar.messaging.HostMessageFormatException;
import com.legstar.messaging.LegStarMessage;
import com.legstar.proxy.invoke.jaxws.CultureinfoJaxwsCases;
import com.legstar.test.coxb.CultureinfoCases;

import junit.framework.TestCase;

/**
 * Test ServiceProxy.
 *
 */
public class ServiceProxyTest extends TestCase {
    
    /**
     * Test that members are created properly at construction times.
     */
    public void testInstantiation() {
        Map < String, String > config = new HashMap < String, String >();
        try {
            new ServiceProxy(config);
            fail();
        } catch (ProxyConfigurationException e) {
            assertEquals("Missing configuration parameter requestJaxbPackageName",
                    e.getMessage());
        }
        try {
            config = CultureinfoJaxwsCases.getReflectConfig();
            ServiceProxy serviceProxy = new ServiceProxy(config);
            assertEquals("com.legstar.proxy.invoke.ReflectOperationProxy",
                    serviceProxy.getOperationProxy().getClass().getName());
        } catch (ProxyConfigurationException e) {
            fail(e.getMessage());
        }
        config.put("operationProxyClassName", "ttt.tt.TT");
        try {
            new ServiceProxy(config);
        } catch (ProxyConfigurationException e) {
            assertEquals("java.lang.ClassNotFoundException: ttt.tt.TT",
                    e.getMessage());
        }
        config.put("operationProxyClassName", "com.legstar.proxy.invoke.MockOperationProxy");
        try {
            ServiceProxy serviceProxy = new ServiceProxy(config);
            assertTrue(serviceProxy.getOperationProxy() != null);
        } catch (ProxyConfigurationException e) {
            fail(e.getMessage());
        }
    }
    

    /**
     * Test invoking the operation proxy with raw (no LegStarMessage) data.
     */
    public void testInvokeRawData() {
        Map < String, String > config = new HashMap < String, String >();
        config.put("operationProxyClassName", "com.legstar.proxy.invoke.MockOperationProxy");
        try {
            ServiceProxy serviceProxy = new ServiceProxy(config);
            byte[] replyBytes = serviceProxy.invoke(config, getName(),
                    HostData.toByteArray(CultureinfoCases.getHostBytesHexRequestFr()));
            assertEquals(CultureinfoCases.getHostBytesHexRequestFr(),
                    HostData.toHexString(replyBytes));
        } catch (ProxyConfigurationException e) {
            fail(e.getMessage());
        } catch (ProxyInvokerException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test invoking the operation proxy with LegStar messaging.
     */
    public void testInvokeLegStarMessage() {
        Map < String, String > config = new HashMap < String, String >();
        config.put("operationProxyClassName", "com.legstar.proxy.invoke.MockOperationProxy");
        try {
            ServiceProxy serviceProxy = new ServiceProxy(config);
            byte[] requestBytes = LegStarMessage.getHostBytesFromContent(
                    HostData.toByteArray(CultureinfoCases.getHostBytesHexRequestFr()));
            byte[] replyBytes = serviceProxy.invoke(config, getName(), requestBytes);
            byte[] replyContent = LegStarMessage.getContentFromHostBytes(replyBytes);
            assertEquals(CultureinfoCases.getHostBytesHexRequestFr(),
                    HostData.toHexString(replyContent));
        } catch (ProxyConfigurationException e) {
            fail(e.getMessage());
        } catch (ProxyInvokerException e) {
            fail(e.getMessage());
        } catch (HostMessageFormatException e) {
            fail(e.getMessage());
        }
    }
}
