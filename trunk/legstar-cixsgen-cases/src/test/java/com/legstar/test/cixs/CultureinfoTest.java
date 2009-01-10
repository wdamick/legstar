package com.legstar.test.cixs;

import com.legstar.coxb.host.HostData;
import com.legstar.proxy.invoke.DirectOperationProxy;
import com.legstar.proxy.invoke.ProxyConfigurationException;
import com.legstar.proxy.invoke.ProxyInvokerException;
import com.legstar.proxy.invoke.jaxws.CultureinfoJaxwsCases;
import com.legstar.test.client.ProxyClientException;
import com.legstar.test.client.ProxyClientHttp;
import com.legstar.test.coxb.CultureinfoCases;

import junit.framework.TestCase;

/**
 * Test the generated cultureinfo proxy.
 *
 */
public class CultureinfoTest extends TestCase {

    /** When deployed, the proxy will be there. */
    public static final String CULTUREINFO_PROXY_URL =
        "http://${host}:8080/c2ws-cultureinfo/cultureinfoProxy";

    /**
     * Test actual invoke using the proxy classes.
     */
    public void testProxyInvokeDirect() {
        try {
            DirectOperationProxy invoker =
                new DirectOperationProxy(CultureinfoJaxwsCases.getDirectConfig());
            byte[] replyBytes = invoker.invoke(
                    CultureinfoJaxwsCases.getDirectConfig(), getName(),
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
            String response = client.invoke(
                    CultureinfoCases.getHostBytesHexRequestFr(),
                    CultureinfoCases.getHostBytesHexReplyFr().length() / 2);
            CultureinfoCases.checkHostBytesReplyFr(HostData.toByteArray(response));
        } catch (ProxyClientException e) {
            fail(e.getMessage());
        }

    }
}
