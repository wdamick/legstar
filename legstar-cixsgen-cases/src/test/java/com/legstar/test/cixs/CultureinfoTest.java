package com.legstar.test.cixs;

import com.legstar.coxb.host.HostData;
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
