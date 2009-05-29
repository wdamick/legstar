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
package com.legstar.test.cixs;

import com.legstar.coxb.host.HostData;
import com.legstar.coxb.transform.HostTransformException;
import com.legstar.test.client.ProxyClientException;
import com.legstar.test.client.ProxyClientHttp;
import com.legstar.test.coxb.MSNSearchCases;
import com.legstar.test.coxb.MSNSearch.SearchResponse;
import com.legstar.test.coxb.MSNSearch.bind.SearchResponseHostToJavaTransformer;

import junit.framework.TestCase;

/**
 * Test the generated MSNSearch proxy.
 *
 */
public class MSNSearchTest extends TestCase {

    /** When deployed, the proxy will be there. */
    public static final String MSNSEARCH_PROXY_URL =
        "http://${host}:8080/c2ws-MSNSearch/MSNSearchProxy";

    /**
     * Assuming the servlet has been deployed. Test remotely.
     */
    public void testProxyInvokeRemote() {
        try {
            ProxyClientHttp client = new ProxyClientHttp(MSNSEARCH_PROXY_URL);
            String response = client.invoke(
                    MSNSearchCases.getHostBytesHexRequest(),
                    MSNSearchCases.getHostBytesHexResponse().length() / 2);
            checkHostBytesResponse(HostData.toByteArray(response));
        } catch (ProxyClientException e) {
            fail(e.getMessage());
        }

    }
    
    /**
     * Check the response.
     * @param replyBytes the host bytes returned
     */
    private void checkHostBytesResponse(final byte[] replyBytes) {
        try {
            SearchResponseHostToJavaTransformer transformer = new SearchResponseHostToJavaTransformer();
            SearchResponse searchResponse = transformer.transform(replyBytes);
            MSNSearchCases.checkJavaObjectResponse(searchResponse);
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }
}
