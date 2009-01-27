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
import com.legstar.test.coxb.jvmquery.bind.JvmQueryReplyHostToJavaTransformer;
import com.legstar.xsdc.test.cases.JvmqueryCases;
import com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply;

import junit.framework.TestCase;

/**
 * Test the generated Jvmquery proxy.
 *
 */
public class JvmqueryTest extends TestCase {

    /** When deployed, the proxy will be there. */
    public static final String JVMQUERY_PROXY_URL =
        "http://${host}:8080/c2ws-Jvmquery/JvmqueryProxy";

    /**
     * Assuming the servlet has been deployed. Test remotely.
     */
    public void testProxyInvokeRemote() {
        try {
            ProxyClientHttp client = new ProxyClientHttp(JVMQUERY_PROXY_URL);
            String response = client.invoke(
                    JvmqueryCases.getHostBytesHexRequest(),
                    JvmqueryCases.getHostBytesHexReplyFrance().length() / 2);
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
            JvmQueryReplyHostToJavaTransformer transformer = new JvmQueryReplyHostToJavaTransformer();
            JVMQueryReply jvmQueryReply = transformer.transform(replyBytes);
            JvmqueryCases.checkJavaObjectReplyFrance(jvmQueryReply);
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }
}
