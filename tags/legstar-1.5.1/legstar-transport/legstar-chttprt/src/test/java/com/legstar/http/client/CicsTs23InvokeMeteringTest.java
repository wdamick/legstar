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
package com.legstar.http.client;

import junit.framework.TestCase;

import com.legstar.coxb.host.HostData;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;
import com.legstar.test.connection.client.AbstractConnectionTester;
import com.legstar.test.coxb.LsfileaeCases;

/**
 * Load test in a configuration that is close to the way invoker works.
 * Invokers recreate a connection on each request without pooling.
 *
 */
public class CicsTs23InvokeMeteringTest extends TestCase {

    /** The endpoint (one per test).*/
    private CicsHttpEndpoint _endpoint;
    
    /** The endpoint name.*/
    private String _endpointName;
    
    /**
     * Construct.
     */
    public CicsTs23InvokeMeteringTest() {
        _endpointName = "CICSTS23";
        _endpoint = AbstractHttpConnectionTester.getCicsTs23Endpoint();
        _endpoint.setHostTraceMode(false);
    }

    /**
     * Perform a round trip.
     */
    public void testLsfileae() {
        try {
            CicsHttp connection = new CicsHttp(
                    _endpointName, _endpoint);
            connection.connect(AbstractConnectionTester.HOST_PASSWORD);
            LegStarRequest request = AbstractConnectionTester.getLsfileaeRequest100(
                    new LegStarAddress(_endpointName));
            connection.sendRequest(request);
            connection.recvResponse(request);
            connection.close();
            assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
            assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                    HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
        } catch (RequestException e) {
            e.printStackTrace();
            fail(e.toString());
        } catch (ConnectionException e) {
            e.printStackTrace();
            fail(e.toString());
        }
    }
}
