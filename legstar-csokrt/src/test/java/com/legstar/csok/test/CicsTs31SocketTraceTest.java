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
package com.legstar.csok.test;

import java.util.HashMap;

import com.legstar.config.Constants;
import com.legstar.coxb.host.HostData;
import com.legstar.csok.client.CicsSocket;
import com.legstar.csok.client.CicsSocketEndpoint;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;
import com.legstar.test.coxb.LsfileaeCases;

/** 
 * Test the Socket transport against CICS TS 3.1 focusing on traces effect.
 * There are reported issues with traces on TS 3.2.
 */
public class CicsTs31SocketTraceTest extends AbstractSocketConnectionTester {

    /**
     * Special setup using an endpoint configuration name.
     * @param endpointName endpoint name
     * @throws Exception if setup fails
     */
    public void setUp(final String endpointName) throws Exception {
    }

    /** {@inheritDoc} */
    public void tearDown() throws Exception {
    }

    /**
     * Test a with traces off.
     */
    public void testTracesOff() {
        invoke(true);
    }

    /**
     * Test a with traces off.
     */
    public void testTracesOn() {
        invoke(false);
    }
    /**

     * Service a request with or without traces.
     * @param tracesOn true if traces on
     */
    private void invoke(final boolean tracesOn) {
        try {
            CicsSocketEndpoint endpoint = getCicsTs31Endpoint();
            endpoint.setHostIPPort(4012);
            endpoint.setHostTraceMode(tracesOn);
            LegStarAddress address = new LegStarAddress(endpoint.getName());

            HashMap < String, Object > map = new HashMap < String, Object >();
            map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAE");
            map.put(Constants.CICS_LENGTH_KEY, "79");
            map.put(Constants.CICS_DATALEN_KEY, "6");
            
            LegStarRequest request = getRequest(map, address);
            request.getRequestMessage().addDataPart(new CommareaPart(
                    HostData.toByteArray(LsfileaeCases.getHostBytesHexRequest100())));
            CicsSocket connection = new CicsSocket(request.getID(), endpoint);
            connection.connect(HOST_PASSWORD);
            connection.sendRequest(request);
            connection.recvResponse(request);
            assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                    HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
            connection.commitUOW();
            connection.close();
        } catch (RequestException e) {
            e.printStackTrace();
            fail("testTraces failed=" + e);
        } catch (ConnectionException e) {
            fail("testTraces failed=" + e);
       }
    }
}
