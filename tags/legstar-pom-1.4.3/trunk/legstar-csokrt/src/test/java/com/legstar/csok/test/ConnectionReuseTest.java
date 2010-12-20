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
package com.legstar.csok.test;

import java.util.HashMap;

import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.LegStarRequest;
import com.legstar.test.coxb.LsfileaeCases;
import com.legstar.config.Constants;
import com.legstar.coxb.host.HostData;

/**
 * Test the reusability of a connection.
 *
 */
public class ConnectionReuseTest extends AbstractSocketConnectionTester {

    /** {@inheritDoc} */
    public void setUp() throws Exception {
        super.setUp("CICSTS23");
    }

    /**
     * Wait for the connection to timeout then send a request. This should
     * re-establish a session then issue a second request immediately which 
     * should reuse the same connection.
     * @throws Exception if test fails
     */
    public void testReuse()throws Exception {
        /* Wait for the server to timeout (2 secs) */
        Thread.sleep(2500L);

        /* At this stage, the server died. Attempt reuse */
        getConnection().connectReuse(HOST_PASSWORD);

        /* Check if connection is usable */
        HashMap < String, Object > map = new HashMap < String, Object >();
        map.put(Constants.CICS_PROGRAM_NAME_KEY, "LSFILEAE");
        map.put(Constants.CICS_LENGTH_KEY, "79");
        map.put(Constants.CICS_DATALEN_KEY, "6");
        
        LegStarRequest request = getRequest(map, getAddress());
        request.getRequestMessage().addDataPart(new CommareaPart(
                HostData.toByteArray(LsfileaeCases.getHostBytesHexRequest100())));
        getConnection().sendRequest(request);
        getConnection().recvResponse(request);
        assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));

        /* This new attempt should find a Server alive */
        getConnection().connectReuse(HOST_USERID);

        getConnection().sendRequest(request);
        getConnection().recvResponse(request);
        assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));

    }

}
