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
package com.legstar.http.client;

import com.legstar.coxb.host.HostData;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;
import com.legstar.test.coxb.LsfileaeCases;


/**
 * Test behavior of multiple requests over same connection.
 *
 */
public class CicsHttpPersistentConnectionTS31Test extends AbstractHttpConnectionTester {

    /** {@inheritDoc} */
    protected void setUp() throws Exception {
        super.setUp("CICSTS31");
    }

    /**
     * With CICS TS 3.1, there is support for HTTP 1.1. HTTPClient will keep
     * the session alive. The log should show 1 physical TCPIP open only.
     */
    public void testSend2RequestsTs31() {
        try {
            LegStarRequest request = getLsfileaeRequest100(getAddress());
            getConnection().sendRequest(request);
            getConnection().recvResponse(request);
            assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
            assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                    HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
            getConnection().sendRequest(request);
            getConnection().recvResponse(request);
            assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
            assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                    HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
        } catch (RequestException e) {
            fail("testSend2Requests failed " + e);
        }
    }

}
